use std::{
    any::TypeId,
    hash::{Hash, Hasher},
    path::PathBuf,
};

use bevy::{
    app::DynEq,
    asset::{load_internal_asset, LoadState},
    ecs::{entity::EntityHashMap, query::ROQueryItem, system::lifetimeless::SRes},
    prelude::*,
    render::{
        render_asset::RenderAssets,
        render_phase::{AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult},
        render_resource::{
            BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingType,
            CachedRenderPipelineId, IntoBinding, PipelineCache, SamplerBindingType, ShaderStage,
            ShaderStages, TextureSampleType, TextureViewDimension,
        },
        renderer::RenderDevice,
        Extract, Render, RenderApp, RenderSet,
    },
    utils::{intern::Interned, HashMap},
    window::PrimaryWindow,
};
use bevy_inspector_egui::{
    bevy_egui::{EguiContext, EguiPlugin},
    DefaultInspectorConfigPlugin,
};
use bevy_layout_ui::{
    loader::Layout,
    math::NodeSize,
    render::{
        BindLayoutUniform, BindVertexBuffer, DrawUiPhaseItem, InvalidNodePipeline,
        NodeDrawFunction, SkipNodeRender, UiNodeItem, UiRenderPlugin,
    },
    EditorUiNode, NodeLabel, UiLayoutPlugin, UiNodeApp, UserUiNode,
};
use egui::Id;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub struct ImageNodeData {
    pub path: PathBuf,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ImageNodeLabel;

impl NodeLabel for ImageNodeLabel {
    fn dyn_clone(&self) -> Box<dyn NodeLabel> {
        Box::new(self.clone())
    }

    fn as_dyn_eq(&self) -> &dyn DynEq {
        self
    }

    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        let ty_id = TypeId::of::<Self>();
        Hash::hash(&ty_id, &mut state);
        Hash::hash(self, &mut state);
    }
}

#[derive(Component, Clone, Default)]
struct ImageNode(Handle<Image>);

impl EditorUiNode for ImageNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        let id = Id::new("image-node-editor").with(entity.id());
        let mut current_path = ui.data_mut(|data| {
            data.get_temp_mut_or_insert_with(id, || {
                let handle = entity.get::<ImageNode>().unwrap().0.id();
                entity
                    .world()
                    .resource::<AssetServer>()
                    .get_path(handle)
                    .map(|path| path.path().display().to_string())
                    .unwrap_or_default()
            })
            .clone()
        });

        ui.horizontal(|ui| {
            ui.label("Image Path");
            ui.text_edit_singleline(&mut current_path);
        });

        let handle = entity.world().resource::<AssetServer>().load(&current_path);
        ui.data_mut(|data| {
            data.insert_temp(id, current_path);
        });

        entity.get_mut::<ImageNode>().unwrap().0 = handle;
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<ImageNode>();
    }
}

impl UserUiNode for ImageNode {
    const NAME: &'static str = "Image";
    type Serde = ImageNodeData;

    fn label() -> Interned<dyn NodeLabel> {
        ImageNodeLabel.intern()
    }

    fn deserialize<E: serde::de::Error>(
        serde: Self::Serde,
        load_context: &mut bevy::asset::LoadContext,
    ) -> Result<Self, E> {
        let handle = load_context.load(serde.path);
        Ok(Self(handle))
    }

    fn serialize<E: serde::ser::Error>(&self, world: &World) -> Result<Self::Serde, E> {
        let server = world.resource::<AssetServer>();
        let path = server.get_path(self.0.id()).ok_or_else(|| {
            E::custom("Failed to get the asset path of image, it should have been loaded from the filesystem")
        })?;

        Ok(Self::Serde {
            path: path.path().to_path_buf(),
        })
    }

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {
        visit_fn(self.0.id().untyped())
    }

    fn spawn(&self, entity: &mut EntityWorldMut) {
        entity.insert(self.clone());
    }

    fn reconstruct(entity: EntityRef) -> Self {
        entity.get::<Self>().unwrap().clone()
    }
}

#[derive(Resource, Deref, DerefMut, Default)]
struct ExtractedImageNodes(EntityHashMap<AssetId<Image>>);

#[derive(Resource)]
struct ImageNodePipeline {
    pipeline_id: CachedRenderPipelineId,
    image_bind_group: BindGroupLayout,
}

impl ImageNodePipeline {
    const SHADER: Handle<Shader> = Handle::weak_from_u128(0xCE849586828149DA85985CC0390F4CD9);
}

impl FromWorld for ImageNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let default_pipeline = world.resource::<InvalidNodePipeline>();
        let device = world.resource::<RenderDevice>();
        let pipeline_cache = world.resource::<PipelineCache>();
        let mut desc = default_pipeline.render_pipeline_descriptor();
        desc.vertex.shader = Self::SHADER.clone();
        desc.fragment.as_mut().unwrap().shader = Self::SHADER.clone();
        let image_bind_group = device.create_bind_group_layout(
            "ImageNodePipeline.image_bind_group",
            &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        );

        desc.layout.push(image_bind_group.clone());

        let pipeline_id = pipeline_cache.queue_render_pipeline(desc);
        Self {
            pipeline_id,
            image_bind_group,
        }
    }
}

fn extract_image_nodes(
    nodes: Extract<Query<(Entity, &ImageNode), Without<SkipNodeRender>>>,
    mut extracted: ResMut<ExtractedImageNodes>,
) {
    extracted.clear();
    for (entity, node) in nodes.iter() {
        extracted.insert(entity, node.0.id());
    }
}

#[derive(Resource, Default, Deref, DerefMut)]
struct PreparedImages(HashMap<AssetId<Image>, BindGroup>);

fn prepare_image_nodes(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    extracted_images: Res<ExtractedImageNodes>,
    mut prepared_images: ResMut<PreparedImages>,
    pipeline: Res<ImageNodePipeline>,
    gpu_images: Res<RenderAssets<Image>>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut batch: Local<Vec<(Entity, (NodeDrawFunction, ImageId))>>,
) {
    let function = draw_functions
        .read()
        .get_id::<ImageNodeDrawFunction>()
        .unwrap();
    for (entity, extracted_image) in extracted_images.iter() {
        if !prepared_images.contains_key(extracted_image) {
            if let Some(image) = gpu_images.get(*extracted_image) {
                prepared_images.insert(
                    *extracted_image,
                    render_device.create_bind_group(
                        "image_bind_group",
                        &pipeline.image_bind_group,
                        &[
                            BindGroupEntry {
                                binding: 0,
                                resource: (&image.texture_view).into_binding(),
                            },
                            BindGroupEntry {
                                binding: 1,
                                resource: (&image.sampler).into_binding(),
                            },
                        ],
                    ),
                );
            }
        }

        batch.push((
            *entity,
            (NodeDrawFunction::new(function), ImageId(*extracted_image)),
        ));
    }

    let new_batch = Vec::with_capacity(batch.len());
    commands.insert_or_spawn_batch(std::mem::replace(&mut *batch, new_batch));
}

type ImageNodeDrawFunction = (
    BindImagePipeline,
    BindVertexBuffer<0>,
    BindLayoutUniform<0>,
    BindImageGroup<1>,
    DrawUiPhaseItem,
);

#[derive(Component)]
struct ImageId(AssetId<Image>);

struct BindImagePipeline;

impl RenderCommand<UiNodeItem> for BindImagePipeline {
    type Param = (SRes<ImageNodePipeline>, SRes<PipelineCache>);
    type ViewQuery = ();
    type ItemQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        (pipeline, cache): bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let pipeline = pipeline.into_inner();
        let cache = cache.into_inner();
        let Some(pipeline) = cache.get_render_pipeline(pipeline.pipeline_id) else {
            return RenderCommandResult::Failure;
        };

        pass.set_render_pipeline(pipeline);

        RenderCommandResult::Success
    }
}

struct BindImageGroup<const I: usize>;

impl<const I: usize> RenderCommand<UiNodeItem> for BindImageGroup<I> {
    type Param = SRes<PreparedImages>;
    type ItemQuery = &'static ImageId;
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> bevy::render::render_phase::RenderCommandResult {
        let param = param.into_inner();
        let Some(entity) = entity else {
            return RenderCommandResult::Failure;
        };

        let Some(prepared) = param.get(&entity.0) else {
            return RenderCommandResult::Failure;
        };

        pass.set_bind_group(I, prepared, &[]);

        RenderCommandResult::Success
    }
}

#[derive(Resource)]
struct WaitingLayout(Handle<Layout>);

impl FromWorld for WaitingLayout {
    fn from_world(world: &mut World) -> Self {
        Self(
            world
                .resource::<AssetServer>()
                .load("basic_node.layout.json"),
        )
    }
}

fn wait_spawn_layout(layout: Res<WaitingLayout>, server: Res<AssetServer>, mut commands: Commands) {
    if server.load_state(layout.0.id()) == LoadState::Loaded {
        commands.add(move |world: &mut World| {
            let handle = world.remove_resource::<WaitingLayout>().unwrap().0;
            world.resource_scope::<Assets<Layout>, _>(|world, assets| {
                let layout = assets.get(&handle).unwrap();
                bevy_layout_ui::loader::spawn_layout(world, layout);
            });
        });
    }
}

struct ImageNodePlugin;

impl Plugin for ImageNodePlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            ImageNodePipeline::SHADER,
            "shaders/image.wgsl",
            Shader::from_wgsl
        );

        app.register_user_ui_node::<ImageNode>()
            .register_editor_ui_node::<ImageNode>();
    }

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<ExtractedImageNodes>()
            .init_resource::<ImageNodePipeline>()
            .init_resource::<PreparedImages>()
            .add_render_command::<UiNodeItem, ImageNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_image_nodes)
            .add_systems(Render, prepare_image_nodes.in_set(RenderSet::Prepare));
    }
}

#[derive(Component, Copy, Clone, Debug, Default)]
struct BasicNode;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct BasicNodeLabel;

impl NodeLabel for BasicNodeLabel {
    fn dyn_clone(&self) -> Box<dyn NodeLabel> {
        Box::new(self.clone())
    }

    fn as_dyn_eq(&self) -> &dyn DynEq {
        self
    }

    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        let ty_id = TypeId::of::<Self>();
        Hash::hash(&ty_id, &mut state);
        Hash::hash(self, &mut state);
    }
}

impl UserUiNode for BasicNode {
    const NAME: &'static str = "Basic";
    type Serde = ();

    fn label() -> Interned<dyn bevy_layout_ui::NodeLabel> {
        BasicNodeLabel.intern()
    }

    fn deserialize<E: serde::de::Error>(
        _: Self::Serde,
        _: &mut bevy::asset::LoadContext,
    ) -> Result<Self, E> {
        Ok(Self)
    }

    fn serialize<E: serde::ser::Error>(&self, _: &World) -> Result<Self::Serde, E> {
        Ok(())
    }

    fn reconstruct(entity: EntityRef) -> Self {
        *entity.get_ref::<BasicNode>().unwrap()
    }

    fn spawn(&self, entity: &mut EntityWorldMut) {
        entity.insert(*self);
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {}
}

impl EditorUiNode for BasicNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {}

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<BasicNode>();
    }
}
#[derive(Resource, Default, Deref, DerefMut, Debug)]
pub struct ExtractedBasicNodes(Vec<Entity>);

fn extract_basic_nodes(
    nodes: Extract<Query<Entity, (With<BasicNode>, Without<SkipNodeRender>)>>,
    mut basic_nodes: ResMut<ExtractedBasicNodes>,
) {
    basic_nodes.clear();
    basic_nodes.extend(nodes.iter());
}

#[derive(Resource)]
struct BasicPipeline {
    cached_pipeline_id: CachedRenderPipelineId,
}

impl BasicPipeline {
    const SHADER: Handle<Shader> = Handle::weak_from_u128(0xCD054AA00730470D83527C8FC4288912);
}

impl FromWorld for BasicPipeline {
    fn from_world(world: &mut World) -> Self {
        let invalid_pipeline = world.resource::<InvalidNodePipeline>();
        let pipeline_cache = world.resource::<PipelineCache>();
        let mut descriptor = invalid_pipeline.render_pipeline_descriptor();
        descriptor.vertex.shader = Self::SHADER.clone();
        descriptor.fragment.as_mut().unwrap().shader = Self::SHADER.clone();

        Self {
            cached_pipeline_id: pipeline_cache.queue_render_pipeline(descriptor),
        }
    }
}

type BasicDrawFunction = (
    BindVertexBuffer<0>,
    BindLayoutUniform<0>,
    BindBasicPipeline,
    DrawUiPhaseItem,
);

struct BindBasicPipeline;

impl RenderCommand<UiNodeItem> for BindBasicPipeline {
    type Param = (SRes<BasicPipeline>, SRes<PipelineCache>);
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        (pipeline, cache): bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> bevy::render::render_phase::RenderCommandResult {
        let pipeline = pipeline.into_inner();
        let cache = cache.into_inner();

        let Some(pipeline) = cache.get_render_pipeline(pipeline.cached_pipeline_id) else {
            return RenderCommandResult::Failure;
        };

        pass.set_render_pipeline(pipeline);

        RenderCommandResult::Success
    }
}

fn prepare_basic_nodes(
    mut commands: Commands,
    nodes: Res<ExtractedBasicNodes>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut batch: Local<Vec<(Entity, NodeDrawFunction)>>,
) {
    let function = draw_functions.read().get_id::<BasicDrawFunction>().unwrap();

    for node in nodes.iter() {
        batch.push((*node, NodeDrawFunction::new(function)));
    }

    let new_batch = Vec::with_capacity(batch.len());
    commands.insert_or_spawn_batch(std::mem::replace(&mut *batch, new_batch));
}

fn ui_system(world: &mut World, mut roots: Local<Vec<Entity>>, mut open_nodes: Local<Vec<Entity>>) {
    let Ok(mut context) = world
        .query_filtered::<&EguiContext, With<PrimaryWindow>>()
        .get_single(world)
        .cloned()
    else {
        return;
    };

    roots.clear();
    roots.extend(
        world
            .query_filtered::<Entity, (With<NodeSize>, Without<Parent>)>()
            .iter(world),
    );

    for root in roots.iter().copied() {
        egui::Window::new(format!("{root:?}")).show(context.get_mut(), |ui| {
            if ui.button("Marshall").clicked() {
                let tree = bevy_layout_ui::loader::marshall_node_tree(world, root);

                let json = bevy_layout_ui::loader::serialize_layout_as_json(&tree, world).unwrap();
                std::fs::write(
                    "assets/basic_node.layout.json",
                    serde_json::to_string_pretty(&json).unwrap(),
                )
                .unwrap();
            }
            let Some(entity) = bevy_layout_ui::editor::display_node_tree(root, world, ui) else {
                return;
            };

            if !open_nodes.contains(&entity) {
                open_nodes.push(entity);
            }
        });
    }

    for node in open_nodes.iter().copied() {
        egui::Window::new(format!("{node:?} - Editor")).show(context.get_mut(), |ui| {
            bevy_layout_ui::editor::node_ui::display_ui_node_editor(node, world, ui);
        });
    }
}

struct BasicPlugin;

impl Plugin for BasicPlugin {
    fn build(&self, app: &mut App) {
        app.register_user_ui_node::<BasicNode>()
            .register_editor_ui_node::<BasicNode>();
    }

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<ExtractedBasicNodes>()
            .init_resource::<BasicPipeline>()
            .add_render_command::<UiNodeItem, BasicDrawFunction>()
            .add_systems(ExtractSchedule, extract_basic_nodes)
            .add_systems(Render, prepare_basic_nodes.in_set(RenderSet::Prepare));
    }
}

pub fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins);

    load_internal_asset!(
        app,
        BasicPipeline::SHADER,
        "shaders/basic.wgsl",
        Shader::from_wgsl
    );

    app.add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(EguiPlugin)
        .add_plugins(DefaultInspectorConfigPlugin)
        .add_plugins(BasicPlugin)
        .add_plugins(ImageNodePlugin)
        .add_systems(
            Update,
            (
                wait_spawn_layout.run_if(resource_exists::<WaitingLayout>),
                ui_system,
            ),
        )
        .init_resource::<WaitingLayout>();

    app.world.spawn(Camera2dBundle::default());

    app.run();
}
