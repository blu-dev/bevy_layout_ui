use std::path::PathBuf;

use bevy::asset::load_internal_asset;
use bevy::prelude::*;
use bevy::render::render_phase::AddRenderCommand;
use bevy::render::{Render, RenderApp, RenderSet};
use bevy::{
    ecs::{entity::EntityHashMap, system::lifetimeless::SRes},
    render::{
        render_asset::RenderAssets,
        render_phase::{DrawFunctions, RenderCommand, RenderCommandResult},
        render_resource::{
            BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingType,
            CachedRenderPipelineId, IntoBinding, PipelineCache, SamplerBindingType, ShaderStages,
            TextureSampleType, TextureViewDimension,
        },
        renderer::RenderDevice,
        Extract,
    },
    utils::{intern::Interned, HashMap},
};
use egui::Id;
use serde::{Deserialize, Serialize};

use crate::render::{
    BindLayoutUniform, BindVertexBuffer, DefaultNodePipeline, DrawUiPhaseItem, NodeDrawFunction,
    SkipNodeRender, UiNodeItem,
};
use crate::{EditorUiNode, NodeLabel, UiNodeApp, UserUiNode};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ImageNodeLabel;

decl_node_label!(ImageNodeLabel);

#[derive(Deserialize, Serialize)]
pub struct ImageNodeData {
    path: PathBuf,
}

#[derive(Debug, Clone, Component, Default)]
pub struct ImageNode {
    image: Handle<Image>,
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
        Ok(Self {
            image: load_context.load(serde.path.clone()),
        })
    }

    fn serialize<E: serde::ser::Error>(
        &self,
        world: &bevy::prelude::World,
    ) -> Result<Self::Serde, E> {
        let path = world
            .resource::<AssetServer>()
            .get_path(self.image.id())
            .ok_or_else(|| E::custom("Image handle is not represented by the filesystem"))?;

        Ok(ImageNodeData {
            path: path.path().to_path_buf(),
        })
    }

    fn reconstruct(entity: bevy::prelude::EntityRef) -> Self {
        entity
            .get::<ImageNode>()
            .cloned()
            .expect("ImageNode::reconstruct expected the entity to contain ImageNode")
    }

    fn spawn(&self, entity: &mut bevy::prelude::EntityWorldMut) {
        entity.insert(self.clone());
    }

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {
        visit_fn(self.image.id().untyped());
    }
}

#[cfg(feature = "editor-ui")]
impl EditorUiNode for ImageNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        let id = Id::new("image-node-editor").with(entity.id());
        let mut current_path = ui.data_mut(|data| {
            data.get_temp_mut_or_insert_with(id, || {
                let handle = entity.get::<ImageNode>().unwrap().image.id();
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

        entity.get_mut::<ImageNode>().unwrap().image = handle;
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Self>();
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
        let default_pipeline = world.resource::<DefaultNodePipeline>();
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
        extracted.insert(entity, node.image.id());
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

pub struct ImageNodePlugin;

impl Plugin for ImageNodePlugin {
    fn build(&self, app: &mut App) {
        app.register_user_ui_node::<ImageNode>();
        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<ImageNode>();
        }

        load_internal_asset!(
            app,
            ImageNodePipeline::SHADER,
            "shaders/image_node.wgsl",
            Shader::from_wgsl
        );
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
