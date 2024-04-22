use bevy::{
    asset::{load_internal_asset, LoadState},
    ecs::{entity::EntityHashMap, system::lifetimeless::SRes},
    prelude::*,
    render::{
        render_asset::RenderAssets,
        render_phase::{AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult},
        render_resource::{
            BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingType,
            CachedRenderPipelineId, IntoBinding, PipelineCache, SamplerBindingType, ShaderStages,
            TextureSampleType, TextureViewDimension,
        },
        renderer::RenderDevice,
        Extract, Render, RenderApp, RenderSet,
    },
    utils::HashMap,
};
use bevy_layout_ui::{
    loader::Layout,
    math::NodeSize,
    render::{
        BindLayoutUniform, BindVertexBuffer, DrawUiPhaseItem, InvalidNodePipeline,
        NodeDrawFunction, SkipNodeRender, UiNodeItem, UiRenderPlugin,
    },
    UiLayoutPlugin,
};

#[derive(Component)]
struct ImageNode(Handle<Image>);

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
                .load("image_node.layout.json"),
        )
    }
}

fn wait_spawn_layout(layout: Res<WaitingLayout>, server: Res<AssetServer>, mut commands: Commands) {
    if server.load_state(layout.0.id()) == LoadState::Loaded {
        let image_handle = server.load::<Image>("images/sarina.png");
        commands.add(move |world: &mut World| {
            let handle = world.remove_resource::<WaitingLayout>().unwrap().0;
            world.resource_scope::<Assets<Layout>, _>(|world, assets| {
                let layout = assets.get(&handle).unwrap();
                bevy_layout_ui::loader::spawn_layout(world, layout);
            });

            let entities =
                Vec::from_iter(world.query_filtered::<Entity, With<NodeSize>>().iter(world));
            for entity in entities {
                world
                    .entity_mut(entity)
                    .insert(ImageNode(image_handle.clone()));
            }
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

pub fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins)
        .add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(ImageNodePlugin)
        .add_systems(Update, bevy::window::close_on_esc)
        .add_systems(
            Update,
            wait_spawn_layout.run_if(resource_exists::<WaitingLayout>),
        )
        .init_resource::<WaitingLayout>();

    app.world.spawn(Camera2dBundle::default());

    app.run();
}
