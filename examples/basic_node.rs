use bevy::{
    asset::{load_internal_asset, LoadState},
    ecs::{query::ROQueryItem, system::lifetimeless::SRes},
    prelude::*,
    render::{
        render_phase::{AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult},
        render_resource::{CachedRenderPipelineId, PipelineCache},
        Extract, Render, RenderApp, RenderSet,
    },
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
    PrepareUiNodes, UiLayoutPlugin,
};

#[derive(Component)]
struct BasicNode;

#[derive(Resource, Default, Deref, DerefMut)]
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
        commands.add(|world: &mut World| {
            let handle = world.remove_resource::<WaitingLayout>().unwrap().0;
            world.resource_scope::<Assets<Layout>, _>(|world, assets| {
                let layout = assets.get(&handle).unwrap();
                bevy_layout_ui::loader::spawn_layout(world, layout);
            });

            let entities =
                Vec::from_iter(world.query_filtered::<Entity, With<NodeSize>>().iter(world));
            for entity in entities {
                world.entity_mut(entity).insert(BasicNode);
            }
        });
    }
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
                println!("{tree:?}",);

                let json = serde_json::to_string_pretty(&tree).unwrap();
                std::fs::write("assets/basic_node.layout.json", &json).unwrap();
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
    fn build(&self, app: &mut App) {}

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<ExtractedBasicNodes>()
            .init_resource::<BasicPipeline>()
            .add_render_command::<UiNodeItem, BasicDrawFunction>()
            .add_systems(ExtractSchedule, extract_basic_nodes)
            .add_systems(
                Render,
                prepare_basic_nodes
                    .in_set(RenderSet::Prepare)
                    .before(PrepareUiNodes),
            );
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

    println!(
        "{}",
        std::any::type_name::<<BasicDrawFunction as RenderCommand<UiNodeItem>>::ViewQuery>()
    );

    app.add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(EguiPlugin)
        .add_plugins(DefaultInspectorConfigPlugin)
        .add_plugins(BasicPlugin)
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
