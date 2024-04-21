use bevy::{
    asset::{load_internal_asset, LoadState},
    ecs::query::ROQueryItem,
    prelude::*,
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
        BindLayoutUniform, BindNodePipeline, BindVertexBuffer, DrawUiPhaseItem,
        SpecializedNodePlugin, SpecializedUiNode, UiRenderPlugin,
    },
    UiLayoutPlugin,
};

pub struct BasicUiNode;

impl BasicUiNode {
    pub const SHADER: Handle<Shader> = Handle::weak_from_u128(0xCDE15DE115FF47E796AF6C535F5AE089);
}

impl SpecializedUiNode for BasicUiNode {
    type DrawFunction = (
        BindNodePipeline<BasicUiNode>,
        BindLayoutUniform<0>,
        BindVertexBuffer<0>,
        DrawUiPhaseItem,
    );
    type QueryData = ();
    type Extracted = ();

    fn extract(_: ROQueryItem<Self::QueryData>) -> Self::Extracted {
        ()
    }

    fn vertex_shader() -> Handle<Shader> {
        Self::SHADER.clone()
    }

    fn fragment_shader() -> Handle<Shader> {
        Self::SHADER.clone()
    }

    fn fragment_shader_entrypoint() -> &'static str {
        "fragment"
    }

    fn vertex_shader_entrypoint() -> &'static str {
        "vertex"
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
        commands.add(|world: &mut World| {
            let handle = world.remove_resource::<WaitingLayout>().unwrap().0;
            world.resource_scope::<Assets<Layout>, _>(|world, assets| {
                let layout = assets.get(&handle).unwrap();
                bevy_layout_ui::loader::spawn_layout(world, layout);
            });
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

pub fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins);

    load_internal_asset!(
        app,
        BasicUiNode::SHADER,
        "shaders/basic.wgsl",
        Shader::from_wgsl
    );

    app.add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(SpecializedNodePlugin::<BasicUiNode>::default())
        .add_plugins(EguiPlugin)
        .add_plugins(DefaultInspectorConfigPlugin)
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
