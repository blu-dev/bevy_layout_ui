use bevy::{asset::LoadState, prelude::*, window::PrimaryWindow};
use bevy_inspector_egui::{
    bevy_egui::{EguiContext, EguiPlugin},
    DefaultInspectorConfigPlugin,
};
use bevy_layout_ui::{
    animations::{PlaybackRequest, UiLayoutAnimationController},
    builtins::DefaultNodePluginGroup,
    loader::Layout,
    math::NodeSize,
    render::UiRenderPlugin,
    UiLayoutPlugin,
};

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
            let layout = world
                .resource_mut::<Assets<Layout>>()
                .remove(&handle)
                .unwrap();
            bevy_layout_ui::loader::spawn_layout(world, &layout);
            world
                .resource_mut::<Assets<Layout>>()
                .insert(handle, layout);
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

                let json = bevy_layout_ui::loader::serialize_layout_as_json(&tree, world).unwrap();
                std::fs::write(
                    "assets/basic_node.layout.json",
                    serde_json::to_string_pretty(&json).unwrap(),
                )
                .unwrap();
            }

            if let Some(entity) = bevy_layout_ui::editor::display_node_tree(root, world, ui) {
                if !open_nodes.contains(&entity) {
                    open_nodes.push(entity);
                }
            }

            if let Some(animation) = bevy_layout_ui::editor::display_animation_list(root, world, ui)
            {
                world
                    .get_mut::<UiLayoutAnimationController>(root)
                    .unwrap()
                    .animations
                    .get_index_mut(animation)
                    .unwrap()
                    .1
                    .requests
                    .push(PlaybackRequest::Play {
                        restore_on_finish: true,
                    });
            }

            // bevy_layout_ui::editor::animation::show_animation_editor(
            //     root,
            //     world,
            //     ui,
            //     "test_animation",
            // );
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
    app.add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(DefaultNodePluginGroup)
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
