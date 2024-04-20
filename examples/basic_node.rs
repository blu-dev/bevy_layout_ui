use bevy::{prelude::*, sprite::Anchor, window::PrimaryWindow};
use bevy_inspector_egui::{
    bevy_egui::{EguiContext, EguiPlugin},
    DefaultInspectorConfigPlugin,
};
use bevy_layout_ui::{
    math::{GlobalTransform, NodeSize, Transform},
    render::{UiNodeSettings, UiRenderPlugin},
    UiLayoutPlugin,
};

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

    app.add_plugins(DefaultPlugins)
        .add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(EguiPlugin)
        .add_plugins(DefaultInspectorConfigPlugin)
        .add_systems(Update, ui_system);

    app.world.spawn(Camera2dBundle::default());
    app.world
        .spawn((
            Transform::from_xy(150.0, 150.0),
            GlobalTransform::default(),
            NodeSize(Vec2::splat(300.0)),
            Anchor::Center,
            UiNodeSettings {
                target_resolution: UVec2::new(960, 540),
            },
        ))
        .with_children(|children| {
            children.spawn((
                Transform::from_xy(50.0, 50.0),
                GlobalTransform::default(),
                NodeSize(Vec2::splat(20.0)),
                Anchor::BottomRight,
                UiNodeSettings {
                    target_resolution: UVec2::new(960, 540),
                },
            ));
        });

    app.run();
}
