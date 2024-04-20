use bevy::{prelude::*, sprite::Anchor};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_layout_ui::{
    math::{GlobalTransform, NodeSize, Transform},
    render::{UiNodeSettings, UiRenderPlugin},
    UiLayoutPlugin,
};
pub fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins)
        .add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(WorldInspectorPlugin::default());

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
