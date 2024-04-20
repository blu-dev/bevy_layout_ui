use bevy::{prelude::*, sprite::Anchor};
use bevy_inspector_egui::quick::WorldInspectorPlugin;
use bevy_layout_ui::{
    math::{GlobalTransform, NodeSize, Transform},
    render::UiRenderPlugin,
    UiLayoutPlugin,
};
pub fn main() {
    let mut app = App::new();

    app.add_plugins(DefaultPlugins)
        .add_plugins(UiLayoutPlugin)
        .add_plugins(UiRenderPlugin)
        .add_plugins(WorldInspectorPlugin::default());

    app.world.spawn(Camera2dBundle::default());
    app.world.spawn((
        Transform::from_xy(150.0, 150.0),
        GlobalTransform::default(),
        NodeSize(Vec2::splat(300.0)),
        Anchor::Center,
    ));
    app.world.spawn((
        Transform::from_xy(500.0, 500.0),
        GlobalTransform::default(),
        NodeSize(Vec2::splat(20.0)),
        Anchor::BottomRight,
    ));

    app.run();
}
