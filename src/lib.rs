use bevy::{
    app::{Plugin, PostUpdate},
    ecs::schedule::SystemSet,
    prelude::*,
};
use loader::{Layout, LayoutAssetLoader};

#[cfg(feature = "editor-ui")]
pub mod editor;

pub mod loader;
pub mod math;
pub mod render;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, SystemSet)]
pub struct PrepareUiNodes;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, SystemSet)]
pub enum UiLayoutSystem {
    PropagateTransforms,
    ComputeBoundingBoxes,
}

pub struct UiLayoutPlugin;

impl Plugin for UiLayoutPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.configure_sets(
            PostUpdate,
            (
                UiLayoutSystem::PropagateTransforms,
                UiLayoutSystem::ComputeBoundingBoxes,
            )
                .chain(),
        );
        app.add_systems(
            PostUpdate,
            (
                (math::sync_simple_transforms, math::propagate_transforms)
                    .chain()
                    .in_set(UiLayoutSystem::PropagateTransforms),
                math::compute_bounding_box.in_set(UiLayoutSystem::ComputeBoundingBoxes),
            ),
        );

        app.init_asset_loader::<LayoutAssetLoader>()
            .init_asset::<Layout>()
            .register_type::<math::Transform>()
            .register_type::<math::GlobalTransform>()
            .register_type::<math::BoundingBox>()
            .register_type::<math::NonAxisAlignedBoundingBox>()
            .register_type::<math::NodeSize>()
            .register_type::<render::UiNodeSettings>()
            .register_type::<loader::UiNode>()
            .register_type::<loader::Layout>();
    }
}
