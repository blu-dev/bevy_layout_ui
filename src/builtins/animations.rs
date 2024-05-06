use bevy::asset::UntypedAssetId;
use bevy::prelude::*;
use bevy::{math::Vec2, utils::intern::Interned};

use crate::UiNodeApp;
use crate::{
    animations::{AnimationTarget, AnimationTargetLabel},
    math::NodeSize,
    NodeLabel,
};

pub struct BuiltinAnimationsPlugin;

impl Plugin for BuiltinAnimationsPlugin {
    fn build(&self, app: &mut App) {
        app.register_animation_target::<NodeSizeAnimation>();
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NodeSizeAnimation;

decl_animation_label!(NodeSizeAnimation);

impl AnimationTarget for NodeSizeAnimation {
    type Content = Vec2;
    type Serde = ();

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Size";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<NodeSize>().unwrap().0 = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<NodeSize>().unwrap().0 = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}
