use bevy::{app::Plugin, ecs::component::Component, reflect::Reflect, utils::intern::Interned};

use crate::{render::SkipNodeRender, NodeLabel, UiNodeApp, UserUiNode};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NullNodeLabel;

decl_node_label!(NullNodeLabel);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Reflect, Component, Default)]
pub struct NullNode;

impl UserUiNode for NullNode {
    const NAME: &'static str = "Null";

    type AnimationId = ();
    type Serde = ();

    fn label() -> Interned<dyn NodeLabel> {
        NullNodeLabel.intern()
    }

    fn deserialize<E: serde::de::Error>(
        _: Self::Serde,
        _: &mut bevy::asset::LoadContext,
    ) -> Result<Self, E> {
        Ok(Self)
    }

    fn serialize<E: serde::ser::Error>(&self, _: &bevy::prelude::World) -> Result<Self::Serde, E> {
        Ok(())
    }

    fn reconstruct(entity: bevy::prelude::EntityRef) -> Self {
        *entity
            .get::<Self>()
            .expect("NullNode::reconstruct expected entity to have NullNode component")
    }

    fn spawn(&self, entity: &mut bevy::prelude::EntityWorldMut) {
        entity.insert((*self, SkipNodeRender));
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {}
}

#[cfg(feature = "editor-ui")]
impl crate::EditorUiNode for NullNode {
    fn edit(_: &mut bevy::prelude::EntityWorldMut, _: &mut egui::Ui) {}

    fn cleanup(entity: &mut bevy::prelude::EntityWorldMut) {
        entity.remove::<(Self, SkipNodeRender)>();
    }
}

pub struct NullNodePlugin;

impl Plugin for NullNodePlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.register_user_ui_node::<NullNode>();

        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<NullNode>();
        }
    }
}
