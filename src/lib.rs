use std::{
    any::Any,
    hash::Hash,
    sync::{Arc, RwLock},
};

use animations::{AnimationTarget, AnimationTargetRegistry};
use bevy::{
    app::{Plugin, PostUpdate},
    asset::{LoadContext, UntypedAssetId},
    ecs::schedule::SystemSet,
    prelude::*,
    utils::{intern::Interned, HashMap},
};
use loader::{Layout, LayoutAssetLoader};
use math::ZIndex;
use serde::{
    de::{DeserializeOwned, Error},
    Deserialize, Serialize,
};
use serde_value::ValueDeserializer;
use user_data::{UserData, UserDataRegistry};

#[cfg(feature = "editor-ui")]
use crate::animations::EditorAnimationTargetRegistry;
#[cfg(feature = "editor-ui")]
use crate::user_data::EditorUserDataRegistry;

#[cfg(feature = "editor-ui")]
pub mod editor;

pub mod view;

#[macro_export]
macro_rules! decl_node_label {
    ($name:ident) => {
        impl $crate::NodeLabel for $name {
            fn dyn_clone(&self) -> Box<dyn $crate::NodeLabel> {
                Box::new(self.clone())
            }

            fn as_dyn_eq(&self) -> &dyn bevy::utils::label::DynEq {
                self
            }

            fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
                let ty_id = std::any::TypeId::of::<Self>();
                std::hash::Hash::hash(&ty_id, &mut state);
                std::hash::Hash::hash(self, &mut state);
            }
        }
    };
}

#[macro_export]
macro_rules! decl_animation_label {
    ($name:ident) => {
        impl $crate::animations::AnimationTargetLabel for $name {
            fn dyn_clone(&self) -> Box<dyn $crate::animations::AnimationTargetLabel> {
                Box::new(self.clone())
            }

            fn as_dyn_eq(&self) -> &dyn bevy::utils::label::DynEq {
                self
            }

            fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
                let ty_id = std::any::TypeId::of::<Self>();
                std::hash::Hash::hash(&ty_id, &mut state);
                std::hash::Hash::hash(self, &mut state);
            }
        }
    };
}

#[macro_export]
macro_rules! decl_user_data_label {
    ($name:ident) => {
        impl $crate::user_data::UserDataLabel for $name {
            fn dyn_clone(&self) -> Box<dyn $crate::user_data::UserDataLabel> {
                Box::new(self.clone())
            }

            fn as_dyn_eq(&self) -> &dyn bevy::utils::label::DynEq {
                self
            }

            fn dyn_hash(&self, mut state: &mut dyn std::hash::Hasher) {
                let ty_id = std::any::TypeId::of::<Self>();
                std::hash::Hash::hash(&ty_id, &mut state);
                std::hash::Hash::hash(self, &mut state);
            }
        }
    };
}

pub mod animations;
pub mod builtins;
pub mod loader;
pub mod math;
pub mod render;
pub mod user_data;

bevy::utils::define_label!(NodeLabel, NODE_LABEL_INTERNER);

/// User trait for including custom node implementations in the asset format.
///
/// There is a corresponding trait for making your node editable: [`EditorUiNode`],
/// which is only available when the `editor-ui` feature is enabled.
pub trait UserUiNode: Send + Sized + Sync + 'static {
    /// The name of your node. This should be unique, the asset loader is going to
    /// associate whatever the most recently assigned node is for any name that it encounters.
    const NAME: &'static str;

    /// Data type that gets serialized into/deserialized from the layout JSON files
    type Serde: Serialize + DeserializeOwned + Send + Sync + 'static;

    /// Returns a unique label that can be associated with this UI node
    ///
    /// This label will be used to ensure that when serializing nodes, we are only calling
    /// [`UserUiNode::reconstruct`] on the appropriate trait.
    ///
    /// It is also used by [`EditorUiNode`] for determining at runtime what kind of UI node
    /// is represented by an entity.
    fn label() -> Interned<dyn NodeLabel>;

    /// Initializes this UI node from the associated serde data
    ///
    /// This method can return an error that will get propagated out through the asset loader.
    ///
    /// This method is called during the asset loader's load implementation.
    fn deserialize<E: serde::de::Error>(
        serde: Self::Serde,
        load_context: &mut LoadContext,
    ) -> Result<Self, E>;

    /// Serializes this UI node into the associated serde data
    ///
    /// This method can return an error that will be propagated to the user that is serializing
    /// the layout.
    fn serialize<E: serde::ser::Error>(&self, world: &World) -> Result<Self::Serde, E>;

    /// Marshalls this data into a representable format.
    ///
    /// This can be called **outside of serialization contexts** and just when writing an
    /// existing layout hierarchy to an asset structure.
    fn reconstruct(entity: EntityRef) -> Self;

    /// Spawns this UI node into the entity
    fn spawn(&self, entity: &mut EntityWorldMut);

    /// Visits all dependencies that this UI node introduces (i.e. a texture file)
    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(UntypedAssetId));
}

#[derive(Copy, Clone)]
pub struct RegisteredUiNode {
    name: &'static str,
    deserialize: fn(
        serde_value::Value,
        &mut LoadContext,
    ) -> Result<Box<dyn Any + Send + Sync + 'static>, serde_json::Error>,
    serialize: fn(&dyn Any, &World) -> Result<serde_value::Value, serde_json::Error>,
    reconstruct: fn(EntityRef) -> Box<dyn Any + Send + Sync + 'static>,
    spawn: fn(&dyn Any, &mut EntityWorldMut),
    visit_asset_dependencies: fn(&dyn Any, &mut dyn FnMut(UntypedAssetId)),
}

#[cfg(feature = "editor-ui")]
#[derive(Copy, Clone)]
pub struct RegisteredEditorNode {
    edit: fn(&mut EntityWorldMut, &mut egui::Ui),
    init_default: fn(&mut EntityWorldMut),
    cleanup: fn(&mut EntityWorldMut),
}

#[derive(Resource, Default, Deref, DerefMut, Clone)]
pub struct UiNodeRegistry(Arc<RwLock<RegisteredUserUiNodes>>);

#[cfg(feature = "editor-ui")]
#[derive(Resource, Default, Deref, DerefMut, Clone)]
pub struct EditorUiNodeRegistry(Arc<RwLock<RegisteredEditorUiNodes>>);

#[cfg(feature = "editor-ui")]
#[derive(Default)]
pub struct RegisteredEditorUiNodes {
    by_label: HashMap<Interned<dyn NodeLabel>, RegisteredEditorNode>,
}

#[cfg(feature = "editor-ui")]
impl RegisteredEditorUiNodes {
    pub fn register<T: EditorUiNode>(&mut self) {
        let label = T::label();
        self.by_label.insert(
            label,
            RegisteredEditorNode {
                edit: T::edit,
                init_default: |entity| {
                    let data = entity.world_scope(|world| T::from_world(world));

                    data.spawn(entity);
                },
                cleanup: T::cleanup,
            },
        );
    }
}

#[derive(Default)]
pub struct RegisteredUserUiNodes {
    by_name: HashMap<&'static str, Interned<dyn NodeLabel>>,
    by_label: HashMap<Interned<dyn NodeLabel>, RegisteredUiNode>,
}

impl RegisteredUserUiNodes {
    pub fn register<T: UserUiNode>(&mut self) {
        let label = T::label();
        self.by_name.insert(T::NAME, label.clone());
        self.by_label.insert(
            label,
            RegisteredUiNode {
                name: T::NAME,
                deserialize: |value, context| {
                    let value = <T::Serde as Deserialize>::deserialize(ValueDeserializer::<
                        serde_json::Error,
                    >::new(
                        value
                    ))?;
                    T::deserialize(value, context).map(|value| Box::new(value) as _)
                },
                serialize: |value, world| {
                    let repr = value.downcast_ref::<T>().unwrap().serialize(world)?;

                    serde_value::to_value(repr).map_err(|e| serde_json::Error::custom(e))
                },
                reconstruct: |entity| Box::new(T::reconstruct(entity)),
                spawn: |value, world| value.downcast_ref::<T>().unwrap().spawn(world),
                visit_asset_dependencies: |value, visitor| {
                    value
                        .downcast_ref::<T>()
                        .unwrap()
                        .visit_asset_dependencies(visitor)
                },
            },
        );
    }
}

#[cfg(feature = "editor-ui")]
pub trait EditorUiNode: UserUiNode + FromWorld {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui);
    fn cleanup(entity: &mut EntityWorldMut);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, SystemSet)]
pub enum UiLayoutSystem {
    UpdateAnimations,
    PropagateTransforms,
    ComputeBoundingBoxes,
}

pub struct UiLayoutPlugin;

impl Plugin for UiLayoutPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        app.configure_sets(
            PostUpdate,
            (
                UiLayoutSystem::UpdateAnimations,
                UiLayoutSystem::PropagateTransforms,
                UiLayoutSystem::ComputeBoundingBoxes,
            )
                .chain(),
        );
        app.add_systems(
            PostUpdate,
            (
                animations::update_animations.in_set(UiLayoutSystem::UpdateAnimations),
                (math::sync_simple_transforms, math::propagate_transforms)
                    .chain()
                    .in_set(UiLayoutSystem::PropagateTransforms),
                math::compute_bounding_box.in_set(UiLayoutSystem::ComputeBoundingBoxes),
            ),
        );

        let node_reg = UiNodeRegistry::default();
        let target_reg = AnimationTargetRegistry::default();
        let ud_reg = UserDataRegistry::default();

        #[cfg(feature = "editor-ui")]
        {
            app.init_resource::<EditorUiNodeRegistry>();
            app.init_resource::<EditorAnimationTargetRegistry>();
            app.init_resource::<EditorUserDataRegistry>();
        }

        app.register_asset_loader(LayoutAssetLoader {
            ui_node_registry: node_reg.0.clone(),
            ui_animation_registry: target_reg.0.clone(),
            user_data_registry: ud_reg.0.clone(),
        })
        .init_asset::<Layout>()
        .insert_resource(node_reg)
        .insert_resource(target_reg)
        .insert_resource(ud_reg)
        .register_type::<math::Transform>()
        .register_type::<math::GlobalTransform>()
        .register_type::<math::BoundingBox>()
        .register_type::<math::NonAxisAlignedBoundingBox>()
        .register_type::<math::NodeSize>()
        .register_type::<render::UiNodeSettings>()
        .register_type::<render::VertexColors>()
        .register_type::<ZIndex>()
        .register_type_data::<bevy::render::color::Color, ReflectDefault>()
        .register_type::<loader::UiNodeAttributes>();
    }
}

pub trait UiNodeApp {
    fn register_user_ui_node<T: UserUiNode>(&mut self) -> &mut Self;
    fn register_animation_target<T: AnimationTarget>(&mut self) -> &mut Self;
    fn register_user_data<T: UserData>(&mut self) -> &mut Self;
    #[cfg(feature = "editor-ui")]
    fn register_editor_ui_node<T: EditorUiNode>(&mut self) -> &mut Self;
    #[cfg(feature = "editor-ui")]
    fn register_editor_animation_target<T: animations::EditorAnimationTarget>(
        &mut self,
    ) -> &mut Self;

    #[cfg(feature = "editor-ui")]
    fn register_editor_user_data<T: user_data::EditorUserData>(&mut self) -> &mut Self;
}

impl UiNodeApp for App {
    fn register_user_ui_node<T: UserUiNode>(&mut self) -> &mut Self {
        self.world
            .resource::<UiNodeRegistry>()
            .write()
            .unwrap()
            .register::<T>();

        self
    }

    fn register_animation_target<T: AnimationTarget>(&mut self) -> &mut Self {
        self.world
            .resource::<AnimationTargetRegistry>()
            .write()
            .unwrap()
            .register::<T>();

        self
    }

    fn register_user_data<T: UserData>(&mut self) -> &mut Self {
        self.world
            .resource::<UserDataRegistry>()
            .write()
            .unwrap()
            .register::<T>();
        self
    }

    #[cfg(feature = "editor-ui")]
    fn register_editor_ui_node<T: EditorUiNode>(&mut self) -> &mut Self {
        self.world
            .resource::<EditorUiNodeRegistry>()
            .write()
            .unwrap()
            .register::<T>();

        self
    }

    #[cfg(feature = "editor-ui")]
    fn register_editor_animation_target<T: animations::EditorAnimationTarget>(
        &mut self,
    ) -> &mut Self {
        self.world
            .resource::<EditorAnimationTargetRegistry>()
            .write()
            .unwrap()
            .register::<T>();

        self
    }

    #[cfg(feature = "editor-ui")]
    fn register_editor_user_data<T: user_data::EditorUserData>(&mut self) -> &mut Self {
        self.world
            .resource::<EditorUserDataRegistry>()
            .write()
            .unwrap()
            .register::<T>();
        self
    }
}
