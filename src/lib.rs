use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use bevy::{
    app::{Plugin, PostUpdate},
    asset::{LoadContext, UntypedAssetId},
    ecs::schedule::SystemSet,
    prelude::*,
    utils::{intern::Interned, HashMap},
};
use loader::{Layout, LayoutAssetLoader};
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use serde_value::ValueDeserializer;

#[cfg(feature = "editor-ui")]
pub mod editor;

pub mod loader;
pub mod math;
pub mod render;

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
    label: Interned<dyn NodeLabel>,
    deserialize: fn(
        serde_value::Value,
        &mut LoadContext,
    ) -> Result<Box<dyn Any + Send + Sync + 'static>, serde_json::Error>,
    serialize:
        fn(&dyn Any, &World) -> Result<Box<dyn Any + Send + Sync + 'static>, serde_json::Error>,
    reconstruct: fn(EntityRef) -> Box<dyn Any + Send + Sync + 'static>,
    spawn: fn(&dyn Any, &mut EntityWorldMut),
    visit_asset_dependencies: fn(&dyn Any, &mut dyn FnMut(UntypedAssetId)),
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct UiNodeRegistry(Arc<RwLock<RegisteredUserUiNodes>>);

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
            label.clone(),
            RegisteredUiNode {
                name: T::NAME,
                label: label.clone(),
                deserialize: |value, context| {
                    let value = <T::Serde as Deserialize>::deserialize(ValueDeserializer::<
                        serde_json::Error,
                    >::new(
                        value
                    ))?;
                    T::deserialize(value, context).map(|value| Box::new(value) as _)
                },
                serialize: |value, world| {
                    value
                        .downcast_ref::<T>()
                        .unwrap()
                        .serialize(world)
                        .map(|value| Box::new(value) as _)
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
pub trait EditorUiNode: UserUiNode {}

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

        let resource = UiNodeRegistry::default();

        app.register_asset_loader(LayoutAssetLoader {
            reader: resource.0.clone(),
        })
        .init_asset::<Layout>()
        .insert_resource(resource)
        .register_type::<math::Transform>()
        .register_type::<math::GlobalTransform>()
        .register_type::<math::BoundingBox>()
        .register_type::<math::NonAxisAlignedBoundingBox>()
        .register_type::<math::NodeSize>()
        .register_type::<render::UiNodeSettings>()
        .register_type::<loader::UiNodeAttributes>();
    }
}

pub trait UiNodeApp {
    fn register_user_ui_node<T: UserUiNode>(&mut self) -> &mut Self;
}

impl UiNodeApp for App {
    fn register_user_ui_node<T: UserUiNode>(&mut self) -> &mut Self {
        self.world
            .resource_mut::<UiNodeRegistry>()
            .write()
            .unwrap()
            .register::<T>();

        self
    }
}
