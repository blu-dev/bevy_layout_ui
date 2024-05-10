use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use bevy::{
    asset::UntypedAssetId,
    ecs::{
        system::Resource,
        world::{EntityRef, EntityWorldMut, FromWorld},
    },
    prelude::{Deref, DerefMut},
    utils::{intern::Interned, HashMap},
};
use serde::{de::DeserializeOwned, ser::Error, Deserialize, Serialize};
use serde_json::Error as JsonError;
use serde_value::{Value, ValueDeserializer};

bevy::utils::define_label!(UserDataLabel, USER_DATA_INTERNER);

pub trait UserData: Sized + Send + Sync + 'static {
    type Serde: Serialize + DeserializeOwned + 'static;

    const NAME: &'static str;

    fn label() -> Interned<dyn UserDataLabel>;

    fn deserialize(serde: Self::Serde) -> Result<Self, JsonError>;
    fn serialize(&self) -> Self::Serde;

    fn initialize(&self, entity: &mut EntityWorldMut);
    fn reconstruct(entity: EntityRef) -> Self;

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(UntypedAssetId));
}

#[cfg(feature = "editor-ui")]
pub trait EditorUserData: UserData + FromWorld {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui);
    fn cleanup(entity: &mut EntityWorldMut);
}

#[derive(Copy, Clone)]
pub struct DynamicUserData {
    pub name: &'static str,
    pub label: Interned<dyn UserDataLabel>,
    pub deserialize: fn(Value) -> Result<Box<dyn Any + Send + Sync + 'static>, JsonError>,
    pub serialize: fn(&dyn Any) -> Result<Value, JsonError>,
    pub initialize: fn(&dyn Any, &mut EntityWorldMut),
    pub reconstruct: fn(EntityRef) -> Box<dyn Any + Send + Sync + 'static>,
    pub visit_asset_dependencies: fn(&dyn Any, &mut dyn FnMut(UntypedAssetId)),
}

impl DynamicUserData {
    pub fn new<T: UserData>() -> Self {
        Self {
            name: T::NAME,
            label: T::label(),
            deserialize: |value| {
                let serde: T::Serde =
                    Deserialize::deserialize(ValueDeserializer::<JsonError>::new(value))?;
                T::deserialize(serde).map(|value| Box::new(value) as _)
            },
            serialize: |this| {
                let this = this.downcast_ref::<T>().unwrap();
                serde_value::to_value(this.serialize()).map_err(JsonError::custom)
            },
            initialize: |this, entity| {
                let this = this.downcast_ref::<T>().unwrap();
                this.initialize(entity)
            },
            reconstruct: |entity| Box::new(T::reconstruct(entity)),
            visit_asset_dependencies: |this, visit| {
                let this = this.downcast_ref::<T>().unwrap();
                this.visit_asset_dependencies(visit)
            },
        }
    }
}

#[cfg(feature = "editor-ui")]
#[derive(Copy, Clone)]
pub struct DynamicEditorUserData {
    pub edit: fn(&mut EntityWorldMut, &mut egui::Ui),
    pub cleanup: fn(&mut EntityWorldMut),
    pub from_world_and_init: fn(&mut EntityWorldMut),
}

#[cfg(feature = "editor-ui")]
impl DynamicEditorUserData {
    pub fn new<T: EditorUserData>() -> Self {
        Self {
            edit: |entity, ui| T::edit(entity, ui),
            cleanup: |entity| T::cleanup(entity),
            from_world_and_init: |entity| {
                let data = entity.world_scope(T::from_world);
                data.initialize(entity)
            },
        }
    }
}

#[derive(Default)]
pub struct RegisteredUserData {
    pub by_name: HashMap<&'static str, Interned<dyn UserDataLabel>>,
    pub by_label: HashMap<Interned<dyn UserDataLabel>, DynamicUserData>,
}

impl RegisteredUserData {
    pub fn register<T: UserData>(&mut self) {
        let dynamic = DynamicUserData::new::<T>();
        self.by_name.insert(dynamic.name, dynamic.label.clone());
        self.by_label.insert(dynamic.label.clone(), dynamic);
    }
}

#[cfg(feature = "editor-ui")]
#[derive(Default)]
pub struct RegisteredEditorUserData {
    pub by_label: HashMap<Interned<dyn UserDataLabel>, DynamicEditorUserData>,
}

#[cfg(feature = "editor-ui")]
impl RegisteredEditorUserData {
    pub fn register<T: EditorUserData>(&mut self) {
        let dynamic = DynamicEditorUserData::new::<T>();
        self.by_label.insert(T::label(), dynamic);
    }
}

#[derive(Resource, Clone, Deref, DerefMut, Default)]
pub struct UserDataRegistry(pub(crate) Arc<RwLock<RegisteredUserData>>);

#[cfg(feature = "editor-ui")]
#[derive(Resource, Clone, Deref, DerefMut, Default)]
pub struct EditorUserDataRegistry(pub(crate) Arc<RwLock<RegisteredEditorUserData>>);
