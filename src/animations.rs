use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use bevy::{
    asset::UntypedAssetId,
    core::Name,
    ecs::{
        component::Component,
        entity::Entity,
        query::With,
        system::Resource,
        world::{EntityRef, EntityWorldMut, FromWorld, World},
    },
    hierarchy::Children,
    math::Vec2,
    prelude::{Deref, DerefMut},
    time::Time,
    utils::{intern::Interned, HashMap},
};
use serde::{de::DeserializeOwned, ser::Error, Deserialize, Serialize};
use serde_json::Error as JsonError;
use serde_value::ValueDeserializer;

use crate::{builtins::sublayout::SpawnedSublayout, loader::DynamicNodeLabel, NodeLabel};

pub trait AnimationTarget: Send + Sized + Sync + 'static {
    const NAME: &'static str;
    type Content: Serialize + DeserializeOwned + Send + Sync + 'static;
    type Serde: Serialize + DeserializeOwned + 'static;

    type BackupData: Send + Sync + 'static;

    fn backup(&self, entity: EntityRef) -> Self::BackupData;
    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut);

    fn label() -> Interned<dyn AnimationTargetLabel>;
    fn node_label() -> Option<Interned<dyn NodeLabel>>;

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content);
    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    );

    fn serialize(&self, world: &World) -> Result<Self::Serde, JsonError>;
    fn deserialize(data: Self::Serde) -> Self;

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(UntypedAssetId));
    fn visit_content_asset_dependencies(
        content: &Self::Content,
        visit_fn: &mut dyn FnMut(UntypedAssetId),
    );
}

#[cfg(feature = "editor-ui")]
pub trait EditorAnimationTarget: FromWorld + AnimationTarget {
    fn default_content() -> Self::Content;
    fn edit(&mut self, ui: &mut egui::Ui);
    fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui);
}

pub struct DynamicAnimationTarget {
    pub name: &'static str,
    pub label: Interned<dyn AnimationTargetLabel>,
    pub node_label: Option<Interned<dyn NodeLabel>>,
    pub backup: fn(&dyn Any, EntityRef) -> Box<dyn Any + Send + Sync + 'static>,
    pub restore: fn(&dyn Any, Box<dyn Any>, &mut EntityWorldMut),
    pub initialize: fn(&dyn Any, &mut EntityWorldMut, &dyn Any),
    pub interpolate: fn(&dyn Any, &mut EntityWorldMut, &dyn Any, &dyn Any, f32),
    pub serialize: fn(&dyn Any, &World) -> Result<serde_value::Value, JsonError>,
    pub deserialize:
        fn(serde_value::Value) -> Result<Box<dyn Any + Send + Sync + 'static>, JsonError>,
    pub serialize_content: fn(&dyn Any) -> Result<serde_value::Value, JsonError>,
    pub deserialize_content:
        fn(serde_value::Value) -> Result<Box<dyn Any + Send + Sync + 'static>, JsonError>,
    pub visit_asset_dependencies: fn(&dyn Any, &mut dyn FnMut(UntypedAssetId)),
    pub visit_content_asset_dependencies: fn(&dyn Any, &mut dyn FnMut(UntypedAssetId)),
}

impl DynamicAnimationTarget {
    pub fn new<T: AnimationTarget>() -> Self {
        Self {
            name: T::NAME,
            label: T::label(),
            node_label: T::node_label(),
            backup: |this, entity| {
                let this = this.downcast_ref::<T>().unwrap();
                Box::new(this.backup(entity))
            },
            restore: |this, backup, entity| {
                let this = this.downcast_ref::<T>().unwrap();
                let backup = backup.downcast::<T::BackupData>().unwrap();
                this.restore(*backup, entity)
            },
            initialize: |this, entity, value| {
                let this = this.downcast_ref::<T>().unwrap();
                let value = value.downcast_ref::<T::Content>().unwrap();
                this.initialize(entity, value)
            },
            interpolate: |this, entity, start, end, interp| {
                let this = this.downcast_ref::<T>().unwrap();
                let start = start.downcast_ref::<T::Content>().unwrap();
                let end = end.downcast_ref::<T::Content>().unwrap();
                this.interpolate(entity, start, end, interp)
            },
            serialize: |this, world| {
                let this = this.downcast_ref::<T>().unwrap();
                let serde = this.serialize(world)?;
                serde_value::to_value(serde).map_err(JsonError::custom)
            },
            deserialize: |this| {
                let data = <T::Serde as Deserialize>::deserialize(
                    ValueDeserializer::<JsonError>::new(this),
                )
                .map_err(JsonError::custom)?;
                Ok(Box::new(T::deserialize(data)))
            },
            serialize_content: |content| {
                let content = content.downcast_ref::<T::Content>().unwrap();
                serde_value::to_value(content).map_err(JsonError::custom)
            },
            deserialize_content: |content| {
                let content =
                    <T::Content as Deserialize>::deserialize(ValueDeserializer::<JsonError>::new(
                        content,
                    ))?;
                Ok(Box::new(content))
            },
            visit_asset_dependencies: |this, visit| {
                let this = this.downcast_ref::<T>().unwrap();
                this.visit_asset_dependencies(visit)
            },
            visit_content_asset_dependencies: |content, visit| {
                let content = content.downcast_ref::<T::Content>().unwrap();
                T::visit_content_asset_dependencies(content, visit)
            },
        }
    }
}

#[cfg(feature = "editor-ui")]
pub struct DynamicEditorAnimationTarget {
    pub from_world: fn(&mut World) -> Box<dyn Any + Send + Sync + 'static>,
    pub default_content: fn() -> Box<dyn Any + Send + Sync + 'static>,
    pub edit: fn(&mut dyn Any, &mut egui::Ui),
    pub edit_content: fn(&mut dyn Any, &mut egui::Ui),
}

#[cfg(feature = "editor-ui")]
impl DynamicEditorAnimationTarget {
    pub fn new<T: EditorAnimationTarget>() -> Self {
        Self {
            from_world: |world| Box::new(T::from_world(world)),
            default_content: || Box::new(T::default_content()),
            edit: |this, ui| {
                let this = this.downcast_mut::<T>().unwrap();
                this.edit(ui)
            },
            edit_content: |content, ui| {
                let content = content.downcast_mut::<T::Content>().unwrap();
                T::edit_content(content, ui)
            },
        }
    }
}

bevy::utils::define_label!(AnimationTargetLabel, NODE_LABEL_INTERNER);

#[derive(Default)]
pub struct RegisteredAnimationTargets {
    pub by_name: HashMap<&'static str, Interned<dyn AnimationTargetLabel>>,
    pub by_label: HashMap<Interned<dyn AnimationTargetLabel>, DynamicAnimationTarget>,
}

impl RegisteredAnimationTargets {
    pub fn register<T: AnimationTarget>(&mut self) {
        let dynamic = DynamicAnimationTarget::new::<T>();
        assert!(self
            .by_name
            .insert(dynamic.name, dynamic.label.clone())
            .is_none());
        assert!(self
            .by_label
            .insert(dynamic.label.clone(), dynamic)
            .is_none());
    }
}

#[cfg(feature = "editor-ui")]
#[derive(Default)]
pub struct RegisteredEditorAnimationTargets {
    pub by_label: HashMap<Interned<dyn AnimationTargetLabel>, DynamicEditorAnimationTarget>,
}

#[cfg(feature = "editor-ui")]
impl RegisteredEditorAnimationTargets {
    pub fn register<T: EditorAnimationTarget>(&mut self) {
        self.by_label
            .insert(T::label(), DynamicEditorAnimationTarget::new::<T>());
    }
}

#[cfg(feature = "editor-ui")]
#[derive(Resource, Default, Deref, DerefMut, Clone)]
pub struct EditorAnimationTargetRegistry(pub Arc<RwLock<RegisteredEditorAnimationTargets>>);

#[derive(Resource, Default, Deref, DerefMut, Clone)]
pub struct AnimationTargetRegistry(pub Arc<RwLock<RegisteredAnimationTargets>>);

#[derive(Deserialize, Serialize, Debug, Clone)]
pub enum EdgeInterpolation {
    Linear,
    BezierQuadratic { point: Vec2 },
    BezierCubic { point_a: Vec2, point_b: Vec2 },
}

impl EdgeInterpolation {
    fn interpolate(&self, time: f32) -> f32 {
        let time = time.clamp(0.0, 1.0);

        match self {
            Self::Linear => time,
            Self::BezierQuadratic { point } => {
                (2.0 * (1.0 - time) * *point + Vec2::splat(time.exp2())).y
            }
            Self::BezierCubic { point_a, point_b } => {
                (3.0 * (1.0 - time).exp2() * time * *point_a
                    + 3.0 * (1.0 - time) * time.exp2() * *point_b
                    + Vec2::splat(time.powi(3)))
                .y
            }
        }
    }
}

pub struct AnimationKeyframe {
    pub timestamp_ms: u32,
    pub edge_interpolation: EdgeInterpolation,
    pub value: Box<dyn Any + Send + Sync + 'static>,
}

pub struct AnimationIdLane {
    pub animation_data: Box<dyn Any + Send + Sync + 'static>,
    pub starting_value: Box<dyn Any + Send + Sync + 'static>,
    pub keyframes: Vec<AnimationKeyframe>,
}

pub struct AnimationNodeLane {
    pub animation_by_id: HashMap<Interned<dyn AnimationTargetLabel>, AnimationIdLane>,
}

pub struct Animation {
    pub animation_by_node: HashMap<String, AnimationNodeLane>,
}

impl Animation {
    pub fn animate(&self, world: &mut World, entity: Entity, timestamp: u32) -> bool {
        let entities_by_name = collect_nodes_by_name(entity, world);

        let mut is_finished = true;
        let registry = world.resource::<AnimationTargetRegistry>().clone();
        let registry = registry.read().unwrap();
        for (name, lane) in self.animation_by_node.iter() {
            let Some(entity) = entities_by_name.get(name) else {
                bevy::log::warn!("Failed to find entity with name {name} when animating");
                continue;
            };

            let mut entity = world.entity_mut(*entity);

            for (id, lane) in lane.animation_by_id.iter() {
                let animation_target = registry.by_label.get(id).unwrap();

                if let Some(node_label) = animation_target.node_label.as_ref() {
                    let entity_node_label = entity.get::<DynamicNodeLabel>().unwrap();
                    if !entity_node_label.0.eq(node_label) {
                        panic!(
                            "Attempting to animate incompatible node {} with {}",
                            name, animation_target.name
                        );
                    }
                }
                let Some(index) = lane
                    .keyframes
                    .iter()
                    .position(|kf| kf.timestamp_ms >= timestamp)
                else {
                    let data = if let Some(last) = lane.keyframes.last() {
                        last.value.as_ref()
                    } else {
                        lane.starting_value.as_ref()
                    };
                    (animation_target.initialize)(lane.animation_data.as_ref(), &mut entity, data);
                    continue;
                };

                is_finished = false;

                if timestamp == 0 {
                    (animation_target.initialize)(
                        lane.animation_data.as_ref(),
                        &mut entity,
                        lane.starting_value.as_ref(),
                    );
                } else {
                    let keyframe = &lane.keyframes[index];

                    let (start, end, interp) = match index {
                        0 => (
                            lane.starting_value.as_ref(),
                            keyframe.value.as_ref(),
                            timestamp as f32 / keyframe.timestamp_ms as f32,
                        ),
                        index => {
                            let prev_keyframe = &lane.keyframes[index - 1];
                            (
                                prev_keyframe.value.as_ref(),
                                keyframe.value.as_ref(),
                                (timestamp - prev_keyframe.timestamp_ms) as f32
                                    / (keyframe.timestamp_ms - prev_keyframe.timestamp_ms) as f32,
                            )
                        }
                    };

                    (animation_target.interpolate)(
                        lane.animation_data.as_ref(),
                        &mut entity,
                        start,
                        end,
                        keyframe.edge_interpolation.interpolate(interp),
                    );
                }
            }
        }

        is_finished
    }
}

pub fn collect_nodes_by_name(
    entity: Entity,
    world: &mut World,
) -> bevy::utils::hashbrown::HashMap<String, Entity> {
    fn map_children(children: &Children) -> impl Iterator<Item = Entity> + '_ {
        children.iter().copied()
    }
    let mut entities_by_name = HashMap::new();
    {
        entities_by_name.insert("root".to_string(), entity);
        let mut current_entity = world.entity(entity);
        let mut children_stack = Vec::new();
        let mut children = current_entity
            .get::<Children>()
            .into_iter()
            .flat_map(map_children);
        loop {
            while let Some(next_child) = children.next() {
                let next = world.entity(next_child);
                if next.contains::<SpawnedSublayout>() {
                    continue;
                }
                children_stack.push((children, current_entity.id()));
                current_entity = next;
                children = current_entity
                    .get::<Children>()
                    .into_iter()
                    .flat_map(map_children);
            }

            let name = current_entity
                .get::<Name>()
                .expect("All UI nodes must have names")
                .to_string();

            entities_by_name.insert(name, current_entity.id());
            if let Some((next_children, next_entity)) = children_stack.pop() {
                children = next_children;
                current_entity = world.entity(next_entity);
            } else {
                break;
            }
        }
    }
    entities_by_name
}

pub enum PlaybackData {
    NotPlaying,
    Playing {
        restore_on_finish: bool,
        paused: bool,
        speed: f32,
        timestamp_ms: f32,
    },
}

pub enum PlaybackRequest {
    Play { restore_on_finish: bool },
    Stop,
    Pause,
    Resume,
}

pub struct AnimationPlaybackState {
    pub animation: Arc<RwLock<Animation>>,
    pub data: PlaybackData,
    pub requests: Vec<PlaybackRequest>,
}

pub struct BackupAnimationTargetState {
    state: HashMap<Interned<dyn AnimationTargetLabel>, Box<dyn Any + Send + Sync + 'static>>,
}

pub struct BackupAnimationNodeState {
    state: HashMap<String, BackupAnimationTargetState>,
}

#[derive(Component)]
pub struct UiLayoutAnimationController {
    pub animations: HashMap<String, AnimationPlaybackState>,
    pub backup_state: HashMap<String, BackupAnimationNodeState>,
}

pub fn update_animations(world: &mut World) {
    let delta_ms = world.resource::<Time>().delta().as_nanos() as f32 / 1_000_000.0;
    let controller_entities = world
        .query_filtered::<Entity, With<UiLayoutAnimationController>>()
        .iter(world)
        .collect::<Vec<_>>();
    for entity in controller_entities {
        let mut animations = std::mem::replace(
            &mut *world
                .get_mut::<UiLayoutAnimationController>(entity)
                .unwrap(),
            UiLayoutAnimationController {
                animations: HashMap::new(),
                backup_state: HashMap::new(),
            },
        );

        let children = collect_nodes_by_name(entity, world);

        for (anim_name, animation) in animations.animations.iter_mut() {
            for request in animation.requests.iter() {
                match request {
                    PlaybackRequest::Play { restore_on_finish } => {
                        if matches!(&animation.data, PlaybackData::NotPlaying) {
                            animation.data = PlaybackData::Playing {
                                restore_on_finish: *restore_on_finish,
                                paused: false,
                                speed: 1.0,
                                timestamp_ms: std::f32::NEG_INFINITY,
                            };

                            if *restore_on_finish {
                                let mut state = BackupAnimationNodeState {
                                    state: HashMap::new(),
                                };

                                let anim = animation.animation.read().unwrap();
                                let registry =
                                    world.resource::<AnimationTargetRegistry>().read().unwrap();

                                for (node_name, lane) in anim.animation_by_node.iter() {
                                    let mut node_state = BackupAnimationTargetState {
                                        state: HashMap::new(),
                                    };

                                    let node = world.entity(*children.get(node_name).unwrap());

                                    for (id, lane) in lane.animation_by_id.iter() {
                                        let target = registry.by_label.get(id).unwrap();
                                        let data = (target.backup)(
                                            lane.animation_data.as_ref(),
                                            node.clone(),
                                        );
                                        node_state.state.insert(id.clone(), data);
                                    }

                                    state.state.insert(node_name.clone(), node_state);
                                }

                                animations.backup_state.insert(anim_name.clone(), state);
                            }
                        }
                    }
                    PlaybackRequest::Stop => {
                        animation.data = PlaybackData::NotPlaying;
                    }
                    PlaybackRequest::Pause => {
                        if let PlaybackData::Playing { paused, .. } = &mut animation.data {
                            *paused = true;
                        }
                    }

                    PlaybackRequest::Resume => {
                        if let PlaybackData::Playing { paused, .. } = &mut animation.data {
                            *paused = false;
                        }
                    }
                }
            }

            animation.requests.clear();

            if let PlaybackData::Playing {
                restore_on_finish,
                paused,
                speed,
                timestamp_ms,
            } = &mut animation.data
            {
                if !*paused {
                    if timestamp_ms.is_infinite() {
                        *timestamp_ms = 0.0;
                    } else {
                        *timestamp_ms += delta_ms * *speed;
                    }

                    let anim = animation.animation.read().unwrap();
                    let is_finished = anim.animate(world, entity, timestamp_ms.trunc() as u32);

                    if is_finished {
                        let registry = world.resource::<AnimationTargetRegistry>().clone();
                        let registry = registry.read().unwrap();

                        if *restore_on_finish {
                            let state = animations.backup_state.remove(anim_name).unwrap();

                            for (node_name, lane) in state.state {
                                let mut node = world.entity_mut(*children.get(&node_name).unwrap());
                                let anim_node = anim.animation_by_node.get(&node_name).unwrap();
                                for (id, lane) in lane.state {
                                    let anim_lane = anim_node.animation_by_id.get(&id).unwrap();
                                    let target = registry.by_label.get(&id).unwrap();
                                    (target.restore)(
                                        anim_lane.animation_data.as_ref(),
                                        lane,
                                        &mut node,
                                    );
                                }
                            }
                        }
                        animation.data = PlaybackData::NotPlaying;
                    }
                }
            }
        }

        *world
            .get_mut::<UiLayoutAnimationController>(entity)
            .unwrap() = animations;
    }
}
