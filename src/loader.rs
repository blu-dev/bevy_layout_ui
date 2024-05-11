use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use bevy::{
    asset::{Asset, AssetLoader, AsyncReadExt, LoadContext, VisitAssetDependencies},
    core::Name,
    ecs::{component::Component, entity::Entity, world::World},
    hierarchy::{BuildWorldChildren, Children, Parent},
    math::{UVec2, Vec2},
    prelude::{Deref, DerefMut},
    reflect::{Reflect, TypePath},
    render::view::VisibilityBundle,
    sprite::Anchor,
    utils::{intern::Interned, HashMap},
};
use indexmap::IndexMap;
use serde::{ser::Error, Deserialize, Serialize};
use thiserror::Error;

use crate::{
    animations::{
        Animation, AnimationIdLane, AnimationKeyframe, AnimationNodeLane, AnimationPlaybackState,
        AnimationTargetRegistry, EdgeInterpolation, PlaybackData, RegisteredAnimationTargets,
        UiLayoutAnimationController,
    },
    builtins::sublayout::SpawnedSublayout,
    math::{GlobalTransform, NodeSize, Transform, ZIndex},
    render::{SkipNodeRender, UiNodeSettings, VertexColors},
    user_data::{DynamicUserData, RegisteredUserData, UserDataLabel, UserDataRegistry},
    NodeLabel, RegisteredUiNode, RegisteredUserUiNodes, UiNodeRegistry,
};

#[derive(Default)]
pub struct LayoutAssetLoader {
    pub(crate) ui_node_registry: Arc<RwLock<RegisteredUserUiNodes>>,
    pub(crate) ui_animation_registry: Arc<RwLock<RegisteredAnimationTargets>>,
    pub(crate) user_data_registry: Arc<RwLock<RegisteredUserData>>,
}

#[derive(Error, Debug)]
pub enum LayoutAssetLoaderError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    JSON(#[from] serde_json::Error),
    #[error("Node kind {0} is missing from the registry")]
    MissingNode(String),
    #[error("AnimationTarget {0} is missing from the registry")]
    MissingAnimationTarget(String),
    #[error("UserData {0} is missing from the registry")]
    MissingUserData(String),
}

fn parse_animations(
    repr: IndexMap<String, UiAnimationRepr>,
    registry: &RegisteredAnimationTargets,
) -> Result<HashMap<String, Arc<RwLock<Animation>>>, LayoutAssetLoaderError> {
    let mut animations = HashMap::new();
    for (name, animation_repr) in repr {
        let mut animation = Animation {
            animation_by_node: HashMap::new(),
        };

        for (node_name, lane) in animation_repr.0 {
            let mut animation_lanes = Vec::new();
            for lane in lane.0 {
                let label = registry.by_name.get(lane.target.as_str()).ok_or_else(|| {
                    LayoutAssetLoaderError::MissingAnimationTarget(lane.target.clone())
                })?;
                let registered = registry.by_label.get(label).unwrap();
                let animation_data = (registered.deserialize)(lane.data)?;
                let starting_value = (registered.deserialize_content)(lane.starting_value)?;
                let keyframes = lane
                    .edges
                    .into_iter()
                    .map(|edge| {
                        let value = (registered.deserialize_content)(edge.value)?;
                        Ok(AnimationKeyframe {
                            timestamp_ms: edge.timestamp_ms,
                            edge_interpolation: edge.interpolation,
                            value,
                        })
                    })
                    .collect::<Result<Vec<AnimationKeyframe>, LayoutAssetLoaderError>>()?;

                animation_lanes.push(AnimationIdLane {
                    target: label.clone(),
                    animation_data,
                    starting_value,
                    keyframes,
                });
            }
            animation.animation_by_node.insert(
                node_name,
                AnimationNodeLane {
                    animation_by_id: animation_lanes,
                },
            );
        }

        animations.insert(name, Arc::new(RwLock::new(animation)));
    }

    Ok(animations)
}

fn parse_nodes(
    repr: Vec<UiNodeRepr>,
    registry: &RegisteredUserUiNodes,
    ud_registry: &RegisteredUserData,
    load_context: &mut LoadContext,
) -> Result<Vec<UiNode>, LayoutAssetLoaderError> {
    let mut nodes = vec![];
    for node_repr in repr {
        let children = parse_nodes(node_repr.children, registry, ud_registry, load_context)?;

        let label = registry
            .by_name
            .get(node_repr.node_kind.as_str())
            .ok_or_else(|| LayoutAssetLoaderError::MissingNode(node_repr.node_kind))?;

        let registered = registry.by_label.get(label).unwrap();

        let data = (registered.deserialize)(node_repr.node_data, load_context)?;

        let user_data = node_repr
            .user_data
            .into_iter()
            .map(|(string, data)| {
                let label = ud_registry
                    .by_name
                    .get(string.as_str())
                    .ok_or_else(|| LayoutAssetLoaderError::MissingUserData(string))?;

                let registered = ud_registry.by_label.get(label).unwrap();
                let data = (registered.deserialize)(data)?;
                Ok(UserDataValue {
                    data,
                    registered: *registered,
                })
            })
            .collect::<Result<Vec<_>, LayoutAssetLoaderError>>()?;

        nodes.push(UiNode {
            name: node_repr.name,
            attributes: node_repr.attributes,
            label: label.clone(),
            data,
            registered_ui_node: *registered,
            children,
            user_data,
        });
    }

    Ok(nodes)
}

impl AssetLoader for LayoutAssetLoader {
    type Asset = Layout;
    type Error = LayoutAssetLoaderError;
    type Settings = ();

    fn extensions(&self) -> &[&str] {
        &["layout.json"]
    }

    fn load<'a>(
        &'a self,
        reader: &'a mut bevy::asset::io::Reader,
        _settings: &'a Self::Settings,
        load_context: &'a mut bevy::asset::LoadContext,
    ) -> bevy::utils::BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
        Box::pin(async move {
            let mut bytes = vec![];
            reader.read_to_end(&mut bytes).await?;
            let repr: LayoutRepr =
                serde_json::from_slice(&bytes).map_err(LayoutAssetLoaderError::from)?;

            let node_reg = self.ui_node_registry.read().unwrap();
            let anim_reg = self.ui_animation_registry.read().unwrap();
            let ud_reg = self.user_data_registry.read().unwrap();

            let nodes = parse_nodes(repr.nodes, &node_reg, &ud_reg, load_context)?;
            let animations = parse_animations(repr.animations, &anim_reg)?;
            let user_data = repr
                .user_data
                .into_iter()
                .map(|(name, value)| {
                    let label = ud_reg
                        .by_name
                        .get(name.as_str())
                        .ok_or_else(|| LayoutAssetLoaderError::MissingUserData(name))?;
                    let registered = ud_reg.by_label.get(label).unwrap();
                    let value = (registered.deserialize)(value)?;
                    Ok(UserDataValue {
                        data: value,
                        registered: *registered,
                    })
                })
                .collect::<Result<_, LayoutAssetLoaderError>>()?;

            Ok(Layout {
                resolution: repr.resolution,
                nodes,
                animations,
                user_data,
            })
        })
    }
}

#[derive(Deserialize, Serialize)]
#[serde(remote = "bevy::sprite::Anchor")]
enum AnchorLocal {
    TopLeft,
    TopCenter,
    TopRight,
    CenterLeft,
    Center,
    CenterRight,
    BottomLeft,
    BottomCenter,
    BottomRight,
    Custom(Vec2),
}

pub struct UserDataValue {
    pub data: Box<dyn Any + Send + Sync + 'static>,
    pub registered: DynamicUserData,
}

pub struct UiNode {
    pub name: String,
    pub attributes: UiNodeAttributes,
    pub label: Interned<dyn NodeLabel>,
    pub data: Box<dyn Any + Send + Sync + 'static>,
    pub registered_ui_node: RegisteredUiNode,
    pub children: Vec<UiNode>,
    pub user_data: Vec<UserDataValue>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiNodeRepr {
    name: String,
    #[serde(flatten)]
    attributes: UiNodeAttributes,
    children: Vec<UiNodeRepr>,
    node_kind: String,
    node_data: serde_value::Value,
    #[serde(default)]
    user_data: IndexMap<String, serde_value::Value>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiNodeAnimationLaneEdge {
    timestamp_ms: u32,
    interpolation: EdgeInterpolation,
    value: serde_value::Value,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiAnimationLane {
    target: String,
    data: serde_value::Value,
    starting_value: serde_value::Value,
    edges: Vec<UiNodeAnimationLaneEdge>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiNodeAnimationLane(Vec<UiAnimationLane>);

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiAnimationRepr(HashMap<String, UiNodeAnimationLane>);

#[derive(Deserialize, Serialize)]
struct LayoutRepr {
    resolution: UVec2,
    nodes: Vec<UiNodeRepr>,
    animations: IndexMap<String, UiAnimationRepr>,
    user_data: IndexMap<String, serde_value::Value>,
}

#[derive(Deserialize, Serialize, Debug, Clone, Reflect)]
#[reflect(no_field_bounds)]
pub struct UiNodeAttributes {
    pub position: Vec2,
    #[serde(with = "AnchorLocal")]
    pub position_anchor: Anchor,
    #[serde(with = "AnchorLocal")]
    pub parent_anchor: Anchor,
    pub size: Vec2,
    pub scale: Vec2,
    pub rotation: f32,
    #[serde(default)]
    pub vertex_colors: VertexColors,
}

#[derive(TypePath)]
pub struct Layout {
    pub resolution: UVec2,
    pub nodes: Vec<UiNode>,
    pub animations: HashMap<String, Arc<RwLock<Animation>>>,
    pub user_data: Vec<UserDataValue>,
}

impl Asset for Layout {}

impl VisitAssetDependencies for Layout {
    fn visit_dependencies(&self, visit: &mut impl FnMut(bevy::asset::UntypedAssetId)) {
        fn visit_nodes(nodes: &[UiNode], visit: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {
            for node in nodes.iter() {
                (node.registered_ui_node.visit_asset_dependencies)(node.data.as_ref(), visit);

                visit_nodes(&node.children, visit);
            }
        }

        visit_nodes(&self.nodes, visit);
    }
}

#[derive(Component, Clone, Deref, DerefMut, Default)]
pub struct NodeUserDataLabels(Vec<Interned<dyn UserDataLabel>>);

#[derive(Component, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DynamicNodeLabel(pub(crate) Interned<dyn NodeLabel>);

pub fn spawn_layout(world: &mut World, layout: &Layout) -> Entity {
    let mut user_data_cache = Vec::new();

    let root_node = world
        .spawn((
            Name::new("root"),
            Transform::from_position(Vec2::ZERO)
                .with_scale(Vec2::ONE)
                .with_rotation(0.0)
                .with_parent_anchor(Anchor::TopLeft),
            GlobalTransform::default(),
            Anchor::TopLeft,
            NodeSize(layout.resolution.as_vec2()),
            UiNodeSettings {
                target_resolution: layout.resolution,
                vertex_colors: VertexColors::default(),
                opacity: 1.0,
            },
            ZIndex(0),
            SkipNodeRender,
            UiLayoutAnimationController {
                animations: layout
                    .animations
                    .iter()
                    .map(|(name, anim)| {
                        (
                            name.clone(),
                            AnimationPlaybackState {
                                animation: anim.clone(),
                                data: PlaybackData::NotPlaying,
                                requests: vec![],
                            },
                        )
                    })
                    .collect(),
                backup_state: HashMap::new(),
            },
            VisibilityBundle::default(),
            NodeUserDataLabels(vec![]),
        ))
        .id();

    user_data_cache.push((root_node, layout.user_data.as_slice()));

    spawn_layout_inner(
        world,
        root_node,
        layout.resolution,
        &layout.nodes,
        &mut 1isize,
        Some("root".to_string()),
        &mut user_data_cache,
    );

    apply_user_data(world, user_data_cache.into_iter().rev());

    root_node
}

fn spawn_layout_inner<'a>(
    world: &mut World,
    root_entity: Entity,
    resolution: UVec2,
    nodes: &'a [UiNode],
    z_index: &mut isize,
    parent_node: Option<String>,
    user_data_cache: &mut Vec<(Entity, &'a [UserDataValue])>,
) {
    for node in nodes.iter() {
        let node_name = match parent_node.as_ref() {
            Some(name) => format!("{name}.{}", node.name),
            None => node.name.clone(),
        };

        let mut entity = world.spawn((
            Transform::from_position(node.attributes.position)
                .with_scale(node.attributes.scale)
                .with_rotation(node.attributes.rotation)
                .with_parent_anchor(node.attributes.parent_anchor),
            GlobalTransform::default(),
            node.attributes.position_anchor,
            NodeSize(node.attributes.size),
            UiNodeSettings {
                target_resolution: resolution,
                vertex_colors: node.attributes.vertex_colors,
                opacity: 1.0,
            },
            ZIndex(*z_index),
            DynamicNodeLabel(node.label),
            Name::new(node_name.clone()),
            VisibilityBundle::default(),
            NodeUserDataLabels(vec![]),
        ));

        user_data_cache.push((entity.id(), node.user_data.as_slice()));

        (node.registered_ui_node.spawn)(node.data.as_ref(), &mut entity);

        let node_id = entity.id();

        *z_index += 1;
        spawn_layout_inner(
            world,
            node_id,
            resolution,
            &node.children,
            z_index,
            Some(node_name),
            user_data_cache,
        );

        world.entity_mut(root_entity).add_child(node_id);
    }
}

fn apply_user_data<'a>(
    world: &mut World,
    user_data: impl IntoIterator<Item = (Entity, &'a [UserDataValue])>,
) {
    for (entity, user_data) in user_data {
        let mut entity = world.entity_mut(entity);
        for ud in user_data.iter() {
            (ud.registered.initialize)(ud.data.as_ref(), &mut entity);
        }

        entity
            .get_mut::<NodeUserDataLabels>()
            .unwrap()
            .extend(user_data.iter().map(|reg| reg.registered.label));
    }
}

fn marshall_ui_node(
    world: &World,
    registry: &RegisteredUserUiNodes,
    ud_registry: &RegisteredUserData,
    entity: Entity,
) -> UiNode {
    let node = world.entity(entity);

    let transform = node.get::<Transform>().unwrap();
    let anchor = node.get::<Anchor>().unwrap();
    let size = node.get::<NodeSize>().unwrap();
    let label = node.get::<DynamicNodeLabel>().unwrap();
    let ud = node.get::<NodeUserDataLabels>().unwrap();

    let registered_node = registry.by_label.get(&label.0).unwrap();

    let reconstructed = (registered_node.reconstruct)(node);

    let user_data =
        ud.0.iter()
            .map(|label| {
                let registered = ud_registry.by_label.get(label).unwrap();
                let data = (registered.reconstruct)(node.clone());
                UserDataValue {
                    data,
                    registered: *registered,
                }
            })
            .collect::<Vec<_>>();

    let mut ui_node = UiNode {
        name: node
            .get::<Name>()
            .unwrap()
            .as_str()
            .split('.')
            .last()
            .unwrap()
            .to_string(),
        attributes: UiNodeAttributes {
            position: transform.position,
            scale: transform.scale,
            rotation: transform.rotation,
            parent_anchor: transform.parent_anchor,
            position_anchor: *anchor,
            size: size.0,
            vertex_colors: node.get::<UiNodeSettings>().unwrap().vertex_colors,
        },
        label: label.0,
        data: reconstructed,
        registered_ui_node: *registered_node,
        user_data,
        children: vec![],
    };

    if let Some(children) = node.get::<Children>() {
        ui_node.children.extend(children.iter().filter_map(|child| {
            world
                .get::<SpawnedSublayout>(*child)
                .is_none()
                .then(|| marshall_ui_node(world, registry, ud_registry, *child))
        }));
    }

    ui_node
}

pub fn marshall_node_tree(world: &World, root_entity: Entity) -> Layout {
    let registry = world.resource::<UiNodeRegistry>();
    let registry = registry.read().unwrap();
    let ud_registry = world.resource::<UserDataRegistry>();
    let ud_registry = ud_registry.read().unwrap();
    let root = world.entity(root_entity);
    if root.contains::<Parent>() {
        panic!("Cannot serialize node tree because the root entity has a parent");
    }

    let resolution = root.get::<UiNodeSettings>().unwrap().target_resolution;
    let mut nodes = vec![];
    if let Some(children) = root.get::<Children>() {
        nodes.extend(children.iter().filter_map(|child| {
            world
                .get::<SpawnedSublayout>(*child)
                .is_none()
                .then(|| marshall_ui_node(world, &registry, &ud_registry, *child))
        }));
    }
    let ud = root.get::<NodeUserDataLabels>().unwrap();
    let user_data =
        ud.0.iter()
            .map(|label| {
                let registered = ud_registry.by_label.get(label).unwrap();
                let data = (registered.reconstruct)(root.clone());
                UserDataValue {
                    data,
                    registered: *registered,
                }
            })
            .collect::<Vec<_>>();

    Layout {
        resolution,
        nodes,
        animations: root
            .get::<UiLayoutAnimationController>()
            .unwrap()
            .animations
            .iter()
            .map(|(name, state)| (name.clone(), state.animation.clone()))
            .collect(),
        user_data,
    }
}

fn get_nodes_as_nodes_repr(
    nodes: &[UiNode],
    world: &World,
) -> Result<Vec<UiNodeRepr>, serde_json::Error> {
    let mut repr_nodes = Vec::with_capacity(nodes.len());
    for node in nodes.iter() {
        repr_nodes.push(UiNodeRepr {
            name: node.name.clone(),
            attributes: node.attributes.clone(),
            node_kind: node.registered_ui_node.name.to_string(),
            node_data: (node.registered_ui_node.serialize)(node.data.as_ref(), world)?,
            children: get_nodes_as_nodes_repr(&node.children, world)?,
            user_data: node
                .user_data
                .iter()
                .map(|ud| {
                    Ok((
                        ud.registered.name.to_string(),
                        (ud.registered.serialize)(ud.data.as_ref())?,
                    ))
                })
                .collect::<Result<_, serde_json::Error>>()?,
        });
    }

    Ok(repr_nodes)
}

pub fn serialize_layout_as_json(
    layout: &Layout,
    world: &World,
) -> Result<serde_json::Value, serde_json::Error> {
    let nodes = get_nodes_as_nodes_repr(&layout.nodes, world)?;
    let target_reg = world.resource::<AnimationTargetRegistry>().clone();
    let target_reg = target_reg.read().unwrap();

    let mut animations = IndexMap::new();

    for (anim_name, anim) in layout.animations.iter() {
        let anim = anim.read().unwrap();
        let mut lanes_by_node_name = HashMap::new();
        for (node_name, lane) in anim.animation_by_node.iter() {
            let mut lanes_by_target_name = Vec::new();
            for lane in lane.animation_by_id.iter() {
                let target = target_reg.by_label.get(&lane.target).ok_or_else(|| {
                    serde_json::Error::custom("Failed to get registered AnimationTarget by ID")
                })?;
                lanes_by_target_name.push(UiAnimationLane {
                    target: target.name.to_string(),
                    data: (target.serialize)(lane.animation_data.as_ref(), world)?,
                    starting_value: (target.serialize_content)(lane.starting_value.as_ref())?,
                    edges: lane
                        .keyframes
                        .iter()
                        .map(|kf| {
                            Ok(UiNodeAnimationLaneEdge {
                                timestamp_ms: kf.timestamp_ms,
                                interpolation: kf.edge_interpolation.clone(),
                                value: (target.serialize_content)(kf.value.as_ref())?,
                            })
                        })
                        .collect::<Result<Vec<_>, serde_json::Error>>()?,
                });
            }
            lanes_by_node_name.insert(node_name.clone(), UiNodeAnimationLane(lanes_by_target_name));
        }
        animations.insert(anim_name.to_string(), UiAnimationRepr(lanes_by_node_name));
    }

    serde_json::to_value(LayoutRepr {
        resolution: layout.resolution,
        nodes,
        animations,
        user_data: layout
            .user_data
            .iter()
            .map(|ud| {
                Ok((
                    ud.registered.name.to_string(),
                    (ud.registered.serialize)(ud.data.as_ref())?,
                ))
            })
            .collect::<Result<_, serde_json::Error>>()?,
    })
}
