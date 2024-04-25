use std::{
    any::Any,
    sync::{Arc, RwLock},
};

use bevy::{
    asset::{Asset, AssetLoader, AsyncReadExt, LoadContext, VisitAssetDependencies},
    ecs::{component::Component, entity::Entity, world::World},
    hierarchy::{BuildWorldChildren, Children, Parent},
    math::{UVec2, Vec2},
    reflect::{Reflect, TypePath},
    sprite::Anchor,
    utils::intern::Interned,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    math::{GlobalTransform, NodeSize, Transform, ZIndex},
    render::{SkipNodeRender, UiNodeSettings, VertexColors},
    NodeLabel, RegisteredUiNode, RegisteredUserUiNodes, UiNodeRegistry,
};

#[derive(Default)]
pub struct LayoutAssetLoader {
    pub(crate) reader: Arc<RwLock<RegisteredUserUiNodes>>,
}

#[derive(Error, Debug)]
pub enum LayoutAssetLoaderError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    JSON(#[from] serde_json::Error),
    #[error("Node kind {0} is missing from the registry")]
    MissingNode(String),
}

fn parse_nodes(
    repr: Vec<UiNodeRepr>,
    registry: &RegisteredUserUiNodes,
    load_context: &mut LoadContext,
) -> Result<Vec<UiNode>, LayoutAssetLoaderError> {
    let mut nodes = vec![];
    for node_repr in repr {
        let children = parse_nodes(node_repr.children, registry, load_context)?;

        let label = registry
            .by_name
            .get(node_repr.node_kind.as_str())
            .ok_or_else(|| LayoutAssetLoaderError::MissingNode(node_repr.node_kind))?;

        let data = registry.by_label.get(label).unwrap();
        nodes.push(UiNode {
            attributes: node_repr.attributes,
            label: label.clone(),
            data: (data.deserialize)(node_repr.node_data, load_context)?,
            registered_ui_node: *data,
            children,
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

            let registry = self.reader.read().unwrap();

            let nodes = parse_nodes(repr.nodes, &registry, load_context)?;

            Ok(Layout {
                resolution: repr.resolution,
                nodes,
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

pub struct UiNode {
    pub attributes: UiNodeAttributes,
    pub label: Interned<dyn NodeLabel>,
    pub data: Box<dyn Any + Send + Sync + 'static>,
    pub registered_ui_node: RegisteredUiNode,
    pub children: Vec<UiNode>,
}

#[derive(Deserialize, Serialize, Debug, Clone)]
struct UiNodeRepr {
    #[serde(flatten)]
    attributes: UiNodeAttributes,
    children: Vec<UiNodeRepr>,
    node_kind: String,
    node_data: serde_value::Value,
}

#[derive(Deserialize, Serialize)]
struct LayoutRepr {
    resolution: UVec2,
    nodes: Vec<UiNodeRepr>,
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
}

#[derive(TypePath)]
pub struct Layout {
    pub resolution: UVec2,
    pub nodes: Vec<UiNode>,
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

#[derive(Component, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct DynamicNodeLabel(Interned<dyn NodeLabel>);

pub fn spawn_layout(world: &mut World, layout: &Layout) -> Entity {
    let root_node = world
        .spawn((
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
        ))
        .id();

    spawn_layout_inner(
        world,
        root_node,
        layout.resolution,
        &layout.nodes,
        &mut 1isize,
    );

    root_node
}

fn spawn_layout_inner(
    world: &mut World,
    root_entity: Entity,
    resolution: UVec2,
    nodes: &[UiNode],
    z_index: &mut isize,
) {
    for node in nodes.iter() {
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
                vertex_colors: VertexColors::default(),
                opacity: 1.0,
            },
            ZIndex(*z_index),
        ));

        (node.registered_ui_node.spawn)(node.data.as_ref(), &mut entity);

        let node_id = entity.id();

        *z_index += 1;
        spawn_layout_inner(world, node_id, resolution, &node.children, z_index);
        world.entity_mut(root_entity).add_child(node_id);
    }
}

fn marshall_ui_node(world: &World, registry: &RegisteredUserUiNodes, entity: Entity) -> UiNode {
    let node = world.entity(entity);

    let transform = node.get::<Transform>().unwrap();
    let anchor = node.get::<Anchor>().unwrap();
    let size = node.get::<NodeSize>().unwrap();
    let label = node.get::<DynamicNodeLabel>().unwrap();

    let registered_node = registry.by_label.get(&label.0).unwrap();

    let reconstructed = (registered_node.reconstruct)(node);

    let mut ui_node = UiNode {
        attributes: UiNodeAttributes {
            position: transform.position,
            scale: transform.scale,
            rotation: transform.rotation,
            parent_anchor: transform.parent_anchor,
            position_anchor: *anchor,
            size: size.0,
        },
        label: label.0,
        data: reconstructed,
        registered_ui_node: *registered_node,
        children: vec![],
    };

    if let Some(children) = node.get::<Children>() {
        ui_node.children.extend(
            children
                .iter()
                .map(|child| marshall_ui_node(world, registry, *child)),
        );
    }

    ui_node
}

pub fn marshall_node_tree(world: &World, root_entity: Entity) -> Layout {
    let registry = world.resource::<UiNodeRegistry>();
    let registry = registry.read().unwrap();
    let root = world.entity(root_entity);
    if root.contains::<Parent>() {
        panic!("Cannot serialize node tree because the root entity has a parent");
    }

    let resolution = root.get::<UiNodeSettings>().unwrap().target_resolution;
    let mut nodes = vec![];
    if let Some(children) = root.get::<Children>() {
        nodes.extend(
            children
                .iter()
                .map(|child| marshall_ui_node(world, &registry, *child)),
        );
    }

    Layout { resolution, nodes }
}

fn get_nodes_as_nodes_repr(
    nodes: &[UiNode],
    world: &World,
) -> Result<Vec<UiNodeRepr>, serde_json::Error> {
    let mut repr_nodes = Vec::with_capacity(nodes.len());
    for node in nodes.iter() {
        repr_nodes.push(UiNodeRepr {
            attributes: node.attributes.clone(),
            node_kind: node.registered_ui_node.name.to_string(),
            node_data: (node.registered_ui_node.serialize)(node.data.as_ref(), world)?,
            children: get_nodes_as_nodes_repr(&node.children, world)?,
        });
    }

    Ok(repr_nodes)
}

pub fn serialize_layout_as_json(
    layout: &Layout,
    world: &World,
) -> Result<serde_json::Value, serde_json::Error> {
    let nodes = get_nodes_as_nodes_repr(&layout.nodes, world)?;

    serde_json::to_value(LayoutRepr {
        resolution: layout.resolution,
        nodes,
    })
}
