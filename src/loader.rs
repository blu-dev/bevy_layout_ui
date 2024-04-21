use bevy::{
    asset::{Asset, AssetLoader, AsyncReadExt},
    ecs::{entity::Entity, world::World},
    hierarchy::{BuildWorldChildren, Children, Parent},
    math::{UVec2, Vec2},
    reflect::Reflect,
    sprite::Anchor,
};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    math::{GlobalTransform, NodeSize, Transform, ZIndex},
    render::UiNodeSettings,
};

#[derive(Default)]
pub struct LayoutAssetLoader;

#[derive(Error, Debug)]
pub enum LayoutAssetLoaderError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    JSON(#[from] serde_json::Error),
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
        _load_context: &'a mut bevy::asset::LoadContext,
    ) -> bevy::utils::BoxedFuture<'a, Result<Self::Asset, Self::Error>> {
        Box::pin(async move {
            let mut bytes = vec![];
            reader.read_to_end(&mut bytes).await?;
            serde_json::from_slice(&bytes).map_err(LayoutAssetLoaderError::from)
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

#[derive(Deserialize, Serialize, Debug, Clone, Reflect)]
#[reflect(no_field_bounds)]
pub struct UiNode {
    pub position: Vec2,
    #[serde(with = "AnchorLocal")]
    pub position_anchor: Anchor,
    #[serde(with = "AnchorLocal")]
    pub parent_anchor: Anchor,
    pub size: Vec2,
    pub scale: Vec2,
    pub rotation: f32,
    pub children: Vec<UiNode>,
}

#[derive(Deserialize, Serialize, Debug, Clone, Reflect, Asset)]
pub struct Layout {
    pub resolution: UVec2,
    pub nodes: Vec<UiNode>,
}

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
            },
            ZIndex(0),
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
        let node_id = world
            .spawn((
                Transform::from_position(node.position)
                    .with_scale(node.scale)
                    .with_rotation(node.rotation)
                    .with_parent_anchor(node.parent_anchor),
                GlobalTransform::default(),
                node.position_anchor,
                NodeSize(node.size),
                UiNodeSettings {
                    target_resolution: resolution,
                },
                ZIndex(*z_index),
            ))
            .id();

        *z_index += 1;
        spawn_layout_inner(world, node_id, resolution, &node.children, z_index);
        world.entity_mut(root_entity).add_child(node_id);
    }
}

fn marshall_ui_node(world: &World, entity: Entity) -> UiNode {
    let node = world.entity(entity);

    let transform = node.get::<Transform>().unwrap();
    let anchor = node.get::<Anchor>().unwrap();
    let size = node.get::<NodeSize>().unwrap();
    let mut ui_node = UiNode {
        position: transform.position,
        scale: transform.scale,
        rotation: transform.rotation,
        parent_anchor: transform.parent_anchor,
        position_anchor: *anchor,
        size: size.0,
        children: vec![],
    };

    if let Some(children) = node.get::<Children>() {
        ui_node
            .children
            .extend(children.iter().map(|child| marshall_ui_node(world, *child)));
    }

    ui_node
}

pub fn marshall_node_tree(world: &World, root_entity: Entity) -> Layout {
    let root = world.entity(root_entity);
    if root.contains::<Parent>() {
        panic!("Cannot serialize node tree because the root entity has a parent");
    }

    let resolution = root.get::<UiNodeSettings>().unwrap().target_resolution;
    let mut nodes = vec![];
    if let Some(children) = root.get::<Children>() {
        nodes.extend(children.iter().map(|child| marshall_ui_node(world, *child)));
    }

    Layout { resolution, nodes }
}
