use std::path::PathBuf;

use bevy::{
    asset::{LoadContext, RecursiveDependencyLoadState},
    prelude::*,
    utils::intern::Interned,
};
use serde::{Deserialize, Serialize};

#[cfg(feature = "editor-ui")]
use crate::EditorUiNode;

use crate::{loader::Layout, render::SkipNodeRender, NodeLabel, UiNodeApp, UserUiNode};

pub struct SublayoutNodePlugin;

impl Plugin for SublayoutNodePlugin {
    fn build(&self, app: &mut App) {
        app.register_user_ui_node::<SublayoutNode>();

        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<SublayoutNode>();
        }
    }
}

#[derive(Deserialize, Serialize)]
pub struct SublayoutNodeData {
    pub layout_path: PathBuf,
}

#[derive(Default)]
pub struct SublayoutNode {
    pub layout: Handle<Layout>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SublayoutNodeLabel;

decl_node_label!(SublayoutNodeLabel);

#[derive(Component, Debug, Copy, Clone)]
pub struct SpawnedSublayout;

impl UserUiNode for SublayoutNode {
    const NAME: &'static str = "Sublayout";

    type Serde = SublayoutNodeData;

    fn label() -> Interned<dyn NodeLabel> {
        SublayoutNodeLabel.intern()
    }

    fn deserialize<E: serde::de::Error>(
        serde: Self::Serde,
        load_context: &mut LoadContext,
    ) -> Result<Self, E> {
        Ok(Self {
            layout: load_context.load(serde.layout_path),
        })
    }

    fn serialize<E: serde::ser::Error>(&self, world: &World) -> Result<Self::Serde, E> {
        let path = world
            .resource::<AssetServer>()
            .get_path(self.layout.id())
            .ok_or_else(|| E::custom("Layout handle is not represented by the filesystem"))?;

        Ok(SublayoutNodeData {
            layout_path: path.path().to_path_buf(),
        })
    }

    fn reconstruct(entity: EntityRef) -> Self {
        let handle = entity.get::<Handle<Layout>>().unwrap();
        Self {
            layout: handle.clone(),
        }
    }

    fn spawn(&self, entity: &mut EntityWorldMut) {
        entity.insert((self.layout.clone(), SkipNodeRender));
        if self.layout.id() == AssetId::default() {
            return;
        }
        let layout_root = entity.world_scope(|world| {
            let layout = world
                .resource_mut::<Assets<Layout>>()
                .remove(&self.layout)
                .unwrap();
            let entity = crate::loader::spawn_layout(world, &layout);

            world
                .resource_mut::<Assets<Layout>>()
                .insert(self.layout.id(), layout);

            world.entity_mut(entity).insert(SpawnedSublayout);
            entity
        });

        entity.add_child(layout_root);
    }

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {
        visit_fn(self.layout.id().untyped())
    }
}

#[cfg(feature = "editor-ui")]
impl EditorUiNode for SublayoutNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        #[allow(dead_code)]
        #[derive(Component)]
        struct TemporaryHandle(Handle<Layout>);

        let id = egui::Id::new("sublayout-node-editor").with(entity.id());
        let current_handle = entity.get::<Handle<Layout>>().unwrap().id();
        let mut current_path = ui.data_mut(|data| {
            data.get_temp_mut_or_insert_with(id, || {
                entity
                    .world()
                    .resource::<AssetServer>()
                    .get_path(current_handle)
                    .map(|path| path.path().display().to_string())
                    .unwrap_or_default()
            })
            .clone()
        });

        ui.horizontal(|ui| {
            ui.label("Layout Path");
            ui.text_edit_singleline(&mut current_path);
        });

        let asset_server = entity.world().resource::<AssetServer>();
        let handle: Handle<Layout> = asset_server.load(&current_path);
        ui.data_mut(|data| {
            data.insert_temp(id, current_path);
        });

        if handle.id() != current_handle {
            match asset_server.recursive_dependency_load_state(handle.id()) {
                RecursiveDependencyLoadState::Loaded => {
                    Self::cleanup(entity);
                    entity.remove::<TemporaryHandle>();
                    let layout_root = entity.world_scope(|world| {
                        let layout = world
                            .resource_mut::<Assets<Layout>>()
                            .remove(handle.id())
                            .unwrap();
                        let entity = crate::loader::spawn_layout(world, &layout);

                        world
                            .resource_mut::<Assets<Layout>>()
                            .insert(handle.id(), layout);

                        world.entity_mut(entity).insert(SpawnedSublayout);
                        entity
                    });
                    entity.insert(handle).add_child(layout_root);
                }
                _ => {
                    entity.insert(TemporaryHandle(handle));
                }
            }
        }
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Handle<Layout>>();

        if entity.contains::<Children>() {
            let children = entity
                .get::<Children>()
                .unwrap()
                .iter()
                .copied()
                .collect::<Vec<_>>();
            entity.world_scope(|world| {
                for child in children {
                    let entity = world.entity_mut(child);
                    if entity.contains::<SpawnedSublayout>() {
                        entity.despawn_recursive();
                    }
                }
            });
        }
    }
}
