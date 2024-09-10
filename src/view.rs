use bevy::{
    core::Name,
    ecs::{
        entity::Entity,
        world::{EntityRef, EntityWorldMut, World},
    },
    hierarchy::{Children, Parent},
};

use crate::{
    animations::{PlaybackRequest, UiLayoutAnimationController},
    builtins::sublayout::SpawnedSublayout,
    render::UiNodeSettings,
};

pub struct NodeView<'a> {
    entity: EntityRef<'a>,
    world: &'a World,
}

impl<'a> NodeView<'a> {
    pub fn new(entity: Entity, world: &'a World) -> Self {
        Self {
            entity: world.entity(entity),
            world,
        }
    }

    pub fn parent(&self) -> Option<NodeView<'a>> {
        let parent = self.entity.get::<Parent>()?.get();
        let entity = self.world.entity(parent);
        // TODO: Find a better way to do this?
        entity.contains::<UiNodeSettings>().then_some(NodeView {
            entity,
            world: self.world,
        })
    }

    pub fn child(&self, name: impl AsRef<str>) -> Option<NodeView<'a>> {
        let name = name.as_ref();
        let mut current = self.entity.clone();

        'outer: for child_name in name.split('.') {
            let children = current.get::<Children>()?;
            for child in children.iter() {
                let child = self.world.entity(*child);
                let Some(name) = child.get::<Name>() else {
                    continue;
                };

                if name.as_str().ends_with(child_name) {
                    current = child;
                    continue 'outer;
                }
            }
            return None;
        }

        Some(NodeView {
            entity: current,
            world: self.world,
        })
    }

    pub fn sibling(&self, name: impl AsRef<str>) -> Option<NodeView<'a>> {
        let name = name.as_ref();
        let parent = self.parent()?;
        parent.child(name)
    }

    pub fn entity(&self) -> EntityRef<'a> {
        self.entity.clone()
    }

    pub fn world(&self) -> &'a World {
        self.world
    }

    pub fn sublayout(&self) -> Option<NodeView<'a>> {
        for child in self.entity.get::<Children>()?.iter() {
            if self.world.get::<SpawnedSublayout>(*child).is_some() {
                return Some(NodeView {
                    entity: self.world.entity(*child),
                    world: self.world,
                });
            }
        }

        None
    }
}

pub struct NodeViewMut<'a> {
    entity: Entity,
    world: &'a mut World,
}

impl<'a> NodeViewMut<'a> {
    pub fn new(entity: Entity, world: &'a mut World) -> Self {
        Self { entity, world }
    }

    pub fn from_entity(entity: EntityWorldMut<'a>) -> Self {
        Self {
            entity: entity.id(),
            world: entity.into_world_mut(),
        }
    }

    pub fn into_entity(self) -> EntityWorldMut<'a> {
        self.world.entity_mut(self.entity)
    }
}

impl NodeViewMut<'_> {
    pub fn parent(&mut self) -> Option<NodeViewMut<'_>> {
        let parent = self.world.get::<Parent>(self.entity)?.get();
        self.world
            .get::<UiNodeSettings>(parent)
            .is_some()
            .then(|| NodeViewMut {
                entity: parent,
                world: self.world,
            })
    }

    pub fn child(&mut self, name: impl AsRef<str>) -> Option<NodeViewMut<'_>> {
        let name = name.as_ref();
        let mut current = self.entity;
        'outer: for child_name in name.split('.') {
            let children = self.world.get::<Children>(current)?;
            for child in children.iter() {
                let Some(name) = self.world.get::<Name>(*child) else {
                    continue;
                };

                if name.as_str().ends_with(child_name) {
                    current = *child;
                    continue 'outer;
                }
            }
            return None;
        }

        Some(NodeViewMut {
            entity: current,
            world: self.world,
        })
    }

    pub fn sibling(&mut self, name: impl AsRef<str>) -> Option<NodeViewMut<'_>> {
        let name = name.as_ref();
        let parent = self.world.get::<Parent>(self.entity)?.get();
        let mut current = parent;
        'outer: for child_name in name.split('.') {
            let children = self.world.get::<Children>(current)?;
            for child in children.iter() {
                let Some(name) = self.world.get::<Name>(*child) else {
                    continue;
                };

                if name.as_str().ends_with(child_name) {
                    current = *child;
                    continue 'outer;
                }
            }
            return None;
        }

        Some(NodeViewMut {
            entity: current,
            world: self.world,
        })
    }

    pub fn entity(&mut self) -> EntityWorldMut<'_> {
        self.world.entity_mut(self.entity)
    }

    pub fn world(&mut self) -> &mut World {
        self.world
    }

    pub fn sublayout(&mut self) -> Option<NodeViewMut<'_>> {
        for child in self.world.get::<Children>(self.entity)?.iter() {
            if self.world.get::<SpawnedSublayout>(*child).is_some() {
                return Some(NodeViewMut {
                    entity: *child,
                    world: self.world,
                });
            }
        }

        None
    }

    #[track_caller]
    pub fn play_animation(&mut self, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.world
            .get_mut::<UiLayoutAnimationController>(self.entity)
            .expect("Failed to get UiLayoutAnimationController, is this a layout?")
            .animations
            .get_mut(name)
            .unwrap_or_else(|| panic!("Failed to play animation {name} because it is missing"))
            .requests
            .push(PlaybackRequest::Play {
                restore_on_finish: false,
            });
    }

    #[track_caller]
    pub fn stop_animation(&mut self, name: impl AsRef<str>) {
        let name = name.as_ref();
        self.world
            .get_mut::<UiLayoutAnimationController>(self.entity)
            .expect("Failed to get UiLayoutAnimationController, is this a layout?")
            .animations
            .get_mut(name)
            .unwrap_or_else(|| panic!("Failed to stop animation {name} because it is missing"))
            .requests
            .push(PlaybackRequest::Stop);
    }
}
