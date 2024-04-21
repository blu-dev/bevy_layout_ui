//! This module contains maths required for performing and propagating positioning information for nodes.

use bevy::{ecs::query::QueryData, math::Affine2, prelude::*, sprite::Anchor};

#[derive(Component, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Reflect)]
pub struct ZIndex(pub isize);

#[derive(Debug, Copy, Clone, Component, Reflect, Deref, DerefMut)]
pub struct NodeSize(pub Vec2);

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct BoundingBox {
    inner: Rect,
}

impl BoundingBox {
    pub const fn top_left(&self) -> Vec2 {
        self.inner.min
    }

    pub const fn top_right(&self) -> Vec2 {
        Vec2::new(self.inner.max.x, self.inner.min.y)
    }

    pub const fn bottom_right(&self) -> Vec2 {
        self.inner.max
    }

    pub const fn bottom_left(&self) -> Vec2 {
        Vec2::new(self.inner.min.x, self.inner.max.y)
    }
}

#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct NonAxisAlignedBoundingBox {
    top_left: Vec2,
    top_right: Vec2,
    bottom_right: Vec2,
    bottom_left: Vec2,
}

impl NonAxisAlignedBoundingBox {
    pub const fn top_left(&self) -> Vec2 {
        self.top_left
    }

    pub const fn top_right(&self) -> Vec2 {
        self.top_right
    }

    pub const fn bottom_right(&self) -> Vec2 {
        self.bottom_right
    }

    pub const fn bottom_left(&self) -> Vec2 {
        self.bottom_left
    }
}

/// Mirror of bevy's [`Transform`] type, adjusted for the limited two dimensions that UI nodes are going to exist in
///
/// Just like bevy's transform, this transform is going to have it's position, scale, and rotation all propagated down
/// to any child nodes
#[derive(Debug, Copy, Clone, Component, Reflect)]
pub struct Transform {
    pub position: Vec2,
    pub scale: Vec2,
    pub rotation: f32,
    pub parent_anchor: Anchor,
}

#[derive(Debug, Copy, Clone, Component, Reflect, Default)]
pub struct GlobalTransform(Affine2);

impl Transform {
    pub const fn new() -> Self {
        Self {
            position: Vec2::ZERO,
            scale: Vec2::ONE,
            rotation: 0.0,
            parent_anchor: Anchor::TopLeft,
        }
    }

    pub const fn from_xy(x: f32, y: f32) -> Self {
        Self {
            position: Vec2::new(x, y),
            ..Self::new()
        }
    }

    pub const fn from_position(pos: Vec2) -> Self {
        Self {
            position: pos,
            ..Self::new()
        }
    }

    pub const fn from_scale(scale: Vec2) -> Self {
        Self {
            scale,
            ..Self::new()
        }
    }

    pub const fn from_rotation(degrees: f32) -> Self {
        Self {
            rotation: degrees,
            ..Self::new()
        }
    }

    pub const fn with_position(self, position: Vec2) -> Self {
        Self { position, ..self }
    }

    pub const fn with_scale(self, scale: Vec2) -> Self {
        Self { scale, ..self }
    }

    pub const fn with_rotation(self, degrees: f32) -> Self {
        Self {
            rotation: degrees,
            ..self
        }
    }

    pub const fn with_parent_anchor(self, anchor: Anchor) -> Self {
        Self {
            parent_anchor: anchor,
            ..self
        }
    }

    pub fn affine(&self) -> Affine2 {
        Affine2::from_scale_angle_translation(self.scale, self.rotation.to_radians(), self.position)
    }
}

impl Default for Transform {
    fn default() -> Self {
        Self::new()
    }
}

impl GlobalTransform {
    pub fn mul_transform(
        &self,
        self_size: &NodeSize,
        self_anchor: &Anchor,
        transform: &Transform,
    ) -> Self {
        let actual_position = Vec2::new(1.0, -1.0)
            * (transform.parent_anchor.as_vec() - self_anchor.as_vec())
            * self_size.0;
        let mut affine = transform.affine();
        affine.translation += actual_position;
        Self(self.0 * affine)
    }

    pub fn affine(&self) -> Affine2 {
        self.0
    }
}

impl From<Transform> for GlobalTransform {
    fn from(value: Transform) -> Self {
        Self(Affine2::from_scale_angle_translation(
            value.scale,
            value.rotation.to_radians(),
            value.position,
        ))
    }
}

#[derive(QueryData)]
#[query_data(mutable)]
pub struct ComputeBoundingBoxData {
    entity: Entity,
    global_transform: &'static GlobalTransform,
    node_size: &'static NodeSize,
    bounding_box: Option<&'static mut BoundingBox>,
    non_aa_bounding_box: Option<&'static mut NonAxisAlignedBoundingBox>,
}

pub fn compute_bounding_box(
    commands: ParallelCommands,
    mut query: Query<
        ComputeBoundingBoxData,
        Or<(
            Changed<GlobalTransform>,
            Changed<NodeSize>,
            Without<BoundingBox>,
            Without<NonAxisAlignedBoundingBox>,
        )>,
    >,
) {
    query.par_iter_mut().for_each(|data| {
        let half_extent = data.node_size.0 / 2.0;
        let affine = data.global_transform.affine();
        let bbox = NonAxisAlignedBoundingBox {
            top_left: affine.transform_point2(-half_extent),
            top_right: affine.transform_point2(Vec2::new(half_extent.x, -half_extent.y)),
            bottom_right: affine.transform_point2(half_extent),
            bottom_left: affine.transform_point2(Vec2::new(-half_extent.x, half_extent.y)),
        };

        let (min, max) = [
            bbox.top_left,
            bbox.top_right,
            bbox.bottom_left,
            bbox.bottom_right,
        ]
        .into_iter()
        .fold((Vec2::MAX, Vec2::MIN), |(min, max), point| {
            (min.min(point), max.max(point))
        });

        let aabb = BoundingBox {
            inner: Rect::from_corners(min, max),
        };

        match (data.bounding_box, data.non_aa_bounding_box) {
            (Some(mut bounding_box), Some(mut non_aa)) => {
                *bounding_box = aabb;
                *non_aa = bbox;
            }
            (Some(mut bounding_box), None) => {
                *bounding_box = aabb;
                commands.command_scope(|mut cmd| {
                    cmd.entity(data.entity).insert(bbox);
                });
            }
            (None, Some(mut non_aa)) => {
                *non_aa = bbox;
                commands.command_scope(|mut cmd| {
                    cmd.entity(data.entity).insert(aabb);
                });
            }
            (None, None) => {
                commands.command_scope(|mut cmd| {
                    cmd.entity(data.entity).insert((bbox, aabb));
                });
            }
        }
    });
}

// NOTE: The following systems are literally copy/pasted from bevy's transform systems

/// Update [`GlobalTransform`] component of entities that aren't in the hierarchy
///
/// Third party plugins should ensure that this is used in concert with [`propagate_transforms`].
pub fn sync_simple_transforms(
    mut query: ParamSet<(
        Query<
            (&Transform, &mut GlobalTransform),
            (
                Or<(Changed<Transform>, Added<GlobalTransform>)>,
                Without<Parent>,
                Without<Children>,
            ),
        >,
        Query<(Ref<Transform>, &mut GlobalTransform), (Without<Parent>, Without<Children>)>,
    )>,
    mut orphaned: RemovedComponents<Parent>,
) {
    // Update changed entities.
    query
        .p0()
        .par_iter_mut()
        .for_each(|(transform, mut global_transform)| {
            *global_transform = GlobalTransform::from(*transform);
        });
    // Update orphaned entities.
    let mut query = query.p1();
    let mut iter = query.iter_many_mut(orphaned.read());
    while let Some((transform, mut global_transform)) = iter.fetch_next() {
        if !transform.is_changed() && !global_transform.is_added() {
            *global_transform = GlobalTransform::from(*transform);
        }
    }
}

/// Update [`GlobalTransform`] component of entities based on entity hierarchy and
/// [`Transform`] component.
///
/// Third party plugins should ensure that this is used in concert with [`sync_simple_transforms`].
pub fn propagate_transforms(
    mut root_query: Query<
        (
            Entity,
            &Children,
            Ref<Transform>,
            &mut GlobalTransform,
            Ref<Anchor>,
            Ref<NodeSize>,
        ),
        Without<Parent>,
    >,
    mut orphaned: RemovedComponents<Parent>,
    transform_query: Query<
        (
            Ref<Transform>,
            &mut GlobalTransform,
            Ref<Anchor>,
            Ref<NodeSize>,
            Option<&Children>,
        ),
        With<Parent>,
    >,
    parent_query: Query<(Entity, Ref<Parent>)>,
    mut orphaned_entities: Local<Vec<Entity>>,
) {
    orphaned_entities.clear();
    orphaned_entities.extend(orphaned.read());
    orphaned_entities.sort_unstable();
    root_query.par_iter_mut().for_each(
        |(entity, children, transform, mut global_transform, anchor, node_size)| {
            let mut changed = transform.is_changed() || global_transform.is_added() || orphaned_entities.binary_search(&entity).is_ok();
            if changed {
                *global_transform = GlobalTransform::from(*transform);
            }

            for (child, actual_parent) in parent_query.iter_many(children) {
                assert_eq!(
                    actual_parent.get(), entity,
                    "Malformed hierarchy. This probably means that your hierarchy has been improperly maintained, or contains a cycle"
                );

                changed |= anchor.is_changed() || node_size.is_changed();

                // SAFETY:
                // - `child` must have consistent parentage, or the above assertion would panic.
                // Since `child` is parented to a root entity, the entire hierarchy leading to it is consistent.
                // - We may operate as if all descendants are consistent, since `propagate_recursive` will panic before
                //   continuing to propagate if it encounters an entity with inconsistent parentage.
                // - Since each root entity is unique and the hierarchy is consistent and forest-like,
                //   other root entities' `propagate_recursive` calls will not conflict with this one.
                // - Since this is the only place where `transform_query` gets used, there will be no conflicting fetches elsewhere.
                #[allow(unsafe_code)]
                unsafe {
                    propagate_recursive(
                        &global_transform,
                        &anchor,
                        &node_size,
                        &transform_query,
                        &parent_query,
                        child,
                        changed || actual_parent.is_changed(),
                    );
                }
            }
        },
    );
}

/// Recursively propagates the transforms for `entity` and all of its descendants.
///
/// # Panics
///
/// If `entity`'s descendants have a malformed hierarchy, this function will panic occur before propagating
/// the transforms of any malformed entities and their descendants.
///
/// # Safety
///
/// - While this function is running, `transform_query` must not have any fetches for `entity`,
/// nor any of its descendants.
/// - The caller must ensure that the hierarchy leading to `entity`
/// is well-formed and must remain as a tree or a forest. Each entity must have at most one parent.
#[allow(unsafe_code)]
unsafe fn propagate_recursive(
    parent_transform: &GlobalTransform,
    parent_anchor: &Anchor,
    parent_node_size: &NodeSize,
    transform_query: &Query<
        (
            Ref<Transform>,
            &mut GlobalTransform,
            Ref<Anchor>,
            Ref<NodeSize>,
            Option<&Children>,
        ),
        With<Parent>,
    >,
    parent_query: &Query<(Entity, Ref<Parent>)>,
    entity: Entity,
    mut changed: bool,
) {
    let (global_matrix, anchor, node_size, children) = {
        let Ok((transform, mut global_transform, anchor, node_size, children)) =
            // SAFETY: This call cannot create aliased mutable references.
            //   - The top level iteration parallelizes on the roots of the hierarchy.
            //   - The caller ensures that each child has one and only one unique parent throughout the entire
            //     hierarchy.
            //
            // For example, consider the following malformed hierarchy:
            //
            //     A
            //   /   \
            //  B     C
            //   \   /
            //     D
            //
            // D has two parents, B and C. If the propagation passes through C, but the Parent component on D points to B,
            // the above check will panic as the origin parent does match the recorded parent.
            //
            // Also consider the following case, where A and B are roots:
            //
            //  A       B
            //   \     /
            //    C   D
            //     \ /
            //      E
            //
            // Even if these A and B start two separate tasks running in parallel, one of them will panic before attempting
            // to mutably access E.
            (unsafe { transform_query.get_unchecked(entity) }) else {
                return;
            };

        changed |= transform.is_changed() || global_transform.is_added();
        if changed {
            *global_transform =
                parent_transform.mul_transform(&parent_node_size, &parent_anchor, &transform);
        }

        changed |= anchor.is_changed() || node_size.is_changed();
        (*global_transform, *anchor, *node_size, children)
    };

    let Some(children) = children else { return };
    for (child, actual_parent) in parent_query.iter_many(children) {
        assert_eq!(
            actual_parent.get(), entity,
            "Malformed hierarchy. This probably means that your hierarchy has been improperly maintained, or contains a cycle"
        );
        // SAFETY: The caller guarantees that `transform_query` will not be fetched
        // for any descendants of `entity`, so it is safe to call `propagate_recursive` for each child.
        //
        // The above assertion ensures that each child has one and only one unique parent throughout the
        // entire hierarchy.
        unsafe {
            propagate_recursive(
                &global_matrix,
                &anchor,
                &node_size,
                transform_query,
                parent_query,
                child,
                changed || actual_parent.is_changed(),
            );
        }
    }
}
