use bevy::{ecs::system::CommandQueue, prelude::*, sprite::Anchor};
use egui::{
    Align2, CollapsingHeader, Color32, Context, FontId, LayerId, Order, Response, Sense, Stroke,
};

use crate::{
    math::{GlobalTransform, NodeSize, Transform, ZIndex},
    render::{UiNodeSettings, VertexColors},
};

use self::selectable_label::DraggableLabel;

pub mod node_ui;
pub mod selectable_label;

fn get_name(entity: Entity, world: &World) -> String {
    let entity = world.entity(entity);

    match entity.get::<Name>() {
        Some(name) => name.to_string(),
        None => format!("Entity {:?}", entity.id()),
    }
}

fn add_button(ui: &mut egui::Ui) -> Response {
    ui.horizontal(|ui| {
        ui.add_space(5.0);
        let (rect, resp) = ui.allocate_exact_size(egui::Vec2::new(30.0, 15.0), Sense::click());

        let visuals = ui.style().interact(&resp);

        ui.painter()
            .rect(rect, visuals.rounding, visuals.bg_fill, visuals.bg_stroke);

        ui.painter().text(
            rect.center(),
            Align2::CENTER_CENTER,
            "+",
            FontId::default(),
            visuals.text_color(),
        );

        resp
    })
    .inner
}

/// Displays a tree of nodes, returning one if it was selected
///
/// # Arguments
/// * `root_entity` - The entity considered the root of this tree.
/// * `world` - The world that `root_entity` is in
/// * `ui` - UI to display the tree in
pub fn display_node_tree(
    root_entity: Entity,
    world: &mut World,
    ui: &mut egui::Ui,
) -> Option<Entity> {
    let mut command_queue = CommandQueue::default();
    let mut commands = Commands::new(&mut command_queue, world);

    let root_id = ui.id();

    if let Some(dragging) = get_dragging_entity(ui.ctx(), root_id) {
        if dragging == root_entity {
            set_dragging_entity(ui.ctx(), root_id, None);
        } else {
            let name = get_name(dragging, world);

            let painter = ui
                .ctx()
                .layer_painter(LayerId::new(Order::Tooltip, "dragging_painter".into()));
            let pointer = ui.ctx().pointer_hover_pos().unwrap();
            painter.text(
                pointer,
                Align2::RIGHT_BOTTOM,
                name,
                FontId::default(),
                Color32::WHITE,
            );
        }
    }
    let mut tracker = DragTracker { location: None };

    let resp = display_node_tree_impl(root_entity, world, ui, &mut commands, root_id, &mut tracker);

    command_queue.apply(world);

    if let Some(dragging) = get_dragging_entity(ui.ctx(), root_id) {
        if ui.ctx().input(|i| i.pointer.primary_released()) {
            set_dragging_entity(ui.ctx(), root_id, None);
            let (entity, new_child_no) = tracker.location.unwrap_or((root_entity, usize::MAX));
            // Get new parent entity
            let mut parent = world.entity_mut(entity);
            // Check if parent:
            // 1. Has children
            // 2. If there are children, check if the current child is one of them
            let local_child_no = parent
                .get::<Children>()
                .and_then(|children| children.iter().position(|child| *child == dragging));
            if let Some(child_no) = local_child_no {
                parent.remove_children(&[dragging]);
                parent.insert_children(
                    if new_child_no <= child_no {
                        new_child_no
                    } else {
                        new_child_no - 1
                    },
                    &[dragging],
                );
            } else {
                parent.insert_children(new_child_no, &[dragging]);
            }
        }
    }

    resp
}

fn needs_reopen(ctx: &Context, id: egui::Id) -> Option<bool> {
    ctx.data_mut(|data| {
        std::mem::take(data.get_temp_mut_or_default::<bool>(id.with("needs_reopen")))
            .then_some(true)
    })
}

fn get_entity_openness(ctx: &Context, id: egui::Id) -> bool {
    ctx.data_mut(|data| *data.get_temp_mut_or_default::<bool>(id.with("open")))
}

fn set_entity_openness(ctx: &Context, id: egui::Id, open: bool) {
    ctx.data_mut(|data| {
        *data.get_temp_mut_or_default(id.with("open")) = open;
        if open {
            *data.get_temp_mut_or_default::<bool>(id.with("needs_reopen")) = true;
        }
    });
}

fn set_dragging_entity(ctx: &Context, id: egui::Id, entity: Option<Entity>) {
    ctx.data_mut(|data| {
        *data.get_temp_mut_or_default(id.with("root_dragging")) = entity;
    })
}

fn get_dragging_entity(ctx: &Context, id: egui::Id) -> Option<Entity> {
    ctx.data_mut(|data| *data.get_temp_mut_or_default(id.with("root_dragging")))
}

struct DragTracker {
    /// The location of the child
    location: Option<(Entity, usize)>,
}

fn display_node_tree_impl(
    root_entity: Entity,
    world: &World,
    ui: &mut egui::Ui,
    commands: &mut Commands,
    root_id: egui::Id,
    tracker: &mut DragTracker,
) -> Option<Entity> {
    let name = get_name(root_entity, world);

    let id = ui.id().with(&name);

    if get_entity_openness(ui.ctx(), id) {
        let resp = CollapsingHeader::new(&name)
            .open(needs_reopen(ui.ctx(), ui.id().with(&name)))
            .default_open(true)
            .show(ui, |ui| {
                let dragging = get_dragging_entity(ui.ctx(), root_id);
                let mut total_number_children = 0;
                let mut resp = if let Some(children) = world.get::<Children>(root_entity) {
                    children.iter().copied().fold(None, |selected, item| {
                        if dragging.is_some() && tracker.location.is_none() {
                            if let Some(hover_pos) = ui.ctx().pointer_hover_pos() {
                                if hover_pos.y < ui.cursor().min.y {
                                    tracker.location = Some((root_entity, total_number_children));
                                    ui.painter().line_segment(
                                        [
                                            ui.cursor().left_top(),
                                            ui.cursor().left_top() + egui::Vec2::new(50.0, 0.0),
                                        ],
                                        Stroke::new(2.0, Color32::WHITE),
                                    );
                                }
                            }
                        }
                        total_number_children += 1;
                        selected.or(display_node_tree_impl(
                            item, world, ui, commands, root_id, tracker,
                        ))
                    })
                } else {
                    None
                };

                if dragging.is_some() && tracker.location.is_none() {
                    if let Some(hover_pos) = ui.ctx().pointer_hover_pos() {
                        if hover_pos.y < ui.cursor().min.y {
                            tracker.location = Some((root_entity, total_number_children));
                            ui.painter().line_segment(
                                [
                                    ui.cursor().left_top(),
                                    ui.cursor().left_top() + egui::Vec2::new(50.0, 0.0),
                                ],
                                Stroke::new(2.0, Color32::WHITE),
                            );
                        }
                    }
                }

                if add_button(ui).clicked() {
                    commands.entity(root_entity).with_children(|children| {
                        resp = Some(
                            children
                                .spawn((
                                    Transform::new(),
                                    GlobalTransform::default(),
                                    Anchor::TopLeft,
                                    NodeSize(Vec2::splat(50.0)),
                                    UiNodeSettings {
                                        target_resolution: UVec2::new(1920, 1080),
                                        vertex_colors: VertexColors::default(),
                                        opacity: 1.0,
                                    },
                                    ZIndex(2),
                                ))
                                .id(),
                        );
                    });
                }

                resp
            });

        if resp.body_returned.is_none() {
            set_entity_openness(ui.ctx(), ui.id().with(&name), false);
        }

        resp.header_response
            .double_clicked()
            .then_some(root_entity)
            .or(resp.body_returned.flatten())
    } else {
        let resp = ui.add(DraggableLabel::new(false, &name));
        if resp.clicked() {
            set_entity_openness(ui.ctx(), ui.id().with(&name), true);
            Some(root_entity)
        } else if resp.drag_started() {
            set_dragging_entity(ui.ctx(), root_id, Some(root_entity));
            None
        } else {
            None
        }
    }
}
