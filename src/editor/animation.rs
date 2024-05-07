use bevy::{prelude::*, utils::intern::Interned};
use egui::{Align, DragValue, SelectableLabel};

use crate::{
    animations::{
        collect_nodes_by_name, AnimationIdLane, AnimationKeyframe, AnimationNodeLane,
        AnimationTargetLabel, AnimationTargetRegistry, EdgeInterpolation,
        EditorAnimationTargetRegistry, UiLayoutAnimationController,
    },
    loader::DynamicNodeLabel,
};

use super::add_button;

enum ColumnWeight {
    Pixels(f32),
    Relative(f32),
}

fn show_columns(ui: &mut egui::Ui, weights: &[ColumnWeight], f: impl FnOnce(&mut [egui::Ui])) {
    let total_relative = weights
        .iter()
        .filter_map(|weight| match weight {
            ColumnWeight::Relative(relative) => Some(*relative),
            _ => None,
        })
        .sum::<f32>();

    // TODO(emilk): ensure there is space
    let spacing = ui.spacing().item_spacing.x;
    let top_left = ui.cursor().min;

    let mut columns = Vec::with_capacity(weights.len());
    let available = ui.available_width()
        - weights
            .iter()
            .filter_map(|weight| match weight {
                ColumnWeight::Pixels(pix) => Some(*pix),
                _ => None,
            })
            .sum::<f32>();

    for idx in 0..weights.len() {
        let offset = weights
            .iter()
            .take(idx)
            .map(|weight| match weight {
                ColumnWeight::Relative(rel) => (*rel / total_relative) * available,
                ColumnWeight::Pixels(pix) => *pix,
            })
            .sum::<f32>()
            + idx as f32 * spacing;
        let width = match &weights[idx] {
            ColumnWeight::Pixels(pix) => *pix,
            ColumnWeight::Relative(rel) => (*rel / total_relative) * available,
        };
        let pos = top_left + egui::Vec2::new(offset, 0.0);
        let child_rect = egui::Rect::from_min_max(
            pos,
            egui::Pos2::new(pos.x + width, ui.max_rect().right_bottom().y),
        );
        let mut column_ui = ui.child_ui_with_id_source(
            child_rect,
            egui::Layout::top_down_justified(Align::LEFT),
            idx,
        );
        column_ui.set_width(width);
        columns.push(column_ui);
    }

    f(&mut columns[..]);

    let mut max_width = 0.0f32;
    let mut max_height = 0.0f32;
    for (column, weight) in columns.iter().zip(weights) {
        max_width = match weight {
            ColumnWeight::Pixels(_) => max_width.max(column.min_rect().width()),
            ColumnWeight::Relative(rel) => {
                max_width.max(column.min_rect().width() * total_relative / *rel)
            }
        };
        max_height = column.min_size().y.max(max_height);
    }

    // Make sure we fit everything next frame:
    let size = egui::Vec2::new(ui.available_width().max(max_width), max_height);
    ui.advance_cursor_after_rect(egui::Rect::from_min_size(top_left, size));
}

#[derive(Default, Clone)]
struct AnimationEditState {
    selected_node: Option<String>,
    selected_target: Option<usize>,
    selected_keyframe: Option<usize>,
}

impl AnimationEditState {
    fn get(ui: &mut egui::Ui) -> Self {
        let id = ui.id().with("animation-edit-state");
        ui.data_mut(|data| data.get_temp_mut_or_default::<Self>(id).clone())
    }

    fn set(&self, ui: &mut egui::Ui) {
        let id = ui.id().with("animation-edit-state");
        ui.data_mut(|data| data.insert_temp(id, self.clone()));
    }
}

pub fn show_animation_editor(
    root_entity: Entity,
    world: &mut World,
    ui: &mut egui::Ui,
    index: usize,
) {
    let children = collect_nodes_by_name(root_entity, world);

    let registry = world.resource::<AnimationTargetRegistry>().clone();
    let registry = registry.read().unwrap();
    let editor_registry = world.resource::<EditorAnimationTargetRegistry>().clone();
    let editor_registry = editor_registry.read().unwrap();
    let mut entity = world.entity_mut(root_entity);
    let controller = entity.get::<UiLayoutAnimationController>().unwrap();
    let animation = controller
        .animations
        .get_index(index)
        .unwrap()
        .1
        .animation
        .clone();

    let mut animation = animation.write().unwrap();
    let mut node_names = animation
        .animation_by_node
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    node_names.sort();

    let mut state = AnimationEditState::get(ui);

    show_columns(
        ui,
        &[
            ColumnWeight::Relative(70.0),
            ColumnWeight::Relative(30.0),
            ColumnWeight::Pixels(320.0),
        ],
        |ui| {
            ui[0].horizontal(|ui| {
                ui.label("Name");
                let mut controller = entity.get_mut::<UiLayoutAnimationController>().unwrap();
                let mut name = controller.animations.get_index(index).unwrap().0.clone();
                if ui.text_edit_singleline(&mut name).changed() && !name.is_empty() {
                    let (_, value) = controller.animations.shift_remove_index(index).unwrap();
                    controller.animations.shift_insert(index, name, value);
                }
            });

            for name in node_names {
                if ui[0]
                    .selectable_label(Some(&name) == state.selected_node.as_ref(), &name)
                    .clicked()
                {
                    if Some(&name) != state.selected_node.as_ref() {
                        state.selected_node = Some(name);
                        state.selected_target = None;
                        state.selected_keyframe = None;
                    }
                }
            }

            let mut available_node_names = children
                .keys()
                .filter(|name| !animation.animation_by_node.contains_key(*name))
                .cloned()
                .collect::<Vec<_>>();
            available_node_names.sort();
            if available_node_names.is_empty() {
                ui[0].horizontal(|ui| {
                    ui.add_enabled_ui(false, |ui| {
                        egui::ComboBox::new("add-new-node", "Add Node")
                            .selected_text("")
                            .show_ui(ui, |_| {});
                        add_button(ui);
                    });
                });
            } else {
                ui[0].horizontal(|ui| {
                    let id = ui.id().with("selected-new-node");
                    let current_node =
                        ui.data_mut(|data| data.get_temp_mut_or_default::<String>(id).clone());
                    egui::ComboBox::new("add-new-node", "Add Node")
                        .selected_text(&current_node)
                        .show_ui(ui, |ui| {
                            for node_name in available_node_names.iter() {
                                if ui
                                    .selectable_label(node_name.eq(&current_node), node_name)
                                    .clicked()
                                {
                                    ui.data_mut(|data| data.insert_temp(id, node_name.clone()));
                                }
                            }
                        });
                    if add_button(ui).clicked() {
                        state.selected_node = Some(current_node.clone());
                        state.selected_target = None;
                        state.selected_keyframe = None;
                        ui.data_mut(|data| data.remove_temp::<String>(id));
                        animation.animation_by_node.insert(
                            current_node,
                            AnimationNodeLane {
                                animation_by_id: Default::default(),
                            },
                        );
                    }
                });
            }

            let Some(name) = state.selected_node.as_ref() else {
                return;
            };

            let lane = animation.animation_by_node.get_mut(name).unwrap();

            for (idx, lane) in lane.animation_by_id.iter().enumerate() {
                let name = registry.by_label.get(&lane.target).unwrap().name;
                if ui[1]
                    .selectable_label(Some(idx) == state.selected_target, name)
                    .clicked()
                {
                    if Some(idx) != state.selected_target {
                        state.selected_target = Some(idx);
                        state.selected_keyframe = None;
                    }
                }
            }

            let entity_node_style = entity
                .world()
                .get::<DynamicNodeLabel>(*children.get(name).unwrap())
                .unwrap()
                .0
                .clone();

            let mut available_targets = registry
                .by_label
                .iter()
                .filter_map(|(id, target)| match target.node_label.as_ref() {
                    Some(label) if !entity_node_style.eq(label) => return None,
                    _ => Some((id, target.name)),
                })
                .collect::<Vec<_>>();

            available_targets.sort_by(|(_, a), (_, b)| a.cmp(b));

            let ui_id = ui[1].id().with("temporary_target");
            let mut selected_id = ui[1].data_mut(|data| {
                data.get_temp_mut_or_default::<Option<Interned<dyn AnimationTargetLabel>>>(ui_id)
                    .clone()
            });

            let selected_text = match selected_id.as_ref() {
                Some(id) => registry.by_label.get(id).unwrap().name,
                None => "",
            };

            ui[1].horizontal(|ui| {
                egui::ComboBox::new("new-target-selector", "Add Target")
                    .selected_text(selected_text)
                    .show_ui(ui, |ui| {
                        for (label, name) in available_targets {
                            let enabled = editor_registry.by_label.contains_key(label);
                            if ui
                                .add_enabled(
                                    enabled,
                                    SelectableLabel::new(Some(label) == selected_id.as_ref(), name),
                                )
                                .clicked()
                            {
                                selected_id = Some(label.clone());
                            }
                        }
                    });

                if ui
                    .add_enabled_ui(selected_id.is_some(), |ui| add_button(ui))
                    .inner
                    .clicked()
                {
                    let id = selected_id.take().unwrap();
                    let editor_target = editor_registry.by_label.get(&id).unwrap();
                    entity.world_scope(|world| {
                        lane.animation_by_id.push(AnimationIdLane {
                            target: id,
                            animation_data: (editor_target.from_world)(world),
                            starting_value: (editor_target.default_content)(),
                            keyframes: vec![],
                        });
                    });
                }
            });

            ui[1].data_mut(|data| {
                data.insert_temp(ui_id, selected_id);
            });

            let Some(target) = state.selected_target else {
                return;
            };

            let lane = &mut lane.animation_by_id[target];

            let target = editor_registry.by_label.get(&lane.target).unwrap();

            ui[2].horizontal(|ui| {
                ui.label("Animation Data");
                (target.edit)(lane.animation_data.as_mut(), ui);
            });

            ui[2].horizontal(|ui| {
                ui.label("Starting Value");
                (target.edit_content)(lane.starting_value.as_mut(), ui);
            });

            show_columns(
                &mut ui[2],
                &[ColumnWeight::Relative(20.0), ColumnWeight::Relative(80.0)],
                |ui| {
                    for (idx, keyframe) in lane.keyframes.iter().enumerate() {
                        if ui[0]
                            .selectable_label(
                                state.selected_keyframe == Some(idx),
                                format!("{}ms", keyframe.timestamp_ms),
                            )
                            .clicked()
                        {
                            state.selected_keyframe = Some(idx);
                        }
                    }

                    if ui[0].button("Add Keyframe").clicked() {
                        let timestamp = lane
                            .keyframes
                            .last()
                            .map(|last| last.timestamp_ms)
                            .unwrap_or_default()
                            + 1;
                        lane.keyframes.push(AnimationKeyframe {
                            timestamp_ms: timestamp,
                            edge_interpolation: EdgeInterpolation::Linear,
                            value: (target.default_content)(),
                        });
                        state.selected_keyframe = Some(lane.keyframes.len() - 1);
                    }

                    let Some(keyframe_idx) = state.selected_keyframe else {
                        return;
                    };

                    let range = match (
                        keyframe_idx
                            .checked_sub(1)
                            .and_then(|idx| lane.keyframes.get(idx)),
                        lane.keyframes.get(keyframe_idx + 1),
                    ) {
                        (None, None) => 1..=u32::MAX,
                        (Some(kf), None) => (kf.timestamp_ms + 1)..=u32::MAX,
                        (None, Some(kf)) => 1..=(kf.timestamp_ms - 1),
                        (Some(before), Some(after)) => {
                            (before.timestamp_ms + 1)..=(after.timestamp_ms - 1)
                        }
                    };
                    let keyframe = &mut lane.keyframes[keyframe_idx];

                    ui[1].horizontal(|ui| {
                        ui.label("Timestamp (ms)");
                        ui.add(DragValue::new(&mut keyframe.timestamp_ms).clamp_range(range));
                    });
                    ui[1].horizontal(|ui| {
                        ui.label("Value");
                        (target.edit_content)(keyframe.value.as_mut(), ui);
                    });
                },
            );
        },
    );

    state.set(ui);
}
