use bevy::{prelude::*, utils::intern::Interned};
use egui::{Align, DragValue, Layout, RichText, SelectableLabel};

use crate::{
    animations::{
        collect_nodes_by_name, AnimationIdLane, AnimationKeyframe, AnimationTargetLabel,
        AnimationTargetRegistry, EdgeInterpolation, EditorAnimationTargetRegistry,
        UiLayoutAnimationController,
    },
    loader::DynamicNodeLabel,
};

fn show_columns(ui: &mut egui::Ui, weights: &[f32], f: impl FnOnce(&mut [egui::Ui])) {
    let total = weights.iter().copied().sum::<f32>();

    // TODO(emilk): ensure there is space
    let spacing = ui.spacing().item_spacing.x;
    let top_left = ui.cursor().min;

    let mut columns = Vec::with_capacity(weights.len());
    let available = ui.available_width();

    for idx in 0..weights.len() {
        let offset = weights
            .iter()
            .take(idx)
            .map(|weight| *weight / total)
            .sum::<f32>()
            * available
            + idx as f32 * spacing;
        let width = weights[idx] / total * available;
        let pos = top_left + egui::Vec2::new(offset, 0.0);
        let child_rect = egui::Rect::from_min_max(
            pos,
            egui::Pos2::new(pos.x + width, ui.max_rect().right_bottom().y),
        );
        let mut column_ui =
            ui.child_ui_with_id_source(child_rect, Layout::top_down_justified(Align::LEFT), idx);
        column_ui.set_width(width);
        columns.push(column_ui);
    }

    f(&mut columns[..]);

    let mut max_width = 0.0f32;
    let mut max_height = 0.0f32;
    for (column, weight) in columns.iter().zip(weights) {
        max_width = max_width.max(column.min_rect().width() * total / *weight);
        max_height = column.min_size().y.max(max_height);
    }

    // Make sure we fit everything next frame:
    let size = egui::Vec2::new(ui.available_width().max(max_width), max_height);
    ui.advance_cursor_after_rect(egui::Rect::from_min_size(top_left, size));
}

#[derive(Default, Clone)]
struct AnimationEditState {
    selected_node: Option<String>,
    selected_target: Option<Interned<dyn AnimationTargetLabel>>,
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
    name: &str,
) {
    let children = collect_nodes_by_name(root_entity, world);

    let registry = world.resource::<AnimationTargetRegistry>().clone();
    let registry = registry.read().unwrap();
    let editor_registry = world.resource::<EditorAnimationTargetRegistry>().clone();
    let editor_registry = editor_registry.read().unwrap();
    let mut entity = world.entity_mut(root_entity);
    let controller = entity.get::<UiLayoutAnimationController>().unwrap();
    let animation = controller.animations.get(name).unwrap().animation.clone();

    let mut animation = animation.write().unwrap();
    let mut node_names = animation
        .animation_by_node
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    node_names.sort();

    let mut state = AnimationEditState::get(ui);

    show_columns(ui, &[20.0, 20.0, 60.0], |ui| {
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

        let Some(name) = state.selected_node.as_ref() else {
            return;
        };

        let lane = animation.animation_by_node.get_mut(name).unwrap();

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

        for (label, name) in available_targets {
            let color = if lane.animation_by_id.contains_key(label) {
                ui[1].visuals().strong_text_color()
            } else {
                ui[1].visuals().weak_text_color()
            };

            let sel_label = SelectableLabel::new(
                Some(label) == state.selected_target.as_ref(),
                RichText::new(name).color(color),
            );

            let enabled = editor_registry.by_label.contains_key(label);

            if ui[1].add_enabled(enabled, sel_label).clicked() {
                if Some(label) != state.selected_target.as_ref() {
                    state.selected_target = Some(label.clone());
                    state.selected_keyframe = None;
                }
            }
        }

        let Some(target) = state.selected_target.as_ref() else {
            return;
        };

        let lane = lane
            .animation_by_id
            .entry(target.clone())
            .or_insert_with(|| {
                let editor_target = editor_registry.by_label.get(target).unwrap();
                AnimationIdLane {
                    animation_data: entity.world_scope(|world| (editor_target.from_world)(world)),
                    starting_value: (editor_target.default_content)(),
                    keyframes: vec![],
                }
            });

        let target = editor_registry.by_label.get(target).unwrap();

        ui[2].horizontal(|ui| {
            ui.label("Animation Data");
            (target.edit)(lane.animation_data.as_mut(), ui);
        });

        ui[2].horizontal(|ui| {
            ui.label("Starting Value");
            (target.edit_content)(lane.starting_value.as_mut(), ui);
        });

        show_columns(&mut ui[2], &[20.0, 80.0], |ui| {
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
                (Some(before), Some(after)) => (before.timestamp_ms + 1)..=(after.timestamp_ms - 1),
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
        });
    });

    state.set(ui);
}
