use bevy::{
    core::Name,
    ecs::{
        entity::Entity,
        world::{EntityWorldMut, World},
    },
    hierarchy::Parent,
    sprite::Anchor,
    utils::intern::Interned,
};
use egui::{Button, DragValue, SelectableLabel};

use crate::{
    animations::UiLayoutAnimationController,
    loader::{DynamicNodeLabel, NodeUserDataLabels},
    math::{NodeSize, Transform, ZIndex},
    render::UiNodeSettings,
    user_data::{EditorUserDataRegistry, UserDataLabel, UserDataRegistry},
    EditorUiNodeRegistry, UiNodeRegistry,
};

fn display_user_node_dropdown(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
    let world = entity.world();
    let ui_node_registry = world.resource::<UiNodeRegistry>().clone();
    let editor_node_registry = world.resource::<EditorUiNodeRegistry>().clone();
    let ui_node_registry = ui_node_registry.read().unwrap();
    let editor_node_registry = editor_node_registry.read().unwrap();
    let Some(label) = entity.get::<DynamicNodeLabel>() else {
        return;
    };

    let mut label = label.0;

    let current_name = ui_node_registry.by_label.get(&label).unwrap().name;

    let mut list = ui_node_registry
        .by_name
        .iter()
        .map(|(name, label)| (*name, *label))
        .collect::<Vec<_>>();

    list.sort_by_key(|(name, _)| *name);

    egui::ComboBox::new("node-kind", "Node Style")
        .selected_text(current_name)
        .show_ui(ui, |ui| {
            let current = editor_node_registry.by_label.get(&label);
            let help_text = if current.is_some() {
                Some(format!(
                    "Cannot change node style since '{}' is not registered",
                    current_name
                ))
            } else {
                None
            };
            for (name, new_label) in list {
                let local_enabled = editor_node_registry.by_label.contains_key(&new_label);
                let mut resp = ui.add_enabled(
                    local_enabled && current.is_some(),
                    SelectableLabel::new(false, name),
                );
                if let Some(help_text) = help_text.clone() {
                    resp = resp.on_hover_text_at_pointer(help_text);
                }
                if resp.clicked() && new_label != label {
                    let editor_node = editor_node_registry.by_label.get(&new_label).unwrap();
                    let current_node = current.unwrap();
                    (current_node.cleanup)(entity);
                    (editor_node.init_default)(entity);
                    entity.insert(DynamicNodeLabel(new_label));
                    label = new_label;
                }
            }
        });

    if let Some(editor_node) = editor_node_registry.by_label.get(&label) {
        (editor_node.edit)(entity, ui);
    }
}

fn find_root_node(mut entity: Entity, world: &World) -> Entity {
    while let Some(parent) = world.get::<Parent>(entity) {
        entity = parent.get();
    }

    entity
}

pub fn display_ui_node_editor(node: Entity, world: &mut World, ui: &mut egui::Ui) {
    let mut entity = world.entity_mut(node);
    if let Some(name) = entity
        .get::<Name>()
        .map(|name| name.as_str().split('.').last().unwrap().to_string())
    {
        ui.horizontal(|ui| {
            ui.label("Name");
            let mut new_name = name.clone();
            if ui.text_edit_singleline(&mut new_name).changed() {
                let new_name = new_name.split('.').collect::<String>();
                if !new_name.is_empty() {
                    let qualified_name = entity.get::<Name>().unwrap();
                    let new_name = if let Some(last_dot) = qualified_name.as_str().rfind('.') {
                        format!("{}.{}", &qualified_name.as_str()[..last_dot], new_name)
                    } else {
                        new_name
                    };
                    let qualified_name = qualified_name.to_string();
                    entity.world_scope(|world| {
                        let root = find_root_node(node, world);
                        let controller = world.get::<UiLayoutAnimationController>(root).unwrap();
                        for animation in controller.animations.values() {
                            let mut animation = animation.animation.write().unwrap();
                            let node_names = animation
                                .animation_by_node
                                .keys()
                                .filter(|name| name.starts_with(qualified_name.as_str()))
                                .cloned()
                                .collect::<Vec<_>>();
                            for node_name in node_names {
                                let data = animation.animation_by_node.remove(&node_name).unwrap();
                                animation.animation_by_node.insert(
                                    node_name.replace(qualified_name.as_str(), &new_name),
                                    data,
                                );
                            }
                        }
                    });
                    entity.insert(Name::new(new_name));
                }
            }
        });
    }
    display_user_node_dropdown(&mut entity, ui);
    let mut transform = entity.get_mut::<Transform>().unwrap();
    ui.horizontal(|ui| {
        ui.label("Position");
        egui::Grid::new("position").show(ui, |ui| {
            ui.add(DragValue::new(&mut transform.position.x));
            ui.add(DragValue::new(&mut transform.position.y));
        });
    });

    let mut anchor = entity.get_mut::<Anchor>().unwrap();
    ui.label("Position Anchor");
    egui::Grid::new("position-anchor").show(ui, |ui| {
        ui.radio_value(&mut *anchor, Anchor::TopLeft, "");
        ui.radio_value(&mut *anchor, Anchor::TopCenter, "");
        ui.radio_value(&mut *anchor, Anchor::TopRight, "");
        ui.end_row();
        ui.radio_value(&mut *anchor, Anchor::CenterLeft, "");
        ui.radio_value(&mut *anchor, Anchor::Center, "");
        ui.radio_value(&mut *anchor, Anchor::CenterRight, "");
        ui.end_row();
        ui.radio_value(&mut *anchor, Anchor::BottomLeft, "");
        ui.radio_value(&mut *anchor, Anchor::BottomCenter, "");
        ui.radio_value(&mut *anchor, Anchor::BottomRight, "");
        ui.end_row();
    });

    let mut transform = entity.get_mut::<Transform>().unwrap();
    ui.horizontal(|ui| {
        ui.label("Rotation");
        ui.add(DragValue::new(&mut transform.rotation));
        while transform.rotation < 0.0 {
            transform.rotation += 360.0
        }

        while transform.rotation >= 360.0 {
            transform.rotation -= 360.0;
        }
    });

    ui.label("Parent Anchor");
    egui::Grid::new("parent-anchor").show(ui, |ui| {
        ui.radio_value(&mut transform.parent_anchor, Anchor::TopLeft, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::TopCenter, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::TopRight, "");
        ui.end_row();
        ui.radio_value(&mut transform.parent_anchor, Anchor::CenterLeft, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::Center, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::CenterRight, "");
        ui.end_row();
        ui.radio_value(&mut transform.parent_anchor, Anchor::BottomLeft, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::BottomCenter, "");
        ui.radio_value(&mut transform.parent_anchor, Anchor::BottomRight, "");
        ui.end_row();
    });

    ui.horizontal(|ui| {
        ui.label("Scale");
        egui::Grid::new("scale").show(ui, |ui| {
            ui.add(DragValue::new(&mut transform.scale.x));
            ui.add(DragValue::new(&mut transform.scale.y));
        });
    });

    let mut size = entity.get_mut::<NodeSize>().unwrap();
    ui.horizontal(|ui| {
        ui.label("Size");
        egui::Grid::new("size").show(ui, |ui| {
            ui.add(DragValue::new(&mut size.x));
            ui.add(DragValue::new(&mut size.y));
        });
    });

    let mut z_index = entity.get_mut::<ZIndex>().unwrap();
    ui.horizontal(|ui| {
        ui.label("Z Index");
        ui.add(DragValue::new(&mut z_index.0));
    });

    let mut settings = entity.take::<UiNodeSettings>().unwrap();
    bevy_inspector_egui::bevy_inspector::ui_for_value(&mut settings, ui, world);
    let mut entity = world.entity_mut(node);
    entity.insert(settings);

    ui.collapsing("User Data", |ui| {
        let registry = entity.world().resource::<UserDataRegistry>().clone();
        let registry = registry.read().unwrap();
        let editor_registry = entity.world().resource::<EditorUserDataRegistry>().clone();
        let editor_registry = editor_registry.read().unwrap();
        let mut labels = entity.take::<NodeUserDataLabels>().unwrap();

        let mut user_datas = registry.by_name.iter().collect::<Vec<_>>();
        user_datas.sort_by(|(a, _), (b, _)| a.cmp(b));

        let id = ui.id().with("current-user-data");
        let mut selected: Option<Interned<dyn UserDataLabel>> =
            ui.data_mut(|data| *data.get_temp_mut_or_default(id));
        let current_text = match selected.as_ref() {
            Some(label) => registry.by_label.get(label).unwrap().name,
            None => "",
        };

        ui.horizontal(|ui| {
            egui::ComboBox::new("user-data-picker", "")
                .selected_text(current_text)
                .show_ui(ui, |ui| {
                    for (name, label) in user_datas {
                        if ui
                            .add_enabled(
                                editor_registry.by_label.contains_key(label),
                                SelectableLabel::new(Some(*label) == selected, *name),
                            )
                            .clicked()
                        {
                            selected = Some(*label);
                        }
                    }
                });

            if let Some(current) = selected {
                if let Some(pos) = labels.iter().position(|label| current.eq(label)) {
                    let enabled = editor_registry.by_label.contains_key(&current);
                    if ui.add_enabled(enabled, Button::new("Remove")).clicked() {
                        labels.remove(pos);
                        let registered = editor_registry.by_label.get(&current).unwrap();
                        (registered.cleanup)(&mut entity);
                        selected = None;
                    }
                } else if ui.button("Add").clicked() {
                    labels.push(current);
                    let registered = editor_registry.by_label.get(&current).unwrap();
                    (registered.from_world_and_init)(&mut entity);
                }
            }
        });

        if let Some(current) = selected {
            if labels.contains(&current) {
                if let Some(registered) = editor_registry.by_label.get(&current) {
                    (registered.edit)(&mut entity, ui);
                }
            }
        }

        ui.data_mut(|data| data.insert_temp(id, selected));

        entity.insert(labels);
    });
}
