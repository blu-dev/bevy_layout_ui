use bevy::{
    ecs::{entity::Entity, world::World},
    sprite::Anchor,
};
use egui::DragValue;

use crate::math::{NodeSize, Transform};

pub fn display_ui_node_editor(node: Entity, world: &mut World, ui: &mut egui::Ui) {
    let mut entity = world.entity_mut(node);
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
}
