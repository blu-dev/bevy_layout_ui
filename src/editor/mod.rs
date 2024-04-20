use bevy::{ecs::system::CommandQueue, prelude::*, sprite::Anchor};
use egui::{Align2, FontId, Response, Sense};

use crate::{
    math::{GlobalTransform, NodeSize, Transform},
    render::UiNodeSettings,
};

pub mod node_ui;

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

    let resp = display_node_tree_impl(root_entity, world, ui, &mut commands);

    command_queue.apply(world);

    resp
}

fn display_node_tree_impl(
    root_entity: Entity,
    world: &World,
    ui: &mut egui::Ui,
    commands: &mut Commands,
) -> Option<Entity> {
    let name = get_name(root_entity, world);

    match world.get::<Children>(root_entity) {
        Some(children) => {
            let response = ui.collapsing(name, |ui| {
                let resp = children.iter().copied().fold(None, |selected, item| {
                    selected.or(display_node_tree_impl(item, world, ui, commands))
                });

                if add_button(ui).clicked() {
                    commands.entity(root_entity).with_children(|children| {
                        children.spawn((
                            Transform::new(),
                            GlobalTransform::default(),
                            Anchor::TopLeft,
                            NodeSize(Vec2::splat(50.0)),
                            UiNodeSettings {
                                target_resolution: UVec2::new(960, 540),
                            },
                        ));
                    });
                }

                resp
            });
            response
                .header_response
                .double_clicked()
                .then_some(root_entity)
                .or(response.body_returned.flatten())
        }
        None => ui
            .selectable_label(false, name)
            .clicked()
            .then_some(root_entity),
    }
}
