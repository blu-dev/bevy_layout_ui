use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use bevy::{
    asset::RecursiveDependencyLoadState,
    prelude::*,
    render::{camera::Viewport, view::RenderLayers},
    utils::{intern::Interned, HashMap},
    window::PrimaryWindow,
};
use bevy_inspector_egui::{
    bevy_egui::{EguiContext, EguiPlugin},
    DefaultInspectorConfigPlugin,
};
use bevy_layout_ui::{
    builtins::DefaultNodePluginGroup,
    decl_user_data_label,
    loader::Layout,
    render::UiRenderPlugin,
    user_data::{EditorUserData, UserData, UserDataLabel},
    UiLayoutPlugin, UiNodeApp,
};
use egui::{Align, ScrollArea};
use egui_dock::{DockArea, DockState, TabViewer};
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditingCurrently {
    Animation(usize),
    Node(Entity),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EditorTab {
    LayoutListing,
    LayoutView {
        handle: Handle<Layout>,
        layer: RenderLayers,
        editing: Option<EditingCurrently>,
    },
}

pub struct EditorViewer<'a> {
    pub world: &'a mut World,
    pub layout_to_entity: &'a mut HashMap<Handle<Layout>, Entity>,
    pub layouts: &'a BTreeMap<PathBuf, Handle<Layout>>,
    pub new_tabs: &'a mut Vec<EditorTab>,
    pub camera_layers: &'a mut Vec<RenderLayers>,
}

impl TabViewer for EditorViewer<'_> {
    type Tab = EditorTab;

    fn on_tab_button(&mut self, tab: &mut Self::Tab, response: &egui::Response) {
        match tab {
            EditorTab::LayoutView { layer, .. } if response.clicked() => {
                let entity = self
                    .world
                    .query_filtered::<Entity, With<Camera2d>>()
                    .single(self.world);
                self.world.entity_mut(entity).insert(*layer);
            }
            _ => {}
        }
    }

    fn on_close(&mut self, tab: &mut Self::Tab) -> bool {
        match tab {
            EditorTab::LayoutView { handle, layer, .. } => {
                self.camera_layers.push(*layer);
                if let Some(entity) = self.layout_to_entity.remove(handle) {
                    self.world.entity_mut(entity).despawn_recursive();
                }
            }
            _ => {}
        }
        true
    }

    fn title(&mut self, tab: &mut Self::Tab) -> egui::WidgetText {
        match tab {
            EditorTab::LayoutListing => "Layouts".into(),
            EditorTab::LayoutView { handle, .. } => self
                .world
                .resource::<AssetServer>()
                .get_path(handle.id())
                .unwrap()
                .path()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .into(),
        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, tab: &mut Self::Tab) {
        match tab {
            EditorTab::LayoutListing => {
                for (name, handle) in self.layouts.iter() {
                    if ui
                        .selectable_label(false, name.display().to_string())
                        .clicked()
                    {
                        self.new_tabs.push(EditorTab::LayoutView {
                            handle: handle.clone(),
                            layer: self.camera_layers.pop().unwrap(),
                            editing: None,
                        });
                    }
                }
            }
            EditorTab::LayoutView {
                handle,
                layer,
                editing,
            } => {
                let entity = if let Some(entity) = self.layout_to_entity.get(handle).copied() {
                    entity
                } else if let RecursiveDependencyLoadState::Loaded = self
                    .world
                    .resource::<AssetServer>()
                    .recursive_dependency_load_state(handle.id())
                {
                    let asset = self
                        .world
                        .resource_mut::<Assets<Layout>>()
                        .remove(handle.id())
                        .unwrap();
                    let entity = bevy_layout_ui::loader::spawn_layout(self.world, &asset);
                    self.layout_to_entity.insert(handle.clone(), entity);
                    self.world
                        .resource_mut::<Assets<Layout>>()
                        .insert(handle.id(), asset);

                    let mut entities = vec![];
                    let mut next_layer = self
                        .world
                        .get::<Children>(entity)
                        .into_iter()
                        .flat_map(|children| children.iter().copied())
                        .collect::<Vec<_>>();
                    loop {
                        if next_layer.is_empty() {
                            break;
                        }
                        entities.extend_from_slice(&next_layer);

                        for child in std::mem::take(&mut next_layer) {
                            next_layer.extend(
                                self.world
                                    .get::<Children>(child)
                                    .into_iter()
                                    .flat_map(|children| children.iter().copied()),
                            );
                        }
                    }

                    for entity in entities {
                        self.world.entity_mut(entity).insert(*layer);
                    }

                    entity
                } else {
                    ui.label("Loading...");
                    return;
                };

                let resolution = self
                    .world
                    .resource::<Assets<Layout>>()
                    .get(handle.id())
                    .unwrap()
                    .resolution;

                show_columns(
                    ui,
                    &[
                        ColumnWeight::Pixels(120.0),
                        ColumnWeight::Relative(40.0),
                        ColumnWeight::Relative(60.0),
                    ],
                    |ui| {
                        ScrollArea::both().show(&mut ui[0], |ui| {
                            ui.painter().rect_filled(
                                ui.clip_rect().with_max_x(ui.max_rect().right()),
                                0.0,
                                ui.visuals().window_fill,
                            );
                            if ui.button("Save").clicked() {
                                let layout =
                                    bevy_layout_ui::loader::marshall_node_tree(self.world, entity);
                                let json = bevy_layout_ui::loader::serialize_layout_as_json(
                                    &layout, self.world,
                                )
                                .unwrap();
                                std::fs::write(
                                    Path::new("./assets").join(
                                        self.world
                                            .resource::<AssetServer>()
                                            .get_path(handle.id())
                                            .unwrap()
                                            .path(),
                                    ),
                                    serde_json::to_string(&json).unwrap(),
                                )
                                .unwrap();
                            }
                            ui.heading("Nodes");
                            if let Some(entity) =
                                bevy_layout_ui::editor::display_node_tree(entity, self.world, ui)
                            {
                                self.world.entity_mut(entity).insert(*layer);
                                *editing = Some(EditingCurrently::Node(entity));
                            }
                            ui.heading("Animations");
                            if let Some(anim) = bevy_layout_ui::editor::display_animation_list(
                                entity, self.world, ui,
                            ) {
                                *editing = Some(EditingCurrently::Animation(anim));
                            }
                        });

                        let max_rect = match editing.as_ref() {
                            None => ui[1].max_rect().union(ui[2].max_rect()).shrink(10.0),
                            Some(EditingCurrently::Node(entity)) => {
                                ui[1].painter().rect_filled(
                                    ui[1]
                                        .clip_rect()
                                        .with_min_x(ui[0].max_rect().right())
                                        .with_max_x(ui[1].max_rect().right()),
                                    0.0,
                                    ui[1].visuals().window_fill,
                                );
                                bevy_layout_ui::editor::node_ui::display_ui_node_editor(
                                    *entity, self.world, &mut ui[1],
                                );
                                ui[2].max_rect().shrink(10.0)
                            }
                            Some(EditingCurrently::Animation(anim)) => {
                                ui[1].painter().rect_filled(
                                    ui[1]
                                        .clip_rect()
                                        .with_min_x(ui[0].max_rect().right())
                                        .with_max_x(ui[1].max_rect().right()),
                                    0.0,
                                    ui[1].visuals().window_fill,
                                );
                                bevy_layout_ui::editor::animation::show_animation_editor(
                                    entity, self.world, &mut ui[1], *anim,
                                );
                                ui[2].max_rect().shrink(10.0)
                            }
                        };

                        let camera_region = max_rect;

                        let size = fit_size_to_aspect_ratio(
                            camera_region.size(),
                            egui::Vec2::new(resolution.x as f32, resolution.y as f32),
                        );
                        let x = (camera_region.width() - size.x) / 2.0 + camera_region.min.x;
                        let y = (camera_region.height() - size.y) / 2.0 + camera_region.min.y;

                        self.world
                            .query::<&mut Camera>()
                            .single_mut(self.world)
                            .viewport = Some(Viewport {
                            physical_position: Vec2::new(x, y).as_uvec2(),
                            physical_size: Vec2::new(size.x, size.y).as_uvec2(),
                            ..default()
                        });
                    },
                );
            }
        }
    }

    fn clear_background(&self, tab: &Self::Tab) -> bool {
        !matches!(tab, EditorTab::LayoutView { .. })
    }
}

fn fit_size_to_aspect_ratio(size: egui::Vec2, aspect: egui::Vec2) -> egui::Vec2 {
    let maybe_width = size.y * aspect.x / aspect.y;
    if maybe_width > size.x {
        egui::Vec2::new(size.x, size.x * aspect.y / aspect.x)
    } else {
        egui::Vec2::new(maybe_width, size.y)
    }
}

#[derive(Resource)]
pub struct EditorUiState {
    pub dock: DockState<EditorTab>,
    pub layouts: BTreeMap<PathBuf, Handle<Layout>>,
    pub layout_to_entity: HashMap<Handle<Layout>, Entity>,
    pub camera_layers: Vec<RenderLayers>,
}

impl FromWorld for EditorUiState {
    fn from_world(world: &mut World) -> Self {
        let asset_server = world.resource::<AssetServer>();
        let layouts = WalkDir::new("./assets")
            .into_iter()
            .filter_map(|entry| {
                let entry = entry.ok()?;
                let file_name = entry.path().file_name()?.to_str()?;
                file_name
                    .ends_with(".layout.json")
                    .then(|| entry.path().strip_prefix("./assets").unwrap().to_path_buf())
            })
            .map(|path| {
                let handle = asset_server.load(path.clone());
                (path, handle)
            })
            .collect();

        Self {
            dock: DockState::new(vec![EditorTab::LayoutListing]),
            layouts,
            layout_to_entity: HashMap::new(),
            camera_layers: (0..RenderLayers::TOTAL_LAYERS)
                .map(|layer| RenderLayers::layer(layer as u8))
                .collect::<Vec<_>>(),
        }
    }
}

pub fn ui_system(world: &mut World) {
    let Ok(mut context) = world
        .query_filtered::<&EguiContext, With<PrimaryWindow>>()
        .get_single(world)
        .cloned()
    else {
        return;
    };

    let mut state = world.remove_resource::<EditorUiState>().unwrap();

    let mut new_tabs = vec![];
    let mut tab_viewer = EditorViewer {
        world,
        layouts: &state.layouts,
        new_tabs: &mut new_tabs,
        layout_to_entity: &mut state.layout_to_entity,
        camera_layers: &mut state.camera_layers,
    };

    DockArea::new(&mut state.dock).show(context.get_mut(), &mut tab_viewer);

    for new_tab in new_tabs {
        if let Some((surface, node, _)) = state.dock.find_tab(&new_tab) {
            state.dock.set_focused_node_and_surface((surface, node));
        } else {
            state.dock.push_to_focused_leaf(new_tab);
        }
    }

    world.insert_resource(state);
}

#[derive(Serialize, Deserialize, Component, Copy, Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct SampleUserData;

decl_user_data_label!(SampleUserData);

impl UserData for SampleUserData {
    type Serde = Self;

    const NAME: &'static str = "Sample";

    fn deserialize(this: Self::Serde) -> Result<Self, serde_json::Error> {
        Ok(this)
    }

    fn serialize(&self) -> Self::Serde {
        *self
    }

    fn initialize(&self, entity: &mut EntityWorldMut) {
        entity.insert(*self);
    }

    fn reconstruct(entity: EntityRef) -> Self {
        *entity.get::<Self>().unwrap()
    }

    fn label() -> Interned<dyn UserDataLabel> {
        SampleUserData.intern()
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {}
}

impl EditorUserData for SampleUserData {
    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Self>();
    }

    fn edit(_: &mut EntityWorldMut, ui: &mut egui::Ui) {
        ui.label("Epic Style!");
    }
}

pub fn main() {
    let mut app = App::new();

    app.add_plugins((
        DefaultPlugins,
        UiLayoutPlugin,
        UiRenderPlugin,
        DefaultNodePluginGroup,
        EguiPlugin,
        DefaultInspectorConfigPlugin,
    ))
    .register_user_data::<SampleUserData>()
    .register_editor_user_data::<SampleUserData>();

    app.add_systems(Update, (bevy::window::close_on_esc, ui_system))
        .init_resource::<EditorUiState>();

    app.world.spawn(Camera2dBundle::default());

    app.run();
}
