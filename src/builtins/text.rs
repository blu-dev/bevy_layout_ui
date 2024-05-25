use bevy::asset::load_internal_asset;
use bevy::ecs::entity::EntityHashMap;
use bevy::ecs::system::lifetimeless::SRes;
use bevy::prelude::*;
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult, RenderPhase,
};
use bevy::render::render_resource::{
    BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingResource, BindingType,
    BufferBinding, BufferBindingType, CachedRenderPipelineId, DynamicUniformBuffer, IndexFormat,
    PipelineCache, ShaderStages, ShaderType, VertexAttribute, VertexBufferLayout, VertexFormat,
    VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bevy::{
    app::Plugin,
    ecs::{component::Component, system::Resource, world::FromWorld},
    utils::intern::Interned,
};
use cosmic_text::{Attrs, Buffer, Family, Metrics, Shaping, Style};
use serde::{Deserialize, Serialize};

use crate::builtins::text::buffer_glyph::GlyphBuffersPlugin;
use crate::math::NodeSize;
use crate::render::{
    BindLayoutUniform, BindVertexBuffer, DefaultNodePipeline, SkipNodeRender, UiNodeItem,
};
use crate::{NodeLabel, UiNodeApp, UserUiNode};

use self::buffer_glyph::{CosmicResources, FontGlyphCache, IndexRange, PrunedCacheKey};

mod buffer_glyph;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TextNodeLabel;

decl_node_label!(TextNodeLabel);

#[derive(Deserialize, Serialize)]
#[serde(remote = "cosmic_text::Style")]
pub enum LocalStyle {
    Normal,
    Italic,
    Oblique,
}

#[derive(Deserialize, Serialize, Default, Copy, Clone, Debug)]
pub enum Alignment {
    #[default]
    Left,
    Center,
    Right,
}

impl From<Alignment> for cosmic_text::Align {
    fn from(value: Alignment) -> Self {
        match value {
            Alignment::Left => Self::Left,
            Alignment::Center => Self::Center,
            Alignment::Right => Self::Right,
        }
    }
}

fn default_style() -> Style {
    Style::Normal
}

#[derive(Deserialize, Serialize)]
pub struct TextNodeData {
    pub font_face: String,
    pub font_size: f32,
    #[serde(with = "LocalStyle", default = "default_style")]
    pub style: Style,
    #[serde(default = "Alignment::default")]
    pub alignment: Alignment,
    pub text: String,
    #[serde(default)]
    pub outline: Option<TextOutline>,
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub struct TextOutline {
    pub color: Color,
    pub thickness: f32,
}

impl Default for TextOutline {
    fn default() -> Self {
        Self {
            color: Color::BLACK,
            thickness: 1.0,
        }
    }
}

#[derive(Clone, Component)]
pub struct TextNode {
    pub font_face: String,
    pub font_size: f32,
    pub alignment: Alignment,
    pub style: Style,
    pub text: String,
    pub outline: Option<TextOutline>,
}

impl Default for TextNode {
    fn default() -> Self {
        Self {
            font_face: "Times New Roman".into(),
            font_size: 64.0,
            alignment: Default::default(),
            style: Style::Normal,
            text: "Enter Text Here".into(),
            outline: None,
        }
    }
}

impl UserUiNode for TextNode {
    const NAME: &'static str = "Text";
    type Serde = TextNodeData;

    fn label() -> Interned<dyn NodeLabel> {
        TextNodeLabel.intern()
    }

    fn spawn(&self, entity: &mut bevy::prelude::EntityWorldMut) {
        entity.insert(self.clone());
    }

    fn serialize<E: serde::ser::Error>(&self, _: &World) -> Result<Self::Serde, E> {
        Ok(TextNodeData {
            font_face: self.font_face.clone(),
            font_size: self.font_size,
            alignment: self.alignment,
            style: self.style,
            text: self.text.clone(),
            outline: self.outline,
        })
    }

    fn deserialize<E: serde::de::Error>(
        serde: Self::Serde,
        _: &mut bevy::asset::LoadContext,
    ) -> Result<Self, E> {
        Ok(Self {
            font_face: serde.font_face,
            font_size: serde.font_size,
            alignment: serde.alignment,
            style: serde.style,
            text: serde.text,
            outline: serde.outline,
        })
    }

    fn reconstruct(entity: EntityRef) -> Self {
        entity
            .get::<Self>()
            .expect("Text node should have TextNode component")
            .clone()
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {}
}

#[cfg(feature = "editor-ui")]
impl crate::EditorUiNode for TextNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        let mut text_node = entity.get_mut::<TextNode>().unwrap();

        ui.horizontal(|ui| {
            ui.label("Font Face");
            ui.text_edit_singleline(&mut text_node.font_face);
        });

        ui.horizontal(|ui| {
            ui.label("Font Size");
            ui.add(egui::DragValue::new(&mut text_node.font_size));
        });

        ui.horizontal(|ui| {
            const STYLES: [&'static str; 3] = ["Normal", "Italic", "Oblique"];

            let mut index = match &text_node.style {
                Style::Normal => 0,
                Style::Italic => 1,
                Style::Oblique => 2,
            };

            ui.label("Font Style");
            egui::ComboBox::new("text-node-font-style", "").show_index(
                ui,
                &mut index,
                STYLES.len(),
                |idx| STYLES[idx],
            );

            text_node.style = match index {
                0 => Style::Normal,
                1 => Style::Italic,
                2 => Style::Oblique,
                _ => unreachable!(),
            };
        });

        ui.horizontal(|ui| {
            const ALIGNS: [&'static str; 3] = ["Left", "Center", "Right"];

            let mut index = match &text_node.alignment {
                Alignment::Left => 0,
                Alignment::Center => 1,
                Alignment::Right => 2,
            };

            ui.label("Alignment");
            egui::ComboBox::new("text-node-alignment", "").show_index(
                ui,
                &mut index,
                ALIGNS.len(),
                |idx| ALIGNS[idx],
            );

            text_node.alignment = match index {
                0 => Alignment::Left,
                1 => Alignment::Center,
                2 => Alignment::Right,
                _ => unreachable!(),
            };
        });

        ui.horizontal(|ui| {
            let mut has_outline = text_node.outline.is_some();
            ui.checkbox(&mut has_outline, "Outline");
            if has_outline {
                if text_node.outline.is_none() {
                    text_node.outline = Some(Default::default());
                }

                let outline = text_node.outline.as_mut().unwrap();
                let [r, g, b, a] = outline.color.as_rgba_u8();
                let mut color = egui::Color32::from_rgba_premultiplied(r, g, b, a);
                ui.color_edit_button_srgba(&mut color);
                let [r, g, b, a] = color.to_array();
                outline.color = Color::rgba_u8(r, g, b, a);
                ui.add(
                    egui::DragValue::new(&mut outline.thickness)
                        .speed(0.05)
                        .clamp_range(0.0..=std::f32::INFINITY),
                );
            } else {
                text_node.outline = None;
            }
        });

        ui.horizontal(|ui| {
            ui.label("Text");
            ui.text_edit_multiline(&mut text_node.text);
        });
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Self>();
    }
}

#[repr(C)]
#[derive(ShaderType, Copy, Clone)]
struct ShaderTextLayout {
    outline_color: Vec4,
    size: Vec2,
    outline_thickness: f32,
    render_size: f32,
}

struct ExtractedTextNode {
    font_face: String,
    font_size: f32,
    style: Style,
    alignment: cosmic_text::Align,
    text: String,
    node_size: Vec2,
    outline: Option<(Vec4, f32)>,
}

#[derive(Resource, Deref, DerefMut, Default)]
struct ExtractedTextNodes(EntityHashMap<ExtractedTextNode>);

#[derive(Resource)]
struct TextNodePipeline {
    pipeline_id: CachedRenderPipelineId,
    uniform_bind_group_layout: BindGroupLayout,
}

impl TextNodePipeline {
    const SHADER: Handle<Shader> = Handle::weak_from_u128(0xB7C271CE9FCD4221A73D01D6B07A4A02);
}

impl FromWorld for TextNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let mut desc = world
            .resource::<DefaultNodePipeline>()
            .render_pipeline_descriptor();
        let device = world.resource::<RenderDevice>();
        let cache = world.resource::<PipelineCache>();

        desc.label = Some("TextNodePipeline".into());
        desc.vertex.shader = Self::SHADER.clone();
        desc.vertex.buffers.push(VertexBufferLayout {
            array_stride: VertexFormat::Float32x2.size() * 2,
            step_mode: VertexStepMode::Vertex,
            attributes: vec![
                VertexAttribute {
                    format: VertexFormat::Float32x2,
                    offset: 0,
                    shader_location: 3,
                },
                VertexAttribute {
                    format: VertexFormat::Float32x2,
                    offset: VertexFormat::Float32x2.size(),
                    shader_location: 4,
                },
            ],
        });
        desc.fragment.as_mut().unwrap().shader = Self::SHADER.clone();

        let uniform_bind_group_layout = device.create_bind_group_layout(
            "TextPipeline.uniform_bind_group_layout",
            &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::VERTEX,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: true,
                        min_binding_size: Some(ShaderTextLayout::min_size()),
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::VERTEX,
                    ty: BindingType::Buffer {
                        ty: BufferBindingType::Uniform,
                        has_dynamic_offset: true,
                        min_binding_size: Some(Vec2::min_size()),
                    },
                    count: None,
                },
            ],
        );

        desc.layout.push(uniform_bind_group_layout.clone());

        Self {
            pipeline_id: cache.queue_render_pipeline(desc),
            uniform_bind_group_layout,
        }
    }
}

fn extract_text_nodes(
    nodes: Extract<Query<(Entity, &TextNode, &NodeSize), Without<SkipNodeRender>>>,
    mut extracted: ResMut<ExtractedTextNodes>,
) {
    extracted.clear();
    for (entity, node, node_size) in nodes.iter() {
        extracted.insert(
            entity,
            ExtractedTextNode {
                font_face: node.font_face.clone(),
                font_size: node.font_size,
                style: node.style,
                alignment: cosmic_text::Align::from(node.alignment),
                text: node.text.clone(),
                node_size: node_size.0,
                outline: node
                    .outline
                    .map(|out| (out.color.as_rgba_f32().into(), out.thickness)),
            },
        );
    }
}

#[derive(Component, Copy, Clone, PartialEq, Eq)]
struct PreparedTextLayout {
    text_layout_offset: u32,
    glyph_range: (u32, u32),
    display_outline: bool,
}

struct PreparedGlyph {
    glyph_layout_offset: u32,
    indexes: IndexRange,
}

#[derive(Resource, Default)]
struct PreparedTextData {
    glyphs: Vec<PreparedGlyph>,
    text_layout_buffer: DynamicUniformBuffer<ShaderTextLayout>,
    glyph_layout_buffer: DynamicUniformBuffer<Vec2>,
    bind_group: Option<BindGroup>,
}

fn prepare_text_nodes(
    mut commands: Commands,
    mut text_data: ResMut<PreparedTextData>,
    mut cosmic: ResMut<CosmicResources>,
    mut glyph_buffer: ResMut<FontGlyphCache>,
    text_pipeline: Res<TextNodePipeline>,
    device: Res<RenderDevice>,
    queue: Res<RenderQueue>,
    extracted: Res<ExtractedTextNodes>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut phases: Query<&mut RenderPhase<UiNodeItem>>,
    mut prev_batch_len: Local<usize>,
) {
    text_data.glyphs.clear();
    text_data.text_layout_buffer.clear();
    text_data.glyph_layout_buffer.clear();

    let function_id = draw_functions
        .read()
        .get_id::<TextNodeDrawFunction>()
        .unwrap();

    let mut batch = Vec::with_capacity(*prev_batch_len);

    for mut phase in phases.iter_mut() {
        for item in phase.items.iter_mut() {
            let Some(extracted) = extracted.get(&item.entity) else {
                continue;
            };

            item.pipeline_id = text_pipeline.pipeline_id;
            item.draw_function_id = function_id;

            let mut buffer = Buffer::new(
                &mut cosmic.font_system,
                Metrics::new(extracted.font_size, extracted.font_size),
            );

            {
                let mut buffer = buffer.borrow_with(&mut cosmic.font_system);
                buffer.set_size(extracted.node_size.x, extracted.node_size.y);
                buffer.set_text(
                    &extracted.text,
                    Attrs {
                        family: Family::Name(&extracted.font_face),
                        style: extracted.style,
                        ..Attrs::new()
                    },
                    Shaping::Basic,
                );

                for idx in 0..buffer.lines.len() {
                    buffer.lines[idx].set_align(Some(extracted.alignment));
                    buffer.line_layout(idx);
                }
            }

            let glyph_start = text_data.glyphs.len();
            for run in buffer.layout_runs() {
                for glyph in run.glyphs.iter() {
                    let physical_glyph = glyph.physical((0.0, 0.0), 1.0);
                    let key = physical_glyph.cache_key;
                    let indexes = glyph_buffer
                        .get_index_range(PrunedCacheKey::new(key), &mut cosmic)
                        .unwrap();

                    let offset = text_data
                        .glyph_layout_buffer
                        .push(&Vec2::new(physical_glyph.x as f32, run.line_y));

                    text_data.glyphs.push(PreparedGlyph {
                        glyph_layout_offset: offset,
                        indexes,
                    });
                }
            }

            let (display, color, thickness) = if let Some((color, thickness)) = extracted.outline {
                (true, color, thickness)
            } else {
                (false, Vec4::ZERO, 0.0f32)
            };

            let offset = text_data.text_layout_buffer.push(&ShaderTextLayout {
                outline_color: color,
                size: extracted.node_size,
                outline_thickness: thickness,
                render_size: extracted.font_size,
            });

            batch.push((
                item.entity,
                PreparedTextLayout {
                    text_layout_offset: offset,
                    glyph_range: (glyph_start as u32, text_data.glyphs.len() as u32),
                    display_outline: display,
                },
            ));
        }
    }

    *prev_batch_len = batch.len();
    commands.insert_or_spawn_batch(batch);

    text_data.glyph_layout_buffer.write_buffer(&device, &queue);
    text_data.text_layout_buffer.write_buffer(&device, &queue);
    glyph_buffer.flush(&device, &queue);

    if !text_data.glyph_layout_buffer.is_empty() && !text_data.text_layout_buffer.is_empty() {
        text_data.bind_group = Some(device.create_bind_group(
            "TextNodePipeline.uniform_bind_group",
            &text_pipeline.uniform_bind_group_layout,
            &[
                BindGroupEntry {
                    binding: 0,
                    resource: BindingResource::Buffer(BufferBinding {
                        buffer: text_data.text_layout_buffer.buffer().unwrap(),
                        offset: 0,
                        size: Some(ShaderTextLayout::min_size()),
                    }),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: BindingResource::Buffer(BufferBinding {
                        buffer: text_data.glyph_layout_buffer.buffer().unwrap(),
                        offset: 0,
                        size: Some(Vec2::min_size()),
                    }),
                },
            ],
        ));
    }
}

type TextNodeDrawFunction = (
    BindTextPipeline,
    BindVertexBuffer<0>,
    BindBuffers,
    BindLayoutUniform<0>,
    DrawGlyphs,
);

struct BindTextPipeline;

impl RenderCommand<UiNodeItem> for BindTextPipeline {
    type Param = (SRes<TextNodePipeline>, SRes<PipelineCache>);

    type ViewQuery = ();

    type ItemQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        (pipeline, cache): bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> bevy::render::render_phase::RenderCommandResult {
        let pipeline = pipeline.into_inner();
        let cache = cache.into_inner();
        let Some(pipeline) = cache.get_render_pipeline(pipeline.pipeline_id) else {
            return RenderCommandResult::Failure;
        };

        pass.set_render_pipeline(pipeline);
        RenderCommandResult::Success
    }
}
struct BindBuffers;

impl RenderCommand<UiNodeItem> for BindBuffers {
    type Param = SRes<FontGlyphCache>;
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let buffers = param.into_inner();

        pass.set_vertex_buffer(1, buffers.vertex_buffer());
        pass.set_index_buffer(buffers.index_buffer(), 0, IndexFormat::Uint32);

        RenderCommandResult::Success
    }
}

struct DrawGlyphs;

impl RenderCommand<UiNodeItem> for DrawGlyphs {
    type Param = SRes<PreparedTextData>;

    type ItemQuery = &'static PreparedTextLayout;
    type ViewQuery = ();

    fn render<'w>(
        item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(entity) = entity else {
            return RenderCommandResult::Failure;
        };

        let param = param.into_inner();

        let range = entity.glyph_range.0 as usize..entity.glyph_range.1 as usize;

        for glyph in &param.glyphs[range] {
            pass.set_bind_group(
                1,
                param.bind_group.as_ref().unwrap(),
                &[entity.text_layout_offset, glyph.glyph_layout_offset],
            );

            pass.draw_indexed(glyph.indexes.fill.clone(), 0, item.batch_range.clone());
            if entity.display_outline {
                pass.draw_indexed(glyph.indexes.stroke.clone(), 0, item.batch_range.clone());
            }
        }

        RenderCommandResult::Success
    }
}

pub struct TextNodePlugin;

impl Plugin for TextNodePlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        load_internal_asset!(
            app,
            TextNodePipeline::SHADER,
            "shaders/text_node.wgsl",
            Shader::from_wgsl
        );
        app.register_user_ui_node::<TextNode>()
            .add_plugins(GlyphBuffersPlugin);

        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<TextNode>();
        }
    }

    fn finish(&self, app: &mut bevy::prelude::App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<ExtractedTextNodes>()
            .init_resource::<TextNodePipeline>()
            .init_resource::<PreparedTextData>()
            .add_render_command::<UiNodeItem, TextNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_text_nodes)
            .add_systems(Render, prepare_text_nodes.in_set(RenderSet::Prepare));
    }
}
