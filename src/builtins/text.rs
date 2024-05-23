use std::ops::Range;

use bevy::asset::load_internal_asset;
use bevy::ecs::entity::EntityHashMap;
use bevy::ecs::system::lifetimeless::SRes;
use bevy::prelude::*;
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctions, RenderCommand, RenderCommandResult,
};
use bevy::render::render_resource::{
    BindGroup, BindGroupLayout, BindGroupLayoutEntry, BindingType, BufferUsages, BufferVec,
    CachedRenderPipelineId, IndexFormat, PipelineCache, SamplerBindingType, ShaderStages,
    TextureSampleType, TextureViewDimension, VertexAttribute, VertexBufferLayout, VertexFormat,
    VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bevy::{
    app::Plugin,
    ecs::{component::Component, system::Resource, world::FromWorld},
    utils::intern::Interned,
};
use bytemuck::{Pod, Zeroable};
use cosmic_text::{Attrs, Family, FontSystem, Metrics, Shaping, Style, SwashCache};
use serde::{Deserialize, Serialize};

use crate::math::NodeSize;
use crate::render::{
    BindLayoutUniform, BindVertexBuffer, DefaultNodePipeline, NodeDrawFunction, SkipNodeRender,
    UiNodeItem,
};
use crate::{NodeLabel, UiNodeApp, UserUiNode};

use self::buffer_glyph::BufferGlyphMaps;

mod buffer_glyph;

#[derive(Resource, Deref, DerefMut)]
pub struct CosmicFontSystem(FontSystem);

#[derive(Resource)]
pub struct CosmicSwashCache(SwashCache);

impl Default for CosmicSwashCache {
    fn default() -> Self {
        Self(SwashCache::new())
    }
}

impl FromWorld for CosmicFontSystem {
    fn from_world(_: &mut bevy::prelude::World) -> Self {
        let mut system = FontSystem::new();

        for file in std::fs::read_dir("./assets/fonts").unwrap() {
            let file = file.unwrap();
            if !file.file_type().unwrap().is_file() {
                continue;
            }

            let path = file.path();
            let ext = path.extension().unwrap();
            if ext.to_str().unwrap() != "ttf" {
                continue;
            }

            system.db_mut().load_font_file(path).unwrap();
        }

        Self(system)
    }
}

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
    pub font_size: u32,
    #[serde(with = "LocalStyle", default = "default_style")]
    pub style: Style,
    #[serde(default = "Alignment::default")]
    pub alignment: Alignment,
    pub text: String,
}

#[derive(Clone, Component)]
pub struct TextNode {
    pub font_face: String,
    pub font_size: u32,
    pub alignment: Alignment,
    pub style: Style,
    pub text: String,
}

impl Default for TextNode {
    fn default() -> Self {
        Self {
            font_face: "Times New Roman".into(),
            font_size: 32,
            alignment: Default::default(),
            style: Style::Normal,
            text: "Enter Text Here".into(),
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
            ui.label("Text");
            ui.text_edit_multiline(&mut text_node.text);
        });
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Self>();
    }
}

struct ExtractedTextNode {
    font_face: String,
    font_size: u32,
    style: Style,
    alignment: cosmic_text::Align,
    text: String,
    node_size: Vec2,
}

#[derive(Resource, Deref, DerefMut, Default)]
struct ExtractedTextNodes(EntityHashMap<ExtractedTextNode>);

#[derive(Resource)]
struct TextNodePipeline {
    pipeline_id: CachedRenderPipelineId,
    glyph_bind_group: BindGroupLayout,
}

impl TextNodePipeline {
    const SHADER: Handle<Shader> = Handle::weak_from_u128(0xB7C271CE9FCD4221A73D01D6B07A4A02);
}

impl FromWorld for TextNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let default_pipeline = world.resource::<DefaultNodePipeline>();
        let device = world.resource::<RenderDevice>();
        let pipeline_cache = world.resource::<PipelineCache>();
        let mut desc = default_pipeline.render_pipeline_descriptor();
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
        desc.vertex.shader = Self::SHADER.clone();
        desc.fragment.as_mut().unwrap().shader = Self::SHADER.clone();
        let glyph_bind_group = device.create_bind_group_layout(
            "TextNodePipeline.glyph_bind_group",
            &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        );

        desc.layout.push(glyph_bind_group.clone());

        let pipeline_id = pipeline_cache.queue_render_pipeline(desc);
        Self {
            pipeline_id,
            glyph_bind_group,
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
            },
        );
    }
}

#[repr(C)]
#[derive(Pod, Zeroable, Copy, Clone)]
struct TextNodeVertex {
    node_uv: [f32; 2],
    tex_uv: [f32; 2],
}

#[derive(Resource)]
struct PreparedBuffers {
    index: BufferVec<u32>,
    vertex: BufferVec<TextNodeVertex>,
}

impl Default for PreparedBuffers {
    fn default() -> Self {
        Self {
            index: BufferVec::new(BufferUsages::INDEX),
            vertex: BufferVec::new(BufferUsages::VERTEX),
        }
    }
}

#[derive(Component)]
struct PreparedTextNode {
    index_range: Range<u32>,
    bind_group: BindGroup,
}

fn prepare_text_nodes(
    mut commands: Commands,
    extracted: Res<ExtractedTextNodes>,
    mut glyphs: ResMut<BufferGlyphMaps>,
    pipeline: Res<TextNodePipeline>,
    device: Res<RenderDevice>,
    queue: Res<RenderQueue>,
    mut font_system: ResMut<CosmicFontSystem>,
    mut swash_cache: ResMut<CosmicSwashCache>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    prepared_buffers: ResMut<PreparedBuffers>,
) {
    let prepared_buffers = prepared_buffers.into_inner();
    prepared_buffers.vertex.clear();
    prepared_buffers.index.clear();
    let function_id = draw_functions
        .read()
        .get_id::<TextNodeDrawFunction>()
        .unwrap();

    let buffers = extracted
        .iter()
        .map(|(entity, extracted_node)| {
            let mut buffer = cosmic_text::Buffer::new(
                &mut font_system.0,
                Metrics::new(
                    extracted_node.font_size as f32,
                    extracted_node.font_size as f32,
                ),
            );
            let mut borrowed = buffer.borrow_with(&mut font_system.0);
            borrowed.set_size(extracted_node.node_size.x, extracted_node.node_size.y);
            borrowed.set_text(
                &extracted_node.text,
                Attrs {
                    family: Family::Name(&extracted_node.font_face),
                    style: extracted_node.style,
                    ..Attrs::new()
                },
                Shaping::Basic,
            );

            for idx in 0..borrowed.lines.len() {
                borrowed.lines[idx].set_align(Some(extracted_node.alignment));
                borrowed.line_layout(idx);
            }
            (*entity, buffer)
        })
        .collect::<Vec<_>>();

    for (_, buffer) in buffers.iter() {
        glyphs.prepare_glyph_sets(
            &mut font_system.0,
            &mut swash_cache.0,
            buffer,
            &device,
            &queue,
            &pipeline.glyph_bind_group,
        );
    }

    // Ensure that all of the glyphs have UVs prepared
    for (entity, buffer) in buffers {
        let result = glyphs.update_vertex_buffers(
            &mut swash_cache.0,
            &mut font_system.0,
            &buffer,
            &mut prepared_buffers.vertex,
            &mut prepared_buffers.index,
        );

        if let Some((index_range, bind_group)) = result {
            commands.get_or_spawn(entity).insert((
                PreparedTextNode {
                    index_range,
                    bind_group,
                },
                NodeDrawFunction::new(function_id),
            ));
        }
    }

    if !prepared_buffers.vertex.is_empty() {
        prepared_buffers.vertex.write_buffer(&device, &queue);
    }

    if !prepared_buffers.index.is_empty() {
        prepared_buffers.index.write_buffer(&device, &queue);
    }
}

type TextNodeDrawFunction = (
    BindTextPipeline,
    BindVertexBuffer<0>,
    BindTextNodeVertexBuffer<1>,
    BindTextNodeIndexBuffer,
    BindLayoutUniform<0>,
    BindGlyphGroup<1>,
    DrawTextNode,
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

struct BindTextNodeVertexBuffer<const I: usize>;

impl<const I: usize> RenderCommand<UiNodeItem> for BindTextNodeVertexBuffer<I> {
    type Param = SRes<PreparedBuffers>;

    type ViewQuery = ();

    type ItemQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let prepared = param.into_inner();
        pass.set_vertex_buffer(I, prepared.vertex.buffer().unwrap().slice(..));
        RenderCommandResult::Success
    }
}

struct BindTextNodeIndexBuffer;

impl RenderCommand<UiNodeItem> for BindTextNodeIndexBuffer {
    type Param = SRes<PreparedBuffers>;

    type ViewQuery = ();

    type ItemQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let prepared = param.into_inner();
        pass.set_index_buffer(
            prepared.index.buffer().unwrap().slice(..),
            0,
            IndexFormat::Uint32,
        );
        RenderCommandResult::Success
    }
}

struct BindGlyphGroup<const I: usize>;

impl<const I: usize> RenderCommand<UiNodeItem> for BindGlyphGroup<I> {
    type Param = ();

    type ViewQuery = ();

    type ItemQuery = &'static PreparedTextNode;

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        _param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(prepared) = entity else {
            return RenderCommandResult::Failure;
        };

        pass.set_bind_group(I, &prepared.bind_group, &[]);
        RenderCommandResult::Success
    }
}

struct DrawTextNode;

impl RenderCommand<UiNodeItem> for DrawTextNode {
    type Param = ();

    type ViewQuery = ();

    type ItemQuery = &'static PreparedTextNode;

    fn render<'w>(
        item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        _param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let Some(prepared) = entity else {
            return RenderCommandResult::Failure;
        };

        pass.draw_indexed(prepared.index_range.clone(), 0, item.batch_range.clone());
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
        app.register_user_ui_node::<TextNode>();

        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<TextNode>();
        }
    }

    fn finish(&self, app: &mut bevy::prelude::App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<CosmicFontSystem>()
            .init_resource::<CosmicSwashCache>()
            .init_resource::<ExtractedTextNodes>()
            .init_resource::<PreparedBuffers>()
            .init_resource::<BufferGlyphMaps>()
            .init_resource::<TextNodePipeline>()
            .add_render_command::<UiNodeItem, TextNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_text_nodes)
            .add_systems(Render, prepare_text_nodes.in_set(RenderSet::Prepare));
    }
}
