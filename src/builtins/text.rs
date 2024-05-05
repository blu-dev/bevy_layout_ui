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
use cosmic_text::{Attrs, Family, FontSystem, Metrics, Shaping, SwashCache};
use egui::DragValue;
use serde::{Deserialize, Serialize};

use crate::math::NodeSize;
use crate::render::{
    BindLayoutUniform, BindVertexBuffer, DefaultNodePipeline, NodeDrawFunction, SkipNodeRender,
    UiNodeItem,
};
use crate::{EditorUiNode, NodeLabel, UiNodeApp, UserUiNode};

use self::buffer_glyph::BufferGlyphMaps;

mod buffer_glyph;

#[derive(Resource)]
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
        let system = FontSystem::new();

        Self(system)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TextNodeLabel;

decl_node_label!(TextNodeLabel);

#[derive(Deserialize, Serialize)]
pub struct TextNodeData {
    pub font_face: String,
    pub font_size: u32,
    pub text: String,
}

#[derive(Clone, Component)]
pub struct TextNode {
    font_face: String,
    font_size: u32,
    text: String,
}

impl Default for TextNode {
    fn default() -> Self {
        Self {
            font_face: "Times New Roman".into(),
            font_size: 32,
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
impl EditorUiNode for TextNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        let mut text_node = entity.get_mut::<TextNode>().unwrap();

        ui.horizontal(|ui| {
            ui.label("Font Face");
            ui.text_edit_singleline(&mut text_node.font_face);
        });

        ui.horizontal(|ui| {
            ui.label("Font Size");
            ui.add(DragValue::new(&mut text_node.font_size));
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
                    ..Attrs::new()
                },
                Shaping::Basic,
            );
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
