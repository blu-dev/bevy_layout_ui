use std::f32::{INFINITY, NEG_INFINITY};
use std::ops::Range;

use bevy::app::Plugin;
use bevy::asset::load_internal_asset;
use bevy::core_pipeline::core_2d::graph::{Core2d, Node2d};
use bevy::ecs::entity::{EntityHash, EntityHashMap};
use bevy::ecs::query::{QueryData, ROQueryItem};
use bevy::ecs::system::lifetimeless::SRes;
use bevy::ecs::system::SystemParamItem;
use bevy::math::Affine2;
use bevy::prelude::*;
use bevy::render::camera::ExtractedCamera;
use bevy::render::mesh::PrimitiveTopology;
use bevy::render::render_graph::{RenderGraphApp, RenderLabel, ViewNode, ViewNodeRunner};
use bevy::render::render_phase::{
    sort_phase_system, AddRenderCommand, DrawFunctionId, DrawFunctions, PhaseItem, RenderCommand,
    RenderCommandResult, RenderPhase, TrackedRenderPass,
};
use bevy::render::render_resource::{
    BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingResource, BindingType,
    BlendState, BufferBinding, BufferBindingType, BufferUsages, BufferVec, CachedRenderPipelineId,
    ColorTargetState, ColorWrites, CompareFunction, DepthBiasState, DepthStencilState,
    DynamicUniformBuffer, Extent3d, FragmentState, FrontFace, IndexFormat, LoadOp,
    MultisampleState, Operations, PipelineCache, PolygonMode, PrimitiveState,
    RenderPassDepthStencilAttachment, RenderPassDescriptor, RenderPipelineDescriptor, ShaderStages,
    ShaderType, StencilFaceState, StencilState, StoreOp, Texture, TextureDescriptor,
    TextureDimension, TextureFormat, TextureUsages, TextureView, VertexBufferLayout, VertexFormat,
    VertexState, VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::texture::BevyDefault;
use bevy::render::view::{RenderLayers, ViewTarget};
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bevy::sprite::Anchor;
use bevy::utils::nonmax::NonMaxU32;
use bytemuck::{Pod, Zeroable};
use serde::{Deserialize, Serialize};

use crate::math::{GlobalTransform, NodeSize, ZIndex};

#[derive(Debug, Copy, Clone, Reflect, Deserialize, Serialize)]
#[reflect(Default)]
pub enum VertexColors {
    Single(Color),
    Corners {
        top_left: Color,
        top_right: Color,
        bottom_left: Color,
        bottom_right: Color,
    },
}

impl VertexColors {
    pub fn top_left(&self) -> Color {
        match self {
            Self::Single(color) => *color,
            Self::Corners { top_left, .. } => *top_left,
        }
    }
    pub fn top_right(&self) -> Color {
        match self {
            Self::Single(color) => *color,
            Self::Corners { top_right, .. } => *top_right,
        }
    }
    pub fn bottom_left(&self) -> Color {
        match self {
            Self::Single(color) => *color,
            Self::Corners { bottom_left, .. } => *bottom_left,
        }
    }
    pub fn bottom_right(&self) -> Color {
        match self {
            Self::Single(color) => *color,
            Self::Corners { bottom_right, .. } => *bottom_right,
        }
    }

    pub fn set_top_left(&mut self, color: Color) {
        match self {
            Self::Single(old_color) => {
                let old = *old_color;
                *self = Self::Corners {
                    top_left: color,
                    top_right: old,
                    bottom_left: old,
                    bottom_right: old,
                };
            }
            Self::Corners { top_left, .. } => *top_left = color,
        }
    }

    pub fn set_top_right(&mut self, color: Color) {
        match self {
            Self::Single(old_color) => {
                let old = *old_color;
                *self = Self::Corners {
                    top_left: old,
                    top_right: color,
                    bottom_left: old,
                    bottom_right: old,
                };
            }
            Self::Corners { top_right, .. } => *top_right = color,
        }
    }

    pub fn set_bottom_left(&mut self, color: Color) {
        match self {
            Self::Single(old_color) => {
                let old = *old_color;
                *self = Self::Corners {
                    top_left: old,
                    top_right: old,
                    bottom_left: color,
                    bottom_right: old,
                };
            }
            Self::Corners { bottom_left, .. } => *bottom_left = color,
        }
    }

    pub fn set_bottom_right(&mut self, color: Color) {
        match self {
            Self::Single(old_color) => {
                let old = *old_color;
                *self = Self::Corners {
                    top_left: old,
                    top_right: old,
                    bottom_left: old,
                    bottom_right: color,
                };
            }
            Self::Corners { bottom_right, .. } => *bottom_right = color,
        }
    }
}

impl Default for VertexColors {
    fn default() -> Self {
        Self::Single(Color::WHITE)
    }
}

/// Marker component used to skip rendering nodes
///
/// This can be used when you are using a node as an information store, or as an intermediate
/// step to building a more complex layout.
#[derive(Component, Debug, Copy, Clone, PartialEq, Eq, Hash, Reflect)]
pub struct SkipNodeRender;

/// Vertex input managed by the UI layouting systems
///
/// This vertex will be the same for all custom implemented nodes, the corresponding WGSL
/// structure can be imported from `bevy_layout_ui::VertexInput`, and is defined as such:
/// ```wgsl
/// struct VertexInput {
///     @location(0) model_col0: vec2<f32>,
///     @location(1) model_col1: vec2<f32>,
///     @location(2) model_col2: vec2<f32>
/// };
/// ```
///
/// You will probably not want to use the fields directly, instead you should use the
/// `bevy_layout_ui::transform_node_point` passing in the [`LayoutUniform`], this input,
/// and your vertex.
#[repr(C)]
#[derive(Pod, Zeroable, Copy, Clone, Debug, Reflect)]
pub struct NodeVertexInput {
    model: [[f32; 2]; 3],
}

impl NodeVertexInput {
    /// Creates a vertex buffer layout that can be used when binding this vertex
    pub fn layout() -> VertexBufferLayout {
        VertexBufferLayout::from_vertex_formats(
            VertexStepMode::Instance,
            [VertexFormat::Float32x2; 3],
        )
    }

    /// Converts this vertex input into an affine
    pub fn into_affine(&self) -> Affine2 {
        Affine2::from_cols_array_2d(&self.model)
    }

    /// Converts an affine into this vertex input
    pub fn from_affine(affine: Affine2) -> Self {
        Self {
            model: affine.to_cols_array_2d(),
        }
    }
}

/// Common node uniform managed by the UI layouting systems
///
/// This uniform is built off of the [`CommonNodeAttributes`] component and can be used
/// by any node as built-ins to enhance the functionality of their own node kind.
///
/// You can import this uniform from `bevy_layout_ui::CommonNodeUniform`
#[repr(C)]
#[derive(Debug, Copy, Clone, ShaderType)]
pub struct CommonNodeUniform {
    layout_to_ndc: [Vec4; 3],
    vertex_colors: [Vec4; 4],
    clip_rect: Vec4,
    opacity: f32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VertexPosition {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl CommonNodeUniform {
    pub const fn new() -> Self {
        Self {
            layout_to_ndc: [Vec4::ZERO; 3],
            vertex_colors: [Vec4::ONE; 4],
            clip_rect: Vec4::new(NEG_INFINITY, NEG_INFINITY, INFINITY, INFINITY),
            opacity: 1.0,
        }
    }

    pub fn from_opacity(opacity: f32) -> Self {
        Self::new().with_opacity(opacity)
    }

    pub fn from_vertex_colors(
        top_left: Color,
        top_right: Color,
        bottom_left: Color,
        bottom_right: Color,
    ) -> Self {
        Self::new().with_vertex_colors(top_left, top_right, bottom_left, bottom_right)
    }

    pub fn with_vertex_colors(
        mut self,
        top_left: Color,
        top_right: Color,
        bottom_left: Color,
        bottom_right: Color,
    ) -> Self {
        self.vertex_colors[0] = top_left.as_linear_rgba_f32().into();
        self.vertex_colors[1] = top_right.as_linear_rgba_f32().into();
        self.vertex_colors[2] = bottom_left.as_linear_rgba_f32().into();
        self.vertex_colors[3] = bottom_right.as_linear_rgba_f32().into();
        self
    }

    pub fn with_clip_rect(mut self, rect: Option<Rect>) -> Self {
        let Some(rect) = rect else {
            return self;
        };
        self.clip_rect.x = rect.min.x;
        self.clip_rect.y = rect.min.y;
        self.clip_rect.z = rect.max.x;
        self.clip_rect.w = rect.max.y;
        self
    }

    pub fn with_opacity(self, opacity: f32) -> Self {
        Self {
            opacity: opacity.clamp(0.0, 1.0),
            ..self
        }
    }

    pub fn opacity(&self) -> f32 {
        self.opacity
    }

    pub fn vertex_color(&self, position: VertexPosition) -> Color {
        let color = match position {
            VertexPosition::TopLeft => self.vertex_colors[0],
            VertexPosition::TopRight => self.vertex_colors[1],
            VertexPosition::BottomLeft => self.vertex_colors[2],
            VertexPosition::BottomRight => self.vertex_colors[3],
        };

        Color::rgba_linear(color.x, color.y, color.z, color.w)
    }

    /// Sets the affine of this uniform
    pub fn with_affine(self, affine: Affine2) -> Self {
        let mat3 = Mat3::from_cols(
            affine.matrix2.x_axis.extend(0.0),
            affine.matrix2.y_axis.extend(0.0),
            affine.translation.extend(1.0),
        );

        Self {
            layout_to_ndc: [
                mat3.x_axis.extend(0.0),
                mat3.y_axis.extend(0.0),
                mat3.z_axis.extend(0.0),
            ],
            ..self
        }
    }

    /// Converts an affine into this uniform
    pub fn from_affine(affine: Affine2) -> Self {
        Self::new().with_affine(affine)
    }

    /// Takes a layout size and constructs a layout -> NDC transformation
    pub fn from_layout_size(size: UVec2) -> Self {
        let size = size.as_vec2();
        let affine = Affine2::from_scale_angle_translation(
            2.0 * size.recip() * Vec2::new(1.0, -1.0),
            0.0,
            Vec2::new(-1.0, 1.0),
        );
        Self::new().with_affine(affine)
    }
}

/// A phase item representing a UI node to be rendered
#[derive(Debug, Clone)]
pub struct UiNodeItem {
    pub entity: Entity,
    pub z_index: isize,
    pub draw_function_id: DrawFunctionId,
    pub batch_range: Range<u32>,
    /// The dynamic offset is used as the offset for the [`LayoutUniform`] buffer for this node
    pub dynamic_offset: Option<NonMaxU32>,
    pub pipeline_id: CachedRenderPipelineId,
}

impl PhaseItem for UiNodeItem {
    type SortKey = isize;

    fn sort_key(&self) -> Self::SortKey {
        self.z_index
    }

    fn entity(&self) -> Entity {
        self.entity
    }

    fn draw_function(&self) -> DrawFunctionId {
        self.draw_function_id
    }

    fn batch_range(&self) -> &Range<u32> {
        &self.batch_range
    }

    fn batch_range_mut(&mut self) -> &mut Range<u32> {
        &mut self.batch_range
    }

    fn dynamic_offset(&self) -> Option<NonMaxU32> {
        self.dynamic_offset
    }

    fn dynamic_offset_mut(&mut self) -> &mut Option<NonMaxU32> {
        &mut self.dynamic_offset
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Component)]
pub struct NodeDrawFunction(DrawFunctionId);

impl NodeDrawFunction {
    pub const fn new(id: DrawFunctionId) -> Self {
        Self(id)
    }
}

/// The default draw function for nodes
///
/// This function should only be used when a user has forgotten to explicitly set the node
pub type DefaultNodeDrawFunction = (
    BindVertexBuffer<0>,
    BindLayoutUniform<0>,
    BindNodePipeline,
    DrawUiPhaseItem,
);

/// Pipeline for UI nodes that do not have their draw function replaced
///
/// If this pipeline is being run, it means that you have forgotten to change your [`DrawFunctionId`]
/// in the [`Prepare`](RenderSet::Prepare) phase. You can do so by either editing the `draw_function_id`
/// field of the `UiNodeItem` for the node or by inserting a [`NodeDrawFunction`] component on
/// the entity in the render world.
#[derive(Resource, Debug)]
pub struct DefaultNodePipeline {
    cached_pipeline_id: CachedRenderPipelineId,
    bind_group_layout: BindGroupLayout,
}

impl DefaultNodePipeline {
    pub const SHADER: Handle<Shader> = Handle::weak_from_u128(0x4A737C1653534235A71B38F556B37B1C);

    pub fn render_pipeline_descriptor(&self) -> RenderPipelineDescriptor {
        RenderPipelineDescriptor {
            label: Some("InvalidNodePipeline.descriptor".into()),
            layout: vec![self.bind_group_layout.clone()],
            push_constant_ranges: vec![],
            vertex: VertexState {
                shader: Self::SHADER.clone(),
                shader_defs: vec![],
                entry_point: "vertex".into(),
                buffers: vec![NodeVertexInput::layout()],
            },
            primitive: PrimitiveState {
                topology: PrimitiveTopology::TriangleList,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: Some(DepthStencilState {
                format: TextureFormat::Stencil8,
                depth_write_enabled: false,
                depth_compare: CompareFunction::Always,
                stencil: StencilState {
                    front: StencilFaceState::IGNORE,
                    back: StencilFaceState::IGNORE,
                    read_mask: !0,
                    write_mask: !0,
                },
                bias: DepthBiasState::default(),
            }),
            multisample: MultisampleState {
                count: 4,
                ..Default::default()
            },
            fragment: Some(FragmentState {
                shader: Self::SHADER.clone(),
                shader_defs: vec![],
                entry_point: "fragment".into(),
                targets: vec![Some(ColorTargetState {
                    format: TextureFormat::bevy_default(),
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::all(),
                })],
            }),
        }
    }
}

impl FromWorld for DefaultNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let pipeline_cache = world.resource::<PipelineCache>();
        let device = world.resource::<RenderDevice>();

        let layout = device.create_bind_group_layout(
            "InvalidNodePipeline.LayoutUniformLayout",
            &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::VERTEX_FRAGMENT,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Uniform,
                    has_dynamic_offset: true,
                    min_binding_size: Some(CommonNodeUniform::min_size()),
                },
                count: None,
            }],
        );

        let mut this = Self {
            cached_pipeline_id: CachedRenderPipelineId::INVALID,
            bind_group_layout: layout,
        };
        let descriptor = this.render_pipeline_descriptor();
        this.cached_pipeline_id = pipeline_cache.queue_render_pipeline(descriptor);

        this
    }
}

#[derive(Component, Debug, Copy, Clone, Reflect)]
pub struct UiNodeSettings {
    pub clip_rect: Option<Rect>,
    pub target_resolution: UVec2,
    pub vertex_colors: VertexColors,
    pub opacity: f32,
    pub z_index: Option<ZIndex>,
}

pub struct UiRenderPlugin;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, RenderLabel, Default)]
pub struct UiLayoutNode;

pub struct ExtractedNode {
    affine: Affine2,
    z_index: isize,
    settings: UiNodeSettings,
    layer: RenderLayers,
    visibility: bool,
}

#[derive(Resource, Deref, DerefMut)]
pub struct ExtractedNodes(EntityHashMap<ExtractedNode>);

impl Default for ExtractedNodes {
    fn default() -> Self {
        Self(EntityHashMap::with_capacity_and_hasher(
            128,
            EntityHash::default(),
        ))
    }
}

#[derive(QueryData)]
pub struct ExtractNodeQuery {
    entity: Entity,
    global_transform: &'static GlobalTransform,
    node_size: &'static NodeSize,
    anchor: &'static Anchor,
    settings: &'static UiNodeSettings,
    z_index: &'static ZIndex,
    inherited: &'static InheritedVisibility,
    layer: Option<&'static RenderLayers>,
}

pub fn extract_nodes(
    nodes: Extract<Query<ExtractNodeQuery, Without<SkipNodeRender>>>,
    mut extracted_nodes: ResMut<ExtractedNodes>,
) {
    extracted_nodes.clear();
    for data in nodes.iter() {
        let mut affine = data.global_transform.affine();
        affine = affine
            * Affine2::from_mat2_translation(
                Mat2::from_scale_angle(data.node_size.0, 0.0),
                -(data.node_size.0 / 2.0
                    + (data.anchor.as_vec() * Vec2::new(1.0, -1.0)) * data.node_size.0),
            );
        extracted_nodes.insert(
            data.entity,
            ExtractedNode {
                affine,
                z_index: data.z_index.0,
                settings: *data.settings,
                layer: data.layer.copied().unwrap_or_default(),
                visibility: data.inherited.get(),
            },
        );
    }
}

pub fn extract_ui_phases(
    mut commands: Commands,
    cameras_2d: Extract<Query<(Entity, &Camera), With<Camera2d>>>,
) {
    for (entity, camera) in cameras_2d.iter() {
        if camera.is_active {
            commands
                .get_or_spawn(entity)
                .insert(RenderPhase::<UiNodeItem>::default());
        }
    }
}

#[derive(Resource)]
pub struct PreparedResources {
    vertex_buffer: BufferVec<[[f32; 2]; 3]>,
    index_buffer: BufferVec<u32>,
    uniform_buffer: DynamicUniformBuffer<CommonNodeUniform>,
    bind_group: Option<BindGroup>,
}

impl Default for PreparedResources {
    fn default() -> Self {
        Self {
            vertex_buffer: BufferVec::new(BufferUsages::VERTEX),
            index_buffer: BufferVec::new(BufferUsages::INDEX),
            uniform_buffer: DynamicUniformBuffer::default(),
            bind_group: None,
        }
    }
}

pub fn queue_ui_nodes(
    default_pipeline: Res<DefaultNodePipeline>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut ui_phases: Query<&mut RenderPhase<UiNodeItem>>,
    nodes: Res<ExtractedNodes>,
) {
    let function = draw_functions
        .read()
        .get_id::<DefaultNodeDrawFunction>()
        .unwrap();
    for mut phase in ui_phases.iter_mut() {
        for (entity, node) in nodes.iter().filter(|node| node.1.visibility) {
            phase.items.push(UiNodeItem {
                entity: *entity,
                z_index: node.z_index,
                draw_function_id: function,
                // batch_range and dynamic_offset are set in the prepare step
                batch_range: 0..0,
                dynamic_offset: None,
                pipeline_id: default_pipeline.cached_pipeline_id,
            });
        }
    }
}

pub fn prepare_ui_nodes(
    mut commands: Commands,
    invalid_pipeline: Res<DefaultNodePipeline>,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    resources: ResMut<PreparedResources>,
    extracted_nodes: Res<ExtractedNodes>,
    mut ui_phases: Query<&mut RenderPhase<UiNodeItem>>,
) {
    let resources = resources.into_inner();
    if resources.index_buffer.is_empty() {
        resources.index_buffer.extend([2, 0, 1, 2, 1, 3]);
        resources
            .index_buffer
            .write_buffer(&render_device, &render_queue);
    }

    resources.vertex_buffer.clear();
    resources.uniform_buffer.clear();

    {
        let total_length = ui_phases.iter().map(|phase| phase.items.len()).sum();

        let Some(mut uniform_writer) =
            resources
                .uniform_buffer
                .get_writer(total_length, &render_device, &render_queue)
        else {
            return;
        };

        for mut phase in ui_phases.iter_mut() {
            for (idx, item) in phase.items.iter_mut().enumerate() {
                let node = extracted_nodes.get(&item.entity).unwrap();
                resources.vertex_buffer.push(node.affine.to_cols_array_2d());

                let offset = uniform_writer.write(
                    &CommonNodeUniform::from_layout_size(node.settings.target_resolution)
                        .with_vertex_colors(
                            node.settings.vertex_colors.top_left(),
                            node.settings.vertex_colors.top_right(),
                            node.settings.vertex_colors.bottom_left(),
                            node.settings.vertex_colors.bottom_right(),
                        )
                        .with_clip_rect(node.settings.clip_rect)
                        .with_opacity(node.settings.opacity),
                );
                item.dynamic_offset = NonMaxU32::new(offset);

                item.batch_range = (idx as u32)..(idx as u32 + 1);

                commands.get_or_spawn(item.entity).insert(node.layer);
            }
        }
    }

    resources
        .vertex_buffer
        .write_buffer(&render_device, &render_queue);

    resources.bind_group = Some(render_device.create_bind_group(
        "invalid_bind_group",
        &invalid_pipeline.bind_group_layout,
        &[BindGroupEntry {
            binding: 0,
            resource: BindingResource::Buffer(BufferBinding {
                buffer: resources.uniform_buffer.buffer().unwrap(),
                offset: 0,
                size: Some(CommonNodeUniform::min_size()),
            }),
        }],
    ));
}

pub struct BindNodePipeline;
pub struct BindLayoutUniform<const I: usize>;
pub struct BindVertexBuffer<const I: usize>;
pub struct DrawUiPhaseItem;

impl RenderCommand<UiNodeItem> for BindNodePipeline {
    type Param = SRes<PipelineCache>;
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        cache: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let cache = cache.into_inner();
        let Some(pipeline) = cache.get_render_pipeline(item.pipeline_id) else {
            return RenderCommandResult::Failure;
        };

        pass.set_render_pipeline(pipeline);
        RenderCommandResult::Success
    }
}

impl<const I: usize> RenderCommand<UiNodeItem> for BindLayoutUniform<I> {
    type Param = SRes<PreparedResources>;
    type ViewQuery = ();
    type ItemQuery = ();

    fn render<'w>(
        item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        param: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let param = param.into_inner();
        pass.set_bind_group(
            I,
            param.bind_group.as_ref().unwrap(),
            &[item.dynamic_offset.unwrap().get()],
        );

        RenderCommandResult::Success
    }
}

impl<const I: usize> RenderCommand<UiNodeItem> for BindVertexBuffer<I> {
    type Param = SRes<PreparedResources>;
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        param: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let param = param.into_inner();

        pass.set_vertex_buffer(I, param.vertex_buffer.buffer().unwrap().slice(..));
        pass.set_index_buffer(
            param.index_buffer.buffer().unwrap().slice(..),
            0,
            IndexFormat::Uint32,
        );

        RenderCommandResult::Success
    }
}

impl RenderCommand<UiNodeItem> for DrawUiPhaseItem {
    type Param = ();
    type ViewQuery = ();
    type ItemQuery = ();

    fn render<'w>(
        item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        _param: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        pass.draw_indexed(0..6, 0, item.batch_range.clone());
        RenderCommandResult::Success
    }
}

#[derive(Component, Clone)]
pub struct RenderTargetDepthStencils {
    main_target_resolution: Extent3d,
    main_target_texture: Texture,
    main_target_texture_view: TextureView,
    alt_target_resolution: Extent3d,
    alt_target_texture: Texture,
    alt_target_texture_view: TextureView,
}

impl RenderTargetDepthStencils {
    pub fn swap(&mut self) {
        std::mem::swap(&mut self.main_target_texture, &mut self.alt_target_texture);
        std::mem::swap(
            &mut self.main_target_texture_view,
            &mut self.alt_target_texture_view,
        );
        std::mem::swap(
            &mut self.main_target_resolution,
            &mut self.alt_target_resolution,
        );
    }
}

#[derive(Resource, Default)]
pub struct RenderTargetLookup(EntityHashMap<RenderTargetDepthStencils>);

pub fn prepare_depth_stencils(
    mut commands: Commands,
    device: Res<RenderDevice>,
    mut lookup: ResMut<RenderTargetLookup>,
    mut cameras: Query<(Entity, &ViewTarget), With<RenderPhase<UiNodeItem>>>,
) {
    for (entity, target) in cameras.iter_mut() {
        let mut is_new = false;
        let targets = lookup.0.entry(entity).or_insert_with(|| {
            is_new = true;
            let main = target.main_texture();
            let main_size = main.size();
            let main_target = device.create_texture(&TextureDescriptor {
                label: Some("render target stencil texture"),
                size: main_size,
                mip_level_count: target.sampled_main_texture().unwrap().mip_level_count(),
                sample_count: target.sampled_main_texture().unwrap().sample_count(),
                dimension: TextureDimension::D2,
                format: TextureFormat::Stencil8,
                usage: TextureUsages::RENDER_ATTACHMENT,
                view_formats: &[],
            });
            let main_target_view = main_target.create_view(&Default::default());

            let alt = target.main_texture_other();
            let alt_size = alt.size();
            let alt_target = device.create_texture(&TextureDescriptor {
                label: Some("render target alt stencil texture"),
                size: alt_size,
                mip_level_count: alt.mip_level_count(),
                sample_count: alt.sample_count(),
                dimension: TextureDimension::D2,
                format: TextureFormat::Stencil8,
                usage: TextureUsages::RENDER_ATTACHMENT,
                view_formats: &[],
            });
            let alt_target_view = alt_target.create_view(&default());
            RenderTargetDepthStencils {
                main_target_resolution: main_size,
                main_target_texture: main_target,
                main_target_texture_view: main_target_view,
                alt_target_resolution: alt_size,
                alt_target_texture: alt_target,
                alt_target_texture_view: alt_target_view,
            }
        });

        if !is_new {
            targets.swap();
        }

        if targets.main_target_resolution != target.main_texture().size()
            || targets.main_target_texture.mip_level_count()
                != target.sampled_main_texture().unwrap().mip_level_count()
            || targets.main_target_texture.sample_count()
                != target.sampled_main_texture().unwrap().sample_count()
        {
            let main = target.main_texture();
            let main_size = main.size();
            let main_target = device.create_texture(&TextureDescriptor {
                label: Some("render target stencil texture"),
                size: main_size,
                mip_level_count: target.sampled_main_texture().unwrap().mip_level_count(),
                sample_count: target.sampled_main_texture().unwrap().sample_count(),
                dimension: TextureDimension::D2,
                format: TextureFormat::Stencil8,
                usage: TextureUsages::RENDER_ATTACHMENT,
                view_formats: &[],
            });
            let main_target_view = main_target.create_view(&Default::default());
            targets.main_target_resolution = main_size;
            targets.main_target_texture = main_target;
            targets.main_target_texture_view = main_target_view;
        }

        commands.entity(entity).insert(targets.clone());
    }
}

impl ViewNode for UiLayoutNode {
    type ViewQuery = (
        Entity,
        &'static ExtractedCamera,
        &'static ViewTarget,
        &'static RenderPhase<UiNodeItem>,
        &'static RenderTargetDepthStencils,
        Option<&'static RenderLayers>,
    );

    fn run<'w>(
        &self,
        _: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        (entity, camera, target, phase, depth, camera_layer): bevy::ecs::query::QueryItem<
            'w,
            Self::ViewQuery,
        >,
        world: &'w World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let draw_functions = world.resource::<DrawFunctions<UiNodeItem>>();

        let device = render_context.render_device().clone();
        let encoder = render_context.command_encoder();
        encoder.push_debug_group("UiLayoutNode");

        {
            let rpass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("UiLayoutNode.rpass"),
                color_attachments: &[Some(target.get_color_attachment())],
                depth_stencil_attachment: Some(RenderPassDepthStencilAttachment {
                    view: &depth.main_target_texture_view,
                    depth_ops: None,
                    stencil_ops: Some(Operations {
                        load: LoadOp::Clear(0),
                        store: StoreOp::Store,
                    }),
                }),
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            let mut pass = TrackedRenderPass::new(&device, rpass);

            if let Some(viewport) = camera.viewport.as_ref() {
                pass.set_camera_viewport(viewport);
            }

            let mut draw_functions = draw_functions.write();
            for item in phase.items.iter() {
                let id =
                    if let Some(node_draw_function) = world.get::<NodeDrawFunction>(item.entity) {
                        node_draw_function.0
                    } else {
                        item.draw_function_id
                    };

                if let Some(render_layers) = world.get::<RenderLayers>(item.entity) {
                    if let Some(camera_layer) = camera_layer {
                        if !render_layers.eq(camera_layer) {
                            continue;
                        }
                    }
                }

                let draw_function = draw_functions.get_mut(id).unwrap();
                draw_function.prepare(world);
                draw_function.draw(world, &mut pass, entity, item);
            }
        }

        encoder.pop_debug_group();

        Ok(())
    }
}

pub const UTILS_SHADER: Handle<Shader> = Handle::weak_from_u128(0x551B640BDBE84944AC5D0A5081E7C956);

impl Plugin for UiRenderPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(app, UTILS_SHADER, "utils.wgsl", Shader::from_wgsl);

        load_internal_asset!(
            app,
            DefaultNodePipeline::SHADER,
            "default_node.wgsl",
            Shader::from_wgsl
        );

        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .add_render_graph_node::<ViewNodeRunner<UiLayoutNode>>(Core2d, UiLayoutNode)
            .add_render_graph_edges(
                Core2d,
                (
                    Node2d::EndMainPassPostProcessing,
                    UiLayoutNode,
                    Node2d::Upscaling,
                ),
            );
    }

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);

        render_app
            .init_resource::<ExtractedNodes>()
            .init_resource::<DefaultNodePipeline>()
            .init_resource::<PreparedResources>()
            .init_resource::<DrawFunctions<UiNodeItem>>()
            .init_resource::<RenderTargetLookup>()
            .add_render_command::<UiNodeItem, DefaultNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_ui_phases)
            .add_systems(ExtractSchedule, extract_nodes)
            .add_systems(
                Render,
                (
                    queue_ui_nodes.in_set(RenderSet::Queue),
                    prepare_depth_stencils.in_set(RenderSet::Prepare),
                    prepare_ui_nodes.in_set(RenderSet::Prepare),
                    sort_phase_system::<UiNodeItem>.in_set(RenderSet::PhaseSort),
                ),
            );
    }
}
