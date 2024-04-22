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
use bevy::render::mesh::PrimitiveTopology;
use bevy::render::render_graph::{RenderGraphApp, RenderLabel, ViewNode, ViewNodeRunner};
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctionId, DrawFunctions, PhaseItem, RenderCommand, RenderCommandResult,
    RenderPhase, TrackedRenderPass,
};
use bevy::render::render_resource::{
    BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingResource, BindingType,
    BlendState, BufferBinding, BufferBindingType, BufferUsages, BufferVec, CachedRenderPipelineId,
    ColorTargetState, ColorWrites, DynamicUniformBuffer, FragmentState, FrontFace, IndexFormat,
    LoadOp, MultisampleState, Operations, PipelineCache, PolygonMode, PrimitiveState,
    RenderPassColorAttachment, RenderPassDescriptor, RenderPipelineDescriptor, ShaderStages,
    ShaderType, StoreOp, TextureFormat, VertexBufferLayout, VertexFormat, VertexState,
    VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::texture::BevyDefault;
use bevy::render::view::ViewTarget;
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bevy::sprite::Anchor;
use bevy::utils::nonmax::NonMaxU32;
use bevy::utils::HashMap;
use bytemuck::{Pod, Zeroable};

use crate::math::{GlobalTransform, NodeSize, ZIndex};

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

/// Layout uniform managed by the UI layouting systems
///
/// This uniform contains a transformation matrix that will convert layout-space screen coordinates
/// to normalized device coordinates (NDC).
///
/// This uniform is built off of the layout's target resolution, and does not depend on the camera's
/// render target size.
///
/// This uniformcan be imported from `bevy_layout_ui::LayoutUniform`, and is defined:
/// ```wgsl
/// struct LayoutUniform {
///     layout_to_ndc: mat3x3
/// };
/// ```
#[repr(C)]
#[derive(Debug, Copy, Clone, ShaderType)]
pub struct LayoutUniform {
    layout_to_ndc: [Vec4; 3],
}

impl LayoutUniform {
    /// Converts this uniform into an affine
    pub fn into_affine(&self) -> Affine2 {
        let mat3 = Mat3::from_cols(
            self.layout_to_ndc[0].xyz(),
            self.layout_to_ndc[1].xyz(),
            self.layout_to_ndc[2].xyz(),
        );

        Affine2::from_mat3(mat3)
    }

    /// Converts an affine into this uniform
    pub fn from_affine(affine: Affine2) -> Self {
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
        }
    }

    /// Takes a layout size and constructs a layout -> NDC transformation
    pub fn from_layout_size(size: UVec2) -> Self {
        let size = size.as_vec2();
        let affine = Affine2::from_scale_angle_translation(
            2.0 * size.recip() * Vec2::new(1.0, -1.0),
            0.0,
            Vec2::new(-1.0, 1.0),
        );
        Self::from_affine(affine)
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
pub type InvalidNodeDrawFunction = (
    BindVertexBuffer<0>,
    BindLayoutUniform<0>,
    BindInvalidPipeline,
    DrawUiPhaseItem,
);

/// Pipeline for UI nodes that do not have their draw function replaced
///
/// If this pipeline is being run, it means that you have forgotten to change your [`DrawFunctionId`]
/// in the [`Prepare`](RenderSet::Prepare) phase. You can do so by either editing the `draw_function_id`
/// field of the `UiNodeItem` for the node or by inserting a [`NodeDrawFunction`] component on
/// the entity in the render world.
#[derive(Resource, Debug)]
pub struct InvalidNodePipeline {
    cached_pipeline_id: CachedRenderPipelineId,
    bind_group_layout: BindGroupLayout,
}

impl InvalidNodePipeline {
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
                topology: PrimitiveTopology::TriangleStrip,
                strip_index_format: None,
                front_face: FrontFace::Ccw,
                cull_mode: None,
                unclipped_depth: false,
                polygon_mode: PolygonMode::Fill,
                conservative: false,
            },
            depth_stencil: None,
            multisample: MultisampleState::default(),
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

impl FromWorld for InvalidNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let pipeline_cache = world.resource::<PipelineCache>();
        let device = world.resource::<RenderDevice>();

        let layout = device.create_bind_group_layout(
            "InvalidNodePipeline.LayoutUniformLayout",
            &[BindGroupLayoutEntry {
                binding: 0,
                visibility: ShaderStages::VERTEX,
                ty: BindingType::Buffer {
                    ty: BufferBindingType::Uniform,
                    has_dynamic_offset: true,
                    min_binding_size: Some(LayoutUniform::min_size()),
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
    pub target_resolution: UVec2,
}

pub struct UiRenderPlugin;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, RenderLabel, Default)]
pub struct UiLayoutNode;

pub struct ExtractedNode {
    target_size: UVec2,
    affine: Affine2,
    z_index: isize,
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
                target_size: data.settings.target_resolution,
                affine,
                z_index: data.z_index.0,
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
    uniform_buffer: DynamicUniformBuffer<LayoutUniform>,
    offsets: HashMap<UVec2, u32>,
    bind_group: Option<BindGroup>,
}

impl Default for PreparedResources {
    fn default() -> Self {
        Self {
            vertex_buffer: BufferVec::new(BufferUsages::VERTEX),
            index_buffer: BufferVec::new(BufferUsages::INDEX),
            uniform_buffer: DynamicUniformBuffer::default(),
            offsets: HashMap::with_capacity(16),
            bind_group: None,
        }
    }
}

pub fn queue_ui_nodes(
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut ui_phases: Query<&mut RenderPhase<UiNodeItem>>,
    nodes: Res<ExtractedNodes>,
) {
    let function = draw_functions
        .read()
        .get_id::<InvalidNodeDrawFunction>()
        .unwrap();
    for mut phase in ui_phases.iter_mut() {
        for (entity, node) in nodes.iter() {
            phase.items.push(UiNodeItem {
                entity: *entity,
                z_index: node.z_index,
                draw_function_id: function,
                // batch_range and dynamic_offset are set in the prepare step
                batch_range: 0..0,
                dynamic_offset: None,
            });
        }
    }
}

pub fn prepare_ui_nodes(
    mut commands: Commands,
    invalid_pipeline: Res<InvalidNodePipeline>,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    resources: ResMut<PreparedResources>,
    extracted_nodes: Res<ExtractedNodes>,
    mut ui_phases: Query<&mut RenderPhase<UiNodeItem>>,
) {
    let resources = resources.into_inner();
    if resources.index_buffer.is_empty() {
        resources.index_buffer.extend([2, 1, 0, 2, 3, 1]);
        resources
            .index_buffer
            .write_buffer(&render_device, &render_queue);
    }

    resources.vertex_buffer.clear();
    resources.uniform_buffer.clear();
    resources.offsets.clear();

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

                match resources.offsets.get(&node.target_size) {
                    Some(offset) => item.dynamic_offset = NonMaxU32::new(*offset),
                    None => {
                        let matrix = Mat3::from_scale_angle_translation(
                            Vec2::new(
                                2.0 / node.target_size.x as f32,
                                -2.0 / node.target_size.y as f32,
                            ),
                            0.0,
                            Vec2::new(-1.0, 1.0),
                        );

                        let cols = matrix.to_cols_array_2d();
                        let cols = [
                            Vec4::new(cols[0][0], cols[0][1], cols[0][2], 0.0),
                            Vec4::new(cols[1][0], cols[1][1], cols[1][2], 0.0),
                            Vec4::new(cols[2][0], cols[2][1], cols[2][2], 0.0),
                        ];

                        let offset = uniform_writer.write(&LayoutUniform {
                            layout_to_ndc: cols,
                        });

                        resources.offsets.insert(node.target_size, offset);
                        item.dynamic_offset = NonMaxU32::new(offset);
                    }
                }

                item.batch_range = (idx as u32)..(idx as u32 + 1);

                commands.get_or_spawn(item.entity);
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
                size: Some(LayoutUniform::min_size()),
            }),
        }],
    ));
}

pub struct BindLayoutUniform<const I: usize>;
pub struct BindVertexBuffer<const I: usize>;
pub struct BindInvalidPipeline;
pub struct DrawUiPhaseItem;

impl RenderCommand<UiNodeItem> for BindInvalidPipeline {
    type Param = (SRes<PipelineCache>, SRes<InvalidNodePipeline>);
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        (cache, pipeline): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let pipeline = pipeline.into_inner();
        let cache = cache.into_inner();
        let Some(pipeline) = cache.get_render_pipeline(pipeline.cached_pipeline_id) else {
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

impl ViewNode for UiLayoutNode {
    type ViewQuery = (
        Entity,
        &'static ViewTarget,
        &'static RenderPhase<UiNodeItem>,
    );

    fn run<'w>(
        &self,
        _: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        (entity, target, phase): bevy::ecs::query::QueryItem<'w, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let draw_functions = world.resource::<DrawFunctions<UiNodeItem>>();

        let device = render_context.render_device().clone();
        let encoder = render_context.command_encoder();
        encoder.push_debug_group("UiLayoutNode");

        {
            let rpass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("UiLayoutNode.rpass"),
                color_attachments: &[Some(RenderPassColorAttachment {
                    view: target.main_texture_view(),
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Load,
                        store: StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
            });

            let mut pass = TrackedRenderPass::new(&device, rpass);

            let mut draw_functions = draw_functions.write();
            for item in phase.items.iter() {
                let id =
                    if let Some(node_draw_function) = world.get::<NodeDrawFunction>(item.entity) {
                        node_draw_function.0
                    } else {
                        item.draw_function_id
                    };

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
            InvalidNodePipeline::SHADER,
            "invalid_node.wgsl",
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
            .init_resource::<InvalidNodePipeline>()
            .init_resource::<PreparedResources>()
            .init_resource::<DrawFunctions<UiNodeItem>>()
            .add_render_command::<UiNodeItem, InvalidNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_ui_phases)
            .add_systems(ExtractSchedule, extract_nodes)
            .add_systems(
                Render,
                (
                    queue_ui_nodes.in_set(RenderSet::Queue),
                    prepare_ui_nodes.in_set(RenderSet::Prepare),
                ),
            );
    }
}
