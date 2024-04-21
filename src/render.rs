use std::marker::PhantomData;
use std::ops::Range;

use bevy::app::Plugin;
use bevy::core_pipeline::core_2d::graph::{Core2d, Node2d};
use bevy::ecs::entity::{EntityHash, EntityHashMap};
use bevy::ecs::query::{QueryData, ROQueryItem};
use bevy::ecs::system::lifetimeless::SRes;
use bevy::ecs::system::{ReadOnlySystemParam, SystemParamItem};
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
    ShaderType, StoreOp, TextureFormat, VertexAttribute, VertexBufferLayout, VertexFormat,
    VertexState, VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::texture::BevyDefault;
use bevy::render::view::ViewTarget;
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bevy::sprite::Anchor;
use bevy::utils::nonmax::NonMaxU32;
use bevy::utils::HashMap;

use crate::math::{GlobalTransform, NodeSize, ZIndex};

/// This trait can be used to extend the node system for your own custom implemented nodes
///
/// All "first-class" nodes (image nodes, text nodes, etc.) should be implemented this way as well.
///
/// The node rendering systems will configure a vertex buffer that contains the following vertex:
/// ```
/// struct VertexInput {
///     @location(0) i_model_col0: vec2<f32>,
///     @location(1) i_model_col1: vec2<f32>,
///     @location(2) i_model_col2: vec2<f32>,
/// };
/// ```
///
/// Additional vertex buffers can be specified in this trait implementation.
pub trait SpecializedUiNode: 'static {
    type DrawFunction: RenderCommand<UiPhaseItem> + Send + Sync + 'static;
    type Extracted: Send + Sync + 'static;
    type QueryData: QueryData;

    fn extract(data: ROQueryItem<Self::QueryData>) -> Self::Extracted;
    fn vertex_shader() -> Handle<Shader>;
    fn vertex_shader_entrypoint() -> &'static str;
    fn fragment_shader() -> Handle<Shader>;
    fn fragment_shader_entrypoint() -> &'static str;
}

#[derive(Debug, Clone)]
pub struct UiPhaseItem {
    entity: Entity,
    z_index: isize,
    draw_function_id: DrawFunctionId,
    batch_range: Range<u32>,
    dynamic_offset: Option<NonMaxU32>,
}

impl PhaseItem for UiPhaseItem {
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

#[derive(Component, Debug, Copy, Clone, Reflect)]
pub struct UiNodeSettings {
    pub target_resolution: UVec2,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, ShaderType)]
pub struct LayoutUniform {
    pub screen_to_ndc: [Vec4; 3],
}

pub struct UiRenderPlugin;

unsafe impl<T: SpecializedUiNode> Send for UiNodePipeline<T> {}
unsafe impl<T: SpecializedUiNode> Sync for UiNodePipeline<T> {}

#[derive(Resource)]
pub struct UiNodePipeline<T: SpecializedUiNode> {
    pub bind_group_layout: BindGroupLayout,
    pub pipeline_id: CachedRenderPipelineId,
    _phantom: PhantomData<T>,
}

impl<T: SpecializedUiNode> FromWorld for UiNodePipeline<T> {
    fn from_world(world: &mut World) -> Self {
        let pipeline_cache = world.resource::<PipelineCache>();
        let render_device = world.resource::<RenderDevice>();

        let bind_group = render_device.create_bind_group_layout(
            "UiNodePipeline.view_uniform_layout",
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

        let pipeline_id = pipeline_cache.queue_render_pipeline(RenderPipelineDescriptor {
            label: Some("RenderPipelineDescriptor".into()),
            layout: vec![bind_group.clone()],
            push_constant_ranges: vec![],
            vertex: VertexState {
                shader: T::vertex_shader(),
                shader_defs: vec![],
                entry_point: T::vertex_shader_entrypoint().into(),
                buffers: vec![
                    VertexBufferLayout::from_vertex_formats(
                        VertexStepMode::Instance,
                        [VertexFormat::Float32x2; 3],
                    ),
                    VertexBufferLayout {
                        array_stride: VertexFormat::Float32x4.size(),
                        step_mode: VertexStepMode::Instance,
                        attributes: vec![VertexAttribute {
                            format: VertexFormat::Float32x4,
                            offset: 0,
                            shader_location: 3,
                        }],
                    },
                ],
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
            depth_stencil: None,
            multisample: MultisampleState {
                count: 1,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            fragment: Some(FragmentState {
                shader: T::fragment_shader(),
                shader_defs: vec![],
                entry_point: T::fragment_shader_entrypoint().into(),
                targets: vec![Some(ColorTargetState {
                    format: TextureFormat::bevy_default(),
                    blend: Some(BlendState::ALPHA_BLENDING),
                    write_mask: ColorWrites::all(),
                })],
            }),
        });

        Self {
            bind_group_layout: bind_group,
            pipeline_id,
            _phantom: PhantomData,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, RenderLabel, Default)]
pub struct UiLayoutNode;

pub struct ExtractedNode<T: SpecializedUiNode> {
    pub target_size: UVec2,
    pub affine: Affine2,
    pub z_index: isize,
    pub extracted: T::Extracted,
}

#[derive(Resource, Deref, DerefMut)]
pub struct ExtractedNodes<T: SpecializedUiNode>(EntityHashMap<ExtractedNode<T>>);

impl<T: SpecializedUiNode> Default for ExtractedNodes<T> {
    fn default() -> Self {
        Self(EntityHashMap::with_capacity_and_hasher(
            128,
            EntityHash::default(),
        ))
    }
}

pub fn extract_nodes<T: SpecializedUiNode>(
    nodes: Extract<
        Query<(
            Entity,
            &GlobalTransform,
            &NodeSize,
            &Anchor,
            &UiNodeSettings,
            &ZIndex,
            <T::QueryData as QueryData>::ReadOnly,
        )>,
    >,
    mut extracted_nodes: ResMut<ExtractedNodes<T>>,
) {
    extracted_nodes.clear();
    for (entity, transform, node_size, anchor, settings, z_index, extracted) in nodes.iter() {
        let mut affine = transform.affine();
        affine = affine
            * Affine2::from_mat2_translation(
                Mat2::from_scale_angle(node_size.0, 0.0),
                -(node_size.0 / 2.0 + (anchor.as_vec() * Vec2::new(1.0, -1.0)) * node_size.0),
            );
        extracted_nodes.insert(
            entity,
            ExtractedNode {
                target_size: settings.target_resolution,
                affine,
                z_index: z_index.0,
                extracted: T::extract(extracted),
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
                .insert(RenderPhase::<UiPhaseItem>::default());
        }
    }
}

#[derive(Resource)]
pub struct PreparedResources {
    pub vertex_buffer: BufferVec<[[f32; 2]; 3]>,
    pub index_buffer: BufferVec<u32>,
    pub uniform_buffer: DynamicUniformBuffer<LayoutUniform>,
    pub bind_group: Option<BindGroup>,
    pub offsets: HashMap<UVec2, u32>,
}

impl Default for PreparedResources {
    fn default() -> Self {
        Self {
            vertex_buffer: BufferVec::new(BufferUsages::VERTEX),
            index_buffer: BufferVec::new(BufferUsages::INDEX),
            uniform_buffer: DynamicUniformBuffer::default(),
            bind_group: None,
            offsets: HashMap::with_capacity(16),
        }
    }
}

pub fn queue_ui_nodes<T: SpecializedUiNode>(
    draw_functions: Res<DrawFunctions<UiPhaseItem>>,
    mut ui_phases: Query<&mut RenderPhase<UiPhaseItem>>,
    nodes: Res<ExtractedNodes<T>>,
) {
    let function = draw_functions.read().get_id::<T::DrawFunction>().unwrap();
    for mut phase in ui_phases.iter_mut() {
        for (entity, node) in nodes.iter() {
            phase.items.push(UiPhaseItem {
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

pub fn prepare_ui_nodes<T: SpecializedUiNode>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    pipeline: Res<UiNodePipeline<T>>,
    resources: ResMut<PreparedResources>,
    extracted_nodes: Res<ExtractedNodes<T>>,
    mut ui_phases: Query<&mut RenderPhase<UiPhaseItem>>,
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
        let mut uniform_writer = resources
            .uniform_buffer
            .get_writer(total_length, &render_device, &render_queue)
            .unwrap();

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
                            screen_to_ndc: cols,
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

    resources.bind_group = Some(render_device.create_bind_group(
        "view_uniform_bind_group",
        &pipeline.bind_group_layout,
        &[BindGroupEntry {
            binding: 0,
            resource: BindingResource::Buffer(BufferBinding {
                buffer: resources.uniform_buffer.buffer().unwrap(),
                offset: 0,
                size: Some(LayoutUniform::min_size()),
            }),
        }],
    ));

    resources
        .vertex_buffer
        .write_buffer(&render_device, &render_queue);
}

pub struct BindLayoutUniform<const I: usize>;
pub struct BindVertexBuffer<const I: usize>;
pub struct DrawUiPhaseItem;
pub struct BindNodePipeline<T: SpecializedUiNode>(PhantomData<T>);

impl<T: SpecializedUiNode> RenderCommand<UiPhaseItem> for BindNodePipeline<T> {
    type Param = (SRes<UiNodePipeline<T>>, SRes<PipelineCache>);
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiPhaseItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        (pipeline, cache): SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let pipeline = pipeline.into_inner();
        let cache = cache.into_inner();
        let Some(pipeline) = cache.get_render_pipeline(pipeline.pipeline_id) else {
            return RenderCommandResult::Failure;
        };

        pass.set_render_pipeline(pipeline);
        RenderCommandResult::Success
    }
}

impl<const I: usize> RenderCommand<UiPhaseItem> for BindLayoutUniform<I> {
    type Param = SRes<PreparedResources>;
    type ViewQuery = ();
    type ItemQuery = ();

    fn render<'w>(
        item: &UiPhaseItem,
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

impl<const I: usize> RenderCommand<UiPhaseItem> for BindVertexBuffer<I> {
    type Param = SRes<PreparedResources>;
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiPhaseItem,
        _view: ROQueryItem<'w, Self::ViewQuery>,
        _entity: Option<ROQueryItem<'w, Self::ItemQuery>>,
        param: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        let param = param.into_inner();

        pass.set_vertex_buffer(I, param.vertex_buffer.buffer().unwrap().slice(..));
        pass.set_vertex_buffer(1, param.vertex_buffer.buffer().unwrap().slice(..));
        pass.set_index_buffer(
            param.index_buffer.buffer().unwrap().slice(..),
            0,
            IndexFormat::Uint32,
        );

        RenderCommandResult::Success
    }
}

impl RenderCommand<UiPhaseItem> for DrawUiPhaseItem {
    type Param = ();
    type ItemQuery = ();
    type ViewQuery = ();

    fn render<'w>(
        item: &UiPhaseItem,
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
    type ViewQuery = (&'static ViewTarget, &'static RenderPhase<UiPhaseItem>);

    fn run<'w>(
        &self,
        _: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        (target, phase): bevy::ecs::query::QueryItem<'w, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let draw_functions = world.resource::<DrawFunctions<UiPhaseItem>>();

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
                let draw_function = draw_functions.get_mut(item.draw_function_id).unwrap();
                draw_function.draw(world, &mut pass, item.entity, item);
            }
        }

        encoder.pop_debug_group();

        Ok(())
    }
}

pub struct SpecializedNodePlugin<T: SpecializedUiNode>(PhantomData<T>)
where
    <T::DrawFunction as RenderCommand<UiPhaseItem>>::Param: ReadOnlySystemParam;

unsafe impl<T: SpecializedUiNode> Send for SpecializedNodePlugin<T> where
    <T::DrawFunction as RenderCommand<UiPhaseItem>>::Param: ReadOnlySystemParam
{
}
unsafe impl<T: SpecializedUiNode> Sync for SpecializedNodePlugin<T> where
    <T::DrawFunction as RenderCommand<UiPhaseItem>>::Param: ReadOnlySystemParam
{
}

impl<T: SpecializedUiNode> Default for SpecializedNodePlugin<T>
where
    <T::DrawFunction as RenderCommand<UiPhaseItem>>::Param: ReadOnlySystemParam,
{
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl<T: SpecializedUiNode> Plugin for SpecializedNodePlugin<T>
where
    <T::DrawFunction as RenderCommand<UiPhaseItem>>::Param: ReadOnlySystemParam,
{
    fn build(&self, _: &mut App) {}

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);

        render_app
            .init_resource::<UiNodePipeline<T>>()
            .init_resource::<ExtractedNodes<T>>()
            .add_render_command::<UiPhaseItem, T::DrawFunction>()
            .add_systems(ExtractSchedule, extract_nodes::<T>)
            .add_systems(
                Render,
                (
                    queue_ui_nodes::<T>.in_set(RenderSet::Queue),
                    prepare_ui_nodes::<T>.in_set(RenderSet::Prepare),
                ),
            );
    }
}

impl Plugin for UiRenderPlugin {
    fn build(&self, app: &mut App) {
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
            .init_resource::<PreparedResources>()
            .init_resource::<DrawFunctions<UiPhaseItem>>()
            .add_systems(ExtractSchedule, extract_ui_phases);
    }
}
