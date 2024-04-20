use std::num::NonZeroU64;

use bevy::app::Plugin;
use bevy::asset::load_internal_asset;
use bevy::core_pipeline::core_2d::graph::{Core2d, Node2d};
use bevy::ecs::entity::{EntityHash, EntityHashMap};
use bevy::math::Affine2;
use bevy::prelude::*;
use bevy::render::mesh::PrimitiveTopology;
use bevy::render::render_graph::{RenderGraphApp, RenderLabel, ViewNode, ViewNodeRunner};
use bevy::render::render_resource::{
    BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingType, BlendState,
    BufferBindingType, BufferUsages, BufferVec, CachedRenderPipelineId, ColorTargetState,
    ColorWrites, FragmentState, FrontFace, IndexFormat, LoadOp, MultisampleState, Operations,
    PipelineCache, PolygonMode, PrimitiveState, RenderPassColorAttachment, RenderPassDescriptor,
    RenderPipelineDescriptor, ShaderStages, ShaderType, StoreOp, TextureFormat, VertexBufferLayout,
    VertexFormat, VertexState, VertexStepMode,
};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::texture::BevyDefault;
use bevy::render::view::ViewTarget;
use bevy::render::{Extract, Render, RenderApp, RenderSet};
use bytemuck::{Pod, Zeroable};

use crate::math::{BoundingBox, GlobalTransform, NodeSize, NonAxisAlignedBoundingBox};

#[repr(C)]
#[derive(Pod, Zeroable, Debug, Copy, Clone, ShaderType)]
pub struct ViewUniform {
    pub render_target_size: Vec2,
    pub padding: Vec2,
    pub screen_to_ndc: [[f32; 4]; 3],
}

pub struct UiRenderPlugin;

#[derive(Resource)]
pub struct UiNodePipeline {
    pub bind_group_layout: BindGroupLayout,
    pub pipeline_id: CachedRenderPipelineId,
}

impl UiNodePipeline {
    pub const SHADER: Handle<Shader> = Handle::weak_from_u128(0xCDE15DE115FF47E796AF6C535F5AE089);
}

impl FromWorld for UiNodePipeline {
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
                    has_dynamic_offset: false,
                    min_binding_size: Some(ViewUniform::min_size()),
                },
                count: None,
            }],
        );

        let pipeline_id = pipeline_cache.queue_render_pipeline(RenderPipelineDescriptor {
            label: Some("RenderPipelineDescriptor".into()),
            layout: vec![bind_group.clone()],
            push_constant_ranges: vec![],
            vertex: VertexState {
                shader: Self::SHADER.clone(),
                shader_defs: vec![],
                entry_point: "vertex".into(),
                buffers: vec![VertexBufferLayout::from_vertex_formats(
                    VertexStepMode::Instance,
                    [VertexFormat::Float32x2; 3],
                )],
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
                shader: Self::SHADER.clone(),
                shader_defs: vec![],
                entry_point: "fragment".into(),
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
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, RenderLabel, Default)]
pub struct UiLayoutNode;

pub struct ExtractedNode {
    pub affine: Affine2,
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

pub fn extract_nodes(
    nodes: Extract<Query<(Entity, &GlobalTransform, &NodeSize)>>,
    mut extracted_nodes: ResMut<ExtractedNodes>,
) {
    extracted_nodes.clear();
    for (entity, transform, node_size) in nodes.iter() {
        let mut affine = transform.affine();
        affine.matrix2 *= Mat2::from_scale_angle(node_size.0, 0.0);
        extracted_nodes.insert(entity, ExtractedNode { affine });
    }
}

#[derive(Resource)]
pub struct PreparedResources {
    pub vertex_buffer: BufferVec<[[f32; 2]; 3]>,
    pub index_buffer: BufferVec<u32>,
    pub uniform_buffer: BufferVec<ViewUniform>,
    pub bind_group: Option<BindGroup>,
}

impl Default for PreparedResources {
    fn default() -> Self {
        Self {
            vertex_buffer: BufferVec::new(BufferUsages::VERTEX),
            index_buffer: BufferVec::new(BufferUsages::INDEX),
            uniform_buffer: BufferVec::new(BufferUsages::UNIFORM),
            bind_group: None,
        }
    }
}

pub fn prepare_extracted_nodes(
    mut resources: ResMut<PreparedResources>,
    pipeline: Res<UiNodePipeline>,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    extracted: Res<ExtractedNodes>,
) {
    if resources.index_buffer.is_empty() {
        resources.index_buffer.extend([2, 1, 0, 2, 3, 1]);
        resources
            .index_buffer
            .write_buffer(&render_device, &render_queue);
    }

    if resources.uniform_buffer.is_empty() {
        let matrix = Mat3::from_scale_angle_translation(
            Vec2::new(960.0f32.recip(), -540.0f32.recip()),
            0.0,
            Vec2::new(-1.0, 1.0),
        );

        let cols = matrix.to_cols_array_2d();
        let cols = [
            [cols[0][0], cols[0][1], cols[0][2], 0.0],
            [cols[1][0], cols[1][1], cols[1][2], 0.0],
            [cols[2][0], cols[2][1], cols[2][2], 0.0],
        ];

        resources.uniform_buffer.push(ViewUniform {
            render_target_size: Vec2::new(1920.0, 1080.0),
            padding: Vec2::ZERO,
            screen_to_ndc: cols,
        });

        resources
            .uniform_buffer
            .write_buffer(&render_device, &render_queue);

        resources.bind_group = Some(
            render_device.create_bind_group(
                "view_uniform_bind_group",
                &pipeline.bind_group_layout,
                &[BindGroupEntry {
                    binding: 0,
                    resource: resources
                        .uniform_buffer
                        .buffer()
                        .unwrap()
                        .as_entire_binding(),
                }],
            ),
        );
    }

    resources.vertex_buffer.clear();

    for extracted in extracted.values() {
        resources
            .vertex_buffer
            .push(extracted.affine.to_cols_array_2d());
    }

    resources
        .vertex_buffer
        .write_buffer(&render_device, &render_queue);
}

impl ViewNode for UiLayoutNode {
    type ViewQuery = &'static ViewTarget;

    fn run<'w>(
        &self,
        graph: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        target: bevy::ecs::query::QueryItem<'w, Self::ViewQuery>,
        world: &'w World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let pipeline_cache = world.resource::<PipelineCache>();
        let resources = world.resource::<PreparedResources>();
        let Some(pipeline) =
            pipeline_cache.get_render_pipeline(world.resource::<UiNodePipeline>().pipeline_id)
        else {
            return Ok(());
        };

        let encoder = render_context.command_encoder();
        encoder.push_debug_group("UiLayoutNode");

        {
            let mut rpass = encoder.begin_render_pass(&RenderPassDescriptor {
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

            rpass.set_pipeline(pipeline);
            rpass.set_bind_group(0, resources.bind_group.as_ref().unwrap(), &[]);
            rpass.set_vertex_buffer(0, *resources.vertex_buffer.buffer().unwrap().slice(..));
            rpass.set_index_buffer(
                *resources.index_buffer.buffer().unwrap().slice(..),
                IndexFormat::Uint32,
            );
            rpass.draw_indexed(0..6, 0, 0..resources.vertex_buffer.len() as u32);
        }

        encoder.pop_debug_group();

        Ok(())
    }
}

impl Plugin for UiRenderPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            UiNodePipeline::SHADER,
            "shaders/basic.wgsl",
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
            .init_resource::<PreparedResources>()
            .init_resource::<UiNodePipeline>()
            .init_resource::<ExtractedNodes>()
            .add_systems(ExtractSchedule, extract_nodes)
            .add_systems(Render, prepare_extracted_nodes.in_set(RenderSet::Prepare));
    }
}
