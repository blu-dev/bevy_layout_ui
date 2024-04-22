#import bevy_layout_ui::{NodeVertexInput, LayoutUniform, transform_node_to_screen}

@group(0) @binding(0) var<uniform> view: LayoutUniform;

@group(1) @binding(0) var color_texture: texture_2d<f32>;
@group(1) @binding(1) var color_sampler: sampler;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vertex(
    @builtin(vertex_index) vertex_index: u32,
    in: NodeVertexInput,
) -> VertexOutput {
    let vertex: vec2<f32> = vec2(
        f32(vertex_index & 0x1u),
        f32((vertex_index & 0x2u) >> 1u),
    );

    var output: VertexOutput;
    let pos = transform_node_to_screen(in, view, vertex);
    output.position = vec4<f32>(pos, 0.0, 1.0);
    output.uv = vertex;

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(color_texture, color_sampler, in.uv);
}
