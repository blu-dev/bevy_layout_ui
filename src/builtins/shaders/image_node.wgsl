#import bevy_layout_ui::{NodeVertexInput, CommonNodeUniform, transform_node_to_screen}

@group(0) @binding(0) var<uniform> view: CommonNodeUniform;

@group(1) @binding(0) var color_texture: texture_2d<f32>;
@group(1) @binding(1) var color_sampler: sampler;

#ifdef USE_MASK_IMAGE
@group(2) @binding(0) var mask_texture: texture_2d<f32>;
@group(2) @binding(1) var mask_sampler: sampler;

#endif

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) vertex_color: vec4<f32>,
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
    let pos = transform_node_to_screen(in, view.layout_to_ndc, vertex);
    output.position = vec4<f32>(pos, 0.0, 1.0);
    output.uv = vertex;
    output.vertex_color = view.vertex_colors[vertex_index];

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    var sample = textureSample(color_texture, color_sampler, in.uv) * in.vertex_color;

#ifdef USE_MASK_IMAGE
    sample.a *= textureSample(mask_texture, mask_sampler, in.uv).a;
#endif

    return sample;
}
