
#import bevy_layout_ui::{
    NodeVertexInput, CommonNodeUniform, transform_node_to_screen, preprocess_fragment,
    postprocess_fragment
}

@group(0) @binding(0) var<uniform> view: CommonNodeUniform;

@group(1) @binding(0) var glyph_texture: texture_2d<f32>;
@group(1) @binding(1) var glyph_sampler: sampler;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) node_uv: vec2<f32>,
};

struct TextNodeVertexInput {
    @location(3) node_uv: vec2<f32>,
    @location(4) tex_uv: vec2<f32>,
};

@vertex
fn vertex(
    instance_in: NodeVertexInput,
    vertex_in: TextNodeVertexInput,
) -> VertexOutput {
    var output: VertexOutput;
    let pos = transform_node_to_screen(instance_in, view.layout_to_ndc, vertex_in.node_uv);
    output.position = vec4<f32>(pos, 0.0, 1.0);
    output.uv = vertex_in.tex_uv;
    output.node_uv = vertex_in.node_uv;

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    preprocess_fragment(in.node_uv, view.clip_rect);
    let alpha = textureSample(glyph_texture, glyph_sampler, in.uv).r;

    return postprocess_fragment(in.node_uv, vec4(1.0, 1.0, 1.0, alpha), view);
}
