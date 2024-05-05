
#import bevy_layout_ui::{NodeVertexInput, CommonNodeUniform, transform_node_to_screen}

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

fn interpolate_vertex_color(pos: vec2<f32>) -> vec4<f32> {
    var total: f32 = 0.0;
    var distances: array<f32, 4> = array(0.0, 0.0, 0.0, 0.0);
    for (var i = 0u; i < 4u; i++) {
        let p = vec2<f32>(f32(i & 0x1u), f32((i & 0x2u) >> 1u));
        var d = distance(p, pos);
        if (d == 0.0) {
            return view.vertex_colors[i];
        }

        d = 1.0 / exp2(d);
        total += d;
        distances[i] = d;
    }

    var color = vec4<f32>(0.0);
    for (var i = 0u; i < 4u; i++) {
        let ratio: f32 = distances[i] / total;
        color += view.vertex_colors[i] * ratio;
    }
    return color;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let alpha = textureSample(glyph_texture, glyph_sampler, in.uv).r * view.opacity;

    // return vec4(1.0);
    return vec4(1.0, 1.0, 1.0, alpha) * interpolate_vertex_color(in.node_uv);
}
