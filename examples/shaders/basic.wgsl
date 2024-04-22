#import bevy_layout_ui::{NodeVertexInput, CommonNodeUniform, transform_node_to_screen}

@group(0) @binding(0) var<uniform> view: CommonNodeUniform;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) color: vec4<f32>,
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
    output.color = view.vertex_colors[vertex_index];

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
