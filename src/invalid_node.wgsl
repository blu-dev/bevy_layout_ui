#import bevy_layout_ui::{LayoutUniform, NodeVertexInput, transform_node_to_screen};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) ndc: vec2<f32>
};

@group(0) @binding(0) var<uniform> view: LayoutUniform;

@vertex
fn vertex(@builtin(vertex_index) vertex_index: u32, in: NodeVertexInput) -> VertexOutput {
    let vertex: vec2<f32> = vec2(
        f32(vertex_index & 0x1u),
        f32((vertex_index & 0x2u) >> 1u),
    );

    var output: VertexOutput;
    let pos = transform_node_to_screen(in, view, vertex);
    output.position = vec4<f32>(pos.xy, 0.0, 1.0);
    output.ndc = pos.xy;

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    let point = abs(in.ndc + vec2<f32>(2.0)) * vec2<f32>(10.0);
    let px = point.x % 2.0;
    let py = point.y % 2.0;
    if (px > 1.0) {
        if (py > 1.0) {
            return vec4<f32>(1.0, 0.0, 1.0, 1.0);
        } else {
            return vec4<f32>(0.0, 0.0, 0.0, 1.0);
        }
    } else {
        if (py > 1.0) {
            return vec4<f32>(0.0, 0.0, 0.0, 1.0);
        } else {
            return vec4<f32>(1.0, 0.0, 1.0, 1.0);
        }
    }
}
