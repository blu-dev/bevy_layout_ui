
struct LayoutUniform {
    layout_to_ndc: mat3x3<f32>
};

struct VertexInput {
    @builtin(vertex_index) vertex_index: u32,
    @location(0) model_col0: vec2<f32>,
    @location(1) model_col1: vec2<f32>,
    @location(2) model_col2: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) ndc: vec2<f32>
};

@group(0) @binding(0) var<uniform> view: LayoutUniform;

@vertex
fn vertex(in: VertexInput) -> VertexOutput {
    let vertex: vec2<f32> = vec2(
        f32(in.vertex_index & 0x1u),
        f32((in.vertex_index & 0x2u) >> 1u),
    );

    var output: VertexOutput;
    let pos = view.layout_to_ndc * mat3x3<f32>(vec3(in.model_col0, 0.0), vec3(in.model_col1, 0.0), vec3(in.model_col2, 1.0)) * vec3(vertex, 1.0);
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
