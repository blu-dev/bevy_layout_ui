#import bevy_render::maths::affine2_to_square

struct ViewUniform {
    screen_size: vec2<f32>,
    screen_to_ndc: mat3x3<f32>
};

@group(0) @binding(0) var<uniform> view: ViewUniform;

struct VertexInput {
    @builtin(vertex_index) vertex_index: u32,
    @location(0) i_model_col0: vec2<f32>,
    @location(1) i_model_col1: vec2<f32>,
    @location(2) i_model_col2: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vertex(in: VertexInput) -> VertexOutput {
    let vertex: vec2<f32> = vec2(
        f32(in.vertex_index & 0x1u),
        f32((in.vertex_index & 0x2u) >> 1u),
    );

    var output: VertexOutput;
    let pos = view.screen_to_ndc * mat3x3<f32>(vec3(in.i_model_col0, 0.0), vec3(in.i_model_col1, 0.0), vec3(in.i_model_col2, 1.0)) * vec3(vertex, 1.0);
    output.position = vec4<f32>(pos.xy, 0.0, 1.0);
    output.uv = vertex;

    return output;
}

@fragment
fn fragment(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4(in.uv.x, in.uv.y, 0.0, 1.0);
}
