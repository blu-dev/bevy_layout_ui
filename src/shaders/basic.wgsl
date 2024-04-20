
struct VertexInput {
    @builtin(vertex_index) vertex_index: u32,
    @location(0) top_left: vec2<f32>,
    @location(1) top_right: vec2<f32>,
    @location(2) bottom_right: vec2<f32>,
    @location(3) bottom_left: vec2<f32>,
};

@vertex
fn vertex(in: VertexInput) -> @builtin(position) vec4<f32> {
    switch in.vertex_index {
        case 0u: {
            return vec4(in.top_left, 0.0, 1.0);
        }
        case 1u: {
            return vec4(in.top_right, 0.0, 1.0);
        }
        case 2u: {
            return vec4(in.bottom_right, 0.0, 1.0);
        }
        case 3u: {
            return vec4(in.bottom_left, 0.0, 1.0);
        }
        default: {
            return vec4(0.0);
        }
    }
}

@fragment
fn fragment() -> @location(0) vec4<f32> {
    return vec4(1.0, 0.5, 0.3, 0.4);
}
