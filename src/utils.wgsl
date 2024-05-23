#define_import_path bevy_layout_ui

struct CommonNodeUniform {
    layout_to_ndc: mat3x3<f32>,
    vertex_colors: array<vec4<f32>, 4>,
    clip_rect: vec4<f32>,
    opacity: f32
};

struct NodeVertexInput {
    @location(0) model_col0_: vec2<f32>,
    @location(1) model_col1_: vec2<f32>,
    @location(2) model_col2_: vec2<f32>,
};

fn transform_node_layout(model: NodeVertexInput, coordinates: vec2<f32>) -> vec2<f32> {
    let matrix = mat3x3<f32>(
        vec3(model.model_col0_, 0.0),
        vec3(model.model_col1_, 0.0),
        vec3(model.model_col2_, 1.0)
    );

    return (matrix * vec3(coordinates, 1.0)).xy;
}

fn transform_node_to_screen(model: NodeVertexInput, layout_to_ndc: mat3x3<f32>, coordinates: vec2<f32>) -> vec2<f32> {
    let matrix = mat3x3<f32>(
        vec3(model.model_col0_, 0.0),
        vec3(model.model_col1_, 0.0),
        vec3(model.model_col2_, 1.0)
    );

    return (layout_to_ndc * matrix * vec3(coordinates, 1.0)).xy;
}

fn interpolate_vertex_color(pos: vec2<f32>, view: CommonNodeUniform) -> vec4<f32> {
    let x1 = view.vertex_colors[0] * (1.0 - pos.x) + view.vertex_colors[1] * pos.x;
    let x2 = view.vertex_colors[2] * (1.0 - pos.x) + view.vertex_colors[3] * pos.x;
    return x1 * (1.0 - pos.y) + x2 * pos.y;
}

fn preprocess_fragment(uv: vec2<f32>, clip: vec4<f32>) {
    if (any(uv < clip.xy) || any(uv > clip.zw)) {
        discard;
    }
}

fn postprocess_fragment(uv: vec2<f32>, sample: vec4<f32>, view: CommonNodeUniform) -> vec4<f32> {
    var new_sample = sample * interpolate_vertex_color(uv, view);
    new_sample.a *= view.opacity;
    return new_sample;
}
