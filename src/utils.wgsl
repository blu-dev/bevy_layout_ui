#define_import_path bevy_layout_ui

struct CommonNodeUniform {
    layout_to_ndc: mat3x3<f32>,
    vertex_colors: array<vec4<f32>, 4>,
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
