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

fn interpolate_vertex_color(pos: vec2<f32>, view_: CommonNodeUniform) -> vec4<f32> {
    var view = view_;
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