#import bevy_layout_ui::{
    NodeVertexInput, CommonNodeUniform, transform_node_to_screen, preprocess_fragment,
    postprocess_fragment
}

// Contains metadata about the layout of the text that is currently being rendered
struct TextLayout {
    outline_color: vec4<f32>,
    size: vec2<f32>,
    outline_thickness: f32,
    render_size: f32,
};

@group(0) @binding(0) var<uniform> view: CommonNodeUniform;
@group(1) @binding(0) var<uniform> text_layout: TextLayout;
@group(1) @binding(1) var<uniform> glyph_origin: vec2<f32>;

struct VertexIn {
    // These should be provided for every vertex, even if it is not an outline (if it is not an outline)
    // then `normal` should be set to vec2(0.0)
    @location(3) glyph_position: vec2<f32>,
    @location(4) normal: vec2<f32>,
};

struct VertexOut {
    @builtin(position) pos: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) node_uv: vec2<f32>,
    @location(2) is_outline: u32,
};

@vertex
fn vertex(instance_in: NodeVertexInput, in: VertexIn) -> VertexOut {
    let cached_render_size: f32 = 64.0;

    let glyph_position =
        in.glyph_position * text_layout.render_size / cached_render_size
        + in.normal * text_layout.outline_thickness
        + glyph_origin;

    let uv = glyph_position / text_layout.size;

    var out: VertexOut;
    out.pos = vec4(transform_node_to_screen(instance_in, view.layout_to_ndc, uv), 0.0, 1.0);
    out.node_uv = uv;
    if (any(in.normal != vec2(0.0))) {
        out.color = text_layout.outline_color;
        out.is_outline = 1u;
    } else {
        out.color = vec4(1.0);
        out.is_outline = 0u;
    }

    return out;
}

@fragment
fn fragment(in: VertexOut) -> @location(0) vec4<f32> {
    if (in.is_outline != 0u) {
        return in.color;
    } else {
        preprocess_fragment(in.node_uv, view.clip_rect);
        return postprocess_fragment(in.node_uv, vec4(1.0), view);
    }
}
