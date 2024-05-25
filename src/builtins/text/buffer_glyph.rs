use std::ffi::OsStr;
use std::ops::Range;

use bevy::prelude::*;
use bevy::render::render_resource::{BufferSlice, BufferUsages, BufferVec};
use bevy::render::renderer::{RenderDevice, RenderQueue};
use bevy::render::RenderApp;
use bevy::utils::HashMap;
use bytemuck::{Pod, Zeroable};
use cosmic_text::fontdb::ID;
use cosmic_text::{CacheKey, CacheKeyFlags, Command, FontSystem, SubpixelBin, SwashCache};
use lyon::math::point;
use lyon::path::{FillRule, Path};
use lyon::tessellation::{
    FillGeometryBuilder, FillOptions, FillTessellator, FillVertex, GeometryBuilder,
    GeometryBuilderError, StrokeGeometryBuilder, StrokeOptions, StrokeTessellator, StrokeVertex,
    VertexId,
};

#[derive(Resource)]
pub struct CosmicResources {
    pub font_system: FontSystem,
    pub swash: SwashCache,
}

impl FromWorld for CosmicResources {
    fn from_world(_: &mut World) -> Self {
        let mut font_system = FontSystem::new();

        match std::fs::read_dir("./assets/fonts") {
            Ok(fonts) => {
                for entry in fonts {
                    let entry = match entry {
                        Ok(entry) => entry,
                        Err(e) => {
                            bevy::log::error!("Failed to read directory entry: {e}");
                            continue;
                        }
                    };

                    let path = entry.path();

                    let ft = match entry.file_type() {
                        Ok(ft) => ft,
                        Err(e) => {
                            bevy::log::error!("Failed to get file type of {}: {e}", path.display());
                            continue;
                        }
                    };

                    if !ft.is_file() {
                        bevy::log::debug!(
                            "Skipping file {} while loading fonts because it is not a file",
                            path.display()
                        );
                        continue;
                    }

                    if path.extension() != Some(OsStr::new("ttf")) {
                        bevy::log::warn!(
                            "Skipping file {} because it is not a TTF file",
                            path.display()
                        );
                        continue;
                    }

                    if let Err(e) = font_system.db_mut().load_font_file(&path) {
                        bevy::log::error!("Failed to load font file {}: {e}", path.display());
                    }
                }
            }
            Err(e) => {
                bevy::log::warn!(
                    "Skipping loading extra fonts because we failed to open the fonts folder: {e}"
                );
            }
        }

        Self {
            font_system,
            swash: SwashCache::new(),
        }
    }
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Pod, Zeroable)]
pub struct CachedGlyphVertex {
    /// The location of a vertex on the glyph path, local to the glyph
    glyph_position: [f32; 2],

    /// The normal vector of the glyph path, if this is a fill vertex
    /// then this should be [0.0; 2],
    normal: [f32; 2],
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PrunedCacheKey {
    font_id: ID,
    glyph_id: u16,
    flags: CacheKeyFlags,
}

impl PrunedCacheKey {
    fn cache_key(&self) -> CacheKey {
        CacheKey {
            font_id: self.font_id,
            glyph_id: self.glyph_id,
            font_size_bits: 64.0f32.to_bits(),
            flags: self.flags,
            x_bin: SubpixelBin::Zero,
            y_bin: SubpixelBin::Zero,
        }
    }

    pub const fn new(key: CacheKey) -> Self {
        Self {
            font_id: key.font_id,
            glyph_id: key.glyph_id,
            flags: key.flags,
        }
    }
}

impl From<CacheKey> for PrunedCacheKey {
    fn from(value: CacheKey) -> Self {
        Self::new(value)
    }
}

struct InPlaceBufferBuilders<'a> {
    vertex_buffer: &'a mut BufferVec<CachedGlyphVertex>,
    index_buffer: &'a mut BufferVec<u32>,
    vertex_start: usize,
    index_start: usize,
}

impl GeometryBuilder for InPlaceBufferBuilders<'_> {
    fn begin_geometry(&mut self) {
        self.vertex_start = self.vertex_buffer.len();
        self.index_start = self.index_buffer.len();
    }
    fn add_triangle(&mut self, a: VertexId, b: VertexId, c: VertexId) {
        debug_assert!(a != b);
        debug_assert!(a != c);
        debug_assert!(b != c);
        debug_assert!(a != VertexId::INVALID);
        debug_assert!(b != VertexId::INVALID);
        debug_assert!(c != VertexId::INVALID);

        self.index_buffer
            .extend([a, b, c].map(|vertex| u32::from(vertex)));
    }

    fn abort_geometry(&mut self) {
        self.vertex_buffer.truncate(self.vertex_start);
        self.index_buffer.truncate(self.index_start);
    }
}

impl FillGeometryBuilder for InPlaceBufferBuilders<'_> {
    fn add_fill_vertex(&mut self, vertex: FillVertex) -> Result<VertexId, GeometryBuilderError> {
        let length = self.vertex_buffer.len();
        if length >= u32::MAX as usize {
            return Err(GeometryBuilderError::TooManyVertices);
        }
        self.vertex_buffer.push(CachedGlyphVertex {
            glyph_position: vertex.position().to_array(),
            normal: [0.0; 2],
        });

        Ok(VertexId(length as u32))
    }
}

impl StrokeGeometryBuilder for InPlaceBufferBuilders<'_> {
    fn add_stroke_vertex(
        &mut self,
        vertex: StrokeVertex,
    ) -> Result<VertexId, GeometryBuilderError> {
        let length = self.vertex_buffer.len();
        if length >= u32::MAX as usize {
            return Err(GeometryBuilderError::TooManyVertices);
        }
        self.vertex_buffer.push(CachedGlyphVertex {
            glyph_position: vertex.position_on_path().to_array(),
            normal: vertex.normal().to_array(),
        });

        Ok(VertexId(length as u32))
    }
}

#[derive(Resource)]
pub struct FontGlyphCache {
    cached: HashMap<PrunedCacheKey, IndexRange>,
    vertex_buffer: BufferVec<CachedGlyphVertex>,
    index_buffer: BufferVec<u32>,
    changed: bool,
}

#[derive(Debug, Clone)]
pub struct IndexRange {
    pub fill: Range<u32>,
    pub stroke: Range<u32>,
}

impl FontGlyphCache {
    pub fn vertex_buffer(&self) -> BufferSlice {
        self.vertex_buffer.buffer().unwrap().slice(..)
    }

    pub fn index_buffer(&self) -> BufferSlice {
        self.index_buffer.buffer().unwrap().slice(..)
    }

    pub fn flush(&mut self, render_device: &RenderDevice, render_queue: &RenderQueue) {
        if self.changed {
            self.vertex_buffer.write_buffer(render_device, render_queue);
            self.index_buffer.write_buffer(render_device, render_queue);
        }

        self.changed = false;
    }

    pub fn get_index_range(
        &mut self,
        key: PrunedCacheKey,
        resources: &mut CosmicResources,
    ) -> Option<IndexRange> {
        if let Some(range) = self.cached.get(&key) {
            return Some(range.clone());
        }

        let Some(commands) = resources
            .swash
            .get_outline_commands(&mut resources.font_system, key.cache_key())
        else {
            bevy::log::warn!("Failed to find glyph outline commands");
            return None;
        };

        let mut builder = Path::builder().with_svg();
        let mut is_open = false;

        for command in commands {
            match command {
                Command::MoveTo(p) => {
                    if is_open {
                        builder.close();
                    }
                    is_open = true;
                    builder.move_to(point(p.x, -p.y));
                }
                Command::Close => {
                    if is_open {
                        builder.close();
                    }
                    is_open = false;
                }
                Command::LineTo(p) => {
                    is_open = true;
                    builder.line_to(point(p.x, -p.y));
                }
                Command::QuadTo(ctrl, p) => {
                    is_open = true;
                    builder.quadratic_bezier_to(point(ctrl.x, -ctrl.y), point(p.x, -p.y));
                }
                Command::CurveTo(ctrl_a, ctrl_b, p) => {
                    is_open = true;
                    builder.cubic_bezier_to(
                        point(ctrl_a.x, -ctrl_a.y),
                        point(ctrl_b.x, -ctrl_b.y),
                        point(p.x, -p.y),
                    );
                }
            }
        }

        let path = builder.build();

        let start = self.index_buffer.len() as u32;
        let mut fill_tesselator = FillTessellator::new();
        let mut stroke_tesselator = StrokeTessellator::new();
        let mut builder = InPlaceBufferBuilders {
            vertex_start: self.vertex_buffer.len(),
            index_start: self.index_buffer.len(),
            vertex_buffer: &mut self.vertex_buffer,
            index_buffer: &mut self.index_buffer,
        };
        fill_tesselator
            .tessellate_path(
                &path,
                &FillOptions::tolerance(0.02).with_fill_rule(FillRule::NonZero),
                &mut builder,
            )
            .unwrap();

        let fill = start..builder.index_buffer.len() as u32;

        stroke_tesselator
            .tessellate_path(&path, &StrokeOptions::tolerance(0.02), &mut builder)
            .unwrap();

        let stroke = fill.end..builder.index_buffer.len() as u32;

        self.changed = true;

        let indexes = IndexRange { fill, stroke };

        self.cached.insert(key, indexes.clone());

        Some(indexes)
    }
}

impl Default for FontGlyphCache {
    fn default() -> Self {
        Self {
            cached: HashMap::new(),
            vertex_buffer: BufferVec::new(BufferUsages::VERTEX),
            index_buffer: BufferVec::new(BufferUsages::INDEX),
            changed: false,
        }
    }
}

pub struct GlyphBuffersPlugin;

impl Plugin for GlyphBuffersPlugin {
    fn build(&self, app: &mut App) {
        app.sub_app_mut(RenderApp)
            .init_resource::<FontGlyphCache>()
            .init_resource::<CosmicResources>();
    }
}
