use std::{hash::Hash, ops::Range};

use bevy::{
    ecs::system::Resource,
    math::{URect, UVec2, Vec2},
    render::{
        render_resource::{
            BindGroup, BindGroupEntry, BindGroupLayout, BufferInitDescriptor, BufferUsages,
            BufferVec, CommandEncoderDescriptor, Extent3d, ImageCopyBuffer, ImageCopyTexture,
            ImageDataLayout, IntoBinding, Origin3d, TextureAspect, TextureDescriptor,
            TextureDimension, TextureFormat, TextureUsages,
        },
        renderer::{RenderDevice, RenderQueue},
        texture::GpuImage,
    },
    utils::HashMap,
};
use cosmic_text::{fontdb::ID, Buffer, CacheKey, Color, FontSystem, LayoutGlyph, SwashCache};

use super::TextNodeVertex;

#[derive(Clone)]
struct RectanglePlacerCursor<T: Hash + Eq> {
    clip: UVec2,
    current: UVec2,
    maximum_y: u32,
    allocations: HashMap<T, URect>,
}

impl<T: Eq + Hash> RectanglePlacerCursor<T> {
    /// Advances this cursor, placing a rectangle if there is room
    ///
    /// If there is no room, this method will return [`None`], which should
    /// informs the caller that they should allocate more room
    fn advance(&mut self, size: UVec2, id: T) -> Option<URect> {
        // If the size of the allocation is larger than our clip in any dimension, no operation can help here
        if size.cmpge(self.clip).any() {
            return None;
        }

        // If the width of this allocation will advance us past the edge of the clip,
        // set our new location back
        if self.current.x + size.x >= self.clip.x {
            self.current = UVec2::new(0, self.maximum_y);
        }

        // We know from the first check that our horizontal must now fit, so ensure that our vertical will fit too
        if self.current.y + size.y >= self.clip.y {
            return None;
        }

        let location = URect::from_corners(self.current, self.current + size);
        self.current = UVec2::new(location.max.x, self.current.y);
        self.maximum_y = self.maximum_y.max(location.max.y);

        self.allocations.insert(id, location);

        Some(location)
    }

    /// Rebuilds the cursor with the new size
    ///
    /// `new_size` must be large than the current clip in EVERY dimension, otherwise this will panic
    fn rebuild(&mut self, new_size: UVec2) {
        if self.clip.cmpge(new_size).all() {
            panic!(
                "Failed too set new size for cursor because {} is not larger than {} on every axis",
                new_size, self.clip
            );
        }

        let mut cursor = Self {
            clip: new_size,
            current: UVec2::ZERO,
            maximum_y: 0,
            allocations: HashMap::with_capacity(self.allocations.len()),
        };

        for (id, allocation) in std::mem::take(&mut self.allocations) {
            assert!(cursor.advance(allocation.size(), id).is_some());
        }

        *self = cursor;
    }
}

/// Unique ID that can be used to cache prepared textures on the GPU
///
/// This ID uses the font ID (which consists of shaping information such as bold, italic, etc.)
/// and font size, since rendering the font at different sizes will adjust the shaping accordingly
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct GlyphMapId {
    font_id: ID,
    font_size: u32,
}

impl From<&LayoutGlyph> for GlyphMapId {
    fn from(value: &LayoutGlyph) -> Self {
        Self {
            font_id: value.font_id,
            font_size: value.font_size.trunc().max(0.0) as u32,
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct GlyphId(u16);

struct PreparedGlyphSet {
    // u16 represents the glyph_id
    cursor: RectanglePlacerCursor<GlyphId>,
    image: GpuImage,
    bind_group: BindGroup,
}

impl PreparedGlyphSet {
    fn create_image_and_bind_group(
        device: &RenderDevice,
        layout: &BindGroupLayout,
        size: UVec2,
    ) -> (GpuImage, BindGroup) {
        let texture = device.create_texture(&TextureDescriptor {
            label: Some("PreparedGlyphSet Texture"),
            size: Extent3d {
                width: size.x,
                height: size.y,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: TextureDimension::D2,
            format: TextureFormat::R8Unorm,
            usage: TextureUsages::COPY_DST
                | TextureUsages::COPY_SRC
                | TextureUsages::TEXTURE_BINDING,
            view_formats: &[],
        });

        let texture_view = texture.create_view(&Default::default());

        let sampler = device.create_sampler(&Default::default());

        let image = GpuImage {
            texture,
            texture_view,
            texture_format: TextureFormat::R8Unorm,
            sampler,
            size: size.as_vec2(),
            mip_level_count: 1,
        };

        let bind_group = device.create_bind_group(
            "PreparedGlyphSet Image Bind Group",
            layout,
            &[
                BindGroupEntry {
                    binding: 0,
                    resource: (&image.texture_view).into_binding(),
                },
                BindGroupEntry {
                    binding: 1,
                    resource: (&image.sampler).into_binding(),
                },
            ],
        );

        (image, bind_group)
    }

    fn new(device: &RenderDevice, layout: &BindGroupLayout) -> PreparedGlyphSet {
        const DEFAULT_SIZE: UVec2 = UVec2::splat(256);
        let cursor = RectanglePlacerCursor {
            clip: DEFAULT_SIZE,
            current: UVec2::ZERO,
            maximum_y: 0,
            allocations: HashMap::new(),
        };

        let (image, bind_group) = Self::create_image_and_bind_group(device, layout, DEFAULT_SIZE);

        Self {
            cursor,
            image,
            bind_group,
        }
    }
}

/// Cached GPU textures that are built once and can be re-used for all text that belongs to a certain fonts
///
/// This can exist because we are **not** using advanced shaping techniques (such as RTL, ligatures, etc.). If we want
/// to support those things we should look into a more sophisticated method.
#[derive(Default, Resource)]
pub struct BufferGlyphMaps(HashMap<GlyphMapId, PreparedGlyphSet>);

impl BufferGlyphMaps {
    // Returns the raw bytes of the strided image as well as how long the stride is
    fn into_strided_image(
        cache: &mut SwashCache,
        font_system: &mut FontSystem,
        key: CacheKey,
    ) -> (Vec<u8>, u32) {
        let image = cache.get_image(font_system, key).as_ref().unwrap();
        let left = image.placement.left;
        let top = image.placement.top;
        let stride = (image.placement.width + 0xff) & !0xff;
        let mut raw_data = vec![0u8; (stride * image.placement.height) as usize];
        cache.with_pixels(font_system, key, Color(0xFFFFFFFF), |x, y, col| {
            let x = x - left;
            let y = y + top;
            assert!(x >= 0 && y >= 0, "{x} {y}");
            raw_data[((y as u32) * stride + x as u32) as usize] = col.a();
        });

        (raw_data, stride)
    }

    // TODO: Have a way to prune unused textures from this cache so we don't overflow our memory when we are in an editor :)
    /// This method prepares all of the glyph sets by ensuring that a rasterized font is stored in GPU memory
    pub fn prepare_glyph_sets(
        &mut self,
        font_system: &mut FontSystem,
        cache: &mut SwashCache,
        buffer: &Buffer,
        device: &RenderDevice,
        queue: &RenderQueue,
        layout: &BindGroupLayout,
    ) {
        let mut glyphs = buffer.layout_runs().flat_map(|run| run.glyphs);
        let Some(first) = glyphs.next() else {
            return;
        };

        let map_id = GlyphMapId::from(first);

        let set = self
            .0
            .entry(map_id)
            .or_insert_with(|| PreparedGlyphSet::new(device, layout));

        let mut existing_set = None;

        let mut new_glyphs = Vec::new();

        for glyph in [first].into_iter().chain(glyphs) {
            let glyph_map_id = GlyphMapId::from(first);
            assert_eq!(
                glyph_map_id, map_id,
                "Each glyph must have the same font ID and font size"
            );

            let glyph_id = GlyphId(glyph.glyph_id);
            if !set.cursor.allocations.contains_key(&glyph_id) {
                let physical_glyph = glyph.physical((0., 0.), 1.0);
                let Some(image) = cache
                    .get_image(font_system, physical_glyph.cache_key)
                    .as_ref()
                else {
                    bevy::log::error!("Failed to get glyph!");
                    continue;
                };
                let plc = image.placement;

                while set
                    .cursor
                    .advance(UVec2::new(plc.width, plc.height), glyph_id)
                    .is_none()
                {
                    existing_set = existing_set.or_else(|| Some(set.cursor.allocations.clone()));
                    set.cursor.rebuild(set.cursor.clip * 2);
                }

                new_glyphs.push((glyph_id, physical_glyph.cache_key));
            }
        }

        if new_glyphs.is_empty() {
            assert!(
                existing_set.is_none(),
                "Rebuilt the glyph map with no new glyphs set"
            );
            return;
        }

        let mut buffers = vec![];

        if let Some(previous_allocations) = existing_set {
            let (image, bind_group) =
                PreparedGlyphSet::create_image_and_bind_group(device, layout, set.cursor.clip);

            let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
                label: Some("PreparedGlyphSet Glyph Texture Rebuild"),
            });

            for (id, location) in previous_allocations {
                let new_location = set
                    .cursor
                    .allocations
                    .get(&id)
                    .expect("Rebuilt cursor should have all the same glyphs");
                assert_eq!(new_location.size(), location.size());
                encoder.copy_texture_to_texture(
                    ImageCopyTexture {
                        texture: &set.image.texture,
                        mip_level: 0,
                        origin: Origin3d {
                            x: location.min.x,
                            y: location.min.y,
                            z: 0,
                        },
                        aspect: TextureAspect::All,
                    },
                    ImageCopyTexture {
                        texture: &set.image.texture,
                        mip_level: 0,
                        origin: Origin3d {
                            x: location.min.x,
                            y: location.min.y,
                            z: 0,
                        },
                        aspect: TextureAspect::All,
                    },
                    Extent3d {
                        width: new_location.width(),
                        height: new_location.height(),
                        depth_or_array_layers: 1,
                    },
                );
            }

            set.image = image;
            set.bind_group = bind_group;

            buffers.push(encoder.finish());
        }

        let mut encoder = device.create_command_encoder(&CommandEncoderDescriptor {
            label: Some("PreparedGlyphSet New Glyph Copy Encoder"),
        });
        for (glyph, cache_key) in new_glyphs {
            let (image_data, stride) = Self::into_strided_image(cache, font_system, cache_key);
            let buffer = device.create_buffer_with_data(&BufferInitDescriptor {
                label: Some("PreparedGlyphSet Glyph Copy Buffer"),
                contents: &image_data,
                usage: BufferUsages::COPY_SRC,
            });

            let location = set
                .cursor
                .allocations
                .get(&glyph)
                .expect("If processing new glyph, it should be allocated in the cursor");

            encoder.copy_buffer_to_texture(
                ImageCopyBuffer {
                    buffer: &buffer,
                    layout: ImageDataLayout {
                        offset: 0,
                        bytes_per_row: Some(stride),
                        rows_per_image: None,
                    },
                },
                ImageCopyTexture {
                    texture: &set.image.texture,
                    mip_level: 0,
                    origin: Origin3d {
                        x: location.min.x,
                        y: location.min.y,
                        z: 0,
                    },
                    aspect: Default::default(),
                },
                Extent3d {
                    width: location.width(),
                    height: location.height(),
                    depth_or_array_layers: 1,
                },
            );
        }

        buffers.push(encoder.finish());
        queue.submit(buffers);
    }

    /// This method will update vertex and index buffers for a singular buffer, it will return the range of indices that need
    /// to be drawn, if there is anything that needs to be drawn.
    ///
    /// The first glyph in the buffer is used to determine what glyph map to use, if any other glyphs do not match in this buffer
    /// an error will be logged and that glyph will be skipped
    pub fn update_vertex_buffers(
        &self,
        cache: &mut SwashCache,
        font_system: &mut FontSystem,
        buffer: &Buffer,
        vertex_buffer: &mut BufferVec<TextNodeVertex>,
        index_buffer: &mut BufferVec<u32>,
    ) -> Option<(Range<u32>, BindGroup)> {
        let glyph_map_id = GlyphMapId::from(buffer.layout_runs().next()?.glyphs.first()?);
        let set = self.0.get(&glyph_map_id)?;

        let index_start = index_buffer.len();

        for run in buffer.layout_runs() {
            for glyph in run.glyphs {
                let new_map_id = GlyphMapId::from(glyph);
                if new_map_id != glyph_map_id {
                    bevy::log::error!("Failed to render glyph because it does not match the glyph map as the first glyph");
                    continue;
                }

                let Some(placement) = set.cursor.allocations.get(&GlyphId(glyph.glyph_id)) else {
                    bevy::log::error!(
                        "Failed to render glyph because it was not cached in the glyph map"
                    );
                    continue;
                };

                let tex_tl = placement.min.as_vec2() / set.image.size;
                let tex_br = placement.max.as_vec2() / set.image.size;

                let physical = glyph.physical((0., 0.), 1.);
                let image = cache
                    .get_image(font_system, physical.cache_key)
                    .as_ref()
                    .unwrap();
                let node_tl = Vec2::new(
                    physical.x as f32 + image.placement.left as f32,
                    physical.y as f32 + run.line_y - image.placement.top as f32,
                );
                let node_br = Vec2::new(
                    node_tl.x + image.placement.width as f32,
                    node_tl.y + image.placement.height as f32,
                );

                let node_tl = node_tl / Vec2::from(buffer.size());
                let node_br = node_br / Vec2::from(buffer.size());
                let vertex_start = vertex_buffer.len() as u32;
                vertex_buffer.push(TextNodeVertex {
                    tex_uv: tex_tl.into(),
                    node_uv: node_tl.into(),
                });
                vertex_buffer.push(TextNodeVertex {
                    tex_uv: [tex_br.x, tex_tl.y],
                    node_uv: [node_br.x, node_tl.y],
                });
                vertex_buffer.push(TextNodeVertex {
                    tex_uv: [tex_tl.x, tex_br.y],
                    node_uv: [node_tl.x, node_br.y],
                });
                vertex_buffer.push(TextNodeVertex {
                    tex_uv: tex_br.into(),
                    node_uv: node_br.into(),
                });
                index_buffer.extend([2, 0, 1, 2, 1, 3].map(|value| vertex_start + value));
            }
        }

        (index_buffer.len() > index_start).then(|| {
            (
                index_start as u32..index_buffer.len() as u32,
                set.bind_group.clone(),
            )
        })
    }
}
