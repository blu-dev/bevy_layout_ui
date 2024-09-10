use std::path::PathBuf;

use bevy::asset::load_internal_asset;
use bevy::prelude::*;
use bevy::render::render_phase::{AddRenderCommand, RenderPhase};
use bevy::render::render_resource::{
    CompareFunction, PipelineCache, RenderPipelineDescriptor, SpecializedRenderPipeline,
    SpecializedRenderPipelines, StencilFaceState, StencilOperation,
};
use bevy::render::{Render, RenderApp, RenderSet};
use bevy::{
    ecs::{entity::EntityHashMap, system::lifetimeless::SRes},
    render::{
        render_asset::RenderAssets,
        render_phase::{DrawFunctions, RenderCommand, RenderCommandResult},
        render_resource::{
            BindGroup, BindGroupEntry, BindGroupLayout, BindGroupLayoutEntry, BindingType,
            IntoBinding, SamplerBindingType, ShaderStages, TextureSampleType, TextureViewDimension,
        },
        renderer::RenderDevice,
        Extract,
    },
    utils::{intern::Interned, HashMap},
};
use egui::DragValue;
use serde::{Deserialize, Serialize};

use crate::render::{
    BindLayoutUniform, BindNodePipeline, BindVertexBuffer, DefaultNodePipeline, DrawUiPhaseItem,
    SkipNodeRender, UiNodeItem,
};
use crate::{NodeLabel, UiNodeApp, UserUiNode};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ImageNodeLabel;

decl_node_label!(ImageNodeLabel);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum StencilReference {
    Writer(u8),
    Reader(u8),
}

#[derive(Deserialize, Serialize)]
pub struct ImageNodeData {
    path: PathBuf,
    stencil_reference: Option<StencilReference>,
    mask_path: Option<PathBuf>,
}

#[derive(Debug, Clone, Component, Default)]
pub struct ImageNode {
    image: Handle<Image>,
    stencil_reference: Option<StencilReference>,
    mask: Option<Handle<Image>>,
}

impl ImageNode {
    pub fn set_image(&mut self, image: Handle<Image>) {
        self.image = image;
    }

    pub fn set_mask(&mut self, image: impl Into<Option<Handle<Image>>>) {
        self.mask = image.into();
    }
}

impl UserUiNode for ImageNode {
    const NAME: &'static str = "Image";
    type Serde = ImageNodeData;

    fn label() -> Interned<dyn NodeLabel> {
        ImageNodeLabel.intern()
    }

    fn deserialize<E: serde::de::Error>(
        serde: Self::Serde,
        load_context: &mut bevy::asset::LoadContext,
    ) -> Result<Self, E> {
        Ok(Self {
            image: load_context.load(serde.path.clone()),
            stencil_reference: serde.stencil_reference,
            mask: serde.mask_path.map(|path| load_context.load(path)),
        })
    }

    fn serialize<E: serde::ser::Error>(
        &self,
        world: &bevy::prelude::World,
    ) -> Result<Self::Serde, E> {
        let server = world.resource::<AssetServer>();
        let path = server
            .get_path(self.image.id())
            .ok_or_else(|| E::custom("Image handle is not represented by the filesystem"))?;

        let mask_path = self
            .mask
            .as_ref()
            .map(|mask| {
                world
                    .resource::<AssetServer>()
                    .get_path(mask.id())
                    .ok_or_else(|| E::custom("Image handle is not represented by the filesystem"))
            })
            .transpose()?;

        Ok(ImageNodeData {
            path: path.path().to_path_buf(),
            stencil_reference: self.stencil_reference,
            mask_path: mask_path.map(|path| path.path().to_path_buf()),
        })
    }

    fn reconstruct(entity: bevy::prelude::EntityRef) -> Self {
        entity
            .get::<ImageNode>()
            .cloned()
            .expect("ImageNode::reconstruct expected the entity to contain ImageNode")
    }

    fn spawn(&self, entity: &mut bevy::prelude::EntityWorldMut) {
        entity.insert(self.clone());
    }

    fn visit_asset_dependencies(&self, visit_fn: &mut dyn FnMut(bevy::asset::UntypedAssetId)) {
        visit_fn(self.image.id().untyped());
        if let Some(mask) = self.mask.as_ref() {
            visit_fn(mask.id().untyped());
        }
    }
}

#[cfg(feature = "editor-ui")]
impl crate::EditorUiNode for ImageNode {
    fn edit(entity: &mut EntityWorldMut, ui: &mut egui::Ui) {
        let id = egui::Id::new("image-node-editor").with(entity.id());
        let mut current_path = ui.data_mut(|data| {
            data.get_temp_mut_or_insert_with(id, || {
                let handle = entity.get::<ImageNode>().unwrap().image.id();
                entity
                    .world()
                    .resource::<AssetServer>()
                    .get_path(handle)
                    .map(|path| path.path().display().to_string())
                    .unwrap_or_default()
            })
            .clone()
        });

        ui.horizontal(|ui| {
            ui.label("Image Path");
            ui.text_edit_singleline(&mut current_path);
        });

        let stencil_reference = &mut entity
            .get_mut::<ImageNode>()
            .unwrap()
            .into_inner()
            .stencil_reference;
        ui.horizontal(|ui| {
            const NAMES: [&'static str; 3] = ["None", "Reader", "Writer"];
            let mut index = match stencil_reference {
                None => 0,
                Some(StencilReference::Reader(_)) => 1,
                Some(StencilReference::Writer(_)) => 2,
            };

            if egui::ComboBox::new("stencil-reference", "Stencil Reference")
                .show_index(ui, &mut index, NAMES.len(), |idx| NAMES[idx])
                .changed()
            {
                *stencil_reference = match index {
                    0 => None,
                    1 => {
                        if let Some(StencilReference::Reader(idx) | StencilReference::Writer(idx)) =
                            *stencil_reference
                        {
                            Some(StencilReference::Reader(idx))
                        } else {
                            Some(StencilReference::Reader(0))
                        }
                    }
                    2 => {
                        if let Some(StencilReference::Reader(idx) | StencilReference::Writer(idx)) =
                            *stencil_reference
                        {
                            Some(StencilReference::Writer(idx))
                        } else {
                            Some(StencilReference::Writer(0))
                        }
                    }
                    _ => unreachable!(),
                }
            }

            match stencil_reference.as_mut() {
                None => {}
                Some(StencilReference::Reader(value) | StencilReference::Writer(value)) => {
                    ui.add(DragValue::new(value));
                }
            }
        });

        let handle = entity.world().resource::<AssetServer>().load(&current_path);
        ui.data_mut(|data| {
            data.insert_temp(id, current_path);
        });

        entity.get_mut::<ImageNode>().unwrap().image = handle;

        let current_mask = entity
            .get::<ImageNode>()
            .unwrap()
            .mask
            .as_ref()
            .map(|handle| handle.id());

        ui.horizontal(|ui| {
            let mut has_mask = current_mask.is_some();
            ui.checkbox(&mut has_mask, "Mask");

            if has_mask {
                let id = egui::Id::new("image-node-editor-mask").with(entity.id());
                let mut current_path = ui.data_mut(|data| {
                    data.get_temp_mut_or_insert_with(id, || {
                        let handle = entity.get::<ImageNode>().unwrap().image.id();
                        entity
                            .world()
                            .resource::<AssetServer>()
                            .get_path(handle)
                            .map(|path| path.path().display().to_string())
                            .unwrap_or_default()
                    })
                    .clone()
                });
                ui.text_edit_singleline(&mut current_path);
                let handle = entity.world().resource::<AssetServer>().load(&current_path);
                entity.get_mut::<ImageNode>().unwrap().mask = Some(handle);
                ui.data_mut(|data| {
                    data.insert_temp(id, current_path);
                });
            } else {
                entity.get_mut::<ImageNode>().unwrap().mask = None;
            }
        });
    }

    fn cleanup(entity: &mut EntityWorldMut) {
        entity.remove::<Self>();
    }
}

#[derive(Copy, Clone)]
struct ExtractedImageNode {
    image: AssetId<Image>,
    mask: Option<AssetId<Image>>,
    stencil_reference: Option<u8>,
    stencil_mask: ImageNodePipelineKey,
}

#[derive(Resource, Deref, DerefMut, Default)]
struct ExtractedImageNodes(EntityHashMap<ExtractedImageNode>);

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct ImageNodePipelineKey : u8 {
        const USES_MASK_TEXTURE = 1 << 0;
        const USES_STENCIL_READER = 1 << 1;
        const USES_STENCIL_WRITER = 1 << 2;
    }
}

#[derive(Resource)]
pub struct ImageNodePipeline {
    default_pipeline: RenderPipelineDescriptor,
    color_texture_bind_group: BindGroupLayout,
    mask_texture_bind_group: BindGroupLayout,
}

impl ImageNodePipeline {
    const SHADER: Handle<Shader> = Handle::weak_from_u128(0xCE849586828149DA85985CC0390F4CD9);
}

impl SpecializedRenderPipeline for ImageNodePipeline {
    type Key = ImageNodePipelineKey;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        assert!(
            !key.contains(
                ImageNodePipelineKey::USES_STENCIL_READER
                    | ImageNodePipelineKey::USES_STENCIL_WRITER
            ),
            "Read/write from/to stencil is mutually exclusive"
        );
        let mut pipeline = self.default_pipeline.clone();
        if key.intersects(ImageNodePipelineKey::USES_MASK_TEXTURE) {
            pipeline.vertex.shader_defs.push("USE_MASK_IMAGE".into());
            pipeline
                .fragment
                .as_mut()
                .unwrap()
                .shader_defs
                .push("USE_MASK_IMAGE".into());
            pipeline.layout.push(self.mask_texture_bind_group.clone());
        }

        if key.intersects(ImageNodePipelineKey::USES_STENCIL_READER) {
            let state = pipeline.depth_stencil.as_mut().unwrap();
            state.stencil.front = StencilFaceState {
                compare: CompareFunction::Equal,
                ..Default::default()
            };

            state.stencil.back = StencilFaceState {
                compare: CompareFunction::Equal,
                ..Default::default()
            };
        } else if key.intersects(ImageNodePipelineKey::USES_STENCIL_WRITER) {
            let state = pipeline.depth_stencil.as_mut().unwrap();
            state.stencil.front = StencilFaceState {
                compare: CompareFunction::Always,
                pass_op: StencilOperation::Replace,
                ..Default::default()
            };

            state.stencil.back = StencilFaceState {
                compare: CompareFunction::Always,
                pass_op: StencilOperation::Replace,
                ..Default::default()
            };
        }

        pipeline
    }
}

impl FromWorld for ImageNodePipeline {
    fn from_world(world: &mut World) -> Self {
        let default_pipeline = world.resource::<DefaultNodePipeline>();
        let device = world.resource::<RenderDevice>();
        let mut default_pipeline = default_pipeline.render_pipeline_descriptor();
        default_pipeline.vertex.shader = Self::SHADER.clone();
        default_pipeline.fragment.as_mut().unwrap().shader = Self::SHADER.clone();
        let color_texture_bind_group = device.create_bind_group_layout(
            "ImageNodePipeline.color_texture_bind_group",
            &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        );
        default_pipeline
            .layout
            .push(color_texture_bind_group.clone());

        let mask_texture_bind_group = device.create_bind_group_layout(
            "ImageNodePipeline.mask_texture_bind_group",
            &[
                BindGroupLayoutEntry {
                    binding: 0,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                BindGroupLayoutEntry {
                    binding: 1,
                    visibility: ShaderStages::FRAGMENT,
                    ty: BindingType::Sampler(SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        );

        Self {
            default_pipeline,
            color_texture_bind_group,
            mask_texture_bind_group,
        }
    }
}

fn extract_image_nodes(
    nodes: Extract<Query<(Entity, &ImageNode), Without<SkipNodeRender>>>,
    mut extracted: ResMut<ExtractedImageNodes>,
) {
    extracted.clear();
    for (entity, node) in nodes.iter() {
        let stencil_mask;
        let stencil_reference;

        match node.stencil_reference {
            Some(StencilReference::Reader(value)) => {
                stencil_mask = ImageNodePipelineKey::USES_STENCIL_READER;
                stencil_reference = Some(value);
            }
            Some(StencilReference::Writer(value)) => {
                stencil_mask = ImageNodePipelineKey::USES_STENCIL_WRITER;
                stencil_reference = Some(value);
            }
            None => {
                stencil_mask = ImageNodePipelineKey::empty();
                stencil_reference = None;
            }
        }
        extracted.insert(
            entity,
            ExtractedImageNode {
                image: node.image.id(),
                mask: node.mask.as_ref().map(|mask| mask.id()),
                stencil_mask,
                stencil_reference,
            },
        );
    }
}

#[derive(Resource, Default, Deref, DerefMut)]
pub struct PreparedImages(HashMap<AssetId<Image>, BindGroup>);

fn prepare_image_nodes(
    mut commands: Commands,
    mut phases: Query<&mut RenderPhase<UiNodeItem>>,
    mut specialized_pipelines: ResMut<SpecializedRenderPipelines<ImageNodePipeline>>,
    pipeline_cache: Res<PipelineCache>,
    pipeline: Res<ImageNodePipeline>,
    render_device: Res<RenderDevice>,
    extracted_images: Res<ExtractedImageNodes>,
    mut prepared_images: ResMut<PreparedImages>,
    gpu_images: Res<RenderAssets<Image>>,
    draw_functions: Res<DrawFunctions<UiNodeItem>>,
    mut batch: Local<Vec<(Entity, RenderImageData)>>,
) {
    let function = draw_functions
        .read()
        .get_id::<ImageNodeDrawFunction>()
        .unwrap();

    for mut phase in phases.iter_mut() {
        for item in phase.items.iter_mut() {
            let Some(extracted_image) = extracted_images.get(&item.entity).copied() else {
                continue;
            };

            let mut key = extracted_image.stencil_mask;

            key.set(
                ImageNodePipelineKey::USES_MASK_TEXTURE,
                extracted_image.mask.is_some(),
            );
            let id = specialized_pipelines.specialize(&pipeline_cache, &pipeline, key);

            item.pipeline_id = id;
            item.draw_function_id = function;

            if !prepared_images.contains_key(&extracted_image.image) {
                if let Some(image) = gpu_images.get(extracted_image.image) {
                    prepared_images.insert(
                        extracted_image.image,
                        render_device.create_bind_group(
                            "image_bind_group",
                            &pipeline.color_texture_bind_group,
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
                        ),
                    );
                }
            }

            if let Some(mask) = extracted_image.mask {
                if !prepared_images.contains_key(&mask) {
                    if let Some(image) = gpu_images.get(mask) {
                        prepared_images.insert(
                            mask,
                            render_device.create_bind_group(
                                "image_bind_group",
                                &pipeline.color_texture_bind_group,
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
                            ),
                        );
                    }
                }
            }

            batch.push((
                item.entity,
                RenderImageData {
                    image: extracted_image.image,
                    mask: extracted_image.mask,
                    stencil_reference: extracted_image.stencil_reference,
                },
            ));
        }
    }

    let new_batch = Vec::with_capacity(batch.len());
    commands.insert_or_spawn_batch(std::mem::replace(&mut *batch, new_batch));
}

pub type ImageNodeDrawFunction = (
    BindNodePipeline,
    BindVertexBuffer<0>,
    BindLayoutUniform<0>,
    BindImageGroups,
    DrawUiPhaseItem,
);

#[derive(Component)]
pub struct RenderImageData {
    image: AssetId<Image>,
    mask: Option<AssetId<Image>>,
    stencil_reference: Option<u8>,
}

pub struct BindImageGroups;

impl RenderCommand<UiNodeItem> for BindImageGroups {
    type Param = SRes<PreparedImages>;
    type ItemQuery = &'static RenderImageData;
    type ViewQuery = ();

    fn render<'w>(
        _item: &UiNodeItem,
        _view: bevy::ecs::query::ROQueryItem<'w, Self::ViewQuery>,
        entity: Option<bevy::ecs::query::ROQueryItem<'w, Self::ItemQuery>>,
        param: bevy::ecs::system::SystemParamItem<'w, '_, Self::Param>,
        pass: &mut bevy::render::render_phase::TrackedRenderPass<'w>,
    ) -> bevy::render::render_phase::RenderCommandResult {
        let param = param.into_inner();
        let Some(entity) = entity else {
            return RenderCommandResult::Failure;
        };

        let Some(prepared) = param.get(&entity.image) else {
            return RenderCommandResult::Failure;
        };

        pass.set_bind_group(1, prepared, &[]);

        if let Some(mask) = entity.mask {
            let Some(prepared) = param.get(&mask) else {
                return RenderCommandResult::Failure;
            };

            pass.set_bind_group(2, prepared, &[]);
        }

        if let Some(stencil) = entity.stencil_reference {
            pass.set_stencil_reference(stencil as u32);
        }

        RenderCommandResult::Success
    }
}

pub struct ImageNodePlugin;

impl Plugin for ImageNodePlugin {
    fn build(&self, app: &mut App) {
        app.register_user_ui_node::<ImageNode>();
        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_ui_node::<ImageNode>();
        }

        load_internal_asset!(
            app,
            ImageNodePipeline::SHADER,
            "shaders/image_node.wgsl",
            Shader::from_wgsl
        );
    }

    fn finish(&self, app: &mut App) {
        let render_app = app.sub_app_mut(RenderApp);
        render_app
            .init_resource::<ExtractedImageNodes>()
            .init_resource::<ImageNodePipeline>()
            .init_resource::<PreparedImages>()
            .init_resource::<SpecializedRenderPipelines<ImageNodePipeline>>()
            .add_render_command::<UiNodeItem, ImageNodeDrawFunction>()
            .add_systems(ExtractSchedule, extract_image_nodes)
            .add_systems(Render, prepare_image_nodes.in_set(RenderSet::Prepare));
    }
}
