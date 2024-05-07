use bevy::asset::UntypedAssetId;
use bevy::prelude::*;
use bevy::{math::Vec2, utils::intern::Interned};
use serde::{Deserialize, Serialize};

use crate::math::Transform;
use crate::render::{UiNodeSettings, VertexColors};
use crate::UiNodeApp;
use crate::{
    animations::{AnimationTarget, AnimationTargetLabel},
    math::NodeSize,
    NodeLabel,
};

pub struct BuiltinAnimationsPlugin;

impl Plugin for BuiltinAnimationsPlugin {
    fn build(&self, app: &mut App) {
        app.register_animation_target::<NodeSizeAnimation>()
            .register_animation_target::<NodeScaleAnimation>()
            .register_animation_target::<NodePositionAnimation>()
            .register_animation_target::<VertexColorAnimation>()
            .register_animation_target::<OpacityAnimation>()
            .register_animation_target::<NodeRotationAnimation>();

        #[cfg(feature = "editor-ui")]
        {
            app.register_editor_animation_target::<NodeSizeAnimation>()
                .register_editor_animation_target::<NodeScaleAnimation>()
                .register_editor_animation_target::<NodePositionAnimation>()
                .register_editor_animation_target::<VertexColorAnimation>()
                .register_editor_animation_target::<OpacityAnimation>()
                .register_editor_animation_target::<NodeRotationAnimation>();
        }
    }
}

#[derive(Deserialize, Serialize, Default, Copy, Clone, Debug)]
pub enum VertexColorId {
    #[default]
    All,
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

#[derive(Debug, Default)]
pub struct VertexColorAnimation(VertexColorId);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct VertexColorLabel;

decl_animation_label!(VertexColorLabel);

pub enum VertexColorBackup {
    All([Color; 4]),
    One(Color),
}

impl AnimationTarget for VertexColorAnimation {
    type Content = Color;
    type Serde = VertexColorId;

    type BackupData = VertexColorBackup;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        use VertexColorId as V;
        let colors = &entity.get::<UiNodeSettings>().unwrap().vertex_colors;
        match self.0 {
            V::All => VertexColorBackup::All([
                colors.top_left(),
                colors.top_right(),
                colors.bottom_left(),
                colors.bottom_right(),
            ]),
            V::TopLeft => VertexColorBackup::One(colors.top_left()),
            V::TopRight => VertexColorBackup::One(colors.top_right()),
            V::BottomLeft => VertexColorBackup::One(colors.bottom_left()),
            V::BottomRight => VertexColorBackup::One(colors.bottom_right()),
        }
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        use VertexColorId as V;
        let colors = &mut entity
            .get_mut::<UiNodeSettings>()
            .unwrap()
            .into_inner()
            .vertex_colors;
        match backup {
            VertexColorBackup::All([top_left, top_right, bottom_left, bottom_right]) => {
                if top_left == top_right && top_right == bottom_left && bottom_left == bottom_right
                {
                    *colors = VertexColors::Single(top_left)
                } else {
                    *colors = VertexColors::Corners {
                        top_left,
                        top_right,
                        bottom_left,
                        bottom_right,
                    };
                }
            }
            VertexColorBackup::One(backup) => match self.0 {
                V::All => panic!("One color backed up for entire color transition"),
                V::TopLeft => colors.set_top_left(backup),
                V::TopRight => colors.set_top_right(backup),
                V::BottomLeft => colors.set_bottom_left(backup),
                V::BottomRight => colors.set_bottom_right(backup),
            },
        }
    }

    const NAME: &'static str = "Builtins.VertexColor";

    fn label() -> Interned<dyn AnimationTargetLabel> {
        VertexColorLabel.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        use VertexColorId as V;
        let mut settings = entity.get_mut::<UiNodeSettings>().unwrap();
        let colors = &mut settings.vertex_colors;
        let setter = match self.0 {
            V::All => {
                *colors = VertexColors::Single(*starting_value);
                return;
            }
            V::TopLeft => VertexColors::set_top_left,
            V::TopRight => VertexColors::set_top_right,
            V::BottomLeft => VertexColors::set_bottom_left,
            V::BottomRight => VertexColors::set_bottom_right,
        };

        setter(colors, *starting_value);
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        fn linear_and_bright(color: Color) -> (Vec4, f32) {
            let [r, g, b, a] = color.as_linear_rgba_f32();
            (Vec4::new(r, g, b, a), (r + g + b + a).powf(0.43))
        }

        use VertexColorId as V;
        let mut settings = entity.get_mut::<UiNodeSettings>().unwrap();
        let colors = &mut settings.vertex_colors;
        let (linear_a, bright_a) = linear_and_bright(*start);
        let (linear_b, bright_b) = linear_and_bright(*end);
        let intensity = (bright_a * (1.0 - interp) + bright_b * interp).powf(0.43f32.recip());
        let mut c = linear_a * (1.0 - interp) + linear_b * interp;
        let sum = c.x + c.y + c.z + c.w;
        if sum != 0.0 {
            c = c * intensity / sum;
        }
        let c = Color::rgba_linear(c.x, c.y, c.z, c.w);
        let setter = match self.0 {
            V::All => {
                *colors = VertexColors::Single(c);
                return;
            }
            V::TopLeft => VertexColors::set_top_left,
            V::TopRight => VertexColors::set_top_right,
            V::BottomLeft => VertexColors::set_bottom_left,
            V::BottomRight => VertexColors::set_bottom_right,
        };

        setter(colors, c);
    }

    fn serialize(&self, _: &World) -> Result<Self::Serde, serde_json::Error> {
        Ok(self.0)
    }

    fn deserialize(data: Self::Serde) -> Self {
        Self(data)
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]

pub struct OpacityAnimation;
decl_animation_label!(OpacityAnimation);

impl AnimationTarget for OpacityAnimation {
    type Content = f32;
    type Serde = ();
    type BackupData = f32;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        entity.get::<UiNodeSettings>().unwrap().opacity
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        entity.get_mut::<UiNodeSettings>().unwrap().opacity = backup;
    }

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Opacity";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<UiNodeSettings>().unwrap().opacity = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<UiNodeSettings>().unwrap().opacity = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct NodeSizeAnimation;

decl_animation_label!(NodeSizeAnimation);

impl AnimationTarget for NodeSizeAnimation {
    type Content = Vec2;
    type Serde = ();
    type BackupData = Vec2;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        entity.get::<NodeSize>().unwrap().0
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        entity.get_mut::<NodeSize>().unwrap().0 = backup;
    }

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Size";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<NodeSize>().unwrap().0 = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<NodeSize>().unwrap().0 = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct NodeRotationAnimation;

decl_animation_label!(NodeRotationAnimation);

impl AnimationTarget for NodeRotationAnimation {
    type Content = f32;
    type Serde = ();
    type BackupData = f32;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        entity.get::<Transform>().unwrap().rotation
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        entity.get_mut::<Transform>().unwrap().rotation = backup;
    }

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Rotation";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<Transform>().unwrap().rotation = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<Transform>().unwrap().rotation = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct NodeScaleAnimation;

decl_animation_label!(NodeScaleAnimation);

impl AnimationTarget for NodeScaleAnimation {
    type Content = Vec2;
    type Serde = ();
    type BackupData = Vec2;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        entity.get::<Transform>().unwrap().scale
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        entity.get_mut::<Transform>().unwrap().scale = backup;
    }

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Scale";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<Transform>().unwrap().scale = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<Transform>().unwrap().scale = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct NodePositionAnimation;

decl_animation_label!(NodePositionAnimation);

impl AnimationTarget for NodePositionAnimation {
    type Content = Vec2;
    type Serde = ();
    type BackupData = Vec2;

    fn backup(&self, entity: EntityRef) -> Self::BackupData {
        entity.get::<Transform>().unwrap().position
    }

    fn restore(&self, backup: Self::BackupData, entity: &mut EntityWorldMut) {
        entity.get_mut::<Transform>().unwrap().position = backup;
    }

    fn label() -> Interned<dyn AnimationTargetLabel> {
        Self.intern()
    }

    fn node_label() -> Option<Interned<dyn NodeLabel>> {
        None
    }

    const NAME: &'static str = "Builtins.Position";

    fn initialize(&self, entity: &mut EntityWorldMut, starting_value: &Self::Content) {
        entity.get_mut::<Transform>().unwrap().position = *starting_value;
    }

    fn interpolate(
        &self,
        entity: &mut EntityWorldMut,
        start: &Self::Content,
        end: &Self::Content,
        interp: f32,
    ) {
        entity.get_mut::<Transform>().unwrap().position = start.lerp(*end, interp);
    }

    fn serialize(&self, _: &bevy::prelude::World) -> Result<Self::Serde, serde_json::Error> {
        Ok(())
    }

    fn deserialize(_: Self::Serde) -> Self {
        Self
    }

    fn visit_asset_dependencies(&self, _: &mut dyn FnMut(UntypedAssetId)) {}

    fn visit_content_asset_dependencies(_: &Self::Content, _: &mut dyn FnMut(UntypedAssetId)) {}
}
#[cfg(feature = "editor-ui")]
const _: () = {
    use std::f32::INFINITY;

    use egui::{Color32, DragValue, Slider};

    use crate::animations::EditorAnimationTarget;

    impl EditorAnimationTarget for NodeSizeAnimation {
        fn default_content() -> Self::Content {
            Vec2::splat(100.0)
        }

        fn edit(&mut self, _: &mut egui::Ui) {}

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            egui::Grid::new("Builtins.Size.edit").show(ui, |ui| {
                ui.add(DragValue::new(&mut content.x).clamp_range(0.0..=INFINITY));
                ui.add(DragValue::new(&mut content.y).clamp_range(0.0..=INFINITY));
            });
        }
    }

    impl EditorAnimationTarget for NodeRotationAnimation {
        fn edit(&mut self, _: &mut egui::Ui) {}

        fn default_content() -> Self::Content {
            0.0
        }

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            ui.add(DragValue::new(content));
            while *content < 0.0 {
                *content += 360.0;
            }

            while *content >= 360.0 {
                *content -= 360.0;
            }
        }
    }

    impl EditorAnimationTarget for NodeScaleAnimation {
        fn edit(&mut self, _: &mut egui::Ui) {}

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            egui::Grid::new("Builtins.Scale.edit").show(ui, |ui| {
                ui.add(DragValue::new(&mut content.x).clamp_range(0.0..=INFINITY));
                ui.add(DragValue::new(&mut content.y).clamp_range(0.0..=INFINITY));
            });
        }

        fn default_content() -> Self::Content {
            Vec2::splat(1.0)
        }
    }

    impl EditorAnimationTarget for NodePositionAnimation {
        fn edit(&mut self, _: &mut egui::Ui) {}

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            egui::Grid::new("Builtins.Position.edit").show(ui, |ui| {
                ui.add(DragValue::new(&mut content.x).clamp_range(0.0..=INFINITY));
                ui.add(DragValue::new(&mut content.y).clamp_range(0.0..=INFINITY));
            });
        }

        fn default_content() -> Self::Content {
            Vec2::splat(1.0)
        }
    }

    impl EditorAnimationTarget for OpacityAnimation {
        fn edit(&mut self, _: &mut egui::Ui) {}

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            ui.add(Slider::new(content, 0.0..=1.0));
        }

        fn default_content() -> Self::Content {
            1.0
        }
    }

    impl EditorAnimationTarget for VertexColorAnimation {
        fn edit(&mut self, ui: &mut egui::Ui) {
            let names = [
                "All",
                "Top Left",
                "Top Right",
                "Bottom Left",
                "Bottom Right",
            ];

            let mut selected = self.0 as usize;

            egui::ComboBox::new("Builtins.VertexColor.edit_id", "Vertex").show_index(
                ui,
                &mut selected,
                names.len(),
                |idx| names[idx],
            );

            match selected {
                0 => self.0 = VertexColorId::All,
                1 => self.0 = VertexColorId::TopLeft,
                2 => self.0 = VertexColorId::TopRight,
                3 => self.0 = VertexColorId::BottomLeft,
                4 => self.0 = VertexColorId::BottomRight,
                _ => unimplemented!(),
            }
        }

        fn edit_content(content: &mut Self::Content, ui: &mut egui::Ui) {
            let [r, g, b, a] = content.as_rgba_u8();
            let mut color = Color32::from_rgba_premultiplied(r, g, b, a);

            ui.color_edit_button_srgba(&mut color);
            *content = Color::rgba_u8(color.r(), color.g(), color.b(), color.a());
        }

        fn default_content() -> Self::Content {
            Color::WHITE
        }
    }
};
