use bevy::app::{PluginGroup, PluginGroupBuilder};

pub mod image;
pub mod null;
pub mod text;

pub struct DefaultNodePluginGroup;

impl PluginGroup for DefaultNodePluginGroup {
    fn build(self) -> bevy::app::PluginGroupBuilder {
        PluginGroupBuilder::start::<Self>()
            .add(null::NullNodePlugin)
            .add(image::ImageNodePlugin)
            .add(text::TextNodePlugin)
    }
}
