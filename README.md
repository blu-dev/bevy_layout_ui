Roadmap

# High Level Goals

- Extendable ui library that can be represented in a trivial format (JSON) while being extended in code
- Simple layouting mechanic where a base resolution is provided and nodes are adjusted to meet that resolution
- Strongly typed userdata mechanism

# UI Node

A UI node has basic properties

- position
- size
- scale
- rotation

- Position should be relative to a parent node, where the anchor position is adjustible
- Size is decoupled from scale, the size of a node can be changed without transforming the inner content
- Scale is self explanatory
- Rotation is self explanator

All nodes should be able to have child nodes

# Animations

- An animation shjould have the concept of keyframes where it will interpolate multiple versions of arbitrary attributes
