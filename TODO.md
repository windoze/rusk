# TODO

## Module System

We should clarify use/re-export syntax and semantics.
We should follow Rust's conventions as closely as possible and introduce `pub` keyword for public items, `use` keyword for importing items, and `pub use` for re-exporting items.
We should support "file modules", "in file modules" and "directory modules" similar to Rust.


## Core vs Std Library

In the current spec, some features require "Standard Library Surface". Change it to "Core Library Surface".
The `core` module should contains only the essential features needed for basic functionality, while the `std` module can build upon it with additional features and utilities, which could vary based on the target platform.

Things should be in `std` module are TBD later.

Things should be in the `core` module:
- `core::intrinsics` module, including functions that can directly map to MIR intrinsics, we may need to extend MIR to support these features:
    - Primitive types operations, like basic arithmetic operations for integers, floats, booleans.
    - `panic` function, may directly or indirectly map to a MIR `trap` intrinsic. This function should be re-exported to `core` root.
    - Other functions that can/should be lowered to MIR intrinsics. (TBD)
    - `Iterator` interface and related features required to support `for`. This interface should be re-exported to `core` root.
- Other essential features. (TBD)
- `core::prelude` module, re-exporting essential items from `core` module to be automatically imported in every module. Right now it may contain only `panic` function.
