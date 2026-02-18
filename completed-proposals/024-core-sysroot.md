# Proposal: Source-authored `core` library (sysroot) with VM intrinsics + specializations

Date: 2026-02-18

Status: Draft

This proposal introduces a **source-authored** `core` library written in Rusk (`.rusk` files),
intended to replace most compiler/VM “hardcoded library” behavior with ordinary library code.

The guiding idea is a strict split:

- **VM / compiler provide a small, stable-ish set of intrinsics and “lang items”** required by
  desugarings (operators, `for`, formatted strings, panic, fundamental container ops).
- **Rusk sources provide ergonomic APIs and default implementations** (e.g. `Option::map`,
  `Iterator::nth`, `Iterator::last`) that are expected to be **high churn** and should not require
  touching Rust code in the compiler/VM.

This also sets up the same technique for a future `std` library: `std` is mostly Rusk sources that
wrap host modules, while `core` stays freestanding and VM-backed.

---

## Motivation

Today (v0.4 implementation status):

- The compiler injects a built-in `core` module tree (`core::intrinsics`, `core::ops`, `core::iter`,
  `core::fmt`, `core::prelude`) and also injects `Option<T>` as a globally visible nominal type.
- The VM implements `core::intrinsics::*` operations directly (bytecode intrinsics).
- Interfaces used by desugarings (`core::iter::Iterator`, `core::ops::*`, `core::fmt::ToString`)
  exist primarily as compiler-recognized shapes.

This is great for bootstrapping, but it has a cost:

- Any “nice” API addition tends to become a compiler/VM change, even if it is trivial.
- Default interface methods (especially on `Iterator`) cannot be authored in Rusk if the interface
  itself is not defined in source.
- There is no clear path to grow a standard library (`std`) without making the VM surface huge.

We want:

1) A **real `core` library** users can read, modify, and iterate on.
2) A small number of **VM-provided intrinsics** that stay “low churn” and portable.
3) A disciplined **specialization story**: Rusk sources define generic algorithms; the VM can
   optionally provide optimized specializations without changing user-visible APIs.

---

## Goals

- Provide a `core` library implemented primarily in `.rusk` sources, with a clear module layout.
- Keep `core::intrinsics::*` as the primary VM/compiler boundary:
  - minimal set
  - well-specified signatures
  - fast paths for primitives and fundamental containers
- Enable “high-churn convenience APIs” to live in Rusk sources:
  - `Option` inherent methods (`map`, `and_then`, `or_else`, `unwrap_or`, …)
  - `Iterator` default methods (`nth`, `last`, `count`, `find`, …)
  - small glue traits/helpers for common patterns
- Make `core` suitable as the foundation for a future `std` implemented in Rusk sources.
- Preserve the spec invariant: the built-in module `core` is always available as `core::...` and
  `core::prelude` is auto-imported (currently: `panic`).

---

## Non-goals

- Designing the full `std` library now (this proposal only sets the pattern).
- Expanding the host ABI; `core` should remain freestanding and not depend on host modules.
- Locking down `core` APIs for backwards compatibility (the project explicitly allows breaking
  changes at this stage).
- Implementing new language features solely for `core` ergonomics unless they are clearly needed
  (see “Open questions” for `extern fn` / `intrinsic fn` declarations).

---

## Design overview

### 1) Two-tier “core” model

Define two layers that cooperate:

1) **VM/compiler layer**
   - Implements `core::intrinsics::*` and any bytecode instructions needed for them.
   - Recognizes a small number of “lang item” paths used by desugarings:
     - operators → `core::ops::*`
     - `for` → `core::iter::Iterator`
     - `f"..."` → `core::fmt::ToString` + `core::intrinsics::string_concat`
2) **Rusk source layer (`sysroot/core/*.rusk`)**
   - Defines the module tree and most user-facing APIs.
   - Provides default methods and convenience helpers.
   - Delegates to `core::intrinsics` where appropriate.

### 2) “Lang items” are validated, not injected (target direction)

Instead of *injecting* entire interfaces/types from the compiler, the compiler should:

- load `core` sources as a sysroot module tree,
- resolve required “lang item” paths,
- validate that their shapes match what desugarings require (method names, readonly-ness,
  signatures, associated types),
- then use them as lowering targets.

This enables default method bodies in Rusk for `Iterator`, `ToString`, and operator interfaces.

**Incremental path:** we can start by source-authoring convenience methods on already-injected
types (e.g. inherent impls for the injected `Option<T>`), then gradually move interfaces into
sources once the loader/validator exists.

---

## Proposed `core` source layout (sysroot)

Add a `sysroot/` directory in the repository (exact location is configurable; see “Open
questions”), containing:

```
sysroot/
  core/
    mod.rusk
    prelude.rusk
    intrinsics.rusk
    option.rusk
    result.rusk              // optional early; can be added later
    ops.rusk
    fmt.rusk
    iter/
      mod.rusk
      adapters.rusk          // optional; later
      range.rusk             // optional; later
```

Responsibilities:

- `core/mod.rusk`
  - defines the public `core` module tree and re-exports
  - keeps “surface discoverability” high
- `core/prelude.rusk`
  - re-exports the prelude surface auto-imported into all modules
  - starts tiny (`panic`), grows later (e.g. `Option`, `Result`, common traits)
- `core/intrinsics.rusk`
  - *declares* intrinsic functions/types (see below)
  - provides small safe wrappers where useful (e.g. `array::len(xs)` calling the right intrinsic)
- `core/option.rusk`
  - inherent impls for `Option<T>` convenience methods
- `core/ops.rusk`
  - operator interfaces used by lowering
  - primitive impls (so generic algorithms can use operators on primitives via interfaces)
- `core/fmt.rusk`
  - `ToString` interface and primitive impls
  - any small formatting glue needed by `f"..."` desugaring
- `core/iter/mod.rusk`
  - `Iterator` interface (with `type Item; fn next() -> Option<Item>`)
  - default methods implemented in terms of `next()`

Notes:

- Keeping `core` split by concern makes it easier to keep “compiler-recognized” items small and
  stable, while allowing aggressive iteration on everything else.
- The proposed structure intentionally mirrors Rust’s `core`/`std` split, but does not try to copy
  Rust’s full API surface.

---

## Intrinsics: declaration and usage model

`core::intrinsics::*` are VM-provided and should remain so. The question is how to make them
available to Rusk sources in a principled way.

### Option A (short term): continue compiler injection (status quo)

- The compiler injects `core::intrinsics` module bindings and function signatures for typechecking.
- Sysroot `core` sources call `core::intrinsics::*` normally.

Pros:

- No language changes required.
- Keeps bootstrapping simple.

Cons:

- Intrinsic surface is not visible in Rusk sources (unless duplicated as docs/stubs).
- The injected surface is “magic”; harder to audit and evolve.

### Option B (long term): add `extern fn` / `intrinsic fn` declarations in Rusk

Add a declaration-only form such as:

```rusk
mod intrinsics {
  intrinsic fn int_add(a: int, b: int) -> int;
  intrinsic fn panic<T>(msg: string) -> T;
}
```

Rules:

- `intrinsic fn` has **no body** in Rusk sources.
- The compiler verifies that the intrinsic exists in the VM/instruction set.
- Typechecking uses the signature from the source declaration (with the compiler/VM still owning
  the implementation).

Pros:

- Intrinsic API becomes explicit and versioned in sysroot sources.
- Reduces compiler “injected signatures” and makes `core` more self-contained.

Cons:

- Requires a language + compiler change (but it is a small, targeted feature).

This proposal recommends starting with Option A and moving to Option B once sysroot loading is
working.

---

## Language-recognized interfaces: default methods in Rusk

The major win for source-authored `core` is enabling default method bodies on interfaces the
language already recognizes.

### `core::iter::Iterator`

`Iterator` is required for `for` desugaring. With a source definition, we can add default methods:

- `nth(n: int) -> Option<Self::Item>`
- `last() -> Option<Self::Item>`
- `count() -> int`
- `find(pred: fn(Self::Item) -> bool) -> Option<Self::Item>`
- `any` / `all`

Guidelines:

- Keep default methods **object-safe** (avoid mentioning bare `Self` in args/return), so they can
  be called on interface-typed receivers and interface-bounded generics.
- Prefer default methods to delegate to **free helper functions** when specialization is desired
  (see below).

### `core::ops::*` and `core::fmt::ToString`

Similarly, move these interfaces into Rusk sources so:

- we can implement primitive impls in Rusk (in terms of intrinsics),
- generic algorithms can rely on interface calls uniformly (even for primitives),
- future library traits can build on them.

The compiler continues to treat these names as lowering targets; it simply validates their shapes
instead of injecting them.

---

## Specialization model (VM “supplementals”)

The VM should be allowed to provide optimized implementations **without** moving high-level APIs
into the VM.

### 1) Baseline behavior: pure Rusk

By default, all `core` library code is compiled and executed like normal user code.

Examples:

- `Option::map` is written in Rusk and compiles to pattern matching.
- `Iterator::nth` is written in Rusk as a loop calling `next()`.

### 2) Optional specialization: VM replaces specific generic instantiations

For hot generic helpers, structure the library so it is easy to specialize:

- Write performance-relevant code as **free functions** with clear generic parameters.
- Make methods thin wrappers around those helpers.

Then the VM can map “function + exact runtime type args” → “host import / intrinsic” when desired.

This is compatible with the existing “generic specialization” concept in the VM (exact API details
are an implementation concern and out of scope for this document).

### 3) Override-based specialization: implementor overrides defaults

For interface default methods, implementors can always provide specialized overrides:

- `impl Iterator for core::intrinsics::ArrayIter<T>` can override `nth` and `last` with index-based
  fast paths if it wants.

This keeps performance optimizations local and does not require VM involvement.

---

## Tooling / development workflow

Desired developer experience:

- Editing `sysroot/core/*.rusk` immediately affects all programs compiled by `ruskc`.
- The compiler reports clear errors if required lang items are missing or have mismatched shapes.
- A `--sysroot <path>` flag (or equivalent compile option) allows testing alternative core
  libraries without rebuilding the compiler.

Tests:

- Add integration tests that compile small programs that use:
  - `Option` convenience methods (`map`, `unwrap_or`, …)
  - `Iterator` default methods (`nth`, `last`, …)
  - operator interfaces (`Add`, `Eq`, …) for both primitives and user-defined types
- Add “shape validation” tests: incorrect `Iterator` signature should fail with a targeted error.

---

## Rollout plan (incremental)

### Phase 0 — Source-only convenience methods (no loader changes)

- Add `sysroot/core/option.rusk` (or equivalent) and compile it as part of the build/test pipeline.
- Implement inherent methods on the built-in `Option<T>` type.
- No changes to desugaring targets yet.

### Phase 1 — Sysroot loader for `core` modules

- Teach the compiler to load the `core` module tree from `sysroot/core` (with a sensible default).
- Continue to inject `core::intrinsics` signatures for bootstrapping.
- Keep validating that `core::prelude` exists and is auto-imported.

### Phase 2 — Move lang-item interfaces into Rusk sources

- Define `core::iter::Iterator`, `core::ops::*`, `core::fmt::ToString` in sysroot sources.
- Replace compiler injection of these interfaces with validation against required shapes.
- Add default methods for `Iterator`.

### Phase 3 — Intrinsics declarations in Rusk (optional)

- Add `intrinsic fn` / `extern fn` declarations so `core/intrinsics.rusk` declares the intrinsic
  surface explicitly.
- Remove more compiler-injected signature tables.

### Phase 4 — `std` as the same pattern

- Add `sysroot/std/*.rusk` wrapping host modules.
- Keep `core` freestanding; keep host I/O, fs, time, etc. in `std`.

---

## Open questions

1) **Where does sysroot live at runtime?**
   - resolved via `--sysroot`, env var, or compile options

2) **How strict should shape validation be?**
   - fail compilation if `core::iter::Iterator` is missing, or allow “no-std-like” builds?
   - do we validate only required items, or the whole module tree?

   We can validate only the required items for now.

3) **What is the long-term story for `Option<T>`?**
   
   move it under `core::option::Option` and make `Option` a prelude alias in `core::prelude`, which is already auto-imported.

4) **How do we avoid cyclic dependencies inside `core`?**
   - e.g. `Option` used by `Iterator`, `Iterator` used by array helpers, etc.
   - recommended rule: `core::intrinsics` depends on nothing; other modules can depend on it and on
     `Option`, but keep cycles out via a clear layering.
   
   Use the recommended rule.

5) **Do we need an “internal” module namespace?**
   - e.g. `core::internal::*` for helpers not intended to be stable / public.

   We don't have anything in it yet, but we can reserve `core::internal` for that purpose if needed later.

