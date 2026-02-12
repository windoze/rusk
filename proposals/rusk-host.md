# Rusk Host Functions: Prototypes + Implementations

Status: Draft (proposal only; no code changes in this document)

## Problem

Rusk programs can call **host functions** via MIR `call <name>(...)`. Host functions are how a
Rusk program interacts with its embedding environment (CLI, WASM, embedded, etc.).

Today there are two mismatches:

1. **Compiler vs. runtime coupling**
   - The compiler must “know” which host functions exist (and their signatures) so that name
     resolution and typechecking succeed.
   - The interpreter must be given concrete implementations, otherwise a `call` traps.

2. **Hardcoded, platform-specific functions in the compiler**
   - As an example, `std::print` / `std::println` are inherently platform-dependent (stdout in a
     CLI, console logging in WASM, UART on embedded, etc.).
   - Hardcoding these into the compiler crate makes the compiler less portable and makes it easy
     for the compiler’s “known functions” set to drift from what a given runtime actually provides.

We need a clean, explicit, and reusable mechanism to:

- Inject host-function **prototypes** into compilation (so compilation can succeed),
- Inject host-function **implementations** into interpretation (so execution can succeed),
- Keep `core` platform-independent, and keep `std`-like surfaces host-defined.

## Goals

- Provide a two-part host-function registration model:
  1. **Compiler side:** inject a set of host-function *prototypes* (placeholders).
  2. **Interpreter side:** inject a set of concrete host-function implementations.
- Provide APIs to **expose** what host functions are:
  - accepted/assumed by the compiler, and
  - actually installed into the interpreter.
- Require prototypes to be grouped under **host modules** with explicit visibility (module + items).
- Be strict by default: before executing MIR, the interpreter should error if the module declares
  any host functions that are not installed.
- Make `rusk` and the planned `rusks` binaries use the same “host set” API rather than hardcoding
  function lists in multiple places.
- Keep `core` portable / platform-independent:
  - `core::intrinsics::*` can remain part of the “core runtime surface” required by the language.
  - Any additional host modules/functions (e.g. `std::println`) should be defined by the embedding
    environment.

## Non-goals

- Defining a full standard library for Rusk.
- Designing capability-based security or sandboxing policies (though the proposed structure makes
  those possible later).
- Changing the Rusk language semantics (this is an integration/architecture change).

## Definitions

- **Host function**: a function callable from MIR via `call <name>(...)`, implemented outside the
  Rusk program.
- **Prototype**: a compiler-visible declaration of a host function: fully-qualified name + type
  signature (and visibility) with no implementation.
- **Implementation**: an interpreter-installed function body (e.g. a Rust closure) for a host
  function name.
- **Host set**: a bundle that can provide compiler-visible declarations (prototypes) and runtime
  implementations (for execution).

## Proposed Design

### 1) Introduce a shared “host spec” data model

Create a small, stable set of types that represent host modules + host function signatures in a
way that doesn’t expose compiler internals:

```rust
pub enum HostVisibility {
    Public,
    Private,
}

pub struct HostModuleDecl {
    pub visibility: HostVisibility,
    pub functions: &'static [HostFunctionDecl],
}

pub struct HostFunctionDecl {
    pub visibility: HostVisibility,
    pub name: &'static str, // module-local name (e.g. "println")
    pub sig: HostFnSig,
}

pub struct HostFnSig {
    pub params: &'static [HostType],
    pub ret: HostType,
}

pub enum HostType {
    Unit,
    Bool,
    Int,
    Float,
    String,
    Bytes,
    Array(Box<HostType>),
    Tuple(Vec<HostType>),
    // Future: Fn, Cont, Nominal, Readonly views, etc.
}
```

Notes:

- **v1 scope** can keep `HostType` minimal (enough for `std::print(string) -> unit` and similar).
- **No generic host functions for now.** We can add generics later once the signature model
  includes a stable representation for things like nominal types and `TypeRep` arguments.
- The compiler will be responsible for converting `HostType` into its internal type representation.
- The interpreter does not need these types for correctness (it is dynamically typed at runtime),
  but they are useful for introspection and to keep the compiler/runtime surfaces aligned.

### 2) Compiler API: register host modules before compilation

Add an API in `rusk-compiler` to accept host modules (containing function prototypes) as an explicit
input.

Host modules are registered **before compilation** (before name resolution and typechecking). The
source program is compiled *against* this host surface.

Illustrative API:

```rust
// Not real code, just for illustration
program.register_host_module("std", HostModuleDecl {
    visibility: HostVisibility::Public,
    functions: &[
        HostFunctionDecl {
            visibility: HostVisibility::Public,
            name: "print",
            sig: HostFnSig { params: &[HostType::String], ret: HostType::Unit },
        },
        HostFunctionDecl {
            visibility: HostVisibility::Public,
            name: "println",
            sig: HostFnSig { params: &[HostType::String], ret: HostType::Unit },
        },
    ],
});
```

```rust
pub struct CompileOptions<'a> {
    /// Host modules and host function declarations visible to this compilation.
    pub host_modules: &'a [(&'a str, HostModuleDecl)],
    // future: feature flags, warnings-as-errors, etc.
}

pub fn compile_to_mir_with_options(
    source: &str,
    opts: CompileOptions<'_>,
) -> Result<rusk_mir::Module, CompileError>;

pub fn compile_file_to_mir_with_options(
    path: &Path,
    opts: CompileOptions<'_>,
) -> Result<rusk_mir::Module, CompileError>;
```

How host module declarations should affect compilation:

- **Name resolution**
  - Host functions are always registered **inside a module**, not at the crate root.
  - Registering a host module creates a module binding at the crate root with the requested
    visibility.
  - There is **no special-casing of `std`**. Any host module name (e.g. `std`, `wasi`, `env`,
    `device`) is treated the same way.
  - Conflicts are **compile-time errors** because host module registration happens before compiling:
    - If the source program declares a module with the same name as a host module (either inline
      `mod std { ... }` or file module `mod std;`), that is an error.
    - If the source program tries to introduce a conflicting binding inside the host module
      (e.g. declaring `fn std::print(...)`, `mod std::print { ... }`, `use ... as print`,
      `pub use ... as print`, etc.), that is an error.
    - If the source program attempts to “change” the host module visibility by redeclaring it,
      that is an error (covered by the first bullet).
  - (Implementation note) the resolver should treat host modules/values as “locked” definitions:
    they participate in lookup like normal items, but cannot be redefined by user code.

- **Typechecking**
  - Host function declarations must be inserted into the program environment function table so
    calls can be typechecked.
  - For v1, host function declarations are limited to monomorphic signatures (no generics).

Backwards compatibility:

- Keep the existing `compile_to_mir` / `compile_file_to_mir` as convenience wrappers that pass
  `CompileOptions { host_modules: &[] }` (i.e. “core-only”, plus whatever built-ins the compiler
  injects by default such as `core`).
- `ruskc` can choose whether it compiles in “core-only” mode or includes a specific host set.

### 3) Interpreter API: inject implementations at runtime

Define a small interface in (or alongside) `rusk-interpreter` that can install host functions:

```rust
pub trait HostFnInstaller {
    fn install(&self, interp: &mut Interpreter);
    fn exported_fns(&self) -> &'static [&'static str];
}
```

Interpreter should also expose what is installed for debugging/introspection:

```rust
impl Interpreter {
    pub fn host_function_names(&self) -> impl Iterator<Item = &str> { ... }
}
```

Runtime behavior (strict by default):

- The MIR module should carry a list of **declared host functions** (see next section).
- Before executing a function, the interpreter checks that every declared host function has a
  concrete implementation installed.
- If any are missing, the interpreter returns an error **before running any MIR**.

This proposal recommends a “preflight check” helper (and in the strict mode, `run_function`
should call it internally):

```rust
pub fn validate_host_functions(module: &Module, interp: &Interpreter) -> Result<(), MissingHosts>;
```

Where `MissingHosts` reports the missing function names (and optionally their signatures).

### 4) Host sets: reusable bundles for compiler + interpreter

Introduce a host-set concept that can provide both compiler-visible declarations and runtime
installer(s).

Two likely layouts:

**Option A: One crate that defines both spec and installers**

- `crates/rusk-host/`
  - `core`: declarations + interpreter installers for `core::intrinsics::*`
  - `std_io`: declarations + interpreter installers for `std::print` / `std::println` (a *convention*,
    not a special module)

Pros: one-stop shop, encourages prototype/implementation alignment.
Cons: may introduce unwanted dependencies unless carefully organized.

**Option B: Split “spec” from “installers”**

- `crates/rusk-host-spec/` (pure data types, no std)
- `crates/rusk-host-core/` (depends on spec + interpreter; provides core installers)
- `crates/rusk-host-std-io/` (depends on spec + interpreter; provides print/println installers)

Pros: cleaner layering; very portable.
Cons: more crates.

Either way, a host set should be able to expose:

- `modules() -> &'static [(&'static str, HostModuleDecl)]` (compiler-facing declarations)
- `installer() -> impl HostFnInstaller` (or `install(interp: &mut Interpreter)`)

### 5) Declared host functions in MIR

To support strict “fail fast before running” behavior, MIR should explicitly declare which host
functions it expects.

Proposed MIR-level shape (exact representation can vary):

```rust
pub struct Module {
    // existing fields...

    /// Declared host function imports (name + signature).
    pub host_imports: BTreeMap<String, HostFnSig>,
}
```

The compiler is responsible for populating `host_imports` from injected host module declarations
that are actually referenced by the program (or, more simply in v1, from all injected host
modules).

In v1, `host_imports` includes the **full monomorphic signature** (`HostFnSig`) for each declared
import. This makes it possible to produce better “missing host function” errors (and later, to
validate host ABI compatibility).

### 6) Module prefix policy

Host function names should be structured as `<module path>::<name>` (at least one `::`).

- This avoids polluting the crate root namespace with host-only functions.
- It keeps “standard library” style APIs as a *convention* (`std::*`) rather than something baked
  into the language.

## Example Usage (Intended)

### CLI runtime (`rusk` binary)

- Compile with `core` (built-in) + `std_io` host module declarations.
- Interpret with `core` host fns + `std_io` host fns.

Pseudo-code:

```rust
let module = compile_file_to_mir_with_options(path, CompileOptions {
    host_modules: rusk_host::std_io::modules(),
});

let mut interp = Interpreter::new(module);
rusk_host::core::install(&mut interp);
rusk_host::std_io::install_stdout(&mut interp);
interp.run_function("main", vec![])?;
```

### Embedded runtime (`rusks` or similar)

The embedded host can:

- compile without `std_io` declarations (and reject programs using `std::*`),
  OR compile with declarations but not install implementations (and fail fast before execution),
  OR compile + install an embedded-specific implementation (UART, logging buffer, etc.).

## Migration Plan (High-level)

1. Add the host spec types (`HostModuleDecl`, `HostFunctionDecl`, `HostFnSig`, `HostType`, etc.) in
   a shared location.
2. Change `rusk-compiler` compilation entrypoints to accept `CompileOptions { host_modules }`.
3. Remove any platform-specific declarations from the compiler (e.g. `std::print` / `std::println`).
4. Provide at least one reference host set for CLI stdout printing.
5. Update `rusk` / `ruskc` (and future `rusks`) binaries to:
   - register host modules for compilation as needed, and
   - install implementations into the interpreter as needed.
6. Keep `core::intrinsics::*` as the only “always-present” portable surface.

## Alternatives Considered

1. **Keep hardcoding `std::*` functions in the compiler**
   - Rejected: prevents portable embeddings and creates drift between compile-time assumptions and
     runtime reality.

2. **Load host declarations from a file (e.g. JSON, TOML)**
   - Viable for tooling, but does not replace the need for a programmatic API.
   - Can be added later as a convenience layer on top of the proposed data model.

3. **Reserve `std` as a keyword/module name**
   - Rejected: `std` should remain a convention, not a special language/module concept. Any host
     module prefix should behave the same way.

## Open Questions

- Should host modules behave like normal modules for privacy and `pub` checking, or should they be
  treated as “extern” definitions?
- Should host module registration support nested module paths (e.g. `wasi::io`), and if so, how do
  we make parent-module visibility explicit and conflict-safe?
