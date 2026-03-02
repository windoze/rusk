# Rusk LSP (Language Server Protocol) Proposal

## Summary

Add an official Language Server Protocol implementation for Rusk (`.rusk`) so editors can provide:

- parse/typecheck diagnostics in-place
- go-to-definition / hover / completion
- document/workspace symbols
- (later) semantic tokens, find references, rename, code actions

This proposal focuses on **feasibility** given the current repo state, and an incremental plan that
starts with “diagnostics-first” without committing to a full rust-analyzer-style architecture.

---

## Motivation

Today, Rusk development flow is primarily:

1. edit `.rusk`
2. run `cargo run --bin rusk -- path/to/file.rusk`
3. read error output and jump manually

An LSP would move the feedback loop into the editor and unlock:

- faster iteration (compile errors as you type / on save)
- discovery (hover types, signature help)
- navigation (jump to definitions across modules)
- confidence (consistent, machine-readable diagnostics rather than parsing CLI output)

---

## Current state (repo findings)

### What we already have (good building blocks)

- **Lexer / Parser / AST with spans**
  - `crates/rusk-compiler/src/lexer.rs`, `parser.rs`, `ast.rs`
  - Most AST nodes carry `Span` (`crates/rusk-compiler/src/source.rs`).
- **Multi-file loader + source mapping**
  - `ModuleLoader` loads `mod foo;` from `foo.rusk` or `foo/mod.rusk` and assigns each file a
    distinct base offset (`crates/rusk-compiler/src/modules.rs`).
  - `SourceMap` maps a global `Span` back to `(file, line, col)` (`crates/rusk-compiler/src/source_map.rs`).
- **Typechecking metadata**
  - `TypeInfo` tracks expression types: `expr_types: HashMap<Span, Ty>` and additional
    instantiation metadata (`crates/rusk-compiler/src/typeck.rs`).
- **Host-module declarations are reusable**
  - The CLI registers a minimal `std` module (`crates/rusk-host/src/std_io.rs`), which the LSP can
    reuse so that `std::println(...)` typechecks in-editor.

### What we’re missing / what will need changes

- **Public analysis API**
  - `ast`, `parser`, `modules`, `typeck` are private to `rusk-compiler` today.
  - An LSP needs structured access to: parsed program, resolution, types, and per-file diagnostics.
- **Structured diagnostics suitable for editor consumption**
  - `CompileError` currently stores `(message, span)` and (optionally) a *string*
    `rendered_location`. LSP needs a structured `(uri, range)` in addition to the message.
- **Position model mismatch**
  - LSP positions use **0-based UTF-16 code unit** columns.
  - Current `SourceMap` exposes **1-based line/col** where col is counted in Unicode scalar
    values (chars). Conversions are required to avoid off-by-N columns for non-ASCII text.
- **Unsaved edits**
  - `ModuleLoader` reads from disk. LSP must support “overlay” content from the editor buffer
    (open documents that are modified but not saved).
- **Multiple diagnostics**
  - The current front-end returns the first error (`Result<T, CompileError>`). Editors expect a
    list of diagnostics; we should plan for gradually improving error accumulation/recovery.
- **Definition spans for all top-level defs**
  - Some typechecker/env structs include spans (e.g. function signatures), but not all nominal
    types currently carry a definition span. Navigation features will need this.

---

## Goals

### Phase-1 goals (MVP, “useful immediately”)

1. Provide a `rusk-lsp` server binary that speaks LSP over stdio.
2. Publish compile diagnostics mapped to correct files and ranges.
3. Support multi-file module trees (`mod foo;`) and “unsaved buffer” overlays.
4. Provide at least one navigation feature:
   - document symbols (top-level items), and/or
   - keyword completion

### Later goals (phase-2+)

- hover inferred type for expressions
- go-to-definition for:
  - functions
  - nominal types (struct/enum/interface)
  - module paths and `use` imports
- completion based on visible names in scope
- workspace symbols
- semantic tokens for syntax highlighting

---

## Non-goals (initially)

- Full incremental parsing/typechecking with “as you type” recovery equivalent to rust-analyzer.
- Refactor/rename, find references, code actions (these require a more complete reference model).
- Formatting (`textDocument/formatting`) unless/until we have a canonical pretty printer.

---

## Proposed architecture

### Workspace structure

Add a new workspace member crate:

```
crates/
  rusk-lsp/
    Cargo.toml
    src/main.rs
```

Optionally add a second crate if we want a clean separation:

```
crates/
  rusk-analysis/
    Cargo.toml
    src/lib.rs
```

#### Recommendation

Start with **one new binary crate** (`rusk-lsp`) and a **new public analysis module** inside
`rusk-compiler` (e.g. `pub mod analysis`) that exposes only the minimal stable surface needed by
the LSP:

- `analyze_project(...) -> AnalysisSnapshot`
- `diagnostics(snapshot) -> Vec<Diagnostic>`
- `hover(snapshot, position) -> Option<Hover>`
- `goto_definition(snapshot, position) -> Option<Location>`
- etc.

This avoids prematurely committing to a long-term crate split while keeping the LSP from reaching
into internal compiler modules directly.

### LSP implementation approach

Use a Rust LSP framework (e.g. `tower-lsp`) and an async runtime (`tokio`).

High-level server responsibilities:

- Track a workspace root and configuration (entry files, host modules, etc.).
- Maintain a “virtual file system overlay” of open documents.
- On `didOpen` / `didChange` / `didSave`:
  - schedule re-analysis (debounced)
  - publish diagnostics per affected document
- Serve request-based features (`hover`, `definition`, `completion`, …) from the latest snapshot.

### Project model (“what is a program?”)

Rusk supports multi-file programs through `mod foo;`, but unlike Rust today there is no explicit
project manifest. LSP still needs to know which file(s) are considered compilation roots.

Proposed strategy:

1. **Configurable entry set**
   - LSP config: `rusk.entryFiles = ["path/to/main.rusk", ...]`
   - CLI flags: `rusk-lsp --entry path/to/main.rusk` (repeatable)
2. **Heuristic fallback**
   - If not configured, scan the workspace for `.rusk` files that define `fn main`.
   - If none found, treat the active document as the root *for diagnostics only* (not ideal, but
     gives syntax/type feedback for small scripts).

### Virtual file system overlay (unsaved edits)

Introduce an abstraction so module loading can read from the editor buffer:

- Add a `SourceProvider` (name bikeshed) trait in `rusk-compiler`:
  - `read_to_string(path) -> String`
  - `canonicalize(path) -> PathBuf`
  - `exists(path) -> bool`
- Provide a default `FsSourceProvider`.
- In `rusk-lsp`, implement `OverlaySourceProvider` that consults:
  - in-memory open documents first
  - filesystem fallback otherwise

This keeps `ModuleLoader` logic intact while enabling correct analysis of unsaved changes.

### Span + position mapping (UTF-8 ↔ UTF-16)

Keep the compiler’s internal spans as UTF-8 byte offsets (as they are today), but introduce
conversion utilities at the tooling boundary:

- byte offset → LSP `Position` (`line`, `character` in UTF-16 code units)
- LSP `Position` → byte offset

Implementation can be done per file using stored `line_starts` plus a per-line scan to convert
between UTF-8 and UTF-16 indices.

### Diagnostics model

Introduce a structured diagnostic type in `rusk-compiler` that is not tied to LSP, but contains
enough information to produce LSP diagnostics:

```text
Diagnostic {
  message: String,
  severity: Error | Warning,
  span: Span,
  // Optional structured mapping when we have sources:
  source: Option<SourceName>,
  range: Option<SourceRange>,
}
```

Then:

- `rusk-compiler` returns `Vec<Diagnostic>` (eventually; phase-1 may still return one).
- `rusk-lsp` maps `SourceName::Path(path)` to `file://` URIs and `SourceRange` to LSP ranges.

### Semantic queries (hover / definition / completion)

To support semantic features beyond diagnostics, the compiler analysis layer should retain:

- parsed `Program` (AST)
- `ProgramEnv` / module resolver (name resolution results)
- `TypeInfo` (expression type map)
- a “resolution map” from spans to resolved symbols (recommended addition)

Suggested additional compiler metadata:

- `ResolvedSymbol { kind, fqn, def_span }`
- `HashMap<Span, ResolvedSymbol>` for:
  - `Expr::Path`
  - `TypeExpr::PathType`
  - `use` paths

This makes `goto_definition` and `hover` straightforward:

- locate the smallest AST node at the cursor
- look up its span in resolution/type maps
- return `Location`/`Hover` based on stored `def_span` and pretty-printed type/signature

---

## Implementation plan

### Phase 0: Compiler plumbing (no editor yet)

- Add a public analysis entrypoint in `rusk-compiler`:
  - load program from entry file using an injectable source provider
  - return a snapshot containing `SourceMap` + analysis outputs
- Introduce structured diagnostics (even if it’s only a single-item `Vec` initially).
- Add UTF-16 position conversion helpers.

### Phase 1: `rusk-lsp` MVP

- Create `crates/rusk-lsp` binary:
  - stdio transport
  - logging (file-based or stderr, configurable)
- Implement:
  - `initialize`, `initialized`, `shutdown`, `exit`
  - `textDocument/didOpen`, `didChange`, `didSave`, `didClose`
  - publish diagnostics (at least for the active root set)
- Provide at least one non-diagnostic feature:
  - `textDocument/documentSymbol` based on AST, or
  - keyword completion

### Phase 2: Navigation + hover

- Add resolution map and definition spans for nominal types.
- Implement:
  - `textDocument/hover` (type + signature)
  - `textDocument/definition` for top-level defs and imports
  - `textDocument/completion` using visible names (module scope)

### Phase 3: Quality improvements

- Improve error accumulation (multiple diagnostics per file).
- Add cancellation and debounce behavior (avoid “typecheck on every keystroke” CPU spikes).
- Add tests:
  - analysis-layer tests (expected diagnostics, hover, definition on fixtures)
  - LSP protocol tests (request/response snapshots)

### Phase 4 (optional): Incremental analysis

If performance becomes a limiting factor:

- switch to a lossless incremental syntax tree representation (e.g. rowan-style)
- move analysis onto a query system (e.g. salsa-like)

This is explicitly optional and can be deferred until the language and compiler are more stable.

---

## Testing strategy

1. **Fixture-driven compiler analysis tests**
   - Use `fixtures/` with known sources.
   - Assert on structured diagnostics and their mapped locations.
2. **Protocol-level LSP tests**
   - Simulate JSON-RPC requests for `didOpen`, `didChange`, `hover`, etc.
   - Assert published diagnostics and returned locations.

---

## Open questions

- **Project configuration**
  - Do we want a `rusk.toml`/`rusk.json` manifest long-term, or rely on editor configuration?
- **Error recovery**
  - How much “incomplete code” support do we want in phase-1 vs later?
- **Unicode correctness**
  - Do we standardize on UTF-16 columns for editor tooling only, or also offer a compiler-internal
    option for UTF-16 in `SourceMap` to reduce conversion duplication?
- **API stability**
  - Which analysis types are “public API”, and which remain internal and can churn freely?

---

## Alternatives considered

1. **Embed LSP into the `rusk` CLI as a subcommand**
   - Pros: fewer binaries to distribute.
   - Cons: CLI currently has a very small surface; mixing concerns may complicate evolution.
2. **Use tree-sitter for parsing**
   - Pros: mature incremental parsing for editor use cases.
   - Cons: duplicates parser logic and risks divergence from the compiler.
3. **Adopt a rust-analyzer-style architecture from day 1**
   - Pros: best-in-class incremental experience.
   - Cons: large upfront complexity; may be premature for an evolving language.

