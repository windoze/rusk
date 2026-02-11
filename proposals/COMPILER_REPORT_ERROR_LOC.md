## Compiler error locations as `filename: <line:col> - <line:col>`

### Summary

Change compiler diagnostics to report source locations using **file name + line/column ranges**
instead of the current global byte offsets.

Desired format:

```
<filename>: <line:col> - <line:col>: <message>
```

If the source does not come from a file (e.g. `compile_to_mir(source: &str)`), use a clear virtual
name such as `<string>`.

---

## Motivation

Today, most front-end errors render like:

```
<message> at <start>..<end>
```

Where `<start>`/`<end>` are **byte offsets** into an implicit “global source space”. This is
especially confusing when compiling from files with modules (`mod foo;`), because the module loader
assigns **base offsets** per file (so offsets are not even relative to a single file).

This makes errors “barely understandable” for humans: users cannot quickly jump to the right place
in an editor, and the offsets don’t correspond to line numbers.

---

## Current state (repo findings)

### Span model

- `src/source.rs` defines `Span { start, end }` as a half-open byte span `[start, end)`.
- Lexer spans are computed as `Span::new(base_offset + start, base_offset + end)` where
  `base_offset` comes from `Lexer::with_base_offset`.

### Multi-file compilation

- `src/modules.rs:ModuleLoader` reads each file, assigns it a unique `base_offset`, and parses it
  with `Parser::with_base_offset(&source, base_offset)`.
- `base_offset` advances by `src.len() + 1`, so each file’s spans live in a distinct global range.
- The AST (`Program`) does not carry file identity, only spans.

### Error types

Several error structs (`ParseError`, `LoadError`, `TypeError`, `CompileError`, …) implement
`Display` as:

```
{message} at {span.start}..{span.end}
```

The CLI (`src/bin/rusk.rs`) prints compile errors using `{e}` (the `Display` impl).

---

## Goals

1. Replace offset-based locations with readable line/column ranges:
   - `path/to/file.rusk: <3:5> - <3:12>: ...`
2. Support multi-file programs (modules) without losing which file a span belongs to.
3. Support non-file sources with a clear “filename” placeholder:
   - `<string>: <1:1> - <1:5>: ...`
4. Keep spans as byte offsets internally (at least initially) to minimize compiler churn.
5. Avoid `unsafe` (repo is `#![forbid(unsafe_code)]`).

Non-goals (for this proposal’s first iteration):
- Pretty multi-line snippets with caret underlines (can be a follow-up).
- Rich “error codes”, notes, or help messages.

---

## Proposed design

### 1) Introduce a `SourceMap` for mapping spans to files + line/col

Add a new module (name bikeshed): `src/diagnostic.rs` or `src/source_map.rs` with:

```rust
/// A display name for a source (either a filesystem path or a virtual label like "<string>").
enum SourceName { Path(PathBuf), Virtual(String) }

struct SourceFile {
  name: SourceName,
  base_offset: usize,
  /// Full UTF-8 source text (owned or Arc<str>).
  src: Arc<str>,
  /// Byte offsets (0-based, relative to `src`) where each line starts.
  line_starts: Vec<usize>,
}

struct SourceMap {
  files: Vec<SourceFile>, // sorted by base_offset
}

struct LineCol { line: usize, col: usize } // 1-based line/col
struct SourceRange {
  name: SourceName,
  start: LineCol,
  end: LineCol,
}
```

Core API:

- `SourceMap::add_source(name, src, base_offset) -> FileId`
  - computes `line_starts` once up front
- `SourceMap::lookup_span(span: Span) -> Option<SourceRange>`
  - finds the owning file by `base_offset` range
  - converts global byte offsets to `(line, col)`

### 2) Line/column computation

For a given file-relative byte offset `b`:

1. Find the line index with binary search:
   - `line_idx = upper_bound(line_starts, b) - 1`
2. `line = line_idx + 1` (1-based)
3. `col = 1 + count_chars(src[line_start..b])`

Notes:
- Counting chars (Unicode scalar values) is more user-friendly than bytes.
- Tabs are treated as a single column for now; a later improvement can expand tabs.
- Handle `\n` and `\r\n` when computing `line_starts`.

### 3) Format in `filename: <line:col> - <line:col>`

Rendering rules:

- Always print the source name first.
  - Paths printed as filesystem strings.
  - Virtual names printed verbatim, e.g. `<string>`.
- Then print the range:
  - `<start_line:start_col> - <end_line:end_col>`
- Then `: {message}`.

Example:

```
fixtures/031_ambiguous_method_call_compile_error.rusk: <10:5> - <10:10>: ambiguous method `foo` on `S`; candidates: A, B
```

### 4) How to attach `SourceMap` at error reporting time

Because most error types currently only store `{message, span}`, and `Display` has no access to the
original sources, we need a place to “join” the span with the sources.

Recommended approach (minimal churn):

1. Keep existing internal error structs unchanged (`ParseError`, `TypeError`, …).
2. Extend `CompileError` to carry an optional rendered location:

```rust
pub struct CompileError {
  pub message: String,
  pub span: Span,
  pub rendered_location: Option<String>, // e.g. "file: <l:c> - <l:c>"
}
```

3. Modify the top-level entrypoints to **enrich** errors before returning:

- `compile_to_mir(source: &str)`:
  - create `SourceMap` with a single `SourceFile { name: "<string>", base_offset: 0 }`
  - on any error, compute the location string via `source_map.lookup_span(err.span)`
- `compile_file_to_mir(entry_path: &Path)`:
  - modify `ModuleLoader` to also build and expose a `SourceMap` as it loads files
  - on any error, compute the location string with that `SourceMap`

4. Update `impl Display for CompileError` to prefer the enriched format:

- If `rendered_location.is_some()`:
  - `"{location}: {message}"`
- Else fallback to existing `"{message} at {start}..{end}"`

This preserves programmatic access to:
- `err.message` (used by fixture tests today)
- `err.span` (for tooling)

Alternative approach (more principled, bigger churn):
- Create a separate `Diagnostic` type that is constructed at the top-level with access to a
  `SourceMap`, rather than storing `rendered_location` in `CompileError`.

---

## Implementation plan (by subsystem)

### A) Add `SourceMap` infrastructure

- Add new module: `src/source_map.rs` (or `src/diagnostic.rs`).
- Add unit tests for:
  - `\n` line mapping
  - `\r\n` line mapping
  - multi-byte UTF-8 column mapping
  - zero-length spans

### B) Extend `ModuleLoader` to record files

File: `src/modules.rs`

- Add a `source_map: SourceMap` field to `ModuleLoader`.
- When loading each file:
  - register `(canonical_path, source, base_offset)` with `source_map`
- Provide an accessor:
  - `fn source_map(&self) -> &SourceMap`

### C) Enrich errors at compilation entrypoints

File: `src/compiler.rs`

- Update `compile_to_mir` to build a one-file `SourceMap` (`<string>`) and enrich errors.
- Update `compile_file_to_mir` to use `ModuleLoader`’s `SourceMap` to enrich errors.
- Update `CompileError`:
  - add `rendered_location: Option<String>`
  - update `Display` to print:
    - `{rendered_location}: {message}`

### D) CLI output

File: `src/bin/rusk.rs`

- No behavior change needed if `Display for CompileError` changes.
- Optional: prefix with `error:` for better UX consistency:
  - `eprintln!("error: {e}")`

### E) (Optional) Adopt across other errors

If desired, apply the same pattern to:
- `TypeError`, `LoadError`, `ParseError`, `ResolveError`

However, only `CompileError` is currently user-facing via the CLI, so improving it alone satisfies
the request with minimal code churn.

---

## Testing plan

1. Add a new `tests/compiler_error_locations.rs` (or extend existing tests) that:
   - compiles a small `<string>` program with a known error span
   - asserts `format!("{err}")` contains:
     - `<string>: <...> - <...>`
2. Add a multi-file fixture with `mod foo;` and an error in `foo.rusk`, and assert:
   - `format!("{err}")` contains `foo.rusk: <...> - <...>`

Note: existing fixture tests that check `err.message` should remain unchanged.

---

## Future extensions

- Add code snippets and caret ranges (Rust-style diagnostics).
- Add “primary” vs “secondary” spans (notes).
- Allow external tooling to consume structured `SourceRange` instead of a pre-rendered string.
