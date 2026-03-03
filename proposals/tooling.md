# Proposal: Tooling (Diagnostics, Formatter, Linter)

Date: 2026-03-03

Status: Draft

This proposal introduces three developer-experience improvements for Rusk:

1) **Friendly compiler diagnostics**: errors should show a small source context with clearer,
   more actionable messages.
2) **A code formatter**: exposed via the umbrella CLI as `rusk fmt`, with Rust-like conventions.
3) **A code linter**: exposed via `rusk lint`, starting with a small set of high-signal checks and
   growing over time.

The intent is to make Rusk feel “production-grade” for everyday use (CLI + editor tooling), while
keeping the implementation incremental and avoiding a big up-front commitment to “rustfmt/clippy
feature parity”.

---

## Motivation

Today, the Rusk toolchain is usable but still feels “prototype-ish” in three areas:

- **Errors are hard to read**: users often see a message but not the immediate context that caused
  it. Even when locations exist, the output typically lacks *the one or two lines* that would make
  the mistake obvious.
- **No canonical formatting**: code style varies across fixtures and user code. This increases diff
  noise, complicates reviews, and makes editor integration inconsistent.
- **No lint pass**: many common footguns (unused variables, redundant constructs, suspicious logic)
  are not caught early. Compiler errors catch *invalid* programs; a linter can catch *probably
  wrong* programs.

Rusk already has important building blocks (a `SourceMap`, compiler error spans, and a diagnostic
type used for analysis tooling). This proposal focuses on packaging and extending these into a
coherent “tooling layer”.

---

## Goals

- **Make errors actionable**:
  - show a short snippet of the source around the span (context),
  - point at the relevant range (caret/underline),
  - provide “what went wrong” + “what to do next” wording whenever feasible.
- **Provide stable, ergonomic CLI tooling**:
  - `rusk fmt` for formatting with `--check` for CI,
  - `rusk lint` for static linting with configurable severity.
- **Share infrastructure**:
  - diagnostics rendering should be used by the compiler, linter, and (later) LSP.
- **Incremental delivery**:
  - start small (few high-signal diagnostics/lints, basic formatting) and expand.

---

## Non-goals

- Full parity with Rust’s `rustfmt` and `clippy` in the first iteration.
- A fully-configurable formatting style (the initial formatter should be opinionated).
- A full “quick fix” engine (though we should design diagnostics to support suggested fixes later).
- Replacing the existing compiler architecture; this is a tooling layer on top.

---

## Overview: `rusk` as an umbrella tool

Add subcommands under the existing `rusk` binary:

- `rusk <file.rusk|file.rbc> [args...]` — unchanged default behavior (compile/run).
- `rusk fmt [--check] [--write] <paths...>` — format Rusk code.
- `rusk lint [--deny-warnings] <paths...>` — run lints and report diagnostics.

This keeps the entry UX simple:

- “One tool” for day-to-day work.
- Friendly defaults for interactive use.
- Script/CI-friendly modes (`--check`, `--json`, exit codes).

---

## 1) Friendly compiler error messages

### 1.1 Desired output format (human-readable)

Diagnostics should include:

1) Severity + optional code: `error[E0123]` / `warning[W0001]`
2) Primary message (1 line).
3) Location: `path/to/file.rusk:<line:col>`
4) A small source snippet with underline/caret.
5) Optional `help:` and `note:` sections.

Example (illustrative):

```text
error[E0007]: expected `;` after expression
  --> fixtures/example/main.rusk:5:18
   |
 5 |     let x = foo()
   |                  ^ add `;` to end the statement
   |
   = help: write `let x = foo();`
```

Rules of thumb:

- Default context: **1 line before, the line of the span, 1 line after**.
- If the span is long or multiline, expand context but keep output compact.
- Prefer pointing at the *smallest* relevant span (e.g., the unexpected token).

### 1.2 Diagnostics model

Rusk already has:

- `Span` (global byte span)
- `SourceMap` (mapping spans to files and line/col, plus source text)
- A `Diagnostic` struct (used in analysis tooling)

Proposal:

- Promote a single “compiler/tooling diagnostic model” to be used consistently:
  - `Diagnostic { severity, code, message, labels, notes, help, ... }`
  - `Label { span, message, style: Primary|Secondary }`
- Keep `Span` as the internal currency for pointing at code.
- Make the renderer responsible for mapping span → file/line/col/snippet.

This keeps the compiler logic focused on *detecting* issues, while the tooling layer focuses on
*presenting* them.

### 1.3 Message quality guidelines

We should gradually make the messages more meaningful:

- Use specific language:
  - “unknown name `foo`” (not “name error”)
  - “type mismatch: expected `int`, found `string`”
- Include context that prevents confusion:
  - show both the expected and actual types,
  - list candidate methods/fields when resolution is ambiguous,
  - point to both the use site and the definition site where helpful.
- Provide a short “next action” in `help:` when we can do so safely.

Not all errors will be improved immediately; the goal is to make the rendering infrastructure good
enough that improving individual errors becomes an incremental “one error at a time” effort.

### 1.4 CLI integration

The CLI currently prints `compile error: {e}` (using `Display`).

Proposal:

- Keep `Display` for “single-line” fallback and logs.
- Add a diagnostic emitter for rich, multi-line rendering:
  - default for interactive terminals (`--color=auto`)
  - `--json` output for tooling/IDE/CI integration
- Ensure consistent exit codes:
  - compile errors → exit code `1`
  - CLI usage errors → exit code `2` (already used)

### 1.5 JSON output (optional, but recommended)

Provide a stable JSON schema (versioned) so editor tooling and CI can consume diagnostics without
re-parsing human text.

This should include:

- file path / virtual source name
- line/col range (1-based)
- severity + code
- message + optional help/note
- optional related locations (secondary labels)

---

## 2) Code formatter: `rusk fmt`

### 2.1 CLI contract

Proposed interface:

```text
rusk fmt [--check] [--write] [--stdin] [--stdout] <paths...>
```

Behavior:

- Default (`rusk fmt <paths...>`): format and write changes back to files (developer-friendly).
- `--check`: do not write; exit `0` if already formatted, else exit `1`.
- `--stdin`/`--stdout`: enable editor integration and “format on save”.

Notes:

- `--check` is the primary CI mode.
- `--stdin` implies a virtual filename for better diagnostics in error cases (e.g. `<stdin>`).

### 2.2 Formatting conventions (Rust-like)

The formatter should be opinionated and stable. Initial rules:

- 4-space indentation.
- Consistent brace placement (`fn f() { ... }`).
- One statement per line; trailing whitespace removed.
- Canonical spacing around operators (`a + b`, not `a+b`), commas, and colons.
- Normalize empty lines and ensure a final newline.

We should explicitly document that:

- The formatter is the canonical style.
- Manual alignment is not preserved (like `rustfmt`).

### 2.3 Implementation approach

Two viable strategies:

1) **AST-based pretty printer (preferred)**:
   - parse with the existing compiler parser,
   - print using a pretty-printing engine (fits future extensions),
   - guarantees syntactic correctness.

2) **Tree-sitter based formatting (fallback / incremental)**:
   - use the existing tree-sitter grammar as a parser,
   - format based on syntax tree edits.

Recommendation:

- Start with AST-based pretty printing to avoid “two parsers that disagree”.
- If the pretty printer cannot yet handle all constructs, allow a staged rollout:
  - formatter supports a subset and clearly errors on unsupported nodes,
  - gradually expand coverage until it can format all Rusk syntax used in fixtures/tests.

### 2.4 Testing strategy

Use golden tests:

- Input `.rusk` file → formatted output matches a checked-in `.formatted.rusk` (or inline expected).
- Roundtrip stability: formatting the already-formatted output should be a no-op.

Also add:

- a small corpus of “ugly” code samples under `fixtures/` or `tests/` dedicated to formatter tests,
  so behavior changes are explicit.

---

## 3) Code linter: `rusk lint`

### 3.1 CLI contract

Proposed interface:

```text
rusk lint [--deny-warnings] [--json] <paths...>
```

Behavior:

- Lints run after parsing (and usually after name resolution and type checking when needed).
- Default severity:
  - lints are warnings (do not fail builds by default),
  - `--deny-warnings` upgrades warnings to errors (CI mode).

### 3.2 Lint framework

Design principles:

- Each lint has a stable identifier: `unused_variable`, `unreachable_code`, ...
- Each lint has a default level: `allow` / `warn` / `deny`.
- Emit lints as diagnostics (same renderer as compiler errors).
- Start with lints that are:
  - high-signal (few false positives),
  - easy to explain,
  - easy to implement using existing AST + type info.

Optional future extension (not required initially):

- in-source configuration (attribute-like) or project configuration file (e.g. `rusk.toml`)
  for per-lint levels.

### 3.3 Initial lint set (minimal, high-value)

Suggested first batch:

- `unused_variable`: bound but never read.
- `unused_import` / `unused_module`: declared but never referenced.
- `unreachable_code`: code after `return` / `break` / `continue` in the same block.
- `redundant_else`: `if cond { return ... } else { ... }` where `else` can be unindented.
- `needless_bool`: `if cond { true } else { false }` and the inverted form.
- `redundant_match_arm`: duplicate arm bodies or patterns that are shadowed by earlier arms.
- `suspicious_comparison`: comparing a value to itself (`x == x`, `x < x`) when not obviously
  intentional.

Notes:

- Some of these require type information (e.g. to avoid flagging `NaN`-related comparisons if/when
  floats exist). The initial implementation can be conservative.

### 3.4 Extending over time

Once the framework exists, we can add lints driven by real-world feedback:

- performance lints (unnecessary allocations, repeated conversions)
- API misuse lints (e.g. `Option` / `Result` patterns)
- style lints (opt-in, if desired)

---

## Shared infrastructure: one diagnostics renderer

To avoid inconsistent UX, the compiler, formatter, and linter should all use:

- one `Diagnostic` data model
- one renderer (human output)
- one JSON schema (tool output)

This also enables:

- consistent output in `rusk` CLI, `ruskc`, and `rusk-lsp`
- consistent “severity policy” (`--deny-warnings`)

---

## Implementation plan (incremental)

### Phase 1 — Diagnostics rendering (compiler)

- Add a rich diagnostic renderer (snippet + underline) using `SourceMap`.
- Upgrade the most common compiler errors to include:
  - a precise span,
  - at least one helpful message line,
  - optional `help:` suggestions when obvious.

Deliverable:
- `rusk` prints multi-line diagnostics by default for compile errors.

### Phase 2 — Formatter MVP

- Implement `rusk fmt` for a core subset of syntax used in fixtures.
- Add `--check` and golden tests.

Deliverable:
- A stable formatter for the common language surface.

### Phase 3 — Linter MVP

- Implement `rusk lint` using the existing analysis/typecheck pipeline where needed.
- Add 5–10 initial lints (high-signal).
- Add `--deny-warnings` and `--json`.

Deliverable:
- Practical warnings that catch common mistakes early.

---

## Open questions

- Should `rusk fmt` be “format on error” tolerant (skip files that fail to parse) or strictly fail?
- Do we want a `rusk check` subcommand as a thin alias for “compile-only, no execution”, to pair with
  `fmt` and `lint`?
- What is the right long-term home for these components?
  - separate crates (`rusk-fmt`, `rusk-lint`) vs. modules inside `rusk-compiler`
- How strongly do we want to commit to diagnostic *stability* (codes and wording) for tests/fixtures?

