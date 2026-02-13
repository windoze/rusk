# Need-Fix Report (Code Review)

Date: 2026-02-13

This document is a **code review findings report**: potential issues, robustness gaps, and build
configuration problems discovered during a read-through + local checks.

It **does not implement fixes**; it captures what seems most important to address next.

---

## Scope / Commands Run

Reviewed (read-through + targeted searches):

- `src/*` (CLI entry points)
- `crates/rusk-compiler/src/*` (loader, resolver, typeck, lowering)
- `crates/rusk-mir/src/lib.rs` (MIR model + feature gating)
- `crates/rusk-interpreter/src/*` (interpreter + GC + host integration)
- `crates/rusk-host/src/*` (std I/O host module)

Local commands run (results summarized below):

- `cargo test` ✅
- `cargo clippy -- -D warnings` ✅

---

## Summary (What Looks Most Urgent)

1) **Interpreter robustness**: there is at least one reachable `panic!()` on invalid MIR that can
   crash the process when executing a crafted `.mir` file.

2) **Diagnostics quality**: many error paths use `Span::new(0, 0)` as a dummy span. With the current
   `SourceMap` behavior, this can render locations that misleadingly point at the beginning of the
   entry file.

Everything else below is lower severity, but worth tracking.

---

## Findings

### P1 — `rusk` can crash on crafted `.mir` via `panic!()` in interpreter

**What happens**

The interpreter panics on invalid internal type representation ids:

- `crates/rusk-interpreter/src/interpreter.rs:788`–`792`
  - `type_rep_node()` uses `.unwrap_or_else(|| panic!(...))`.

The `rusk` CLI explicitly supports running `.mir` files (`src/main.rs:22`–`46`). This means a
malformed/crafted `.mir` can potentially trigger process abort (panic) instead of a structured
`RuntimeError`.

**Where**

- `crates/rusk-interpreter/src/interpreter.rs:788`

**Why it matters**

- For a CLI that accepts a binary format, “panic on invalid input” is a footgun:
  - reduces robustness for users
  - complicates embedding (host app crash)
  - makes fuzzing / hardening harder

**Suggested direction**

- Consider returning `RuntimeError::Trap { ... }` or a dedicated `RuntimeError::InvalidTypeRepId`
  instead of panicking.
- If the invariant truly “cannot happen” for valid MIR, adding a MIR validation pass on load could
  also be an option (especially if `.mir` is intended as an interchange format).

---

### P1 — Diagnostics can point at misleading locations due to “dummy spans”

**What happens**

A large amount of compiler error plumbing uses `Span::new(0, 0)` as a placeholder for errors that
don’t have a natural source location (builtins, host module injection, internal errors, etc.).

With the current `SourceMap` behavior, `Span(0,0)` will map to the start of the *first* loaded file
(`crates/rusk-compiler/src/source_map.rs:89`–`117`). That means some “not really source” errors can
render as if they occurred at `<file:1:1>`, which is confusing.

**Where**

Representative examples (there are many):

- `crates/rusk-compiler/src/modules.rs:271`–`276` (duplicate module internal error)
- `crates/rusk-compiler/src/modules.rs:476`–`504` (builtin `Option` injection)
- `crates/rusk-compiler/src/typeck.rs:1257`–`1278` (cycle error falls back to `Span(0,0)`)
- Count: `Span::new(0, 0)` appears ~70 times in `crates/rusk-compiler/src/*` (quick grep)

**Suggested direction**

- Introduce an explicit “no-span” concept:
  - e.g., `Option<Span>` on error types, or a `Span::DUMMY` sentinel that `SourceMap` refuses to map
- Ensure “internal error” paths don’t pretend to have a real location.

---

### P2 — `SourceMap` range bookkeeping can silently break on offset overflow

**What happens**

`ModuleLoader::alloc_base_offset` uses saturating arithmetic:

- `crates/rusk-compiler/src/modules.rs:1456`–`1459`
  - `self.next_base_offset = self.next_base_offset.saturating_add(src.len() + 1);`

`SourceMap::add_source` only asserts non-overlap via `debug_assert!`:

- `crates/rusk-compiler/src/source_map.rs:73`–`80`

In release builds, if offsets ever saturate/overlap, span lookups may misbehave silently. This is
probably “only” a theoretical edge case (it requires extremely large total source size), but since
the compiler is otherwise careful about spans, it’s worth making the failure mode explicit.

**Suggested direction**

- Prefer checked arithmetic and return an error if the source map would overflow.
- At minimum, avoid the `src.len() + 1` overflow edge by using `src.len().saturating_add(1)`.

---

### P3 — Host function signature metadata is not used by the interpreter

**What happens**

MIR modules carry `host_imports: BTreeMap<String, HostFnSig>` as a declared import set, but the
interpreter currently only checks **presence** by name:

- `crates/rusk-interpreter/src/interpreter.rs:494`–`506`

The signature data is not used to validate arity/types at runtime. This may be intentional (host
functions are dynamically typed by `Value`), but as an API it’s easy to assume “signature mismatch
will be caught”, when it currently won’t be.

**Suggested direction**

- Either:
  - document explicitly that signatures are “tooling only” (not enforced), or
  - add an interpreter-side pre-call validation for arg count (and perhaps basic `ValueKind`
    checks where possible)

---

### P3 — `std` host functions ignore `flush()` errors

**Where**

- `crates/rusk-host/src/std_io.rs:44`
- `crates/rusk-host/src/std_io.rs:63`

**Why it matters**

If stdout becomes broken (e.g., closed pipe), ignoring flush errors can hide real failures and make
program behavior depend on buffering.

**Suggested direction**

- Treat flush failures like write failures and return a `RuntimeError::Trap`.

---

### P4 — Handle generation wrap-around is theoretically possible

Both GC handles and rooted handles use `u32` generations and advance them with `wrapping_add(1)`:

- GC slot generation: `crates/rusk-interpreter/src/gc.rs:147`
- Root handle generation: `crates/rusk-interpreter/src/interpreter.rs:455`

This is almost certainly fine in practice, but the theoretical edge case is:

- if a slot is freed/reused 2^32 times, the generation wraps and a stale handle could become valid
  again.

**Suggested direction**

- If you want to fully rule this out, use a wider generation (`u64`) or treat generation overflow as
  “poison the slot forever”.

---

### P4 — Panics / `expect()` density in non-test code

Quick grep shows:

- ~142 occurrences of `.expect(...)` in `crates/*` + `src/*`
- 1 occurrence of `.unwrap()` in non-test code:
  - `crates/rusk-compiler/src/compiler.rs:1500` (safe due to preceding `out_args.len() == 1` check)

Most of these `expect()`s are likely “this is an internal invariant” checks, which is fine for a
prototype. However, the cost is that internal bugs tend to become *process crashes* rather than
structured `CompileError`/`RuntimeError`.

**Suggested direction**

- For invariants that can be influenced by user input (especially `.mir` input), prefer returning an
  error over panicking.
- For invariants that truly cannot happen, consider `debug_assert!` (and a fallback error in
  release) to keep release builds more robust.

---

## Suggested Next Steps

If prioritizing for robustness + usability:

1) Replace the interpreter `panic!()` on invalid `TypeRepId` with a `RuntimeError` (P1).
2) Improve the “dummy span” story so internal/builtin errors don’t point at `<file:1:1>` (P1).
3) Decide whether host signatures are “metadata only” or should be enforced at runtime (P3).

