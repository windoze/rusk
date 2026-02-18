# Proposal: `byte` + `char` primitives and zero-copy slicing for `bytes` / `string`

Date: 2026-02-17

Status: Draft

This proposal introduces two new primitive types (`byte`, `char`), adds a small set of explicit
conversion APIs, and defines efficient (“view”, zero-copy) slicing for `bytes` and `string`.

It also adds O(1) indexing helpers for `bytes` while explicitly **not** adding indexing for
`string` (to avoid accidentally introducing O(n) operations that look O(1)).

The host ABI stays unchanged for now (host functions continue to accept/return `int/float/bool/unit/string/bytes` only).

---

## Motivation

Today:

- `bytes` is an opaque byte vector (`Vec<u8>` in the VM).
- `string` is UTF-8 (`String` in the VM); iterating yields Unicode scalar values as `int`.
- There is no “byte element” type, so `bytes` cannot conveniently interoperate with arrays.
- There is no zero-copy “slice view” type for `bytes`/`string`.

We want:

1) **A real `byte` element type** so `[byte]` can be used for mutable byte-level operations while
   keeping `bytes` immutable.
2) **A real `char` type** so string iteration is more meaningful than “`int` codepoints”.
3) **Zero-copy slicing** for `bytes` and `string` that:
   - does not copy underlying data,
   - returns the same surface type (`bytes` / `string`),
   - keeps the backing storage alive via the VM GC even if the original value is dropped.
4) **O(1) indexing for `bytes`** (`b[idx]` and `b.get(idx)`), but **no indexing for `string`**.

---

## Goals

- Add primitive types:
  - `byte`: an unsigned 8-bit value (`0..=255`)
  - `char`: a Unicode scalar value (`0..=0x10FFFF`, excluding surrogate range `0xD800..=0xDFFF`)
- Keep `bytes` and `string` *immutably* typed at the surface.
- Provide explicit, predictable conversions between `int` and `byte`/`char`:
  - `int`↔`byte`: truncating (`to_byte`) and checked (`try_byte`) variants.
  - `int`↔`char`: checked trapping (`to_char`) and checked optional (`try_char`) variants.
- Add:
  - `bytes[idx]` (trap on out-of-bounds)
  - `bytes.get(idx) -> Option<byte>` (returns `None` on out-of-bounds)
- Add zero-copy slice views:
  - `bytes.slice(from: int, to: Option<int>) -> bytes`
  - `string.slice(from: int, to: Option<int>) -> string`
  where `to = None` means “to the end”.
- Keep the **host function interface unchanged** (no new ABI types; no GC handles exposed).

---

## Non-goals (for this proposal)

- Changing the host ABI (`AbiValue` / `HostType`) to carry `byte`/`char` or GC-backed views.
- Adding `string[idx]` or `string.get(idx)` (these are O(n) if “idx is the Nth char” and too easy to misuse).
- Guaranteeing that converting between `bytes` and `[byte]` is zero-copy (copying is acceptable and keeps the immutability story simple).
- Adding new literal syntaxes (e.g. `'\n'` for `char` or `0xffu8` for `byte`). Conversions cover construction for now.

---

## User-facing surface

### 1) New primitive types

Extend `PrimType` with:

- `byte`
- `char`

Intended semantics:

- `byte` is a value type representing a single byte (`0..=255`).
- `char` is a value type representing a Unicode scalar value.

### 2) Conversions: `int` ↔ `byte`/`char`

Provide explicit conversion APIs (method style shown; UFCS style is optional but recommended).

#### `int` → `byte`

- `readonly fn to_byte() -> byte`
  - Truncating conversion.
  - **Semantics:** keep the low 8 bits of the integer (wrap mod 256).
- `readonly fn try_byte() -> Option<byte>`
  - Checked conversion.
  - **Semantics:** returns `Some(b)` iff `0 <= self <= 255`, else `None`.

#### `byte` → `int`

- `readonly fn to_int() -> int`
  - Always succeeds.
  - **Semantics:** returns the numeric value in `0..=255`.

#### `int` → `char`

- `readonly fn to_char() -> char`
  - Checked conversion that **traps** on invalid values.
- `readonly fn try_char() -> Option<char>`
  - Checked conversion.
  - **Semantics:** returns `Some(c)` iff `self` is a valid Unicode scalar value, else `None`.

**Precise semantics (shared):** `self` is valid iff:

- If `self < 0`: invalid.
- If `self > 0x10FFFF`: invalid.
- Let `u = self as u32`.
- If `u` is in `0xD800..=0xDFFF` (surrogates): invalid.
- Otherwise: valid, and the resulting value is `char(u)`.

Then:

- `try_char()` returns `Some(char(u))` for valid values, else `None`.
- `to_char()` returns `char(u)` for valid values, else **traps**.

Note:

- Surrogates are often thought of as a UTF-16 concern, but they are a reserved Unicode code point
  range and are **not valid Unicode scalar values**. They also cannot appear in well-formed UTF-8.
  Therefore `try_char()` returns `None` for surrogates and `to_char()` traps.

#### `char` → `int`

- `readonly fn to_int() -> int`
  - Always succeeds (by construction, `char` is always valid).
  - Returns the Unicode scalar value as an `int`.

### 3) `bytes` operations

#### Indexing (`[]`) — O(1)

Add indexing for `bytes`:

- `b[idx] : byte`
  - traps if:
    - `idx < 0`, or
    - `idx >= len(b)`.

This is intended to be O(1) and consistent with `bytes` being a contiguous byte sequence.

#### `get(idx)` — safe access

Add:

- `readonly fn get(idx: int) -> Option<byte>`
  - returns `Option::None` if `idx` is out of bounds.

#### Slice views — zero-copy

Add:

- `readonly fn slice(from: int, to: Option<int>) -> bytes`

Semantics:

- Uses half-open ranges: `[from, to)`.
- `to = None` means “use `len(bytes)`”.
- Traps if `from < 0` or `to < 0` (when `to = Some(_)`).
- Traps if `from > to` or `to > len`.

**Key property:** the returned `bytes` must be a view into the same underlying storage (no data copy).

#### Conversions between `bytes` and `[byte]`

Add explicit conversion helpers:

- `readonly fn to_array() -> [byte]`
  - Copies bytes into a fresh array (mutable array semantics remain separate from immutable `bytes`).
- `static fn from_array(xs: readonly [byte]) -> bytes`
  - Copies array contents into a fresh `bytes` value.

Notes:

- Copying is intentional: it prevents mutations of `[byte]` from affecting any `bytes` aliases, keeping
  `bytes` immutable without needing copy-on-write or uniqueness typing.
- A future optimization may allow a “zero-copy freeze” when it can be proven safe (e.g., consuming a fresh array), but this is explicitly out of scope here.

### 4) `string` operations

#### Slice views — zero-copy

Add:

- `readonly fn slice(from: int, to: Option<int>) -> string`

**Index meaning (important):**

- `from` and `to` are **byte offsets** into the UTF-8 string, not “character indices”.

Semantics:

- Uses half-open ranges: `[from, to)`.
- `to = None` means “use `len_bytes(string)`”.
- Traps if:
  - `from < 0` or `to < 0` (when `to = Some(_)`),
  - `from > to` or `to > len_bytes`,
  - `from` or `to` is not a UTF-8 character boundary.

Rationale:

- Slicing by byte offsets can be implemented in O(1) with boundary checks.
- This matches the goal of **not** exposing operations that appear O(1) but are actually O(n).

#### No indexing / `get` on `string`

Do **not** add:

- `s[idx]`
- `s.get(idx)`

because “`idx` as character index” is inherently O(n) for UTF-8 strings.

If we later want char-indexing utilities, they should be explicit about complexity (e.g. `nth_char(idx)`), and are out of scope for this proposal.

---

## Iteration changes (`for` over `string` / `bytes`)

Update the built-in iterable element types:

- Iterating over `string` yields `char` (instead of `int`).
- Iterating over `bytes` yields `byte` (instead of `int`).

This keeps iteration strongly-typed and reduces manual “codepoint-as-int” plumbing.

---

## Core intrinsics (lowering targets)

Rusk typically lowers “core operations” to `core::intrinsics::*` targets. The exact surface syntax
can be methods, but the compiler needs canonical intrinsic call targets.

Proposed additions (signatures at the Rusk level):

### Primitive conversions

- `core::intrinsics::int_to_byte(x: int) -> byte`
- `core::intrinsics::int_try_byte(x: int) -> Option<byte>`
- `core::intrinsics::byte_to_int(x: byte) -> int`

- `core::intrinsics::int_to_char(x: int) -> char`
- `core::intrinsics::int_try_char(x: int) -> Option<char>`
- `core::intrinsics::char_to_int(x: char) -> int`

### `bytes`

- `core::intrinsics::bytes_get(b: bytes, idx: int) -> Option<byte>`
- `core::intrinsics::bytes_slice(b: bytes, from: int, to: Option<int>) -> bytes`
- `core::intrinsics::bytes_to_array(b: bytes) -> [byte]`
- `core::intrinsics::bytes_from_array(xs: readonly [byte]) -> bytes`

For the `b[idx]` syntax, two options:

1) Special-case `IndexGet` lowering in the compiler/VM when the base is `bytes`, OR
2) Lower `b[idx]` to a dedicated intrinsic: `core::intrinsics::bytes_index(b, idx) -> byte`.

Either is acceptable; (1) keeps surface syntax consistent with arrays, (2) keeps bytecode semantics simpler.

### `string`

- `core::intrinsics::string_slice(s: string, from: int, to: Option<int>) -> string`

### Iteration

Update existing intrinsics:

- `core::intrinsics::string_next(StringIter) -> Option<char>`
- `core::intrinsics::bytes_next(BytesIter) -> Option<byte>`

---

## Runtime / VM representation (high-level)

To satisfy “slice views do not copy and outlive the original via GC”, `bytes`/`string` must be backed by GC-managed storage.

Today the VM stores them as owned Rust containers (`String` / `Vec<u8>`), so a slice view cannot safely borrow data without copying.

### Proposed representation

Introduce GC-managed backing buffers:

- `HeapValue::BytesBuf { data: Box<[u8]> }` (or equivalent)
- `HeapValue::StringBuf { data: Box<str> }` (or equivalent)

Represent surface values as lightweight views:

- `bytes` value contains:
  - `buf: GcRef` (points to `BytesBuf`)
  - `start: u32`
  - `len: u32`
- `string` value contains:
  - `buf: GcRef` (points to `StringBuf`)
  - `start: u32` / `len: u32` (byte offsets; guaranteed to be UTF-8 boundaries)

This makes:

- `slice` O(1) (new view value, same backing)
- `b[idx]` O(1) (bounds check + read from backing at `start + idx`)

### GC tracing requirement

Whatever concrete VM representation is chosen, the GC root tracer must treat `bytes`/`string` values as roots by marking their backing handles.

This ensures that a slice view keeps the underlying buffer alive even if the original value is dropped.

---

## Host ABI impact (kept as-is)

Host functions should remain **GC-unaware** and continue using:

- `AbiValue::String(String)`
- `AbiValue::Bytes(Vec<u8>)`

Implications:

- Passing a `bytes`/`string` slice view to a host function will require materializing (copying) into an owned `String`/`Vec<u8>` at the boundary.
- Returning a host `String`/`Vec<u8>` to the VM will require allocating a new GC-backed buffer (copying into it).

This is acceptable for now and keeps the embedding API stable while the internal VM representation evolves.

`byte` and `char` are not part of the host ABI in this proposal; users must explicitly convert via `to_int()` when crossing the boundary.

---

## Compiler + bytecode impact (high-level)

Implementing this will require changes across layers (details intentionally omitted here):

- Spec updates:
  - `RUSK_SPEC.md`: add `byte`/`char`, define conversions, `bytes` indexing + get, slice APIs, update iterator item types.
  - `MIR_SPEC.md` / `BYTECODE_SPEC.md`: add the new primitive types and intrinsics.
- Compiler:
  - Typechecker: recognize `byte`/`char`, update built-in iterable types for `for`, typecheck `bytes[idx]`, add builtin signatures for conversions + slicing.
  - Lowering: map new intrinsics into bytecode.
- Bytecode format:
  - Add intrinsics and type reps; extend encoder/decoder tag tables.
- VM:
  - Add `byte`/`char` value representations and operations.
  - Rework `bytes`/`string` to be GC-backed view types.
  - Implement new intrinsics and/or `IndexGet` special-case for `bytes`.

---

## Test plan (suggested)

Add tests covering:

- `byte` range + conversions:
  - `(-1).to_byte()` wrap behavior
  - `(-1).try_byte() == None`
  - `255.try_byte() == Some(255.to_byte())`
- `char` conversions:
  - `65.try_char() == Some('A')` (constructed via conversion; literal may not exist)
  - `0xD800.try_char() == None` (surrogate)
  - `to_char()` traps on invalid values (surrogates, negative, overflow)
- `bytes`:
  - indexing traps on oob
  - `get` returns `None` on oob
  - slicing `slice(0, None)` equals original
  - slicing is a view (can’t directly assert “no copy” in surface semantics, but can assert GC/liveness by allocating, dropping, then using the slice)
- `string`:
  - slicing traps on non-boundary
  - slicing with `None` end works
  - `for c in s` yields `char` and roundtrips via `c.to_int()`

---

## Open questions

1) Slice API ergonomics vs allocation overhead:
   - `slice(from, to: Option<int>)` is convenient but may allocate `Option` values today.
   - Alternative/addition: `slice_from(from)` / `slice_to(to)` to avoid `Option` construction.

2) Should `bytes` also gain `len()` as a first-class operation in the same change?
   - Not required by the user-facing API above, but likely useful once indexing exists.
