# Proposal: Public Core Interfaces for Arrays, Strings, and Hashing

Date: 2026-02-18

Status: Draft

This proposal improves ergonomics by exposing a **public, stable core API** for common operations
that today require calling `core::intrinsics::*` directly (or cannot be expressed efficiently at
all).

The focus areas are:

1) Arrays (`[T]`): provide method-style wrappers over the existing `core::intrinsics::array_*`
   operations.
2) `string`: add `core::ops::Add` (concatenation), add conversion from `[char]`, and add UTF-8 /
   UTF-16 decoding helpers (lossy + strict).
3) Hashing: add a `core::hash::Hash` interface (returning `int`) with built-in impls for common
   types to enable `HashMap` / `HashSet`.

---

## Motivation

Rusk intentionally keeps a small core surface and routes “fast paths” through VM intrinsics.
However, the current public API has some sharp edges:

- Arrays support indexing and `len()` (via `core::len::Len`), but lack the usual “collection
  methods” (`push`, `pop`, `insert`, `remove`, …), even though the intrinsics already exist.
- `string` has slicing and iteration (via `.chars()`), but cannot be concatenated via `+` and lacks
  essential conversion/decoding APIs.
- Hash-based collections are a natural next step, but we need a way for user types to define a
  hash and for common types to have efficient default hashing.

This proposal aims to make common code “feel standard” without exposing `core::intrinsics::*` as
the everyday user-facing API.

---

## Goals

- Provide ergonomic, method-style array operations backed by existing intrinsics.
- Make `string + string` work by implementing `core::ops::Add` for `string`.
- Provide a practical way to build strings from character data and decode byte/code-unit data into
  strings (lossy and strict).
- Introduce a minimal `core::hash::Hash` interface for user-defined hashing, plus built-in impls
  for common types.

---

## Non-goals

- Adding new collection types in this proposal (`HashMap`, `HashSet`, `Vec`, …). This proposal only
  introduces the **prerequisite interfaces and wrappers**.
- Adding general-purpose bitwise integer operations (these would help implement hashing/decoding
  in pure Rusk, but are out of scope).
- Providing cryptographic hashes (this is about hash tables, not security primitives).
- Guaranteeing that a specific hash algorithm will never change across all future versions. We can
  aim for “stable for a given language/VM version” and leave room to revise if needed.

---

## 1) Arrays: public methods over existing intrinsics

### 1.1 Surface API

Expose an “array method set” for `[T]` using method-call sugar:

```rusk
let xs = [1, 2];
xs.push(3);
let last = xs.pop(); // Option<int>
xs.insert(0, 0);
let v = xs.remove(1);
xs.clear();
```

Proposed methods (all backed by existing `core::intrinsics::array_*`):

```rusk
// Mutating operations: require a mutable receiver ([T]).
fn array::push<T>(value: T) -> unit;
fn array::pop<T>() -> Option<T>;
fn array::clear<T>() -> unit;
fn array::insert<T>(idx: int, value: T) -> unit;
fn array::remove<T>(idx: int) -> T;
fn array::resize<T>(new_len: int, fill: T) -> unit;
fn array::extend<T>(other: [T]) -> unit;

// Non-mutating operations.
fn array::slice<T>(start: int, end: int) -> [T];                 // requires [T]
readonly fn array::slice_ro<T>(start: int, end: int) -> [readonly T]; // requires readonly [T]

// Copying helpers.
fn array::copy<T>() -> [T];                   // shallow copy of elements
readonly fn array::copy_ro<T>() -> [readonly T];

// Construction helpers (non-mutating, return a new array).
fn array::concat<T>(other: [T]) -> [T];                   // returns new array
readonly fn array::concat_ro<T>(other: readonly [T]) -> [readonly T];
```

Notes:

- The exact naming (`slice_ro`, `copy_ro`, `concat_ro`) is flexible, but the important part is to
  provide *some* way to access the readonly-returning intrinsics without `core::intrinsics::*`.
- Arrays already implement `core::len::Len`, so `xs.len()` remains the canonical way to obtain
  element count.

### 1.2 Mapping to intrinsics

These should be thin wrappers:

- `push` → `core::intrinsics::array_push`
- `pop` → `core::intrinsics::array_pop`
- `clear` → `core::intrinsics::array_clear`
- `insert` → `core::intrinsics::array_insert`
- `remove` → `core::intrinsics::array_remove`
- `resize` → `core::intrinsics::array_resize`
- `extend` → `core::intrinsics::array_extend`
- `concat` → `core::intrinsics::array_concat`
- `concat_ro` → `core::intrinsics::array_concat_ro`
- `slice` → `core::intrinsics::array_slice`
- `slice_ro` → `core::intrinsics::array_slice_ro`

`copy` / `copy_ro` can be defined via slicing:

- `copy`: `array_slice(self, 0, self.len())`
- `copy_ro`: `array_slice_ro(self, 0, self.len())`

### 1.3 “default value” helpers for resize/extend

The existing intrinsic `array_resize<T>(xs: [T], new_len: int, fill: T)` already supports “resize
with a provided fill”.

For ergonomics, we likely also want “resize with a default fill”:

```rusk
fn array::resize_default<T: core::default::Default>(new_len: int) -> unit;
fn array::extend_default<T: core::default::Default>(extra: int) -> unit;
```

This requires a `core::default::Default` interface:

```rusk
pub interface Default {
    static fn default() -> Self;
}
```

This proposal treats `Default` as a dependency we can either:

- include in this proposal (recommended if we want `resize_default` now), or
- split into a small precursor proposal and keep default-based array helpers as follow-up work.

### 1.4 Copying “into self” (open question)

We already have `copy()` returning a new array. The open question is whether we also want an API
that overwrites an existing array:

```rusk
fn array::copy_from<T>(other: [T]) -> unit;
```

Possible semantics:

- `self.clear(); self.extend(other);`

Pros:

- Avoids allocating a new array if the caller wants to reuse an existing buffer (future: capacity
  reuse if we add `reserve`).

Cons:

- Without a notion of capacity/reserve today, the main win is “avoid re-binding a variable”, not
  a big performance improvement.

Recommendation:

- Add `copy()` / `copy_ro()` now.
- Defer `copy_from` until we have clearer performance motivation (or until we add capacity APIs).

---

## 2) `string` improvements

### 2.1 `+` concatenation via `core::ops::Add`

Implement `core::ops::Add` for `string`:

```rusk
impl core::ops::Add for string {
    readonly fn add(other: string) -> string;
}
```

Lowering target:

- `core::intrinsics::string_concat(a: string, b: string) -> string`

This makes:

```rusk
let s = "hello, " + "world";
```

behave as expected, and aligns string concatenation ergonomics with `f"..."`.

### 2.2 Converting `[char]` → `string`

Today, there is no efficient way to build a string from an array of characters (`[char]`) without
O(n²) repeated concatenation.

Proposed API:

```rusk
static fn string::from_chars(chars: readonly [char]) -> string;
```

This likely requires a new intrinsic (or a small set of intrinsics) because:

- we do not have a “string builder” API in core,
- we do not have bitwise operations needed for efficient UTF-8 encoding in pure Rusk.

Proposed intrinsic:

```rusk
pub intrinsic fn string_from_chars(chars: readonly [char]) -> string;
```

### 2.3 Decoding: `from_utf8` / `from_utf16_le` / `from_utf16_be`

We want decoding helpers that cover common interop scenarios:

- **Lossy** decoding: invalid sequences are replaced with U+FFFD.
- **Strict** decoding: returns `Option<string>` and fails on invalid input.

#### 2.3.1 UTF-8

Proposed API (lossy + strict):

```rusk
static fn string::from_utf8(data: bytes) -> string;
static fn string::from_utf8_strict(data: bytes) -> Option<string>;
```

Optionally, we can add overload-like variants for arrays:

```rusk
static fn string::from_utf8_array(data: readonly [byte]) -> string;
static fn string::from_utf8_array_strict(data: readonly [byte]) -> Option<string>;
```

…but we should be mindful that Rusk does not have ad-hoc overloading, so the names must remain
distinct.

#### 2.3.2 UTF-16 (LE/BE)

Rusk does not currently have a `u16` primitive type. This proposal follows a pragmatic approach:

- Accept `readonly [int]` where each element must be in `0..=65535` (strict) and is treated as a
  UTF-16 code unit.

Proposed APIs:

```rusk
static fn string::from_utf16_le(data: readonly [int]) -> string;
static fn string::from_utf16_le_strict(data: readonly [int]) -> Option<string>;

static fn string::from_utf16_be(data: readonly [int]) -> string;
static fn string::from_utf16_be_strict(data: readonly [int]) -> Option<string>;
```

Rationale:

- We avoid introducing new integer widths in this proposal.
- The “loose” decoders can map invalid surrogate structure to U+FFFD.
- The “strict” decoders can reject:
  - any element outside `0..=65535`,
  - any invalid surrogate pairing.

#### 2.3.3 Implementation note: these likely need intrinsics

Given current core arithmetic (no bitwise ops and no byte-level string builder), decoding should be
implemented in the VM via intrinsics for correctness and performance.

Suggested intrinsic set:

```rusk
pub intrinsic fn string_from_utf8(data: bytes) -> string;
pub intrinsic fn string_from_utf8_strict(data: bytes) -> Option<string>;

pub intrinsic fn string_from_utf16_le(data: readonly [int]) -> string;
pub intrinsic fn string_from_utf16_le_strict(data: readonly [int]) -> Option<string>;

pub intrinsic fn string_from_utf16_be(data: readonly [int]) -> string;
pub intrinsic fn string_from_utf16_be_strict(data: readonly [int]) -> Option<string>;
```

---

## 3) `core::hash::Hash` interface

### 3.1 Surface API

Introduce:

```rusk
pub interface Hash {
    readonly fn hash() -> int;
}
```

This supports user-defined hashing:

```rusk
struct Point { x: int, y: int }

impl core::hash::Hash for Point {
    readonly fn hash() -> int {
        // Example only; exact combine helper TBD.
        core::hash::combine(self.x.hash(), self.y.hash())
    }
}
```

This interface is the prerequisite for implementing efficient hash-based collections:

- `HashMap<K: Hash + core::ops::Eq, V>`
- `HashSet<T: Hash + core::ops::Eq>`

### 3.2 Built-in implementations for common types

We should provide built-in implementations for:

- `int`
- `bool`
- `byte`
- `char`
- `string`
- `bytes`
- `unit` (can return a fixed constant)

Arrays can be postponed (generic + readonly-element concerns, and uncommon as hash keys).

### 3.3 Hash algorithm and stability

We need to decide what “hash” means in the language:

- Deterministic within a given VM version (recommended).
- Not cryptographically secure (fine).
- Preferably stable across platforms (since `int` is a fixed VM type).

Open question:

- Do we want per-process randomization to mitigate hash-flooding DoS?
  - If yes, the API likely needs a `Hasher` state/seed (or a `Hash` method that accepts a seed),
    which is a larger design than this minimal interface.

This proposal keeps the interface minimal and leaves room for a future “seeded hashing” expansion
if needed.

### 3.4 Supporting helpers

To make user-defined hashing pleasant, add a helper:

```rusk
pub fn combine(a: int, b: int) -> int;
```

The exact mixing function can be specified in core (and optimized in the VM later if desired).

### 3.5 Implementation strategy

Because Rusk currently does not allow source-authored `impl Hash for int/string/...` for primitive
types, we should mirror the existing approach used for `core::ops::*` and `core::fmt::ToString`:

- the compiler registers built-in impls for primitive types in the type environment,
- the compiler synthesizes wrapper functions like:
  - `impl::core::hash::Hash::for::int::hash`
  - `impl::core::hash::Hash::for::string::hash`
- wrappers lower to VM intrinsics or minimal MIR.

This likely requires new intrinsics for fast hashing of `string` and `bytes` (and possibly `int`):

```rusk
pub intrinsic fn hash_int(v: int) -> int;
pub intrinsic fn hash_string(s: string) -> int;
pub intrinsic fn hash_bytes(b: bytes) -> int;
```

---

## Rollout plan (suggested)

1) Add array wrapper methods (pure “ergonomics”, backed by existing intrinsics).
2) Add `string` `Add` impl + `string::from_chars` (requires at least one new intrinsic).
3) Add string decoding intrinsics + wrappers (larger runtime feature).
4) Add `core::hash::Hash` interface + built-in impls + hashing intrinsics (enables `HashMap`/`HashSet`).

---

## Appendix: Core intrinsics without a public wrapper (2026-02-18)

The goal of this proposal is to make most common code avoid calling `core::intrinsics::*`
directly. After the wrappers in this proposal, the remaining intrinsics that still lack a
dedicated “ergonomic wrapper” are mostly **iteration constructors** that are already used by
compiler desugarings:

- `core::intrinsics::into_iter` (used by `for` loop lowering for arrays)
- `core::intrinsics::bytes_into_iter` (no public `bytes::iter()` wrapper yet)

Some intrinsics are intentionally not wrapped 1:1 because they are already reachable via a public
interface or syntax sugar:

- Arithmetic and comparisons: `int_*`, `float_*`, `bool_*` (operators and `core::ops::*`)
- Formatting: `to_string` (via `core::fmt::ToString` and `f"..."`)
- Equality: `string_eq`/`bytes_eq`/`unit_eq` (via `==` / `!=` and `core::ops::*`)
- Iterator stepping: `next`/`string_next`/`bytes_next` (via `core::iter::Iterator` on iterator
  state types)

Each step can be landed independently, and none requires backwards compatibility guarantees at this
stage.
