# Serde Fixes: JSON bytes input, incremental decoding, and core serde infrastructure

Date: 2026-03-06  
Status: Draft proposal

## Summary

This proposal addresses a set of practical limitations in Rusk’s current serialization stack:

- `std::json` only accepts `string` input (`from_string`), which is inconvenient for host APIs that naturally produce `bytes` (HTTP bodies, file IO, sockets).
- `std::json` float deserialization was previously blocked by the lack of a primitive “parse float from string” operation.
- `core::serde` includes an incremental decoding interface, but there is no concrete implementation for any format.
- “Serde infrastructure” gaps (not JSON-specific) make it hard to use derived `Serialize/Deserialize` in real programs:
  - built-in arrays (`[T]`) and tuples are common building blocks (e.g. `[(string, string)]` headers) but need first-class serde coverage.
  - impl specialization is not supported yet (erased generics), which affects how generic collections can implement serde.

This proposal introduces:

- `std::json::from_bytes<T: Deserialize>(src: bytes) -> Result<T, SerdeError>`
- `std::json::Decoder` implementing `core::serde::IncrementalDecoder`
- a small set of core intrinsics needed by serde (notably `float_from_string_strict`)
- compiler/runtime + sysroot changes to make serde work smoothly for common composite types

## Current State and Limitations

### 1) “Deserialize from JSON bytes”

Current API surface:

- `std::json::from_string<T: Deserialize>(src: string) -> Result<T, SerdeError>`

There is no `std::json::from_bytes` helper, so users must manually convert:

- `bytes -> string` via `core::intrinsics::string_from_utf8` (lossy) or `string_from_utf8_strict` (Option)
- then call `from_string`

This is both inconvenient and easy to get wrong (lossy conversion can silently change the input).

### 2) Float decoding

JSON number tokens can include fractional / exponent parts, but float parsing was previously not available as a primitive operation, so `std::json` could not implement `Deserializer::float()`.

### 3) Incremental / chunked decoding

`core::serde` defines an incremental decoding interface:

- `core::serde::IncrementalDecoder`
- `core::serde::DecodePoll<T> = Pending | Ready(T)`

But no concrete format provides an implementation, so users can’t decode streams or chunked inputs incrementally.

### 4) Infrastructure limitations beyond JSON

Some issues are intrinsic to the serde stack rather than JSON specifically:

- **Common composite types need serde coverage**: arrays and tuples are ubiquitous and should “just work” with derives.
- **No impl specialization** (erased generics): implementing an interface for a partially-applied nominal type such as `Map<string, V>` is not supported. This constrains how we can encode key/value maps via `core::serde`.
- **String-keyed maps in `core::serde`**: `core::serde`’s “map” model is string-keyed. This matches JSON objects but is restrictive for other formats and for generic `Map<K, V>` collections.

## Proposal

### A) JSON: bytes input

Add:

- `std::json::from_bytes<T: core::serde::Deserialize>(src: bytes) -> Result<T, core::serde::SerdeError>`

Semantics:

- Parses JSON text from `bytes`.
- JSON strings must decode to valid Rusk `string` (UTF-8); invalid UTF-8 yields `SerdeError::InvalidUtf8` (or a format-specific parse error).
- Rejects trailing non-whitespace after the top-level value, same as `from_string`.

### B) JSON: incremental decoding

Provide a concrete incremental decoder:

- `std::json::Decoder` implements `core::serde::IncrementalDecoder`
- `feed_bytes(chunk: bytes)` / `feed_string(chunk: string)` append input
- `poll<T: Deserialize>() -> Result<DecodePoll<T>, SerdeError>`
  - returns `Pending` when input is a valid prefix but not complete yet
  - returns `Ready(v)` once a full value is available and successfully decoded
  - after `complete()`, `poll` must not return `Pending`

This unlocks decoding:

- streaming IO (chunked reads)
- HTTP responses where the body arrives in fragments
- interactive protocols

### C) Core intrinsics required by serde (format-agnostic)

Add a strict float parser:

- `core::intrinsics::float_from_string_strict(s: string) -> Option<float>`

Notes:

- “strict” means: no whitespace, no partial parsing; the whole string must be consumed.
- For JSON use, we additionally require finite floats (reject NaN / infinities).

Additional intrinsics (useful for serde beyond JSON):

- `core::intrinsics::int_from_string_strict(s: string) -> Option<int>` (fast numeric parsing for text formats)
- `core::intrinsics::string_to_utf8_bytes(s: string) -> bytes` (owned bytes, not a view)

### D) Serde infrastructure improvements

#### 1) Built-in arrays and tuples

Make derived serde work for common composite types:

- `[T]` implements `core::serde::{Serialize, Deserialize}` by encoding as a serde sequence
- tuples (at least `(T0, T1)`; optionally a small fixed arity set like 2..=8) implement serde by encoding as a fixed-length sequence

This is necessary for ergonomic APIs like:

- HTTP headers: `[(string, string)]`
- common ad-hoc records: `(int, string)`

#### 2) `core::map::Map<K, V>`

Because impl specialization is not supported yet, `Map<string, V>` cannot implement serde as a string-keyed map.

Instead, `Map<K, V>` should implement serde using a format-agnostic sequence encoding:

- `[k0, v0, k1, v1, ...]`

This keeps `Map<K, V>` usable with derives across formats, while leaving room for a future specialized “string map” type once specialization (or another mechanism) is available.

## Compatibility / Migration

This is a breaking-change-friendly phase of the project, but we still aim to be practical:

- Existing `std::json` users can continue using `from_string`/`to_string`.
- New helpers (`from_bytes`, incremental decoder) are additive.
- `core::map::Map` serde encoding may change; code relying on a JSON-object representation should move to a dedicated string-keyed map type once available.

## Implementation Checklist

1. Add `core::intrinsics::float_from_string_strict` (compiler typeck + bytecode + VM).
2. Align `std::json` serializer/deserializer with `core::serde` seq/map APIs.
3. Implement `std::json::from_bytes`.
4. Implement `std::json::Decoder` (`core::serde::IncrementalDecoder`).
5. Add serde coverage for arrays and tuples.
6. Add/update fixtures + tests and update `RUSK_SPEC.md` §9.9.
