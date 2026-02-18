# Rusk Examples

This directory contains small, step-by-step Rusk programs, ordered from basic to advanced.

## Running

From the repository root:

```sh
cargo run --bin rusk -- examples/01-hello-world.rusk
cargo run --bin rusk -- examples/10-modules/main.rusk
```

Notes:

- Many examples use `std::print` / `std::println`. In this repo, those functions are **host
  functions** registered by the `rusk` CLI binary (not part of `core`), so they may not exist in
  other embedding environments unless you register them.
- The language spec is in `RUSK_SPEC.md` and the MIR spec is in `MIR_SPEC.md`.

## Index

- `01-hello-world.rusk`: printing via `std::print` / `std::println`
- `02-values-and-functions.rusk`: values, function definitions, return values
- `03-control-flow-if-match.rusk`: `if` and `match` as expressions
- `04-arrays-and-for-loops.rusk`: arrays, indexing, `for` loops
- `05-strings-and-fmt.rusk`: strings and `f"..."` formatted strings
- `06-structs-and-methods.rusk`: structs + inherent methods (`impl Type { ... }`)
- `07-enums-and-patterns.rusk`: enums and pattern matching
- `08-closures-and-functions-as-values.rusk`: lambdas/closures and `fn(...) -> ...` values
- `09-generics.rusk`: generic structs + generic functions
- `10-modules/`: a multi-file example using `mod` and `use`
- `11-interfaces-and-dynamic-dispatch.rusk`: interfaces, `as Interface`, dynamic dispatch
- `12-effects-basic.rusk`: algebraic effects + `match` effect handlers
- `13-effects-typed.rusk`: typed (generic) effects
- `14-type-tests-is-asq.rusk`: runtime type tests with `is` and `as?`
- `15-effects-generator.rusk`: implementing generators using algebraic effects with `yield` and `resume`
- `16-fixed-point-combinator.rusk`: Y-combinator and fixed-point recursion using generics and higher-order functions
- `17-effects-state-management.rusk`: React-like state management, demonstrating how effects overcome `useEffect` limitations
- `18-newtype-structs.rusk`: new-type structs (nominal wrappers) for type safety
- `19-destructuring-patterns.rusk`: destructuring in `let`/`const`/`readonly` statements
- `20-associated-types.rusk`: associated types in interfaces with `Self::Item` and qualified projections
- `21-byte-char-slicing.rusk`: `byte` and `char` primitives, zero-copy string/bytes slicing
- `22-option-methods.rusk`: `Option` methods (`map`, `and_then`, `unwrap_or`, etc.)
