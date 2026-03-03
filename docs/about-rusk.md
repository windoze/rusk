# About Rusk

![Rusk logo](assets/Rusk-256x256.png)

Rusk is an experimental programming language and runtime, implemented in Rust.
It aims to combine:

- **Rust-like syntax** (block expressions, `match`, explicit mutability controls),
- **TypeScript-like ergonomics** (type inference, generics, interface-driven abstraction), and
- **Algebraic effects** as a first-class mechanism for control-flow extensions (exceptions, async,
  generators, and more).

Rusk source code (`.rusk`) is compiled into a compact bytecode module (`.rbc`) via an internal
mid-level IR (“MIR”), and then executed by a small bytecode VM. The VM is designed to be embedded
in other applications (CLIs, editors, servers, etc.). Platform integration is provided through
**host functions** installed by the embedding environment.

## Example

```rusk
fn main() -> int {
    let xs = [10, 20, 30];
    xs[1] = 99;
    xs[1]
}
```

## Where to go next

- Getting started: `guides/quick_start.md`
- Language overview: `guides/concepts_and_syntax.md`
- Embedding the VM: `embedding-vm.md`
- Editor integration (LSP): `rusk-lsp.md`
- Specifications:
  - `../RUSK_SPEC.md`
  - `../MIR_SPEC.md`
  - `../BYTECODE_SPEC.md`

