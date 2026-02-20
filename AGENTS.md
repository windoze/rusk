# Repository Guidelines

## Non-goals

- Backwards compatibility: we are free to make breaking changes as needed in the current stage of development.

## Project Structure & Module Organization

- `Cargo.toml`: Workspace definition plus root `rusk` package.
- `src/`: Root binary crate source (entry point: `src/main.rs`).
- `crates/`: Workspace member crates.
- `crates/rusk-compiler/`: Compiler crate (entry point: `crates/rusk-compiler/src/lib.rs`).
- `crates/rusk-mir/`: MIR crate (compiler-internal IR; entry point: `crates/rusk-mir/src/lib.rs`).
- `crates/rusk-bytecode/`: Bytecode module + `.rbc` format (entry point: `crates/rusk-bytecode/src/lib.rs`).
- `crates/rusk-vm/`: Bytecode VM runtime (entry point: `crates/rusk-vm/src/lib.rs`).
- `crates/rusk-host/`: Host module declarations + installers (entry point: `crates/rusk-host/src/lib.rs`).
- `crates/rusk-gc/`: GC primitives used by the VM (entry point: `crates/rusk-gc/src/lib.rs`).
- `tests/`: Integration tests for the workspace.
- `fixtures/`: Test fixtures and sample inputs.
- `RUSK_SPEC.md`, `MIR_SPEC.md`, `BYTECODE_SPEC.md`: Design/spec notes—update these when behavior changes.
- `target/`: Build artifacts (gitignored; do not commit).
- `proposals/`: RFC-style design proposals for major features/changes.
- `completed-proposals/`: Proposals that have been accepted and implemented; these are now part of the project history and can be read for context on design decisions.
- `uncommitted-proposals/`: Drafts of proposals not yet ready for review. **DO NOT READ ANYTHING IN THIS FOLDER WITHOUT EXPLICIT REQUEST.**

## Build, Test, and Development Commands

- `cargo check`: Fast compile/type-check during development.
- `cargo build`: Build the crate (debug by default).
- `cargo test --all`: Run full test suite across all workspace crates.
- `cargo test`: Run unit + integration tests.
- `cargo test <name>`: Run a single test (substring match).
- `cargo test -p <crate>`: Run tests for a specific workspace crate.
- `cargo fmt`: Format code with `rustfmt`.
- `cargo clippy -- -D warnings`: Lint and fail on warnings (preferred for PRs).
- `cargo doc --open`: Generate and view local API docs.

## Coding Style & Naming Conventions

- Rust edition: **2024** (see `Cargo.toml`).
- Formatting: use `cargo fmt` before pushing; avoid manual alignment/spacing tweaks.
- Names follow Rust conventions: `snake_case` (functions/vars/modules), `CamelCase` (types/traits), `SCREAMING_SNAKE_CASE` (consts/statics).
- Prefer small, focused modules and explicit types at public boundaries (`pub` items should be documented with `///`).
- Always use newest stable versions of dependencies unless there's a specific reason not to.
- Always create thorough tests for new features and bug fixes, and ensure all tests pass after changes.

## Testing Guidelines

- Unit tests live next to code under `#[cfg(test)] mod tests { ... }`.
- Integration tests (when needed) go in `tests/*.rs` and use the public API.
- Name tests for the behavior under test (e.g., `parses_empty_input`, `rejects_invalid_opcode`).

## Commit & Pull Request Guidelines

- This repo currently has **no commit history**, so no established message convention. Use **Conventional Commits** going forward: `feat: ...`, `fix: ...`, `docs: ...`, `refactor: ...`, `test: ...`, `chore: ...`.
- PRs should include: what changed, why, how to test (`cargo test`), and any spec/doc updates (`RUSK_SPEC.md`, `MIR_SPEC.md`) if behavior is user-visible.
