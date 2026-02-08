# Repository Guidelines

## Project Structure & Module Organization

- `src/`: Rust library source (entry point: `src/lib.rs`).
- `tests/`: Integration tests (create as needed; not present yet).
- `RUSK_SPEC.md`, `MIR_SPEC.md`: Design/spec notes—update these when behavior changes.
- `target/`: Build artifacts (gitignored; do not commit).

## Build, Test, and Development Commands

- `cargo check`: Fast compile/type-check during development.
- `cargo build`: Build the crate (debug by default).
- `cargo test`: Run unit + integration tests.
- `cargo test <name>`: Run a single test (substring match).
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
