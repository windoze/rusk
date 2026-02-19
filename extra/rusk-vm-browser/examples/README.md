# Browser VM Examples

These examples show how to run Rusk bytecode (`.rbc`) in the browser via `rusk-vm-browser`,
including:

- a minimal “run to completion” demo
- a host-import demo (sync JS ↔ Rusk bridge)
- a counter demo that uses **JavaScript + external effects** as a lightweight DOM bridge (no DOM
  bindings in Rusk required)

## Prereqs

- `wasm-pack` (for building the WASM + JS wrapper)
- A local static file server (any of: `python3 -m http.server`, `npx http-server`, etc.)

## 1) Build the WASM package

From `extra/rusk-vm-browser`:

```sh
wasm-pack build --target web --release --out-dir pkg
```

## 2) Build the example bytecode (`.rbc`)

The example `.rbc` files are generated (and gitignored).

From `extra/rusk-vm-browser`:

```sh
cargo run --example build_examples
```

This writes `program.rbc` next to each `program.rusk` under `examples/*/`.

## 3) Serve and open

From `extra/rusk-vm-browser`:

```sh
python3 -m http.server 8000
```

Then open:

- `http://localhost:8000/examples/basic-run/`
- `http://localhost:8000/examples/host-imports/`
- `http://localhost:8000/examples/counter/`

