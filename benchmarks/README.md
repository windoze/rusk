# Benchmarks

This folder contains small, stable Rusk programs intended for local performance measurements.

## Running

Use the in-tree measurement runner:

```sh
cargo run --release --bin rusk-measure -- --json --warmup 2 --iters 10 <file.rusk>
```

For consistent comparisons:
- use the same machine / power profile
- run each benchmark multiple times and compare medians
- keep `--warmup`/`--iters` constant across phases

## Files

- `phase2_object_access.rusk`: struct/tuple field access hot path
- `phase3_closure_capture.rusk`: closure creation with many visible but unused bindings
- `phase4_call_dispatch.rusk`: call dispatch / call-heavy loop
- `phase5_gc_epoch.rusk`: GC stress (large heap high-water mark + many GC cycles)
