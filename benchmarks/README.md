# Benchmarks

This folder contains small, stable Rusk programs intended for local performance measurements.

## Running

Use the in-tree measurement runner:

```sh
# Unoptimized baseline (bytecode opt level O0):
cargo run --release --bin rusk-measure -- --opt-level o0 --json --warmup 2 --iters 10 <file.rusk>

# Optimized (bytecode opt level O2; also the default):
cargo run --release --bin rusk-measure -- --opt-level o2 --metrics --json --warmup 2 --iters 10 <file.rusk>
```

To compare against Python (using the repo's `.venv`):

```sh
# Rusk vs Python suite (writes `benchmarks/results.md` + `benchmarks/results.json`):
.venv/bin/python benchmarks/compare.py --validate --opt-level o2
```

Defaults are tuned to finish quickly (`--warmup 1 --iters 3 --repeats 5`), and a few heavy cases
may override these internally (e.g. the GC benchmark).

For consistent comparisons:
- use the same machine / power profile
- run each benchmark multiple times and compare medians
- keep `--warmup`/`--iters` constant across phases

## Files

- `phase1_var_loop.rusk`: locals + while loop baseline
- `phase2_object_access.rusk`: struct/tuple field access hot path
- `phase3_closure_capture.rusk`: closure creation with many visible but unused bindings
- `phase4_call_dispatch.rusk`: call dispatch / call-heavy loop
- `phase5_gc_epoch.rusk`: GC stress (large heap high-water mark + many GC cycles)
- `phase6_effects_generator.rusk`: generator/coroutine-style yield/resume via effects

Python equivalents live next to the `.rusk` benchmarks as `phase*.py` and expose `main()` plus an
`EXPECTED` constant for validation.
