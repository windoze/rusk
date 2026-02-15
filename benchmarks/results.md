# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-15T13:26:19Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.2 (main, Dec  5 2025, 16:49:16) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `2ee4a88b0b49e023e9066c6771d1ece034a5f021`

## Parameters

- Rusk backend: `bytecode`
- Bytecode opt level: `o2`
- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 7.370ms | 8.439ms | 0.87x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.600ms | 20.884ms | 1.32x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 8.997ms | 5.090ms | 1.77x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 17.450ms | 21.567ms | 0.81x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 223.724ms | 91.742ms | 2.44x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.173ms | 447.152us | 11.57x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
