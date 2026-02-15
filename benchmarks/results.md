# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-15T07:28:44Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.2 (main, Dec  5 2025, 16:49:16) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `a98ad24ab8cc1c9083346b0645fca031e0dc56fc`

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
| phase1_var_loop | 6.657ms | 8.519ms | 0.78x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 25.890ms | 21.096ms | 1.23x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 9.162ms | 5.121ms | 1.79x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 15.490ms | 21.512ms | 0.72x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 213.554ms | 91.221ms | 2.34x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 23.032ms | 417.597us | 55.15x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
