# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-14T06:50:07Z`
- Platform: `macOS-26.2-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.2 (main, Dec  5 2025, 16:49:16) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `38770dbdc9ad7fdf638752b70090b9e11924750c`

## Parameters

- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 164.822ms | 8.338ms | 19.77x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 292.841ms | 21.106ms | 13.87x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 79.513ms | 5.022ms | 15.83x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 280.173ms | 21.518ms | 13.02x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 1.987s | 89.952ms | 22.09x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 30.368ms | 420.444us | 72.23x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
