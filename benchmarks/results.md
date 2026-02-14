# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-14T11:24:48Z`
- Platform: `macOS-26.2-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.2 (main, Dec  5 2025, 16:49:16) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `9597a2b084ae6d097bb2c7437e9db30ac008a4d7`

## Parameters

- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 44.451ms | 8.488ms | 5.24x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 96.234ms | 21.087ms | 4.56x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 27.371ms | 5.292ms | 5.17x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 99.420ms | 21.890ms | 4.54x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 736.712ms | 90.962ms | 8.10x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 24.796ms | 421.444us | 58.84x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
