# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-14T18:32:46Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.2 (main, Dec  5 2025, 16:49:16) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `bfc23e2d53cf0982ae544c08b9bc6c1a326573ef`

## Parameters

- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 44.714ms | 8.486ms | 5.27x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 95.977ms | 21.000ms | 4.57x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 27.064ms | 5.091ms | 5.32x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 98.104ms | 21.499ms | 4.56x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 725.617ms | 91.932ms | 7.89x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 24.939ms | 417.555us | 59.73x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
