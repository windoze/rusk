# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-18T06:04:04Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.3 (main, Feb  3 2026, 15:32:20) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `8fda8d0cb59bf77fc1ef96f8060b7c539d8df1e8`

## Parameters

- Bytecode opt level: `o2`
- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 7.579ms | 8.399ms | 0.90x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.602ms | 20.428ms | 1.35x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 9.746ms | 5.146ms | 1.89x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 18.009ms | 21.539ms | 0.84x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 231.274ms | 91.812ms | 2.52x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.268ms | 417.111us | 12.63x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
