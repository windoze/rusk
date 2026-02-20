# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-19T21:58:11Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.3 (main, Feb  3 2026, 15:32:20) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `9af8e00e8a9814522aa312928124d678d420c04c`
- Git tree: `dirty`

## Parameters

- Bytecode opt level: `o2`
- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 7.500ms | 8.629ms | 0.87x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.058ms | 20.346ms | 1.33x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 9.531ms | 4.999ms | 1.91x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 25.575ms | 21.696ms | 1.18x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 226.151ms | 90.757ms | 2.49x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.097ms | 426.125us | 11.96x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |
| phase7_map_dict | 161.244ms | 4.621ms | 34.89x | (warmup=0, iters=1) core::map::Map (generic Hash/Eq dispatch) vs Python dict. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
