# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-19T14:25:15Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.3 (main, Feb  3 2026, 15:32:20) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `76744c29d87fda8e9119efaa12e5d9d243c7f3fa`

## Parameters

- Bytecode opt level: `o2`
- Warmup: `1`
- Iters: `3`
- Repeats: `5` (outer repeats for median/stdev)

## Summary (median run time)

- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.

| benchmark | rusk | python | rusk/py | notes |
|---|---:|---:|---:|---|
| phase1_var_loop | 7.448ms | 8.373ms | 0.89x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.404ms | 20.486ms | 1.34x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 10.271ms | 5.094ms | 2.02x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 25.745ms | 21.409ms | 1.20x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 230.255ms | 91.791ms | 2.51x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.017ms | 411.597us | 12.19x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |
| phase7_map_dict | 218.794ms | 4.702ms | 46.54x | (warmup=0, iters=1) core::map::Map (generic Hash/Eq dispatch) vs Python dict. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
