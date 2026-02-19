# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-19T19:15:13Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.3 (main, Feb  3 2026, 15:32:20) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `e99f3697d9f381ee79b717a96119a66102728c77`
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
| phase1_var_loop | 7.524ms | 8.444ms | 0.89x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.125ms | 20.407ms | 1.33x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 9.795ms | 5.019ms | 1.95x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 25.514ms | 21.487ms | 1.19x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 231.973ms | 91.298ms | 2.54x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.141ms | 442.861us | 11.61x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |
| phase7_map_dict | 179.332ms | 4.703ms | 38.13x | (warmup=0, iters=1) core::map::Map (generic Hash/Eq dispatch) vs Python dict. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
