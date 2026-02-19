# Rusk vs Python micro-benchmarks

- Timestamp (UTC): `2026-02-19T17:15:54Z`
- Platform: `macOS-26.3-arm64-arm-64bit-Mach-O`
- Machine: `arm64`
- Python: `3.14.3 (main, Feb  3 2026, 15:32:20) [Clang 17.0.0 (clang-1700.6.3.2)]`
- Rust: `rustc 1.93.0 (254b59607 2026-01-19)`
- Cargo: `cargo 1.93.0 (083ac5135 2025-12-15)`
- Git commit: `077c0f9bca06e4568afabb1d540f8ebd3e78d4af`
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
| phase1_var_loop | 8.099ms | 8.696ms | 0.93x | (warmup=1, iters=3) Local var load/store + while loop + int arithmetic. |
| phase2_object_access | 27.555ms | 20.698ms | 1.33x | (warmup=1, iters=3) Tuple/struct field get/set hot paths. |
| phase3_closure_capture | 9.201ms | 5.284ms | 1.74x | (warmup=1, iters=3) Closure/lambda in a tight loop (Rusk: capture analysis). |
| phase4_call_dispatch | 26.479ms | 21.487ms | 1.23x | (warmup=1, iters=3) Call-heavy loop (inc/dispatch). |
| phase5_gc_epoch | 231.968ms | 90.895ms | 2.55x | (warmup=0, iters=1) Allocation churn / GC stress (not semantically identical across runtimes). |
| phase6_effects_generator | 5.032ms | 417.625us | 12.05x | (warmup=1, iters=3) Generator-like yield/resume (Rusk effects) vs Python generator. |
| phase7_map_dict | 199.085ms | 4.716ms | 42.22x | (warmup=0, iters=1) core::map::Map (generic Hash/Eq dispatch) vs Python dict. |

## Raw data

- See `benchmarks/results.json` for the full per-run JSON output.
