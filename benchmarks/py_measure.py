#!/usr/bin/env python3
from __future__ import annotations

import argparse
import importlib.util
import json
import statistics
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from types import ModuleType
from typing import Any


@dataclass(frozen=True)
class MeasureResult:
    input: str
    warmup: int
    iters: int
    import_ns: int
    run_total_ns: int
    run_avg_ns: int
    last_result_repr: str | None

    def to_json(self) -> dict[str, Any]:
        return {
            "input": self.input,
            "warmup": self.warmup,
            "iters": self.iters,
            "import_ns": self.import_ns,
            "run": {"total_ns": self.run_total_ns, "avg_ns": self.run_avg_ns},
            "last_result_repr": self.last_result_repr,
        }


def _load_module_from_path(path: Path) -> tuple[ModuleType, int]:
    start = time.perf_counter_ns()
    module_name = f"_rusk_pybench_{path.stem}_{start}"
    spec = importlib.util.spec_from_file_location(module_name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"failed to load python module from {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    end = time.perf_counter_ns()
    return module, end - start


def _median_ns(samples: list[int]) -> int:
    if not samples:
        return 0
    return int(statistics.median(samples))


def measure_file(path: Path, *, warmup: int, iters: int, validate: bool) -> MeasureResult:
    module, import_ns = _load_module_from_path(path)
    try:
        main = getattr(module, "main")
    except AttributeError as e:
        raise RuntimeError(f"{path} does not define main()") from e

    expected: int | None = getattr(module, "EXPECTED", None)

    last = None
    for _ in range(warmup):
        last = main()

    start = time.perf_counter_ns()
    for _ in range(iters):
        last = main()
    end = time.perf_counter_ns()

    if validate and expected is not None and last != expected:
        raise RuntimeError(f"{path}: unexpected result: got {last!r}, expected {expected!r}")

    total = end - start
    avg = total // iters
    return MeasureResult(
        input=str(path),
        warmup=warmup,
        iters=iters,
        import_ns=import_ns,
        run_total_ns=total,
        run_avg_ns=avg,
        last_result_repr=repr(last),
    )


def _fmt_ns(ns: int) -> str:
    if ns >= 1_000_000_000:
        return f"{ns/1_000_000_000:.3f}s"
    if ns >= 1_000_000:
        return f"{ns/1_000_000:.3f}ms"
    if ns >= 1_000:
        return f"{ns/1_000:.3f}us"
    return f"{ns}ns"


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(description="Measure a Python microbenchmark (expects main()).")
    p.add_argument("--json", action="store_true", help="Emit machine-readable JSON.")
    p.add_argument("--warmup", type=int, default=0, help="Warmup runs (not timed).")
    p.add_argument("--iters", type=int, default=1, help="Timed runs.")
    p.add_argument(
        "--validate",
        action="store_true",
        help="If module defines EXPECTED, validate last result matches.",
    )
    p.add_argument("path", type=Path, help="Path to a benchmark .py file")
    args = p.parse_args(argv)

    if args.iters < 1:
        raise SystemExit("--iters must be >= 1")
    if args.warmup < 0:
        raise SystemExit("--warmup must be >= 0")

    result = measure_file(args.path, warmup=args.warmup, iters=args.iters, validate=args.validate)
    if args.json:
        print(json.dumps(result.to_json(), separators=(",", ":"), sort_keys=True))
        return 0

    print(f"Input: {result.input}")
    print(f"Import:  {_fmt_ns(result.import_ns)}")
    print("Run:")
    print(f"  iters:  {result.iters} (warmup: {result.warmup})")
    print(f"  total:  {_fmt_ns(result.run_total_ns)}")
    print(f"  avg:    {_fmt_ns(result.run_avg_ns)}")
    if result.last_result_repr is not None:
        print(f"Result: {result.last_result_repr}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))

