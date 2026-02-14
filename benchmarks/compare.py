#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import os
import platform
import statistics
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from py_measure import measure_file


@dataclass(frozen=True)
class BenchCase:
    name: str
    rusk_path: Path
    python_path: Path
    expected: int | None
    notes: str
    warmup: int | None = None
    iters: int | None = None


def _exe_suffix() -> str:
    return ".exe" if os.name == "nt" else ""


def _repo_root() -> Path:
    return Path(__file__).resolve().parent.parent


def _bench_dir() -> Path:
    return Path(__file__).resolve().parent


def _run(cmd: list[str], *, cwd: Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def _check(cmd: list[str], *, cwd: Path) -> None:
    proc = _run(cmd, cwd=cwd)
    if proc.returncode != 0:
        raise RuntimeError(
            "command failed:\n"
            + "  cmd: " + " ".join(cmd) + "\n"
            + "  cwd: " + str(cwd) + "\n"
            + "  stdout:\n" + proc.stdout + "\n"
            + "  stderr:\n" + proc.stderr + "\n"
        )


def _check_json(cmd: list[str], *, cwd: Path) -> dict[str, Any]:
    proc = _run(cmd, cwd=cwd)
    if proc.returncode != 0:
        raise RuntimeError(
            "command failed:\n"
            + "  cmd: " + " ".join(cmd) + "\n"
            + "  cwd: " + str(cwd) + "\n"
            + "  stdout:\n" + proc.stdout + "\n"
            + "  stderr:\n" + proc.stderr + "\n"
        )
    try:
        return json.loads(proc.stdout)
    except json.JSONDecodeError as e:
        raise RuntimeError(f"failed to parse JSON from: {' '.join(cmd)}\nstdout:\n{proc.stdout}") from e


def _median_int(values: list[int]) -> int:
    if not values:
        return 0
    return int(statistics.median(values))


def _mean_int(values: list[int]) -> int:
    if not values:
        return 0
    return int(statistics.mean(values))


def _stdev_int(values: list[int]) -> int:
    if len(values) < 2:
        return 0
    return int(statistics.stdev(values))


def _fmt_ns(ns: int) -> str:
    if ns >= 1_000_000_000:
        return f"{ns/1_000_000_000:.3f}s"
    if ns >= 1_000_000:
        return f"{ns/1_000_000:.3f}ms"
    if ns >= 1_000:
        return f"{ns/1_000:.3f}us"
    return f"{ns}ns"


def _try_cmd_version(cmd: list[str]) -> str | None:
    try:
        out = subprocess.check_output(cmd, text=True).strip()
    except Exception:
        return None
    return out


def _gather_env(repo_root: Path) -> dict[str, Any]:
    env: dict[str, Any] = {
        "timestamp_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "platform": platform.platform(),
        "machine": platform.machine(),
        "python": {
            "executable": sys.executable,
            "version": sys.version.replace("\n", " "),
        },
        "cargo": _try_cmd_version(["cargo", "--version"]),
        "rustc": _try_cmd_version(["rustc", "--version"]),
    }
    try:
        env["git_commit"] = (
            subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=repo_root, text=True).strip()
        )
    except Exception:
        env["git_commit"] = None
    return env


def _write_text(path: Path, content: str) -> None:
    path.write_text(content, encoding="utf-8")


def _default_cases() -> list[BenchCase]:
    bdir = _bench_dir()
    return [
        BenchCase(
            name="phase1_var_loop",
            rusk_path=bdir / "phase1_var_loop.rusk",
            python_path=bdir / "phase1_var_loop.py",
            expected=20000300000,
            notes="Local var load/store + while loop + int arithmetic.",
        ),
        BenchCase(
            name="phase2_object_access",
            rusk_path=bdir / "phase2_object_access.rusk",
            python_path=bdir / "phase2_object_access.py",
            expected=45001350000,
            notes="Tuple/struct field get/set hot paths.",
        ),
        BenchCase(
            name="phase3_closure_capture",
            rusk_path=bdir / "phase3_closure_capture.rusk",
            python_path=bdir / "phase3_closure_capture.py",
            expected=4999950063,
            notes="Closure/lambda in a tight loop (Rusk: capture analysis).",
        ),
        BenchCase(
            name="phase4_call_dispatch",
            rusk_path=bdir / "phase4_call_dispatch.rusk",
            python_path=bdir / "phase4_call_dispatch.py",
            expected=500000,
            notes="Call-heavy loop (inc/dispatch).",
        ),
        BenchCase(
            name="phase5_gc_epoch",
            rusk_path=bdir / "phase5_gc_epoch.rusk",
            python_path=bdir / "phase5_gc_epoch.py",
            expected=200001,
            notes="Allocation churn / GC stress (not semantically identical across runtimes).",
            warmup=0,
            iters=1,
        ),
        BenchCase(
            name="phase6_effects_generator",
            rusk_path=bdir / "phase6_effects_generator.rusk",
            python_path=bdir / "phase6_effects_generator.py",
            expected=49995000,
            notes="Generator-like yield/resume (Rusk effects) vs Python generator.",
        ),
    ]


def _build_rusk_binaries(repo_root: Path) -> None:
    _check(["cargo", "build", "--release", "--bin", "rusk", "--bin", "rusk-measure"], cwd=repo_root)


def _rusk_bin(repo_root: Path, name: str) -> Path:
    return repo_root / "target" / "release" / f"{name}{_exe_suffix()}"


def _validate_rusk_output(repo_root: Path, rusk_bin: Path, case: BenchCase) -> None:
    if case.expected is None:
        return
    proc = _run([str(rusk_bin), str(case.rusk_path)], cwd=repo_root)
    if proc.returncode != 0:
        raise RuntimeError(f"rusk failed for {case.rusk_path}:\n{proc.stderr}")
    out = proc.stdout.strip()
    if out == "":
        got: int | None = 0
    else:
        try:
            got = int(out)
        except ValueError as e:
            raise RuntimeError(f"unexpected rusk output for {case.rusk_path}: {out!r}") from e
    if got != case.expected:
        raise RuntimeError(f"{case.name}: rusk returned {got}, expected {case.expected}")


def run_suite(
    *,
    warmup: int,
    iters: int,
    repeats: int,
    validate: bool,
) -> dict[str, Any]:
    repo_root = _repo_root()
    cases = _default_cases()
    env = _gather_env(repo_root)

    if validate:
        _build_rusk_binaries(repo_root)
        rusk_bin = _rusk_bin(repo_root, "rusk")
        for case in cases:
            _validate_rusk_output(repo_root, rusk_bin, case)

    _build_rusk_binaries(repo_root)
    rusk_measure = _rusk_bin(repo_root, "rusk-measure")

    benches: list[dict[str, Any]] = []
    for case in cases:
        case_warmup = warmup if case.warmup is None else case.warmup
        case_iters = iters if case.iters is None else case.iters

        rusk_runs: list[dict[str, Any]] = []
        py_runs: list[dict[str, Any]] = []

        print(f"[{case.name}] warmup={case_warmup} iters={case_iters} repeats={repeats}", file=sys.stderr)
        for rep in range(repeats):
            print(f"[{case.name}] repeat {rep + 1}/{repeats}: rusk", file=sys.stderr)
            rusk_json = _check_json(
                [
                    str(rusk_measure),
                    "--json",
                    "--warmup",
                    str(case_warmup),
                    "--iters",
                    str(case_iters),
                    str(case.rusk_path),
                ],
                cwd=repo_root,
            )
            rusk_runs.append(rusk_json)

            print(f"[{case.name}] repeat {rep + 1}/{repeats}: python", file=sys.stderr)
            py_result = measure_file(
                case.python_path, warmup=case_warmup, iters=case_iters, validate=validate
            )
            py_runs.append(py_result.to_json())

        rusk_avg_ns_samples = [int(r["run"]["avg_ns"]) for r in rusk_runs]
        rusk_compile_ns_samples = [int(r["compile"]["total_ns"]) for r in rusk_runs]
        py_avg_ns_samples = [int(r["run"]["avg_ns"]) for r in py_runs]

        benches.append(
            {
                "name": case.name,
                "notes": case.notes,
                "expected": case.expected,
                "params": {"warmup": case_warmup, "iters": case_iters},
                "rusk": {
                    "path": str(case.rusk_path),
                    "runs": rusk_runs,
                    "stats": {
                        "run_avg_ns": {
                            "median": _median_int(rusk_avg_ns_samples),
                            "mean": _mean_int(rusk_avg_ns_samples),
                            "stdev": _stdev_int(rusk_avg_ns_samples),
                            "min": min(rusk_avg_ns_samples),
                            "max": max(rusk_avg_ns_samples),
                        },
                        "compile_total_ns": {
                            "median": _median_int(rusk_compile_ns_samples),
                            "mean": _mean_int(rusk_compile_ns_samples),
                            "stdev": _stdev_int(rusk_compile_ns_samples),
                            "min": min(rusk_compile_ns_samples),
                            "max": max(rusk_compile_ns_samples),
                        },
                    },
                },
                "python": {
                    "path": str(case.python_path),
                    "runs": py_runs,
                    "stats": {
                        "run_avg_ns": {
                            "median": _median_int(py_avg_ns_samples),
                            "mean": _mean_int(py_avg_ns_samples),
                            "stdev": _stdev_int(py_avg_ns_samples),
                            "min": min(py_avg_ns_samples),
                            "max": max(py_avg_ns_samples),
                        }
                    },
                },
            }
        )

    return {"env": env, "params": {"warmup": warmup, "iters": iters, "repeats": repeats}, "benchmarks": benches}


def _render_markdown(report: dict[str, Any]) -> str:
    env = report["env"]
    params = report["params"]
    lines: list[str] = []
    lines.append("# Rusk vs Python micro-benchmarks")
    lines.append("")
    lines.append(f"- Timestamp (UTC): `{env['timestamp_utc']}`")
    lines.append(f"- Platform: `{env['platform']}`")
    lines.append(f"- Machine: `{env['machine']}`")
    lines.append(f"- Python: `{env['python']['version']}`")
    if env.get("rustc"):
        lines.append(f"- Rust: `{env['rustc']}`")
    if env.get("cargo"):
        lines.append(f"- Cargo: `{env['cargo']}`")
    if env.get("git_commit"):
        lines.append(f"- Git commit: `{env['git_commit']}`")
    lines.append("")
    lines.append("## Parameters")
    lines.append("")
    lines.append(f"- Warmup: `{params['warmup']}`")
    lines.append(f"- Iters: `{params['iters']}`")
    lines.append(f"- Repeats: `{params['repeats']}` (outer repeats for median/stdev)")
    lines.append("")
    lines.append("## Summary (median run time)")
    lines.append("")
    lines.append("- `rusk/py` > 1.0x means Rusk is slower; < 1.0x means Rusk is faster.")
    lines.append("")
    lines.append("| benchmark | rusk | python | rusk/py | notes |")
    lines.append("|---|---:|---:|---:|---|")
    for b in report["benchmarks"]:
        r_ns = int(b["rusk"]["stats"]["run_avg_ns"]["median"])
        p_ns = int(b["python"]["stats"]["run_avg_ns"]["median"])
        ratio = (r_ns / p_ns) if p_ns else float("inf")
        params_s = f"(warmup={b['params']['warmup']}, iters={b['params']['iters']}) "
        lines.append(
            "| "
            + b["name"]
            + " | "
            + _fmt_ns(r_ns)
            + " | "
            + _fmt_ns(p_ns)
            + " | "
            + f"{ratio:.2f}x"
            + " | "
            + params_s
            + b["notes"]
            + " |"
        )
    lines.append("")
    lines.append("## Raw data")
    lines.append("")
    lines.append("- See `benchmarks/results.json` for the full per-run JSON output.")
    lines.append("")
    return "\n".join(lines)


def main(argv: list[str]) -> int:
    p = argparse.ArgumentParser(description="Run Rusk vs Python micro-benchmarks.")
    p.add_argument("--warmup", type=int, default=1)
    p.add_argument("--iters", type=int, default=3)
    p.add_argument("--repeats", type=int, default=5, help="Outer repeats to reduce noise.")
    p.add_argument(
        "--validate",
        action="store_true",
        help="Validate benchmark return values before measuring.",
    )
    p.add_argument(
        "--json-out",
        type=Path,
        default=_bench_dir() / "results.json",
        help="Write JSON report to this file.",
    )
    p.add_argument(
        "--md-out",
        type=Path,
        default=_bench_dir() / "results.md",
        help="Write Markdown summary to this file.",
    )
    args = p.parse_args(argv)

    report = run_suite(
        warmup=args.warmup,
        iters=args.iters,
        repeats=args.repeats,
        validate=args.validate,
    )
    args.json_out.parent.mkdir(parents=True, exist_ok=True)
    args.md_out.parent.mkdir(parents=True, exist_ok=True)
    args.json_out.write_text(json.dumps(report, indent=2, sort_keys=True), encoding="utf-8")
    args.md_out.write_text(_render_markdown(report), encoding="utf-8")
    print(f"Wrote: {args.md_out}")
    print(f"Wrote: {args.json_out}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
