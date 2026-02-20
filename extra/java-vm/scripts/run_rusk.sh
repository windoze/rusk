#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REPO_ROOT="$(cd "$ROOT/../.." && pwd)"

if [[ $# -lt 1 ]]; then
  echo "usage: run_rusk.sh <file.rusk> [args...]" >&2
  exit 2
fi

INPUT="$1"
shift || true

if [[ "$INPUT" = /* ]]; then
  INPUT_ABS="$INPUT"
else
  INPUT_ABS="$PWD/$INPUT"
fi

cd "$REPO_ROOT"
cargo run -q --bin ruskc -- "$INPUT_ABS"

RBC="${INPUT_ABS%.rusk}.rbc"
if [[ ! -f "$RBC" ]]; then
  echo "error: expected output rbc not found: $RBC" >&2
  exit 1
fi

cd "$ROOT"
./scripts/run.sh "$RBC" "$@"
