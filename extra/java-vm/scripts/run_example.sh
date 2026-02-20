#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if [[ $# -lt 1 ]]; then
  echo "usage: run_example.sh <MainClass> [args...]" >&2
  exit 2
fi

MAIN_CLASS="$1"
shift || true

OUT_DIR="build/examples-classes"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

MAIN_SRC=$(find src/main/java -name '*.java' | sort)
EXAMPLE_SRC=$(find examples -name '*.java' | sort)

javac --release 21 -d "$OUT_DIR" $MAIN_SRC $EXAMPLE_SRC

java -ea -cp "$OUT_DIR" "$MAIN_CLASS" "$@"

