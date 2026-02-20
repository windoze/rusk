#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="build/classes"
mkdir -p "$OUT_DIR"

MAIN_SRC=$(find src/main/java -name '*.java' | sort)

javac --release 21 -d "$OUT_DIR" $MAIN_SRC

java -ea -cp "$OUT_DIR" dev.rusk.cli.Main "$@"

