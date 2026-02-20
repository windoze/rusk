#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

OUT_DIR="build/classes"
rm -rf "$OUT_DIR"
mkdir -p "$OUT_DIR"

MAIN_SRC=$(find src/main/java -name '*.java' | sort)
TEST_SRC=$(find src/test/java -name '*.java' | sort)

# JDK 21+（用于 sealed/records 等语法），同时不依赖 Maven/Gradle。
javac --release 21 -d "$OUT_DIR" $MAIN_SRC $TEST_SRC

java -ea -cp "$OUT_DIR" dev.rusk.vm.TestRunner

