#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
#
# run-corpus.sh — acceptance test for the KitchenSpeak v2.0 reference checker.
#
# Builds the OCaml compiler, then asserts:
#   * every recipe in examples/v2/*.ks type-checks (exit 0);
#   * every recipe in examples/v2/negative/*.ks is REJECTED (exit 1) AND the
#     error code named in its `// EXPECT: CODE` header appears in the output.
#
# Exit 0 iff the whole corpus behaves as specified.

set -uo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
COMPILER="$ROOT/src/compiler"
KSC="$COMPILER/_build/default/main.exe"

echo "== building ksc =="
( cd "$COMPILER" && dune build 2>&1 ) || { echo "BUILD FAILED"; exit 2; }

pass=0; fail=0

echo
echo "== positive corpus (must type-check) =="
for f in "$ROOT"/examples/v2/*.ks; do
    [ -e "$f" ] || continue
    out="$("$KSC" check "$f" 2>&1)"; code=$?
    if [ "$code" -eq 0 ]; then
        echo "  PASS  $(basename "$f")"; pass=$((pass+1))
    else
        echo "  FAIL  $(basename "$f")  (expected accept, got exit $code)"; echo "$out" | sed 's/^/        /'; fail=$((fail+1))
    fi
done

echo
echo "== negative corpus (must be rejected with the expected code) =="
for f in "$ROOT"/examples/v2/negative/*.ks; do
    [ -e "$f" ] || continue
    want="$(grep -oE 'EXPECT:[[:space:]]*[A-Z_]+' "$f" | head -1 | sed -E 's/EXPECT:[[:space:]]*//')"
    out="$("$KSC" check "$f" 2>&1)"; code=$?
    if [ "$code" -ne 0 ] && echo "$out" | grep -q "\[$want\]"; then
        echo "  PASS  $(basename "$f")  -> $want"; pass=$((pass+1))
    else
        echo "  FAIL  $(basename "$f")  (wanted reject with [$want], got exit $code)"; echo "$out" | sed 's/^/        /'; fail=$((fail+1))
    fi
done

echo
echo "== summary: $pass passed, $fail failed =="
[ "$fail" -eq 0 ]
