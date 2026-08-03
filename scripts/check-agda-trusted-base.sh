#!/bin/sh
# SPDX-License-Identifier: MPL-2.0
#
# Trusted-base budget for the Agda proofs.
#
# The recipe proofs deliberately postulate the physical world (sensor
# streams, controller contracts) — those axioms are audited and listed
# in proofs/agda/TRUSTED-BASE.txt with per-file annotations explaining
# each one. This gate fails when ANY postulate appears that is not on
# that list, so the trusted base cannot grow silently.
# (Same doctrine as the Lean `axiom` gate: provers accept axioms
# without complaint, so the budget must be enforced outside the prover.)
set -eu
cd "$(dirname "$0")/.."

expected="proofs/agda/TRUSTED-BASE.txt"
actual=$(mktemp)
trap 'rm -f "$actual"' EXIT

awk '
  /^postulate/ { inpost = 1; next }
  inpost && /^[[:space:]]+[A-Za-z0-9\x27-]+[[:space:]]*:/ {
    line = $0
    sub(/^[[:space:]]+/, "", line)
    sub(/[[:space:]]*:.*/, "", line)
    print FILENAME ":" line
    next
  }
  inpost && !/^[[:space:]]/ { inpost = 0 }
' proofs/agda/*.agda | sort > "$actual"

if ! diff -u "$expected" "$actual"; then
  echo "FAIL: postulate inventory differs from the audited trusted base." >&2
  echo "New axioms need an audit note in the proof file AND an entry in $expected." >&2
  exit 1
fi
echo "OK: trusted base unchanged ($(wc -l < "$expected") audited postulates)."
