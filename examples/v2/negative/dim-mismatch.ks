// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: DIM_MISMATCH
recipe DimMismatch {
  use Water as w;
  sync(Hob) {
    Hob.Heat(w, target: 5min) until ~ temp >= 80C max_duration 8min on_fail ABORT;
  }
}
