// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: CURDLE
// No explicit target: — the danger is hidden in the `until` guard (95C >= curdle 82C).
// The checker's soundness backstop must still catch it.
recipe CurdleViaUntil {
  use Milk as m;
  sync(Hob) {
    Hob.Heat(m, slope: GENTLE) until ~ temp >= 95C max_duration 5min on_fail ABORT;
  }
}
