// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: CURDLE
recipe CurdleMilk {
  use Milk as m;
  sync(Hob) {
    Hob.Heat(m, target: 95C, slope: GENTLE)
      until ~ temp >= 90C max_duration 5min on_fail ABORT;
  }
}
