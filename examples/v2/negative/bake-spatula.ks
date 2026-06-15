// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: MELT_TOOL
recipe BakeSpatula {
  use Steak as s;
  sync(Oven) {
    Oven.Bake(s, Spatula, target: 300C)
      until ~ temp >= 290C max_duration 20min on_fail ABORT;
  }
}
