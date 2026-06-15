// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Tool safety + tropical: a silicone spatula (melt 260C) is fine in a 200C sear.
recipe SearSteak {
  use Steak as s;
  sync(Hob) {
    Hob.Sear(s, Spatula, target: 200C, slope: MODERATE)
      until ~ temp >= 190C
      max_duration 6min
      on_fail ABORT;
  }
}
