// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// v2 port of the canonical poached egg: Linear + Tropical + Echo + Ceremonial.
echo @water_shimmer on Hob.visual;
echo @white_set on Hob.visual;
ceremony Breakfast { pace: BRISK; alert: 4; priority: TIMING; }

recipe PoachedEgg under Breakfast {
  use Water as w;
  use Egg as e;
  use Vinegar as v;
  sync(Hob) proving @water_shimmer {
    Hob.Heat(w, target: 82C, slope: GENTLE)
      until ~ temp >= 80C
      max_duration 8min
      on_fail ABORT;
  }
  sync(Hob) proving @white_set {
    Hob.Poach(e, v, depth: 4cm)
      until @white_set == true
      max_duration 4min
      on_fail RECOVER;
  }
}
