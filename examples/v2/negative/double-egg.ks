// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: DOUBLE_USE
recipe DoubleEgg {
  use Egg as e;
  use Vinegar as v;
  sync(Hob) proving @ws {
    Hob.Poach(e, v, depth: 4cm) until @ws == true max_duration 4min on_fail RECOVER;
    Hob.Poach(e, v, depth: 4cm) until @ws == true max_duration 4min on_fail RECOVER;
  }
}
echo @ws on Hob.visual;
