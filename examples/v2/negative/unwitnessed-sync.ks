// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: UNWITNESSED_SYNC
echo @white_set on Hob.visual;
recipe Unwitnessed {
  use Egg as e;
  use Vinegar as v;
  sync(Hob) {
    Hob.Poach(e, v) until @white_set == true max_duration 4min on_fail RECOVER;
  }
}
