// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: MISSING_ONFAIL
recipe MissingOnFail {
  use Milk as m;
  sync(Hob) {
    Hob.Heat(m, target: 60C) until ~ temp >= 58C max_duration 5min;
  }
}
