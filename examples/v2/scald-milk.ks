// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Heats milk to just below its curdle envelope. Passes, with a soft 'scald'
// warning at 70C (quality threshold, not a ruin). Change 70C -> 95C to see CURDLE.
recipe ScaldMilk {
  use Milk as m;
  sync(Hob) {
    Hob.Heat(m, target: 70C, slope: GENTLE)
      until ~ temp >= 68C
      max_duration 5min
      on_fail ABORT;
  }
}
