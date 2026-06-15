// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// EXPECT: OVER_TORQUE
recipe OverTorque {
  use Steak as p;
  sync(Arm) {
    Arm.Move(p, torque: 12Nm, reach: 30cm)
      until ~ position >= 1cm max_duration 10s on_fail ABORT;
  }
}
