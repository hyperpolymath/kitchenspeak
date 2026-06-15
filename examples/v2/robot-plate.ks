// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Consumer robotics: an arm plates a portion within its torque + reach envelope.
echo @placed on Arm.visual;
recipe PlateGarnish {
  use Steak as portion;
  sync(Arm) proving @placed {
    Arm.Plate(portion, torque: 3Nm, reach: 40cm)
      until @placed == true
      max_duration 30s
      on_fail ABORT;
  }
}
