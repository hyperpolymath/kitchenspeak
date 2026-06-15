// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk>
// Linear + Dyadic + Choreographic + Echo + robotics: emulsify, then a robot plates it.
material Mayo { phase: EMULSION }
echo @plated on Arm.visual;

recipe Mayonnaise {
  use Egg as yolk;
  use Oil as oil;
  (yolk, oil) <~> Blender -> Mayo;
  sync(Blender) {
    Blender.Whisk(Mayo, speed: 800rpm)
      until ~ viscosity >= 1Pas
      max_duration 3min
      on_fail RECOVER;
  }
  sync(Arm) proving @plated {
    Arm.Plate(Mayo, torque: 2Nm, reach: 30cm)
      until @plated == true
      max_duration 1min
      on_fail ABORT;
  }
}
