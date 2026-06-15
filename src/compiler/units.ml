(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   Units of measure (the Primitive type).

   Maps a dimensioned literal's unit suffix to its SI dimension and normalises
   the magnitude to a canonical base unit, so the checker can compare two
   quantities of the same dimension by raw magnitude. Temperature conversions
   are affine (F/K), normalised to degrees Celsius.

   The full suffix table is the normative one in docs/spec/0001-lexical-grammar.adoc.
*)

open Ast

let normalize (mag : float) (unit : string) : quantity =
  match unit with
  | "" -> { q_mag = mag; q_dim = Dimensionless }
  (* temperature, base = Celsius (affine for F/K) *)
  | "C" | "degC" -> { q_mag = mag; q_dim = Temperature }
  | "K" -> { q_mag = mag -. 273.15; q_dim = Temperature }
  | "F" | "degF" -> { q_mag = (mag -. 32.0) *. 5.0 /. 9.0; q_dim = Temperature }
  (* time, base = seconds *)
  | "s" | "sec" -> { q_mag = mag; q_dim = Time }
  | "min" -> { q_mag = mag *. 60.0; q_dim = Time }
  | "h" | "hr" -> { q_mag = mag *. 3600.0; q_dim = Time }
  (* mass, base = grams *)
  | "g" -> { q_mag = mag; q_dim = Mass }
  | "kg" -> { q_mag = mag *. 1000.0; q_dim = Mass }
  | "mg" -> { q_mag = mag /. 1000.0; q_dim = Mass }
  (* torque, base = Nm *)
  | "Nm" -> { q_mag = mag; q_dim = Torque }
  (* angular rate, base = rpm *)
  | "rpm" -> { q_mag = mag; q_dim = AngularRate }
  (* length, base = cm *)
  | "cm" -> { q_mag = mag; q_dim = Length }
  | "mm" -> { q_mag = mag /. 10.0; q_dim = Length }
  | "m" -> { q_mag = mag *. 100.0; q_dim = Length }
  (* power, base = W *)
  | "W" -> { q_mag = mag; q_dim = Power }
  | "kW" -> { q_mag = mag *. 1000.0; q_dim = Power }
  (* viscosity, base = Pa.s ; accept several spellings incl. the UTF-8 middot *)
  | "Pa\xc2\xb7s" | "Pa.s" | "Pas" -> { q_mag = mag; q_dim = Viscosity }
  (* thermal slope, base = C/s *)
  | "C/s" -> { q_mag = mag; q_dim = TempRate }
  | "C/min" -> { q_mag = mag /. 60.0; q_dim = TempRate }
  | u -> { q_mag = mag; q_dim = DUnknown u }
