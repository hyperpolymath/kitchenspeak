(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   The KitchenSpeak standard ontology: built-in materials, tools, and appliances
   with REAL physical safety envelopes. Recipes may `use` these without
   redeclaring them; a file-level declaration of the same name overrides the
   built-in. The narrative version (with sources and soft/hard rationale) is
   docs/design/0004-stdlib-ontology.adoc.

   Convention used by the checker (see check.ml): only UPPER thermal limits are
   hazards in the heating direction. `soft` limits warn (quality); `hard` limits
   reject (ruin / safety). Numbers are conservative engineering defaults,
   overridable per recipe.
*)

open Ast

let degC m = { q_mag = m; q_dim = Temperature }
let nm m   = { q_mag = m; q_dim = Torque }
let rpm m  = { q_mag = m; q_dim = AngularRate }
let cm m   = { q_mag = m; q_dim = Length }
let cps m  = { q_mag = m; q_dim = TempRate }

let hard n v = { l_kind = Hard; l_name = n; l_qty = v }
let soft n v = { l_kind = Soft; l_name = n; l_qty = v }

let mat name phase th =
  { m_name = name; m_phase = phase;
    m_env = { e_thermal = th; e_mech = []; e_chem = [] };
    m_is_tool = false; m_tool_material = None }

let tool name tm th =
  { m_name = name; m_phase = "SOLID";
    m_env = { e_thermal = th; e_mech = []; e_chem = [] };
    m_is_tool = true; m_tool_material = Some tm }

let cap ?(slope = None) ?(reach = None) ?(consumes = false) ?(mods = []) verb qd lo hi =
  { c_verb = verb; c_quantity = qd; c_lo = lo; c_hi = hi;
    c_slope = slope; c_reach = reach; c_consumes = consumes; c_modalities = mods }

(* ── Materials ────────────────────────────────────────────────────────────── *)
let materials : material list = [
  (* Milk: scalds at ~70C (quality), curdles/denatures hard at ~82C. *)
  mat "Milk" "LIQUID" [ soft "scald" (degC 70.); hard "curdle" (degC 82.) ];
  (* Egg: white coagulates ~63C (desired, so soft), scorches if seared dry. *)
  mat "Egg" "RAW" [ soft "coagulate" (degC 63.); hard "scorch" (degC 200.) ];
  mat "Water" "LIQUID" [ soft "boil" (degC 100.) ];
  mat "Vinegar" "LIQUID" [];
  (* Chocolate: tempering band tops out ~32C (quality), scorches/seizes hot. *)
  mat "Chocolate" "SOLID" [ soft "temper" (degC 32.); hard "scorch" (degC 55.) ];
  mat "Butter" "SOLID" [ soft "brown" (degC 150.); hard "burn" (degC 175.) ];
  mat "Oil" "LIQUID" [ soft "smoke" (degC 190.); hard "scorch" (degC 230.) ];
  mat "Cream" "LIQUID" [ soft "split" (degC 80.); hard "scorch" (degC 95.) ];
  mat "Steak" "RAW" [ soft "maillard" (degC 140.); hard "scorch" (degC 260.) ];
  mat "Flour" "RAW_POWDER" [];
  mat "Sugar" "SOLID" [ soft "caramel" (degC 160.); hard "burn" (degC 190.) ];
  mat "Dough" "KNEADABLE" [];
]

(* ── Tools (inert implements with material limits) ────────────────────────── *)
let tools : material list = [
  tool "Spatula" "SILICONE" [ hard "melt" (degC 260.); hard "char" (degC 320.) ];
  tool "WoodenSpoon" "WOOD" [ hard "char" (degC 250.) ];
  tool "NylonSpatula" "NYLON" [ hard "melt" (degC 190.) ];
  tool "SteelWhisk" "STAINLESS" [ hard "anneal" (degC 800.) ];
]

(* ── Appliances (capability envelopes) ────────────────────────────────────── *)
let appliances : appliance list = [
  { a_name = "Hob"; a_caps = [
      cap "Heat"  Temperature (degC 0.) (degC 250.)
        ~slope:(Some (cps 0., cps 5.)) ~mods:["visual"; "acoustic"];
      cap "Poach" Temperature (degC 0.) (degC 100.)
        ~consumes:true ~mods:["visual"];
      cap "Sear"  Temperature (degC 0.) (degC 250.)
        ~slope:(Some (cps 0., cps 8.)) ~mods:["visual"; "acoustic"];
    ] };
  { a_name = "Oven"; a_caps = [
      cap "Bake" Temperature (degC 50.) (degC 300.) ~mods:["visual"];
    ] };
  { a_name = "SousVide"; a_caps = [
      cap "Heat" Temperature (degC 0.) (degC 95.)
        ~slope:(Some (cps 0., cps 0.5)) ~mods:["thermal"];
    ] };
  { a_name = "Blender"; a_caps = [
      cap "Whisk" AngularRate (rpm 0.) (rpm 1200.) ~mods:["acoustic"];
    ] };
  { a_name = "Fridge"; a_caps = [
      cap "Cool" Temperature (degC (-20.)) (degC 8.) ~mods:["thermal"];
    ] };
  (* ROS 2 consumer-robotics arm. *)
  { a_name = "Arm"; a_caps = [
      cap "Move"  Torque (nm 0.) (nm 8.)
        ~reach:(Some (cm 0., cm 85.)) ~mods:["visual"];
      cap "Whisk" AngularRate (rpm 0.) (rpm 1500.);
      cap "Plate" Torque (nm 0.) (nm 5.)
        ~reach:(Some (cm 0., cm 85.)) ~consumes:true ~mods:["visual"];
    ] };
]
