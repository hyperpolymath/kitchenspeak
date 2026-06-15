(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   KitchenSpeak v2.0 — Abstract Syntax Tree.

   This is the binding AST shape from docs/design/0001-v2-constitution.adoc §4.
   Field names are prefixed per-type to avoid OCaml's record-field disambiguation
   pitfalls. Every quantity is normalised to a base unit at parse time (see
   units.ml), so the checker compares magnitudes directly within a dimension.
*)

(* ── Dimensions (Primitive type / units of measure) ───────────────────────── *)
type dim =
  | Temperature
  | Time
  | Mass
  | Torque
  | AngularRate
  | Length
  | Power
  | Viscosity
  | TempRate          (* temperature / time, e.g. C/s — for thermal slopes *)
  | Dimensionless
  | DUnknown of string

type quantity = { q_mag : float; q_dim : dim }   (* q_mag is in the base unit *)

(* ── Material / tool safety envelopes (the keystone) ───────────────────────── *)
type limit_kind = Soft | Hard

type limit = { l_kind : limit_kind; l_name : string; l_qty : quantity }

type envelope = { e_thermal : limit list; e_mech : limit list; e_chem : limit list }

type material = {
  m_name         : string;
  m_phase        : string;
  m_env          : envelope;
  m_is_tool      : bool;
  m_tool_material : string option;   (* e.g. Some "SILICONE" for a spatula *)
}

(* ── Appliance capability envelopes ───────────────────────────────────────── *)
type capability = {
  c_verb       : string;
  c_quantity   : dim;                  (* the primary controlled dimension *)
  c_lo         : quantity;
  c_hi         : quantity;
  c_slope      : (quantity * quantity) option;   (* rate envelope, if any *)
  c_reach      : (quantity * quantity) option;   (* spatial envelope, if any *)
  c_consumes   : bool;                 (* does the verb consume its target? *)
  c_modalities : string list;          (* sensor modalities offered, e.g. visual *)
}

type appliance = { a_name : string; a_caps : capability list }

type chef = { ch_name : string; ch_controls : string; ch_style : string }

type echo = { ec_name : string; ec_appliance : string; ec_modality : string }

type ceremony = { ce_name : string; ce_settings : (string * string) list }

(* ── Steps & mission body ─────────────────────────────────────────────────── *)
type cmp = Ge | Le | Gt | Lt | Eq

type cond =
  | TropCond of string * cmp * quantity   (* ~ metric cmp qty   *)
  | EchoCond of string * cmp * string     (* @ echo  cmp value  *)

type fail = Abort | Recover | Warm

type argval = AQty of quantity | ATag of string | AIdent of string

type arg =
  | APos   of string             (* positional target (a resource or tool name) *)
  | ANamed of string * argval    (* name : value *)

type step = {
  s_actor  : string;
  s_verb   : string;
  s_args   : arg list;
  s_until  : cond option;
  s_maxdur : quantity option;
  s_onfail : fail option;
  s_line   : int;
}

type sync_block = {
  sy_actors  : string list;
  sy_proving : string option;
  sy_steps   : step list;
  sy_line    : int;
}

type bind_step = { b_inputs : string list; b_actor : string; b_output : string; b_line : int }

type use_decl = { u_material : string; u_as : string; u_line : int }

type rstmt =
  | Sync of sync_block
  | Bind of bind_step
  | Use  of use_decl

type decl =
  | DMaterial  of material
  | DTool      of material
  | DAppliance of appliance
  | DChef      of chef
  | DEcho      of echo
  | DCeremony  of ceremony

type recipe = { r_name : string; r_under : string option; r_body : rstmt list }

type program = { p_decls : decl list; p_recipes : recipe list }

(* ── Display helpers ──────────────────────────────────────────────────────── *)
let dim_name = function
  | Temperature -> "Temperature" | Time -> "Time" | Mass -> "Mass"
  | Torque -> "Torque" | AngularRate -> "AngularRate" | Length -> "Length"
  | Power -> "Power" | Viscosity -> "Viscosity" | TempRate -> "TempRate"
  | Dimensionless -> "Dimensionless" | DUnknown u -> "Unknown(" ^ u ^ ")"

let dim_eq a b = match a, b with
  | DUnknown x, DUnknown y -> x = y
  | _ -> a = b

(* Pretty-print a quantity in a readable canonical unit. *)
let show_qty q =
  let u = match q.q_dim with
    | Temperature -> "C" | Time -> "s" | Mass -> "g" | Torque -> "Nm"
    | AngularRate -> "rpm" | Length -> "cm" | Power -> "W"
    | Viscosity -> "Pa.s" | TempRate -> "C/s" | Dimensionless -> ""
    | DUnknown u -> u in
  let m = q.q_mag in
  let ms = if Float.equal (Float.round m) m then Printf.sprintf "%.0f" m
           else Printf.sprintf "%g" m in
  ms ^ u
