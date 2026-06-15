(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   Recursive-descent parser for KitchenSpeak v2.0.

   Follows the concrete grammar in docs/spec/0002-concrete-grammar.adoc. Keywords
   are recognised as identifier-shaped tokens and matched by spelling, so the
   token set stays small. Step clauses (until / max_duration / on_fail) are
   accepted in any order and their *presence* is enforced later by the checker,
   which yields precise diagnostics (MISSING_ONFAIL, …) rather than parse noise.
*)

open Ast
open Lexer

exception Parse_error of string * int

let dim_of_name s =
  match String.lowercase_ascii s with
  | "temperature" | "temp" -> Temperature
  | "time" -> Time
  | "mass" -> Mass
  | "torque" -> Torque
  | "angularrate" | "rate" | "speed" | "rpm" -> AngularRate
  | "length" | "reach" | "depth" -> Length
  | "power" -> Power
  | "viscosity" -> Viscosity
  | "temprate" -> TempRate
  | _ -> Dimensionless

let parse (arr : (token * int) array) : program =
  let p = ref 0 in
  let peek () = fst arr.(!p) in
  let line () = snd arr.(!p) in
  let adv () = incr p in
  let err msg = raise (Parse_error (msg, line ())) in
  let next () = let t = peek () in adv (); t in
  let expect t = if peek () = t then adv () else err "unexpected token" in
  let ident () = match peek () with
    | TIDENT s -> adv (); s
    | _ -> err "expected identifier" in
  let is_kw s = match peek () with TIDENT k -> k = s | _ -> false in
  let kw s = if is_kw s then adv () else err ("expected keyword '" ^ s ^ "'") in
  let accept t = if peek () = t then (adv (); true) else false in
  let parse_qty () = match peek () with
    | TQTY (m, u) -> adv (); Units.normalize m u
    | _ -> err "expected a dimensioned quantity" in

  (* ── limit blocks: thermal/mechanical/chemical { (hard|soft)? name: qty ; } ── *)
  let parse_limits () =
    expect TLBRACE;
    let ls = ref [] in
    let rec lp () = match peek () with
      | TRBRACE -> ()
      | TIDENT k ->
        let kind = match k with
          | "hard" -> adv (); Hard
          | "soft" -> adv (); Soft
          | _ -> Hard in
        let lname = ident () in
        expect TCOLON;
        let q = parse_qty () in
        ignore (accept TSEMI);
        ls := { l_kind = kind; l_name = lname; l_qty = q } :: !ls;
        lp ()
      | _ -> err "expected a limit name or '}'" in
    lp ();
    expect TRBRACE;
    List.rev !ls in

  let empty_env = { e_thermal = []; e_mech = []; e_chem = [] } in

  let parse_envelope_into env =
    (* peek already at thermal/mechanical/chemical *)
    match peek () with
    | TIDENT "thermal" -> adv (); { env with e_thermal = env.e_thermal @ parse_limits () }
    | TIDENT "mechanical" -> adv (); { env with e_mech = env.e_mech @ parse_limits () }
    | TIDENT "chemical" -> adv (); { env with e_chem = env.e_chem @ parse_limits () }
    | _ -> err "expected thermal/mechanical/chemical" in

  let parse_material () =
    let name = ident () in
    expect TLBRACE;
    let phase = ref "UNSPECIFIED" in
    let env = ref empty_env in
    let rec body () = match peek () with
      | TRBRACE -> ()
      | TIDENT "phase" -> adv (); expect TCOLON; phase := ident (); ignore (accept TSEMI); body ()
      | TIDENT ("thermal" | "mechanical" | "chemical") -> env := parse_envelope_into !env; body ()
      | _ -> err "expected phase/thermal/mechanical/chemical or '}'" in
    body ();
    expect TRBRACE;
    { m_name = name; m_phase = !phase; m_env = !env; m_is_tool = false; m_tool_material = None } in

  let parse_tool () =
    let name = ident () in
    expect TLBRACE;
    let tmat = ref None in
    let env = ref empty_env in
    let rec body () = match peek () with
      | TRBRACE -> ()
      | TIDENT "material" -> adv (); expect TCOLON; tmat := Some (ident ()); ignore (accept TSEMI); body ()
      | TIDENT ("thermal" | "mechanical" | "chemical") -> env := parse_envelope_into !env; body ()
      | _ -> err "expected material/thermal/mechanical/chemical or '}'" in
    body ();
    expect TRBRACE;
    { m_name = name; m_phase = "SOLID"; m_env = !env; m_is_tool = true; m_tool_material = !tmat } in

  let parse_range () =
    let a = parse_qty () in expect TDOTDOT; let b = parse_qty () in (a, b) in

  let parse_capability () =
    let verb = ident () in
    expect TLBRACE;
    let quantity = ref Dimensionless in
    let rng = ref None and slope = ref None and reach = ref None in
    let consumes = ref false and mods = ref [] in
    let rec body () = match peek () with
      | TRBRACE -> ()
      | TIDENT "quantity" -> adv (); expect TCOLON; quantity := dim_of_name (ident ()); ignore (accept TSEMI); body ()
      | TIDENT "range" -> adv (); expect TCOLON; rng := Some (parse_range ()); ignore (accept TSEMI); body ()
      | TIDENT "slope" -> adv (); expect TCOLON; slope := Some (parse_range ()); ignore (accept TSEMI); body ()
      | TIDENT "reach" -> adv (); expect TCOLON; reach := Some (parse_range ()); ignore (accept TSEMI); body ()
      | TIDENT "consumes" -> adv (); expect TCOLON; consumes := (ident () = "true"); ignore (accept TSEMI); body ()
      | TIDENT "modalities" -> adv (); expect TCOLON;
        let rec ml () = let m = ident () in mods := m :: !mods; if accept TCOMMA then ml () in
        ml (); ignore (accept TSEMI); body ()
      | _ -> err "expected capability field or '}'" in
    body ();
    expect TRBRACE;
    let lo, hi = match !rng with
      | Some (a, b) -> a, b
      | None -> { q_mag = 0.0; q_dim = !quantity }, { q_mag = 1.0e12; q_dim = !quantity } in
    { c_verb = verb; c_quantity = !quantity; c_lo = lo; c_hi = hi;
      c_slope = !slope; c_reach = !reach; c_consumes = !consumes; c_modalities = List.rev !mods } in

  let parse_appliance () =
    let name = ident () in
    expect TLBRACE;
    let caps = ref [] in
    let rec body () = match peek () with
      | TRBRACE -> ()
      | TIDENT "capability" -> adv (); caps := parse_capability () :: !caps; body ()
      | _ -> err "expected 'capability' or '}'" in
    body ();
    expect TRBRACE;
    { a_name = name; a_caps = List.rev !caps } in

  let parse_chef () =
    let name = ident () in
    kw "controls";
    let appl = ident () in
    let style = if is_kw "style" then (adv (); ident ()) else "BALANCED" in
    expect TSEMI;
    { ch_name = name; ch_controls = appl; ch_style = style } in

  let parse_echo () =
    expect TAT;
    let name = ident () in
    kw "on";
    let appl = ident () in
    expect TDOT;
    let modn = ident () in
    expect TSEMI;
    { ec_name = name; ec_appliance = appl; ec_modality = modn } in

  let parse_ceremony () =
    let name = ident () in
    expect TLBRACE;
    let settings = ref [] in
    let rec body () = match peek () with
      | TRBRACE -> ()
      | TIDENT k -> adv (); expect TCOLON;
        let v = match peek () with
          | TIDENT s -> adv (); s
          | TQTY (m, u) -> adv (); Ast.show_qty (Units.normalize m u)
          | _ -> err "expected ceremony setting value" in
        ignore (accept TSEMI); ignore (accept TCOMMA);
        settings := (k, v) :: !settings; body ()
      | _ -> err "expected ceremony setting or '}'" in
    body ();
    expect TRBRACE;
    { ce_name = name; ce_settings = List.rev !settings } in

  let parse_decl () = match next () with
    | TIDENT "material" -> DMaterial (parse_material ())
    | TIDENT "tool" -> DTool (parse_tool ())
    | TIDENT "appliance" -> DAppliance (parse_appliance ())
    | TIDENT "chef" -> DChef (parse_chef ())
    | TIDENT "echo" -> DEcho (parse_echo ())
    | TIDENT "ceremony" -> DCeremony (parse_ceremony ())
    | _ -> err "expected a declaration head" in

  (* ── steps ───────────────────────────────────────────────────────────────── *)
  let parse_cmp () = match next () with
    | TGE -> Ge | TLE -> Le | TGT -> Gt | TLT -> Lt | TEQ -> Eq
    | _ -> err "expected a comparison operator" in

  let parse_cond () = match peek () with
    | TTILDE -> adv ();
      let metric = ident () in let c = parse_cmp () in let q = parse_qty () in
      TropCond (metric, c, q)
    | TAT -> adv ();
      let e = ident () in let c = parse_cmp () in
      let v = match peek () with
        | TIDENT s -> adv (); s
        | TQTY (m, u) -> adv (); Ast.show_qty (Units.normalize m u)
        | _ -> err "expected echo comparison value" in
      EchoCond (e, c, v)
    | _ -> err "expected '~' or '@' condition" in

  let parse_fail () = match next () with
    | TIDENT "ABORT" -> Abort | TIDENT "RECOVER" -> Recover | TIDENT "WARM" -> Warm
    | _ -> err "expected ABORT/RECOVER/WARM" in

  let parse_argval () = match peek () with
    | TQTY (m, u) -> adv (); AQty (Units.normalize m u)
    | TIDENT s -> adv (); if String.uppercase_ascii s = s then ATag s else AIdent s
    | _ -> err "expected argument value" in

  let parse_arg () = match peek () with
    | TIDENT name -> adv ();
      if accept TCOLON then ANamed (name, parse_argval ()) else APos name
    | _ -> err "expected an argument" in

  let parse_args () =
    if peek () = TRPAREN then []
    else
      let rec go acc =
        let a = parse_arg () in
        let acc = a :: acc in
        if accept TCOMMA then go acc else List.rev acc in
      go [] in

  let parse_step () =
    let ln = line () in
    let actor = ident () in
    expect TDOT;
    let verb = ident () in
    expect TLPAREN;
    let args = parse_args () in
    expect TRPAREN;
    let untilc = ref None and maxd = ref None and onf = ref None in
    let rec clauses () = match peek () with
      | TIDENT "until" -> adv (); untilc := Some (parse_cond ()); clauses ()
      | TIDENT "max_duration" -> adv (); maxd := Some (parse_qty ()); clauses ()
      | TIDENT "on_fail" -> adv (); onf := Some (parse_fail ()); clauses ()
      | TSEMI -> adv ()
      | _ -> err "expected until / max_duration / on_fail / ';'" in
    clauses ();
    { s_actor = actor; s_verb = verb; s_args = args;
      s_until = !untilc; s_maxdur = !maxd; s_onfail = !onf; s_line = ln } in

  let parse_sync () =
    let ln = line () in
    kw "sync";
    expect TLPAREN;
    let actors = ref [] in
    let rec al () = actors := ident () :: !actors; if accept TCOMMA then al () in
    al ();
    expect TRPAREN;
    let proving = if is_kw "proving" then (adv (); expect TAT; Some (ident ())) else None in
    expect TLBRACE;
    let steps = ref [] in
    let rec sl () = match peek () with
      | TRBRACE -> ()
      | _ -> steps := parse_step () :: !steps; sl () in
    sl ();
    expect TRBRACE;
    { sy_actors = List.rev !actors; sy_proving = proving; sy_steps = List.rev !steps; sy_line = ln } in

  let parse_bind () =
    let ln = line () in
    expect TLPAREN;
    let inputs = ref [] in
    let rec il () = inputs := ident () :: !inputs; if accept TCOMMA then il () in
    il ();
    expect TRPAREN;
    expect TBIND;
    let actor = ident () in
    expect TARROW;
    let out = ident () in
    ignore (accept TSEMI);
    { b_inputs = List.rev !inputs; b_actor = actor; b_output = out; b_line = ln } in

  let parse_rstmt () = match peek () with
    | TIDENT "use" -> adv ();
      let ln = line () in
      let m = ident () in
      let asn = if is_kw "as" then (adv (); ident ()) else m in
      ignore (accept TSEMI);
      Use { u_material = m; u_as = asn; u_line = ln }
    | TIDENT "sync" -> Sync (parse_sync ())
    | TLPAREN -> Bind (parse_bind ())
    | _ -> err "expected use / sync / dyadic-bind" in

  let parse_recipe () =
    kw "recipe";
    let name = ident () in
    let under = if is_kw "under" then (adv (); Some (ident ())) else None in
    expect TLBRACE;
    let body = ref [] in
    let rec bl () = match peek () with
      | TRBRACE -> ()
      | _ -> body := parse_rstmt () :: !body; bl () in
    bl ();
    expect TRBRACE;
    { r_name = name; r_under = under; r_body = List.rev !body } in

  let decls = ref [] and recipes = ref [] in
  let rec top () = match peek () with
    | TEOF -> ()
    | TIDENT ("material" | "tool" | "appliance" | "chef" | "echo" | "ceremony") ->
      decls := parse_decl () :: !decls; top ()
    | TIDENT "recipe" -> recipes := parse_recipe () :: !recipes; top ()
    | _ -> err "expected a declaration or 'recipe'" in
  top ();
  { p_decls = List.rev !decls; p_recipes = List.rev !recipes }
