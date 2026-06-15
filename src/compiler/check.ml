(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   The KitchenSpeak v2.0 checker: the physically-grounded substructural type
   system from docs/spec/0003-type-system.adoc and the safety judgement of
   docs/design/0001-v2-constitution.adoc §5.

   For each step `actor.Verb(target, args…)` it discharges:
     1. capability  — the controlling appliance has the verb;
     2. device env  — every requested quantity is inside the appliance envelope
                      (after the chef-style modifier, which may only shrink it);
     3. material    — the requested exposure is strictly below every HARD limit
                      of each touched material/tool (soft limits warn);
   plus linearity (use-once), the three mandatory constructs, and echo witnesses.
*)

open Ast

type sev = Err | Warn
type diag = { d_sev : sev; d_code : string; d_msg : string; d_line : int }

type env = {
  mats       : (string, material) Hashtbl.t;   (* materials AND tools *)
  apps       : (string, appliance) Hashtbl.t;
  chefs      : (string, chef) Hashtbl.t;
  echoes     : (string, echo) Hashtbl.t;
  ceremonies : (string, ceremony) Hashtbl.t;
}

type res = { mutable live : bool; r_mat : material; r_line : int; from_bind : bool }

let eps = 1.0e-9

(* ── environment construction (stdlib ⊕ file declarations) ─────────────────── *)
let build_env (prog : program) : env =
  let e = { mats = Hashtbl.create 64; apps = Hashtbl.create 32;
            chefs = Hashtbl.create 16; echoes = Hashtbl.create 16;
            ceremonies = Hashtbl.create 16 } in
  List.iter (fun m -> Hashtbl.replace e.mats m.m_name m) Stdlib_ks.materials;
  List.iter (fun t -> Hashtbl.replace e.mats t.m_name t) Stdlib_ks.tools;
  List.iter (fun a -> Hashtbl.replace e.apps a.a_name a) Stdlib_ks.appliances;
  List.iter (function
      | DMaterial m | DTool m -> Hashtbl.replace e.mats m.m_name m
      | DAppliance a -> Hashtbl.replace e.apps a.a_name a
      | DChef c -> Hashtbl.replace e.chefs c.ch_name c
      | DEcho ec -> Hashtbl.replace e.echoes ec.ec_name ec
      | DCeremony ce -> Hashtbl.replace e.ceremonies ce.ce_name ce)
    prog.p_decls;
  e

let resolve_actor (e : env) (name : string) : (appliance * string) option =
  match Hashtbl.find_opt e.chefs name with
  | Some ch ->
    (match Hashtbl.find_opt e.apps ch.ch_controls with
     | Some a -> Some (a, ch.ch_style)
     | None -> None)
  | None ->
    (match Hashtbl.find_opt e.apps name with
     | Some a -> Some (a, "BALANCED")
     | None -> None)

let find_cap (a : appliance) verb = List.find_opt (fun c -> c.c_verb = verb) a.a_caps

(* device-envelope violation code, by controlled dimension *)
let over_code = function
  | Torque -> "OVER_TORQUE"
  | AngularRate -> "OVER_SPEED"
  | Length -> "OVER_REACH"
  | _ -> "OVER_RANGE"

(* material hard-limit violation code, by limit name *)
let limit_error name = match String.lowercase_ascii name with
  | "curdle" -> "CURDLE"
  | "scorch" | "burn" -> "SCORCH"
  | "melt" | "char" | "anneal" -> "MELT_TOOL"
  | "seize" -> "SEIZE"
  | "split" -> "SPLIT"
  | _ -> "MATERIAL_LIMIT"

let slope_tag_rate = function
  | "GENTLE" -> Some { q_mag = 1.0; q_dim = TempRate }
  | "MODERATE" -> Some { q_mag = 3.0; q_dim = TempRate }
  | "AGGRESSIVE" -> Some { q_mag = 8.0; q_dim = TempRate }
  | _ -> None

(* the chef style modifier may only SHRINK an envelope's upper bound *)
let style_factor = function
  | "PRECISION" -> 0.5
  | "POWER" -> 1.0
  | _ -> 0.8

(* limits relevant to a controlled dimension *)
let limits_for_dim (m : material) = function
  | Temperature -> m.m_env.e_thermal
  | Torque | AngularRate -> m.m_env.e_mech
  | _ -> []

let check (prog : program) : diag list =
  let e = build_env prog in
  let diags = ref [] in
  let add sev code line msg = diags := { d_sev = sev; d_code = code; d_msg = msg; d_line = line } :: !diags in

  let check_recipe (r : recipe) =
    let resources : (string, res) Hashtbl.t = Hashtbl.create 16 in
    let bind_outputs = ref [] in

    (match r.r_under with
     | Some c when not (Hashtbl.mem e.ceremonies c) ->
       add Warn "UNKNOWN_CEREMONY" 0 (Printf.sprintf "recipe '%s' runs under ceremony '%s', which is not declared" r.r_name c)
     | _ -> ());

    let material_of tname =
      match Hashtbl.find_opt resources tname with
      | Some rr -> Some rr.r_mat
      | None -> Hashtbl.find_opt e.mats tname in

    let check_step ~proving (s : step) =
      (* mandatory constructs (constitution §3) *)
      if s.s_until = None then
        add Err "MISSING_UNTIL" s.s_line (Printf.sprintf "step %s.%s has no 'until' postcondition" s.s_actor s.s_verb);
      if s.s_maxdur = None then
        add Err "MISSING_MAXDUR" s.s_line (Printf.sprintf "step %s.%s has no 'max_duration' (termination witness)" s.s_actor s.s_verb);
      if s.s_onfail = None then
        add Err "MISSING_ONFAIL" s.s_line (Printf.sprintf "step %s.%s has no 'on_fail' handler" s.s_actor s.s_verb);

      match resolve_actor e s.s_actor with
      | None -> add Err "UNKNOWN_ACTOR" s.s_line (Printf.sprintf "actor '%s' is not a declared appliance or chef" s.s_actor)
      | Some (app, style) ->
        let targets = List.filter_map (function APos n -> Some n | _ -> None) s.s_args in
        let named = List.filter_map (function ANamed (n, v) -> Some (n, v) | _ -> None) s.s_args in
        (match find_cap app s.s_verb with
         | None ->
           add Err "NO_CAPABILITY" s.s_line
             (Printf.sprintf "appliance '%s' has no capability '%s'" app.a_name s.s_verb)
         | Some capb ->
           let sf = style_factor style in

           let check_targets_exposure (q : quantity) =
             List.iter (fun tname ->
                 match material_of tname with
                 | None -> ()  (* liveness pass reports the bad name *)
                 | Some m ->
                   List.iter (fun l ->
                       if q.q_mag >= l.l_qty.q_mag -. eps then
                         match l.l_kind with
                         | Hard ->
                           add Err (limit_error l.l_name) s.s_line
                             (Printf.sprintf
                                "would expose %s '%s' to %s; hard %s limit is %s"
                                (if m.m_is_tool then "tool" else "material")
                                tname (show_qty q) l.l_name (show_qty l.l_qty))
                         | Soft ->
                           add Warn ("APPROACHING_" ^ String.uppercase_ascii l.l_name) s.s_line
                             (Printf.sprintf "%s '%s' at %s reaches its soft %s threshold %s"
                                (if m.m_is_tool then "tool" else "material")
                                tname (show_qty q) l.l_name (show_qty l.l_qty)))
                     (limits_for_dim m capb.c_quantity))
               targets in

           let check_device_range (q : quantity) =
             if q.q_mag > capb.c_hi.q_mag +. eps then
               add Err (over_code capb.c_quantity) s.s_line
                 (Printf.sprintf "%s.%s requested %s; device envelope tops out at %s"
                    app.a_name s.s_verb (show_qty q) (show_qty capb.c_hi))
             else if q.q_mag < capb.c_lo.q_mag -. eps then
               add Err "UNDER_RANGE" s.s_line
                 (Printf.sprintf "%s.%s requested %s; below device minimum %s"
                    app.a_name s.s_verb (show_qty q) (show_qty capb.c_lo)) in

           let check_reach (q : quantity) =
             match capb.c_reach with
             | Some (_, rhi) when q.q_mag > rhi.q_mag +. eps ->
               add Err "OVER_REACH" s.s_line
                 (Printf.sprintf "%s.%s requested reach %s; arm reach envelope is %s"
                    app.a_name s.s_verb (show_qty q) (show_qty rhi))
             | _ -> () in

           let check_slope (q : quantity) =
             match capb.c_slope with
             | Some (_, shi) ->
               let eff = shi.q_mag *. sf in
               if q.q_mag > eff +. eps then
                 add Err "OVER_SLOPE" s.s_line
                   (Printf.sprintf "%s.%s slope %s exceeds %s envelope %s (style %s)"
                      app.a_name s.s_verb (show_qty q) app.a_name
                      (show_qty { q_mag = eff; q_dim = TempRate }) style)
             | None -> () in

           let primary_seen = ref false in
           List.iter (fun (n, v) ->
               let ln = String.lowercase_ascii n in
               match v with
               | AQty q ->
                 if ln = "reach" && dim_eq q.q_dim Length then check_reach q
                 else if ln = "slope" && dim_eq q.q_dim TempRate then check_slope q
                 else if dim_eq q.q_dim capb.c_quantity then
                   (primary_seen := true; check_device_range q; check_targets_exposure q)
                 else if List.mem ln ["target"; "temp"; "to"; "torque"; "speed"; "rate"; "power"] then
                   add Err "DIM_MISMATCH" s.s_line
                     (Printf.sprintf "argument '%s' is a %s but %s.%s controls %s"
                        n (dim_name q.q_dim) app.a_name s.s_verb (dim_name capb.c_quantity))
                 else ()  (* benign auxiliary (e.g. depth: 4cm on a Poach) *)
               | ATag t ->
                 if ln = "slope" then
                   (match slope_tag_rate t with Some r -> check_slope r | None -> ())
                 else ()
               | AIdent _ -> ())
             named;

           (* Soundness backstop: if no explicit primary quantity was given but
              the `until ~ metric >= X` guard drives the controlled quantity to X,
              treat X as the exposure so a dangerous guard cannot slip past. *)
           (match s.s_until with
            | Some (TropCond (_, _, q))
              when (not !primary_seen) && dim_eq q.q_dim capb.c_quantity ->
              check_device_range q; check_targets_exposure q
            | _ -> ());

           (* linearity: targets liveness + consumption *)
           List.iter (fun tname ->
               match Hashtbl.find_opt resources tname with
               | Some rr ->
                 if not rr.live then
                   add Err "DOUBLE_USE" s.s_line
                     (Printf.sprintf "resource '%s' is used again after being consumed" tname)
                 else if capb.c_consumes then rr.live <- false
               | None ->
                 (match Hashtbl.find_opt e.mats tname with
                  | Some m when m.m_is_tool -> ()  (* tools are non-linear *)
                  | Some _ ->
                    add Err "UNUSED_RESOURCE" s.s_line
                      (Printf.sprintf "material '%s' must be brought into scope with 'use' before it is acted on" tname)
                  | None ->
                    add Err "UNDECLARED_TARGET" s.s_line
                      (Printf.sprintf "target '%s' is neither a 'use'd resource nor a known tool" tname)))
             targets;

           (* echo witness obligation *)
           (match s.s_until with
            | Some (EchoCond (ec, _, _)) ->
              if not (Hashtbl.mem e.echoes ec) then
                add Err "UNKNOWN_ECHO" s.s_line
                  (Printf.sprintf "step witnesses @%s but no such echo is declared" ec)
              else (match proving with
                  | Some pe when pe = ec -> ()
                  | _ ->
                    add Err "UNWITNESSED_SYNC" s.s_line
                      (Printf.sprintf "step completes on @%s but the enclosing sync has no matching 'proving @%s'" ec ec))
            | _ -> ()))
    in

    let check_sync (sb : sync_block) =
      List.iter (fun a ->
          if resolve_actor e a = None then
            add Err "UNKNOWN_ACTOR" sb.sy_line (Printf.sprintf "sync actor '%s' is not a declared appliance or chef" a))
        sb.sy_actors;
      (match sb.sy_proving with
       | Some pe when not (Hashtbl.mem e.echoes pe) ->
         add Err "UNKNOWN_ECHO" sb.sy_line (Printf.sprintf "sync proves @%s but no such echo is declared" pe)
       | _ -> ());
      List.iter (check_step ~proving:sb.sy_proving) sb.sy_steps in

    let check_bind (b : bind_step) =
      List.iter (fun inp ->
          match Hashtbl.find_opt resources inp with
          | Some rr ->
            if not rr.live then
              add Err "DOUBLE_USE" b.b_line (Printf.sprintf "bind input '%s' was already consumed" inp)
            else rr.live <- false
          | None ->
            add Err "UNDECLARED_TARGET" b.b_line
              (Printf.sprintf "bind input '%s' is not a 'use'd resource" inp))
        b.b_inputs;
      if resolve_actor e b.b_actor = None then
        add Err "UNKNOWN_ACTOR" b.b_line (Printf.sprintf "bind actor '%s' is not a declared appliance or chef" b.b_actor);
      (match Hashtbl.find_opt e.mats b.b_output with
       | Some m ->
         Hashtbl.replace resources b.b_output { live = true; r_mat = m; r_line = b.b_line; from_bind = true };
         bind_outputs := b.b_output :: !bind_outputs
       | None ->
         add Err "UNKNOWN_MATERIAL" b.b_line
           (Printf.sprintf "dyadic bind produces '%s', which is not a known material" b.b_output)) in

    let do_stmt = function
      | Use u ->
        (match Hashtbl.find_opt e.mats u.u_material with
         | Some m when not m.m_is_tool ->
           Hashtbl.replace resources u.u_as { live = true; r_mat = m; r_line = u.u_line; from_bind = false }
         | Some _ ->
           add Warn "TOOL_USED" u.u_line (Printf.sprintf "'%s' is a tool and need not be brought in with 'use'" u.u_material)
         | None ->
           add Err "UNKNOWN_MATERIAL" u.u_line (Printf.sprintf "'use %s' names an unknown material" u.u_material))
      | Sync sb -> check_sync sb
      | Bind b -> check_bind b in

    List.iter do_stmt r.r_body;

    (* leak check: a dyadic-bound product nobody consumed *)
    List.iter (fun name ->
        match Hashtbl.find_opt resources name with
        | Some rr when rr.live ->
          add Warn "LEAK" rr.r_line (Printf.sprintf "resource '%s' is produced but never consumed" name)
        | _ -> ())
      !bind_outputs
  in
  List.iter check_recipe prog.p_recipes;
  List.rev !diags
