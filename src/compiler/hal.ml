(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   Hardware Abstraction Layer: lower a well-typed program to the abstract
   capability trace (docs/design/0003-hal-and-hardware.adoc) and project it to a
   concrete backend (Matter / MQTT / ROS 2). Lowering is only ever invoked AFTER
   the checker is clean, so the trust boundary holds: the type that made the
   recipe safe is the type that gates which devices it may lower to.

   Dependency-free JSON: emitted by hand so the prototype needs no opam installs.
*)

open Ast
open Check

type target = Abstract | Matter | Mqtt | Ros2

let buf = Buffer.create 4096
let out s = Buffer.add_string buf s

let jstr s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (fun c -> match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c) s;
  Buffer.add_char b '"';
  Buffer.contents b

let jnum f =
  if Float.equal (Float.round f) f then Printf.sprintf "%.0f" f else Printf.sprintf "%g" f

let fail_name = function Abort -> "ABORT" | Recover -> "RECOVER" | Warm -> "WARM"

let cond_str = function
  | TropCond (m, _, q) -> Printf.sprintf "~ %s -> %s" m (show_qty q)
  | EchoCond (e, _, v) -> Printf.sprintf "@%s == %s" e v

(* primary requested value in a step, matching the capability's controlled dim *)
let primary_value capb args =
  List.fold_left (fun acc a -> match a with
      | ANamed (_, AQty q) when dim_eq q.q_dim capb.c_quantity -> Some q
      | _ -> acc) None args

let named_qty name args =
  List.fold_left (fun acc a -> match a with
      | ANamed (n, AQty q) when String.lowercase_ascii n = name -> Some q
      | _ -> acc) None args

(* Matter projection table: verb -> (cluster, attribute) *)
let matter_map verb = match verb with
  | "Heat" | "Bake" | "Sear" -> ("Thermostat", "OccupiedHeatingSetpoint")
  | "Poach" -> ("TemperatureControl", "TemperatureSetpoint")
  | "Cool" -> ("Thermostat", "OccupiedCoolingSetpoint")
  | "Whisk" -> ("FanControl", "PercentSetting")
  | _ -> ("Custom", "Setpoint")

let ros2_action verb = match verb with
  | "Move" | "Plate" -> "control_msgs/action/FollowJointTrajectory"
  | "Whisk" -> "control_msgs/action/GripperCommand"
  | _ -> "control_msgs/action/JointGroupCommand"

let emit_step target (app : appliance) (capb : capability) (s : step) ~first =
  if not first then out ",\n";
  let pv = primary_value capb s.s_args in
  let value_json = match pv with
    | Some q -> Printf.sprintf "{ \"quantity\": %s, \"value\": %s, \"unit\": %s }"
                  (jstr (dim_name capb.c_quantity)) (jnum q.q_mag) (jstr (show_qty q))
    | None -> "null" in
  let reach_json = match named_qty "reach" s.s_args with
    | Some q -> Printf.sprintf ", \"reach\": %s" (jnum q.q_mag) | None -> "" in
  let maxd = match s.s_maxdur with Some q -> jnum q.q_mag | None -> "null" in
  let onf = match s.s_onfail with Some f -> jstr (fail_name f) | None -> "null" in
  let wit = match s.s_until with
    | Some (EchoCond (e, _, _)) -> jstr ("@" ^ e)
    | _ -> "null" in
  let until = match s.s_until with Some c -> jstr (cond_str c) | None -> "null" in
  out "      {\n";
  out (Printf.sprintf "        \"actor\": %s, \"appliance\": %s, \"verb\": %s,\n"
         (jstr s.s_actor) (jstr app.a_name) (jstr s.s_verb));
  out (Printf.sprintf "        \"envelope\": %s%s,\n" value_json reach_json);
  out (Printf.sprintf "        \"until\": %s, \"max_duration_s\": %s, \"on_fail\": %s, \"witness\": %s"
         until maxd onf wit);
  (match target with
   | Matter ->
     let cl, attr = matter_map s.s_verb in
     out (Printf.sprintf ",\n        \"matter\": { \"cluster\": %s, \"attribute\": %s, \"value\": %s }"
            (jstr cl) (jstr attr) (match pv with Some q -> jnum q.q_mag | None -> "null"))
   | Mqtt ->
     out (Printf.sprintf ",\n        \"mqtt\": { \"topic\": %s, \"payload\": %s }"
            (jstr (Printf.sprintf "kitchenspeak/%s/%s" app.a_name s.s_verb))
            value_json)
   | Ros2 ->
     out (Printf.sprintf ",\n        \"ros2\": { \"server\": %s, \"action\": %s }"
            (jstr (Printf.sprintf "/%s/%s" (String.lowercase_ascii app.a_name) (String.lowercase_ascii s.s_verb)))
            (jstr (ros2_action s.s_verb)))
   | Abstract -> ());
  out "\n      }"

let lower_recipe target (e : env) (r : recipe) =
  out "  {\n";
  out (Printf.sprintf "    \"recipe\": %s,\n" (jstr r.r_name));
  out (Printf.sprintf "    \"ceremony\": %s,\n"
         (match r.r_under with Some c -> jstr c | None -> "null"));
  out (Printf.sprintf "    \"target\": %s,\n"
         (jstr (match target with Abstract -> "abstract" | Matter -> "matter"
                                | Mqtt -> "mqtt" | Ros2 -> "ros2")));
  out "    \"steps\": [\n";
  let first = ref true in
  List.iter (function
      | Sync sb ->
        List.iter (fun s ->
            match resolve_actor e s.s_actor with
            | Some (app, _) ->
              (match find_cap app s.s_verb with
               | Some capb -> emit_step target app capb s ~first:!first; first := false
               | None -> ())
            | None -> ())
          sb.sy_steps
      | _ -> ())
    r.r_body;
  out "\n    ]\n  }"

let lower_program target (prog : program) : string =
  Buffer.clear buf;
  let e = build_env prog in
  out "[\n";
  let first = ref true in
  List.iter (fun r ->
      if not !first then out ",\n";
      first := false;
      lower_recipe target e r)
    prog.p_recipes;
  out "\n]\n";
  Buffer.contents buf
