(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   ksc — the KitchenSpeak v2.0 reference CLI.

     ksc check  FILE                 type-check + physical-safety check
     ksc lower [--target T] FILE     lower a clean recipe to the HAL (T = abstract|matter|mqtt|ros2)
     ksc parse  FILE                 parse only (smoke test)

   Exit codes: 0 = OK, 1 = errors found, 2 = usage error.
*)

let read_file p =
  let ic = open_in_bin p in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic; s

let print_diag file (d : Check.diag) =
  let tag = match d.d_sev with Check.Err -> "error" | Check.Warn -> "warning" in
  Printf.printf "%s:%d: %s [%s] %s\n" file d.d_line tag d.d_code d.d_msg

let frontend file =
  let src = read_file file in
  let toks = Lexer.tokenize src in
  Parser.parse toks

let do_check file =
  try
    let prog = frontend file in
    let diags = Check.check prog in
    List.iter (print_diag file) diags;
    let errs = List.filter (fun (d : Check.diag) -> d.d_sev = Check.Err) diags in
    let warns = List.filter (fun (d : Check.diag) -> d.d_sev = Check.Warn) diags in
    if errs = [] then begin
      Printf.printf "OK: %s — %d recipe(s) type-check, %d warning(s).\n"
        file (List.length prog.p_recipes) (List.length warns);
      exit 0
    end else begin
      Printf.printf "FAIL: %s — %d error(s), %d warning(s).\n"
        file (List.length errs) (List.length warns);
      exit 1
    end
  with
  | Lexer.Lex_error (m, l) -> Printf.printf "%s:%d: error [LEX] %s\n" file l m; exit 1
  | Parser.Parse_error (m, l) -> Printf.printf "%s:%d: error [PARSE] %s\n" file l m; exit 1

let do_lower targ file =
  let target = match targ with
    | "matter" -> Hal.Matter | "mqtt" -> Hal.Mqtt | "ros2" -> Hal.Ros2 | _ -> Hal.Abstract in
  try
    let prog = frontend file in
    let diags = Check.check prog in
    let errs = List.filter (fun (d : Check.diag) -> d.d_sev = Check.Err) diags in
    if errs <> [] then begin
      List.iter (print_diag file) errs;
      Printf.eprintf "refusing to lower %s: %d safety error(s) — the trust boundary holds.\n"
        file (List.length errs);
      exit 1
    end else
      print_string (Hal.lower_program target prog)
  with
  | Lexer.Lex_error (m, l) -> Printf.printf "%s:%d: error [LEX] %s\n" file l m; exit 1
  | Parser.Parse_error (m, l) -> Printf.printf "%s:%d: error [PARSE] %s\n" file l m; exit 1

let do_parse file =
  try
    let prog = frontend file in
    Printf.printf "parsed %s: %d declaration(s), %d recipe(s).\n"
      file (List.length prog.p_decls) (List.length prog.p_recipes);
    exit 0
  with
  | Lexer.Lex_error (m, l) -> Printf.printf "%s:%d: error [LEX] %s\n" file l m; exit 1
  | Parser.Parse_error (m, l) -> Printf.printf "%s:%d: error [PARSE] %s\n" file l m; exit 1

let usage () =
  prerr_string
    "ksc — KitchenSpeak v2.0 reference compiler\n\
     usage:\n\
    \  ksc check FILE\n\
    \  ksc lower [--target abstract|matter|mqtt|ros2] FILE\n\
    \  ksc parse FILE\n";
  exit 2

let () =
  match Array.to_list Sys.argv with
  | _ :: "check" :: file :: _ -> do_check file
  | _ :: "lower" :: "--target" :: t :: file :: _ -> do_lower t file
  | _ :: "lower" :: file :: _ -> do_lower "abstract" file
  | _ :: "parse" :: file :: _ -> do_parse file
  | _ -> usage ()
