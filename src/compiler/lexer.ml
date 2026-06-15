(* SPDX-License-Identifier: MPL-2.0 *)
(* Copyright (c) 2026 Jonathan D.A. Jewell <j.d.a.jewell@open.ac.uk> *)
(*
   Hand-written lexer (closes grammar.ebnf open item O3).

   Maximal-munch over a UTF-8 byte string. Keywords are not distinguished from
   identifiers here — the parser matches keyword spellings on TIDENT — which
   keeps the token set small and the lexer total. Dimensioned literals are a
   single token: a digit-led magnitude immediately followed by a unit run of
   letters, '/', and UTF-8 continuation bytes (so "0.8Pa·s" and "2C/s" lex whole).
*)

type token =
  | TIDENT of string
  | TQTY of float * string   (* magnitude, raw unit string ("" = dimensionless) *)
  | TAT | TTILDE | TBIND | TARROW | TDOTDOT | TDOT
  | TCOLON | TSEMI | TCOMMA
  | TLPAREN | TRPAREN | TLBRACE | TRBRACE
  | TGE | TLE | TGT | TLT | TEQ
  | TEOF

exception Lex_error of string * int

let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_digit c = c >= '0' && c <= '9'
let is_ident_char c = is_alpha c || is_digit c
(* unit characters: letters, '/', and any UTF-8 continuation/lead byte (>=0x80) *)
let is_unit_char c = is_alpha c || c = '/' || Char.code c >= 0x80

let tokenize (src : string) : (token * int) array =
  let n = String.length src in
  let i = ref 0 in
  let line = ref 1 in
  let out = ref [] in
  let emit t = out := (t, !line) :: !out in
  let peek k = if !i + k < n then src.[!i + k] else '\000' in
  while !i < n do
    let c = src.[!i] in
    if c = '\n' then (incr line; incr i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if c = '/' && peek 1 = '/' then begin
      (* line comment *)
      while !i < n && src.[!i] <> '\n' do incr i done
    end
    else if c = '/' && peek 1 = '*' then begin
      (* block comment *)
      i := !i + 2;
      let closed = ref false in
      while !i < n && not !closed do
        if src.[!i] = '\n' then incr line;
        if src.[!i] = '*' && peek 1 = '/' then (i := !i + 2; closed := true)
        else incr i
      done;
      if not !closed then raise (Lex_error ("unterminated block comment", !line))
    end
    else if is_digit c then begin
      (* dimensioned literal: number then optional unit run *)
      let start = !i in
      while !i < n && is_digit src.[!i] do incr i done;
      if !i < n && src.[!i] = '.' && !i + 1 < n && is_digit src.[!i + 1] then begin
        incr i;
        while !i < n && is_digit src.[!i] do incr i done
      end;
      let num = float_of_string (String.sub src start (!i - start)) in
      let us = !i in
      while !i < n && is_unit_char src.[!i] do incr i done;
      let unit = String.sub src us (!i - us) in
      emit (TQTY (num, unit))
    end
    else if is_alpha c then begin
      let start = !i in
      while !i < n && is_ident_char src.[!i] do incr i done;
      emit (TIDENT (String.sub src start (!i - start)))
    end
    else begin
      (match c with
       | '@' -> emit TAT; incr i
       | '~' -> emit TTILDE; incr i
       | '(' -> emit TLPAREN; incr i
       | ')' -> emit TRPAREN; incr i
       | '{' -> emit TLBRACE; incr i
       | '}' -> emit TRBRACE; incr i
       | ':' -> emit TCOLON; incr i
       | ';' -> emit TSEMI; incr i
       | ',' -> emit TCOMMA; incr i
       | '.' -> if peek 1 = '.' then (emit TDOTDOT; i := !i + 2)
                else (emit TDOT; incr i)
       | '<' -> if peek 1 = '~' && peek 2 = '>' then (emit TBIND; i := !i + 3)
                else if peek 1 = '=' then (emit TLE; i := !i + 2)
                else (emit TLT; incr i)
       | '>' -> if peek 1 = '=' then (emit TGE; i := !i + 2)
                else (emit TGT; incr i)
       | '=' -> if peek 1 = '=' then (emit TEQ; i := !i + 2)
                else (emit TEQ; incr i)
       | '-' -> if peek 1 = '>' then (emit TARROW; i := !i + 2)
                else raise (Lex_error ("unexpected '-'", !line))
       | _ -> raise (Lex_error (Printf.sprintf "unexpected character %C" c, !line)))
    end
  done;
  emit TEOF;
  Array.of_list (List.rev !out)
