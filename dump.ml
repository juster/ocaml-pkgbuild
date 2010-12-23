open Printf
open Pbparsel
open Pbparsey

let string_of_token = function
  | ASSIGN(line,name) -> sprintf "ASSIGN(%d, %s)" line name
  | WORD(line,str) -> sprintf "WORD(%d, %s)" line str
  | ASSIGNWORD(str) -> sprintf "ASSIGNWORD(%s)" str
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LARROW -> "LARROW"
  | RARROW -> "RARROW"
  | LCURLY -> "LCURLY"
  | RCURLY(line) -> "RCURLY"
  | SEMI   -> "SEMI"
  | ENDL   -> "ENDL"
  | EOF    -> "EOF"
  | FOR    -> "FOR"
  | IN     -> "IN"
  | DO     -> "DO"
  | DONE   -> "DONE"
  | AND    -> "AND"
  | AND_AND -> "AND_AND"
  | OR_OR   -> "OR_OR"

let print_token tok =
  print_endline (string_of_token tok)

let rec stream_tokens lexbuf =
  let tok = pblex lexbuf in
  print_token tok ;
  match tok with EOF -> () | _ -> stream_tokens lexbuf

let _ =
  let lexbuf = Lexing.from_channel (open_in "PKGBUILD") in
  stream_tokens lexbuf
