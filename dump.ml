open Printf
open Pbparsel

let print_token tok =
  print_endline (string_of_token tok)

let rec stream_tokens lexbuf =
  let tok = pblex lexbuf in
  print_token tok ;
  match tok with EOF -> () | _ -> stream_tokens lexbuf

let _ =
  let lexbuf = Lexing.from_channel (open_in "PKGBUILD") in
  stream_tokens lexbuf
