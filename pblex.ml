open Pbparsey
open Lexing

type lex_state = Begin | EscQuote

exception EOF

let buffer_peek buff =
  if buff.lex_eof_reached then raise EOF
  else
    try buff.str.[0]
    with Failure -> buff.refill_buff () ; buffer_peek buff

let buffer_incr buff =
  incr buff.pos ;
  buff.str <- String.sub buff.str 1 ((String.length buff.str) - 1) ;
  () ;

let buffer_read_char buff =
  let nextchar = buffer_peek_char buff in
  buffer_incr buff ; nextchar

let buffer_next_is_word buff =
  let nextchar = buffer_peek_char buff in
  match nextchar with
    ' ' | '\t' | '\n' -> false | _ -> true

(* let buffer_read_word buff = *)
(*   let nextchar = buffer_peek_char buff in *)
(*   match nextchar with *)
(*     '\n' | ' ' | '\t' -> "" *)
(*   | buffer_incr buff ; (String.make 1 nextchar) ^ (buffer_read_word buff) *)

let rec lex_word buff =
  let nextchar = buffer_read_char buff in
  match nextchar with
    ' ' | '\t' | '\n' -> ""
  | _ -> (String.make 1 nextchar) ^ (lex_word buff)

and lex_squote buff =
  let nextchar = buffer_read_char buff in
  match nextchar with '\'' -> ""
  | _ -> (String.make 1 nextchar) ^ (lex_squote buff)

and lex_dquote buff =
  let nextchar = buffer_read_char buff in
  match (buff.state, nextchar) with
    (NA, '"')     -> ""
  | (NA, '\\')    -> buff.state <- EscQuote; lex_dquote buff ;
  | (NA, _)       ->
      (String.make 1 nextchar) ^ (lex_dquote buff) 
  | (EscQuote, _) ->
      buff.state <- NA ;
      (String.make 1 nextchar) ^ (lex_dquote buff)

and pblex buff =
  if buff.eof then EOF
  else try
    match buffer_read_char buff with
      '"'     -> lex_dquote buff
    | '\''    -> lex_squote buff
    | _ as ch ->
        let str = String.make 1 ch in
        let wordtok = EXPAND( str ^ (pbplex buff) ) in
  with EOF -> Pbparsey.EOF
