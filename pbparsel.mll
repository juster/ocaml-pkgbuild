(* First attempt at a scanner/tokenizer for PKGBUILDs
   I really only care about identifying assignment and function defs.
   The tricky part with assignments are with the different quotings
   available and with parameter expansion. *)

{
open Pbparsey
open Lexing
open Printf
open Str

(* We are in a wordlist when using constructs like "for" or "while" *)
type lexing_state = PreCmd | PostCmd | WordList

let string_of_lexstate = function
  | PreCmd     -> "PreCmd"
  | PostCmd    -> "PostCmd"
  | WordList   -> "WordList"

let quote_start      = ref 0
let func_quote_count = ref 0
let lex_state        = ref PreCmd
let compound_count   = ref 0

(* When we match a string literal token, call this *)
let state_record_word unit =
  match !lex_state with
    PreCmd   -> lex_state := PostCmd ; ()
  | PostCmd  -> ()
  | WordList -> ()

let print_lexstate unit =
  print_endline ("*DBG* LEX STATE = " ^ (string_of_lexstate !lex_state))

let lex_linenum lbuf =
  let pos = Lexing.lexeme_start_p lbuf in
  pos.pos_lnum

let rec count_newlines str idx =
  try
    let nlidx = String.index_from str idx '\n' in
    1 + (count_newlines str (nlidx + 1))
  with Not_found -> 0

let note_newlines str lexbuf =
  for unused = 1 to (count_newlines str 0) do
    Lexing.new_line lexbuf
  done ;
  ()

let assign_regexp = Str.regexp "\\([a-zA-Z0-9_]+\\)="

(* type token = *)
(*   | STR of string *)
(*   | ASSIGN of int * string * string *)
(*   | LPAREN | RPAREN *)
(*   | LCURLY | RCURLY *)
(*   | LARROW | RARROW *)
(*   | COMMENT of string *)
(*   | FOR | IN | DO *)
(*   | DONE *)
(*   | SEMI *)
(*   | ENDL *)
(*   | EOF *)

(* let string_of_token tok = *)
(*   match tok with *)
(*     ASSIGN (lineno, name, str) -> *)
(*       sprintf "ASSIGN (line %d) %s := %s" lineno name str *)
(*   | STR str -> sprintf "STR %s" str *)
(*   | COMMENT str -> "COMMENT " ^ str *)
(*   | FOR -> "FOR" | IN -> "IN" | DO -> "DO" *)
(*   | DONE -> "DONE" *)
(*   | LPAREN -> "LPAREN" *)
(*   | RPAREN -> "RPAREN" *)
(*   | LARROW -> "LARROW" *)
(*   | RARROW -> "RARROW" *)
(*   | LCURLY -> "LCURLY" *)
(*   | RCURLY -> "RCURLY" *)
(*   (\* | FUNCDEF (lineno, name, contents) -> *\) *)
(*   (\*     sprintf "FUNCDEF line:%d name:%s" lineno name *\) *)
(*   (\* | SPACE -> "SPACE" *\) *)
(*   | ENDL -> "ENDL" *)
(*   | SEMI -> "SEMI" *)
(*   | EOF  -> "EOF" *)

(* Reserved words must be at the beginning of a command. *)
let reserved_words = [ ("for", FOR); ("done", DONE) ]

let max_queue_size = 5
let last_tokens = Array.make max_queue_size EOF

let token_push tok =
  let last = (max_queue_size - 1) in
  begin
    match (tok, last_tokens.(last)) with
    (* Don't record spacer tokens. *)
      (SEMI, _) | (ENDL, _) -> ()
    (* Make matching a wordlist the same as matching a word.
       We will have to catch any problems in the grammar. *)
    | (STR(_), STR(_)) -> last_tokens.(last) <- tok
    | _ ->
        Array.blit last_tokens 1 last_tokens 0 (max_queue_size - 1) ;
        last_tokens.(last) <- tok ;
  end ;
  tok

(* let dump_history unit = *)
(*   Array.iter (fun t -> printf "*DBG* token: %s\n" *)
(*       (string_of_token t)) last_tokens *)

exception Cmd_context

let reserved_word_token word =
  List.assoc word reserved_words

(* Similar to the function at 2665 of bash's parse.y *)
let special_case_token word =
  match (word, last_tokens)  with
    ("in", [| _; _; _; FOR; STR(_) |]) -> IN
  | ("do", [| _; FOR; STR(_); IN; STR(_) |]) ->
      lex_state := PreCmd ; DO
  | _ -> raise Not_found

(* Parse tokens that cannot come after a command. *)
let command_start_token lexbuf word =
  try reserved_word_token word
  with Not_found ->
    if Str.string_match assign_regexp word 0
    then
      let valstart = (Str.group_end 1) + 1 in
      let newval =
        try String.sub word valstart ((String.length word) - valstart)
        with Invalid_argument(_) -> ""  in
      ASSIGN(lex_linenum lexbuf,
             Str.matched_group 1 word,
             newval)
    else raise Cmd_context

let context_token lexbuf word =
  (* dump_history () ; *)
  (* These tokens can even come after what appear to be commands. *)
  try special_case_token word
  with Not_found ->
    try
      if !lex_state == PreCmd && !compound_count == 0
      then command_start_token lexbuf word
      else raise Cmd_context
    (* This word is a command/argument reserved words are disabled. *)
    with Cmd_context -> state_record_word () ; STR(word)
}

let word = [ '0'-'9' 'a'-'z' 'A'-'Z' '_' '-' ] +

(* Try to parse as many special characters as possible using
   ocamllex's rules. *)
rule pkgbuildlex = parse
| '#' [ ' ' '\t' ]* ( [^'\n']* as text )
    { COMMENT(text) }
| [ ' ' '\t' ] + { pkgbuildlex lexbuf }
| '\n' { Lexing.new_line lexbuf ; lex_state := PreCmd ; ENDL }
| '\'' {
  (* A singlequote... it may have a word directly behind it! *)
  (* This can also start a command string, since commands can be quoted. *)
  state_record_word () ;
  let qc = single_quoted lexbuf in
  let trail = lexword lexbuf    in
  STR("'" ^ qc ^ "'" ^ trail)
}
| '"' {
  state_record_word () ;
  let qc = double_quoted lexbuf in
  let trail = lexword lexbuf    in
  STR("\"" ^ qc ^ "\"" ^ trail)
}
(* When unquoted these usually have special meaning *)
| ';' { lex_state := PreCmd ; SEMI }
| '<' { LARROW }
| '>' { RARROW }
| '{' ( [ ' ' '\t' '\n' ]+ as ws ) {
  note_newlines ws lexbuf ;
  (* Subshell lists must be at the beginning of the command line *)
  if !lex_state == PreCmd then LCURLY
  else
    (* Since function defs follow a string we will be in PostCmd state *)
    match last_tokens with
      [| _; _; STR(_); LPAREN; RPAREN |] -> LCURLY
    | _ -> STR("{")
  (* TODO: Arithmetic for loops? *)
}
(* Should we count open curly brackets? *)
| '}' { if !lex_state == PreCmd then RCURLY else STR("}") }
| '(' {
  incr compound_count ; LPAREN
}
| ')' {
  begin if !compound_count > 0 then decr compound_count end ;
  RPAREN
}
| eof { EOF }
| _ as ch {
  let word = (String.make 1 ch) ^ (lexword lexbuf) in
  context_token lexbuf word
}

and lexword = parse
| ( [ ^ ';' '{' '}' '<' '>' '(' ')' '#' '\'' '"' ' ' '\t' '\n' ] * as word )
  ( [ '\'' '"' ] ? as quote )
    {
  (* If a quoted string abuts a non-quoted string concat them *)
  match (word, quote) with
    ("", "") -> ""
  | (_,  "") -> word
  | (_, "'") ->
      let qc = single_quoted lexbuf in
      word ^ "'"  ^  qc ^ "'" ^ (lexword lexbuf)
  | (_,"\"") ->
      let qc = double_quoted lexbuf in
      word ^ "\"" ^ qc ^ "\"" ^ (lexword lexbuf)
  | (_, _)   -> failwith( "lexword match error: "
                           ^ "word=" ^ word ^ "; quote=" ^ quote )
}

and single_quoted = parse
| ( [^'\''] * as contents ) '\''
    { note_newlines contents lexbuf; contents }
| _ 
    { failwith ("No closing single-quote (') found.") }
| eof
    { failwith ("No closing single-quote (') found.") }

and double_quoted = parse
| "\\\""
    { "\\\"" ^ (double_quoted lexbuf) }
| '"'
    { "" }
| _ as ch
    { (String.make 1 ch) ^ (double_quoted lexbuf) }
| eof
    { failwith (sprintf "Line %d: No closing double-quote (\") found.\n"
                  !quote_start) }

(* and funcbody = parse *)
(* | '{' *)
(*     { incr func_quote_count; "{" ^ (funcbody lexbuf) } *)
(* | '}' *)
(*     { decr func_quote_count; *)
(*       if !func_quote_count == 0 then *)
(*         "}" *)
(*       else if !func_quote_count < 0 then *)
(*         failwith "Invalid function definition: unbalanced brackets" *)
(*       else *)
(*         "}" ^ (funcbody lexbuf) *)
(*     } *)
(* | '\n' *)
(*     { Lexing.new_line lexbuf; "\n" ^ (funcbody lexbuf) }  *)
(* | _ as ch *)
(*     { (String.make 1 ch) ^ (funcbody lexbuf) } *)

(* { (\* empty *\) } *)

{
  (* Wrap the ocamlyacc generated function in order to keep track of
     the last few tokens. *)
let pblex lexbuffer =
  token_push (pkgbuildlex lexbuffer)
} 

(* Local Variables: *)
(* mode: caml       *)
(* End:             *)
