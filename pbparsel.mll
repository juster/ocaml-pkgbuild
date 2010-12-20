(* First attempt at a scanner/tokenizer for PKGBUILDs
   I really only care about identifying assignment and function defs.
   The tricky part with assignments are with the different quotings
   available and with parameter expansion. *)

{
open Pbparsey
open Lexing
open Printf

(* We are in a wordlist when using constructs like "for" or "while" *)
type lexing_state = PreCommand | AssignVal | CompoundVal | Command

let string_of_lexstate = function
  | PreCommand  -> "PreCommand"
  | AssignVal   -> "AssignVal"
  | CompoundVal -> "CompoundVal"
  | Command     -> "Command"

let quote_start      = ref 0
let func_quote_count = ref 0
let lex_state        = ref PreCommand
let compound_started = ref false

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

(* Use this to tokenize a word (string literal) and adjust our state. *)
let tokenize_word str lbuf =
  match !lex_state with
    PreCommand  -> lex_state := Command ; WORD(lex_linenum lbuf, str)
  | AssignVal   -> lex_state := PreCommand ; ASSIGNWORD(str)
  | CompoundVal -> ASSIGNWORD(str)
  | Command     -> WORD(lex_linenum lbuf, str)

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
    | (WORD(_,_), WORD(_,_)) -> last_tokens.(last) <- tok
    | _ ->
        Array.blit last_tokens 1 last_tokens 0 (max_queue_size - 1) ;
        last_tokens.(last) <- tok ;
  end ;
  tok

(* let dump_history unit = *)
(*   Array.iter (fun t -> printf "*DBG* token: %s\n" *)
(*       (string_of_token t)) last_tokens *)

exception Cmd_context

(* If a given word matches a reserved word then return its token. *)
let reserved_word_token word =
  List.assoc word reserved_words

(* Similar to the function at line 2665 of bash's parse.y *)
let special_case_token word =
  match (word, last_tokens)  with
    ("in", [| _; _; _; FOR; WORD(_,_) |]) -> IN
  | ("do", [| _; FOR; WORD(_,_); IN; WORD(_,_) |]) ->
      lex_state := PreCommand ; DO
  | _ -> raise Not_found

let match_assignment word =
  let len = String.length word in
  if len < 2 then false
  else word.[len-1] == '='

(* Returns a token which depends on several kinds of context. *)
let context_token lexbuf word =
  let linenum = lex_linenum lexbuf in
  try
    match !lex_state with
    (* Assignments or reserved words come before commands. *)
      PreCommand ->
      (* TODO: find a way not to match the '=' char twice
         (we do this in lexword too) *)
        if match_assignment word then begin
          let name = String.sub word 0 ((String.length word) - 1) in
          lex_state := AssignVal ;
          ASSIGN(linenum, name)
        end
        else reserved_word_token word
    (* tokenize_word takes care of state and token type for us *)
    | AssignVal | CompoundVal -> raise Not_found
    (* Special tokens can even come after what appear to be commands. *)
    | Command -> special_case_token word
  with Not_found -> tokenize_word word lexbuf
}

let word = [ '0'-'9' 'a'-'z' 'A'-'Z' '_' '-' ] +

(* Try to parse as many special characters as possible using
   ocamllex's rules. *)
rule pkgbuildlex = parse
| '#' [ ' ' '\t' ]* [^'\n']*
    { (* TODO: record comments using Pbcollect? *)
      pkgbuildlex lexbuf }
| [ ' ' '\t' ] + { pkgbuildlex lexbuf }
| '\n' { Lexing.new_line lexbuf ; lex_state := PreCommand ; ENDL }
| '\'' {
  (* A singlequote... it may have a word directly behind it! *)
  (* This can also start a command string, since commands can be quoted. *)
  let qc = single_quoted lexbuf in
  let trail = lexword lexbuf    in
  tokenize_word ("'" ^ qc ^ "'" ^ trail) lexbuf 
}
| '"' {
  let qc = double_quoted lexbuf in
  let trail = lexword lexbuf    in
  tokenize_word ("\"" ^ qc ^ "\"" ^ trail) lexbuf 
}
(* When unquoted these usually have special meaning *)
| ';' { lex_state := PreCommand ; SEMI }
| '<' { LARROW }
| '>' { RARROW }
| '{' ( [ ' ' '\t' '\n' ]+ as ws ) {
  note_newlines ws lexbuf ;
  (* Subshell lists must be at the beginning of the command line *)
  if !lex_state == PreCommand then LCURLY
  else
    (* Since function defs follow a string we will be in Command state *)
    match last_tokens with
      [| _; _; WORD(_,_); LPAREN; RPAREN |] -> LCURLY
    | _ -> WORD(lex_linenum lexbuf, "{")
  (* TODO: Arithmetic for loops? *)
}
(* Should we count open curly brackets? *)
| '}' {
  if !lex_state == PreCommand then (RCURLY (lex_linenum lexbuf))
  else WORD(lex_linenum lexbuf, "}")
}
| '(' {
  if !lex_state == AssignVal then lex_state := CompoundVal else () ;
  LPAREN
}
| ')' {
  if !lex_state == CompoundVal then lex_state := PreCommand else () ;
  RPAREN
}
| eof { EOF }
| _ as ch {
  let word = (String.make 1 ch) ^ (lexword lexbuf) in
  context_token lexbuf word
}

and lexword = parse
| ( [ ^ ';' '{' '}' '<' '>' '(' ')' '#' '\'' '"' ' ' '=' '\t' '\n' ] * as word )
  ( [ '\'' '"' '=' ] ? as suffix )
{
  match (word, suffix) with
  (* Check for an assignment. *)
    (_, "=") -> 
      if !lex_state == PreCommand then word ^ "="
      else
        (* Treat it just as a simple word. *)
        word ^ "=" ^ (lexword lexbuf)

  (* If a quoted string abuts a non-quoted string concat them. *)
  | ("", "") -> ""
  | (_,  "") -> word
  | (_, "'") ->
      let qc = single_quoted lexbuf in
      word ^ "'"  ^  qc ^ "'" ^ (lexword lexbuf)
  | (_,"\"") ->
      let qc = double_quoted lexbuf in
      word ^ "\"" ^ qc ^ "\"" ^ (lexword lexbuf)
  | (_, _)   -> failwith(sprintf "lexword match error: word=%s; quote=%s"
                           word suffix)
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

{

(* Wrap the ocamlyacc generated function in order to keep track of
   the last few tokens. *)
let pblex lexbuffer =
  token_push (pkgbuildlex lexbuffer)
} 

(* Local Variables: *)
(* mode: caml       *)
(* End:             *)
