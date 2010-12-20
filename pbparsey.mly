%{

open Pbcollect
open Pbexpand
open Pbparams

open Lexing
open Printf

let parse_error msg = ()

let collect_error () =
  let pos = Parsing.symbol_start_pos () in
  Pbcollect.collect pos.pos_lnum SyntaxError

%}

%token <int * string * string> ASSIGN
%token <int * string> STR
%token LPAREN RPAREN LARROW RARROW
%token LCURLY RCURLY
%token <int> RCURLY 
%token <string> COMMENT
%token FOR IN DO DONE
%token SEMI ENDL EOF

%left SEMI ENDL EOF

%start pbparse
%type <unit> pbparse

%%

pbparse:
| simple_list pbparse {
  List.iter (function (line, cmd) -> Pbcollect.collect line cmd) $1 ; ()
}
| ENDL pbparse { () }
| error ENDL { collect_error () }
| EOF { () }

simple_list:
| simple_list1 { $1 (* May or may not end in semi-colon. *) }
| simple_list1 SEMI { $1 }

simple_list1:
| simple_list1 SEMI simple_list1 { $1 @ $3 }
| command { $1 }

command:
| simple_command {
  (* Must create lists of one element to match shell_command which
     could be a list of commands (via group_command) *)
   match $1 with
     (_,"") -> [] (* Ignore commands that are only assignments. *)
   | (n, v) -> let x = Pbexpand.expand_string v in [ (n, Command (v, x)) ]
}
| function_def {
  (* Don't expand function names. *)
  match $1 with (begl, name, cmds) ->
    [ (begl, Function (name, cmds)) ]
}
| shell_command { $1 }

simple_command:
| simple_command_element { $1 }
| simple_command simple_command_element {
  (* We concatenate command words into one "word". *)
  match ($1, $2) with
    (* Empty strings are assignments, not commands. Ignore them. *)
    ((n,""), (_,"")) -> (n, "")
  | ((n,""), (_, s)) -> (n, s)
  | ((n, s), (_,"")) -> (n, s)
  | ((n, l), (_, r)) -> (n, l ^ " " ^ r)
}

simple_command_element:
| STR    { $1 }
| ASSIGN {
  (* Remember that the lexer only puts ASSIGNs in front... *)
  match $1 with (line, name, newval) ->
    let x = Pbexpand.expand_string newval in
    Pbcollect.collect line (Assignment(name, newval, x)) ;
    Pbparams.assign_string name x ;
    (line, "")
}

function_def:
| STR LPAREN RPAREN newline_list function_body {
  (* The function data type contains a list of commands inside it. *)
  match $1, $5 with ((begl, name), cmds) -> (begl, name, cmds)
}

newline_list:
| {} | ENDL newline_list {}

function_body:
| shell_command { $1 (* TODO: redirection_list *) }

shell_command:
| group_command { $1 (* TODO: if_command *) }

group_command:
| LCURLY compound_list RCURLY { $2 }

compound_list:
| list { $1 } | newline_list list1 { $2 }

list:
| newline_list { [] } | list0 { $1 }

list0:
| list1 ENDL newline_list { $1 }
| list1 SEMI newline_list { $1 }

list1:
| list1 ENDL newline_list list1 { $1 @ $4 }
| list1 SEMI newline_list list1 { $1 @ $4 }
| command { (* command already returns a list of one element *) $1 }
