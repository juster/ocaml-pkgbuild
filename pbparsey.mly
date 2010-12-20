%{

open Pbcollect
open Pbexpand
open Pbparams
open Printf

let parse_error msg =
  print_endline ("Error while parsing PKGBUILD.")

%}

%token <int * string * string> ASSIGN
%token <int * string> STR
%token LPAREN RPAREN LARROW RARROW
%token LCURLY
%token <int> RCURLY 
%token <string> COMMENT
%token FOR IN DO DONE
%token SEMI ENDL EOF

%left SEMI ENDL EOF

%start pbparse
%type <unit> pbparse

%%

pbparse:
| simple_list pbparse { () }
| ENDL pbparse { () }
| error ENDL {}
| EOF { () }

simple_list:
| simple_list1 { (* May or may not end in semi-colon. *) }
| simple_list1 SEMI {}

simple_list1:
| simple_list1 SEMI simple_list1 { () }
| command { (* TODO: pipeline_command *) }

command:
| simple_command {
  match $1 with
  | (_,"") -> ()
  | (n, v) -> let x = Pbexpand.expand_string v in
    Pbcollect.collect n (Command (v, x))
}
| function_def {
  (* Don't expand function names. *)
  match $1 with (begl, endl, v) ->
    Pbcollect.collect begl (Function (endl, v))
}
| shell_command {}

simple_command:
| simple_command_element { $1 }
| simple_command simple_command_element {
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
  match $1, $5 with ((bl,v),el) -> (bl, el, v)
}

newline_list:
| {} | ENDL newline_list {}

function_body:
| shell_command { $1 (* TODO: redirection_list *) }

shell_command:
| group_command { $1 (* TODO: if_command *) }

group_command:
| LCURLY compound_list RCURLY { $3 }

compound_list:
| list {} | newline_list list1 {}

list:
| newline_list {} | list0 {}

list0:
| list1 ENDL newline_list {}
| list1 SEMI newline_list {}

list1:
| list1 ENDL newline_list list1 {}
| list1 SEMI newline_list list1 {}
| command {}
