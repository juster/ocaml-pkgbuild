%{

open Pbcollect
open Pbexpand
open Pbparams
open Printf

let parse_error msg =
  print_endline ("Error while parsing PKGBUILD.")

%}

%token <int * string * string> ASSIGN
%token <string> STR
%token LPAREN RPAREN LCURLY RCURLY LARROW RARROW
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
  | "" -> ()
  | _  -> let x = Pbexpand.expand_string $1 in
    Pbcollect.collect 0 (Command ($1, x))
}
| function_def {
  let x = Pbexpand.expand_string $1 in
  Pbcollect.collect 0 (Function x)
}
| shell_command {}

simple_command:
| simple_command_element { $1 }
| simple_command simple_command_element {
  match ($1, $2) with
    ("", _) -> $2 | (_, "") -> $1 | (_, _) -> $1 ^ " " ^ $2
}

simple_command_element:
| STR    { $1 }
| ASSIGN {
  (* Remember that the lexer only puts ASSIGNs in front... *)
  match $1 with (line, name, newval) ->
    let x = Pbexpand.expand_string newval in
    Pbcollect.collect line (Assignment(name, newval, x)) ;
    Pbparams.assign_string name x ;
    ""
}

function_def:
| STR LPAREN RPAREN newline_list function_body {
  $1
}

newline_list:
| {} | ENDL newline_list {}

function_body:
| shell_command { (* TODO: redirection_list *) }

shell_command:
| group_command { (* TODO: if_command *) }

group_command:
| LCURLY compound_list RCURLY {}

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
