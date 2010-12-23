%{

open Pbexpand
open Pbparams
open Pbwarn

open Lexing
open Printf

(* let parse_error msg = () *)

(* let collect_error () = *)
(*   let pos = Parsing.symbol_start_pos () in *)
(*   Pbcollect.collect pos.pos_lnum SyntaxError *)

let warn_level  = ref Pbwarn.Style
let level_check = Pbwarn.is_level_active !warn_level

let style = Pbwarn.style level_check
let bad   = Pbwarn.bad level_check
let omg   = Pbwarn.horrible level_check

let functions_defined = ref []

let check_toplevel cmds =
  List.iter begin function
      (line, []) -> () ;
    | (line, cmd) ->
        omg line ("Top-level command: "
                  ^ (String.concat " " cmd) ^ "\n")
  end cmds

let kill_options cmdargs =
  List.filter begin fun str ->
    match str.[0] with '-' -> false | _ -> true
  end cmdargs

let invalid_paths cmdargs =
  List.filter begin fun str ->
    not (Str.string_match (Str.regexp "SRCDIR|PKGDIR") str 0)
  end cmdargs

let check_function name cmds =
  functions_defined := name :: !functions_defined ;
  List.iter begin function
      (line, "rm"::tl) ->
        if List.length (invalid_paths (kill_options tl)) > 0 then
          omg line (name ^ "():Malicious 'rm' command: "
                    ^ (String.concat "\n" ("rm"::tl)) ^ "\n")
    | _ -> ()
  end cmds

%}

%token <int * string> ASSIGN WORD
%token <string> ASSIGNWORD
%token LPAREN RPAREN LARROW RARROW
%token LCURLY RCURLY
%token <int> RCURLY 
%token FOR IN DO DONE
%token AND AND_AND OR_OR
%token SEMI ENDL EOF

%left AND OR_OR AND_AND SEMI ENDL EOF

%start pbparse
%type <unit> pbparse

%%

pbparse:
| simple_list pbparse {
  check_toplevel $1 ;
}
| ENDL pbparse { () }
| error ENDL {
  let pos = Parsing.symbol_start_pos () in
  omg pos.pos_lnum "Syntax error\n"
}
| EOF { () }

simple_list:
| simple_list1 { $1 (* May or may not end in semi-colon. *) }
| simple_list1 SEMI { $1 }

simple_list1:
| simple_list1 SEMI  simple_list1 { $1 @ $3 }
| simple_list1 AND   simple_list1 { $1 @ $3 }
| simple_list1 OR_OR newline_list simple_list1 {
  begin
    match $4 with
      (line, ["return"; "1"]) :: _ ->
        style line ("'|| return 1' is no longer required, "
                    ^ "makepkg notices errors")
    | _ -> ()
  end ; $1 @ $4
}
| command { $1 }

command:
| simple_command { [ $1 ] }
| function_def   { []     }
| shell_command  { $1     }

simple_command:
| simple_command_element {
  match $1 with
    (line,  "") -> (line, []) (* Ignore assignments. *)
  | (line, arg) -> (line, [arg])
}
| simple_command_element simple_command  {
  match ($1, $2) with
    ((line,  ""), (_, strs)) -> (line, strs)
  | ((line, cmd), (_, args)) -> (line, cmd :: args)
}

simple_command_element:
| WORD { $1 }
| ASSIGN assignment_value {
  (* Must have the same return value as the WORD rule above. *)
  match ($1, $2) with ((line, name), raw) ->
    let x = Pbexpand.list raw in
    Pbparams.assign_array name x ;
    (line, "")
}

assignment_value:
| { [ "" ] }
| ASSIGNWORD { [ $1 ] }
| LPAREN compound_assignment RPAREN { $2 }

compound_assignment:
| { [] }
| ASSIGNWORD compound_assignment { $1 :: $2 }

function_def:
| WORD LPAREN RPAREN newline_list function_body {
  printf "*DBG* function definition\n" ;
  begin match $1 with
    (_, "build") | (_, "package") -> ();
  | (n, _) ->
      style n "A function is defined that is not 'build' or 'package'.\n"
  end ;
  eprintf "*DBG* command count = %d\n" (List.length $5) ;
  check_function (snd $1) $5
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
| list1 OR_OR newline_list list1 {
  begin
    match $4 with
      (line, ["return"; "1"]) :: _ ->
        style line ("'|| return 1' is no longer required, "
                    ^ "makepkg notices errors\n")
    | _ -> ()
  end ; $1 @ $4
}
| command { (* command already returns a list of one element *) $1 }
