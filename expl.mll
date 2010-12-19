{
open List
open Expy

let needs_ident = ref false

let bracket_stack = ref []

let begin_bracket unit =
  bracket_stack := 1 :: !bracket_stack

let active_bracket unit =
  match !bracket_stack with
    [] -> false | hd::tl -> true

let push_bracket unit =
  match !bracket_stack with
    [] -> failwith "Bracket stack is empty"
  | count :: tl ->
      bracket_stack := ( count + 1 ) :: tl ; ()

let pop_bracket unit =
  match !bracket_stack with
    [] -> failwith "Bracket stack is empty"
  | count :: tl ->
      begin
        if count - 1 == 0 then
          bracket_stack := tl
        else
          bracket_stack := ( count - 1 ) :: tl
      end ;
      count - 1
}

let ident = ['a'-'z' 'A'-'Z' '0'-'9' '_'] +

rule pbexplex = parse
| "\\$"
    { RAWTEXT( "\\$" ) }
| '$'
    { needs_ident := true; SIGIL }
| ident as name
    { if !needs_ident then ( needs_ident := false; IDENT( name ) )
    else RAWTEXT( name ) }
| "${"
    { needs_ident := true; begin_bracket (); SOPEN }
| '{'
    {
  ( if active_bracket () then push_bracket () );
  RAWTEXT( "{" )
}
| '}'
    {
  if active_bracket () then
    if pop_bracket () == 0 then SCLOSE else RAWTEXT( "}" )
  else RAWTEXT( "}" )
}
| ":-"
    { if active_bracket () then SUBDEF else RAWTEXT( ":-" ) }
| ":="
    { if active_bracket () then SUBSET else RAWTEXT( ":=" ) }
| ":?"
    { if active_bracket () then SUBERR else RAWTEXT( ":?" ) }
| _ as ch
    { RAWTEXT( String.make 1 ch ) }
| eof
    { EOF }

