open Pbparse
open Pbparams
open Pbcollect

open Printf

let _ =
  try
(*     ignore (Parsing.set_trace true) ; *)
    pbparse_channel (open_in "PKGBUILD") ;
(*     ignore (Parsing.set_trace false) ; *)
    List.iter begin fun pair -> match pair with (n,v) ->
      printf "%s=%s\n" n (string_of_param v)
    end (Pbparams.list ()) ;

    print_endline "" ;

    List.iter begin fun x -> match x with (line, data) ->
      printf "line %d: %s\n" line
        begin match data with
          Assignment(n,v,x) -> n ^ " := " ^ v ^ " (" ^ x ^ ")"
        | Command(s,x) ->
            "CMD: " ^ (if s <> x then s ^ " (" ^ x ^ ")" else s)

        | Function(endl,n) -> sprintf "FUNCDEF: %s (ends at line %d)"
              n endl
        end
    end (Pbcollect.results ()) ;
      
  with ex -> ignore (Parsing.set_trace false) ; raise ex
      
      
  
