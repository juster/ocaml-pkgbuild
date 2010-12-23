open Pbparse
open Pbparams

open Printf

(* let words wl = String.concat " " wl *)

(* let rec string_of_pbrec = function (line, data) -> *)
(*   sprintf "%d: %s" line (string_of_pbdata data) *)

(* and string_of_pbdata = function *)
(*     Assignment(n,v,x) -> *)
(*       sprintf "VAR %s := %s (%s)" n (words v) (words x) *)
(*   | Command(s,x) -> *)
(*       "CMD " ^ (if s <> x then s ^ " (" ^ x ^ ")" else s) *)
(*   | Async(cmd) -> *)
(*       "ASYNC" ^ (string_of_pbdata cmd) *)
(*   | Function(name,cmds) -> sprintf "FUN %s\n%s" name *)
(*         (String.concat "\n" *)
(*            ([ "{{{" ] @ (List.map string_of_pbrec cmds) @ [ "}}}" ])) *)
(*   | SyntaxError -> "SYNTAX ERROR!" *)

let _ =
  try
    Pbparams.assign_string "srcdir" "SRCDIR" ;
    Pbparams.assign_string "pkgdir" "PKGDIR" ;
    ignore (Parsing.set_trace true) ;
    pbparse_channel (open_in "PKGBUILD") ;
    ignore (Parsing.set_trace false) ;
(*     List.iter print_endline *)
(*       (List.map string_of_pbrec (Pbcollect.results ())) *)

  with ex -> ignore (Parsing.set_trace false) ; raise ex
      
      
  
