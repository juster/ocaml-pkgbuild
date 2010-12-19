(* Modules required to collect information on a PKGBUILD *)
open Pbcollect
open Pbparse
open Pblex

(* Modules for expanding bash string parameters and storing results
   for use in future expansions *)
open Pbparams
open Expy
open Expl

let pbscan lexbuf =
  pbparse pblex lexbuf;
  let collected = !pb_collection in
  pbcollect_reset ();
  collected

let pbscan_channel pbinchan =
  pbscan (Lexing.from_channel pbinchan)

let pbscan_string pbstr =
  pbscan (Lexing.from_string pbstr)

let pbexpand expstr =
  pbexp pbexplex (Lexing.from_string expstr)

(* let pbfields reclist = *)
(*   let rec expand_aval_str = function *)
(*     | PBLit str -> str *)
(*     | PBExp str -> pbexpand str *)
(*     | PBArray _  -> failwith "Cannot expand a list to string" in *)
(*   let expand_aval = function *)
(*     | PBLit str -> PBString str *)
(*     | PBExp str -> PBString (pbexpand str) *)
(*     | PBList alist -> PBStrings (List.map expand_aval_str alist) in *)
(*   let alist = List.filter *)
(*       (function (lineno,data) -> match data with *)
(*         PBAssignment(_) -> true | _ -> false) *)
(*       reclist in *)
(*   List.map (function (_, PBAssignment(name, aval)) -> *)
(*     let expval = expand_aval aval in *)
(*     set_param_val name expval ; (name, expval) *)
(*     | _ -> raise Exit) *)
(*     alist *)
