module Pbexpand :
    sig
      val expand_string : string -> string

      (* val expand_string : (string -> string) -> *)
      (*   (string -> string -> unit) -> *)
      (*     string -> string *)

      exception Unbalanced_quotes of int
    end

  
