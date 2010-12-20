module Pbexpand :
    sig
      val string : string -> string
      val list : string list -> string list

      exception Unbalanced_quotes of int
    end

  
