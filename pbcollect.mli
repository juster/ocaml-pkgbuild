type pbdata =
    Assignment of (string * string list * string list)
  | Command    of (string * string)
  | Async      of (cmd)
  | Function   of (string * (int * pbdata) list)
  | SyntaxError

module Pbcollect :
  sig
    (** The type of data we can collect on a PKGBUILD. *)
    val reset   : unit -> unit
    val collect : int -> pbdata -> unit
    val results : unit -> (int * pbdata) list
  end


