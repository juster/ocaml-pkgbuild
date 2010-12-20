type pbdata =
    Assignment of (string * string * string)
  | Command of (string * string)
  | Function of (string)

module Pbcollect :
  sig
    (** The type of data we can collect on a PKGBUILD. *)
    val reset   : unit -> unit
    val collect : int -> pbdata -> unit
    val results : unit -> (int * pbdata) list
  end


