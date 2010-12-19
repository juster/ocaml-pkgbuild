module Pbcollect :
  sig
    type param  = PString of string | PArray of string list
    val assign_string : string -> string -> unit
    val assign_array  : string -> string list -> unit
    val param_array   : string -> string list
    val param_string  : string -> string

    (** The type of data we can collect on a PKGBUILD. *)
    type pbdata =
        Assignment of (string * string)
      | Command of (string)
      | Function of (string)

    val reset   : unit -> unit
    val collect : int -> pbdata -> unit
    val results : unit -> (int * pbdata) list
  end

val string_of_param : Pbcollect.param -> string
