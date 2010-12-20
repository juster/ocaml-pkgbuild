module Pbparams :
    sig
      type t = PString of string | PArray of string list
      val assign_string : string -> string -> unit
      val assign_array  : string -> string list -> unit
      val param_array   : string -> string list
      val param_string  : string -> string
      val list  : unit -> (string * t) list
      val reset : unit -> unit
    end

val string_of_param : Pbparams.t -> string

