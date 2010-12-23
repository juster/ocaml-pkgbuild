module Pbwarn :
  sig
    type warnlevel = Style | Bad | Horrible
    val is_level_active : warnlevel -> warnlevel -> bool
    val warning : (warnlevel -> bool) -> warnlevel -> int -> string -> unit

    val style : (warnlevel -> bool) -> int -> string -> unit
    val bad   : (warnlevel -> bool) -> int -> string -> unit
    val horrible : (warnlevel -> bool) -> int -> string -> unit
  end
