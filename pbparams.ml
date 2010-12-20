module Pbparams =
  struct
    type t = PString of string | PArray of string list

    let param_values : (string * t) list ref = ref []

    let prepend_value pname newval =
      param_values := (pname, newval) :: !param_values ; ()

    let assign_string pname strval =
      prepend_value pname (PString strval)

    let assign_array pname strlist =
      prepend_value pname (PArray strlist)

    let param_array pname =
      match List.assoc pname !param_values with
        PString(str)    -> [ str ]
      | PArray(strlist) -> strlist

    let param_string pname =
      match List.assoc pname !param_values with
        PString(str)    -> str
      | PArray(strlist) -> String.concat " " strlist

    let list unit = List.rev !param_values

    let reset unit =
      param_values := [] ; ()
  end

let string_of_param pbv =
  match pbv with
    Pbparams.PString(str) -> "'" ^ str ^ "'"
  | Pbparams.PArray(strlist) ->
      "(" ^
      (String.concat " "
         (List.map (fun str -> "'" ^ str ^ "'") strlist))
      ^ ")"
