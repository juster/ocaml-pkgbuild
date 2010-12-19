open Pbexpand

module Pbcollect =
  struct
    type param = PString of string | PArray of string list

    let param_values : (string * param) list ref = ref []

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

    let params unit = List.rev !param_values

    let reset_params unit =
      param_values := [] ; ()

    (** The type of data we can collect on a PKGBUILD. *)
    type pbdata =
        Assignment of (string * string)
      | Command of (string)
      | Function of (string)

    let pb_collection : (int * pbdata) list ref = ref []

    let reset unit =
      reset_params () ;
      pb_collection := [] ; ()

    let expand_string str =
      Pbexpand.expand_string str

    let collect lineno newrec =
      (* Expand strings when needed. *)
      let record = match newrec with
        Command(str) -> Command(expand_string str)
      | Assignment(name, str) -> Assignment(name, expand_string str)
      | _ -> newrec in

      pb_collection := (lineno, record) :: !pb_collection ; ()

    let results unit =
      List.rev !pb_collection
  end

let string_of_param pbv =
  match pbv with
    Pbcollect.PString(str) -> "'" ^ str ^ "'"
  | Pbcollect.PArray(strlist) ->
      "(" ^
      (String.concat " "
         (List.map (fun str -> "'" ^ str ^ "'") strlist))
      ^ ")"
