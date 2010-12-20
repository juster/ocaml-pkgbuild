type pbdata =
    Assignment of (string * string * string)
  | Command of (string * string)
  | Function of (int * string)

module Pbcollect =
  struct
    (** The type of data we can collect on a PKGBUILD. *)
    let pb_collection : (int * pbdata) list ref = ref []

    let reset unit =
      pb_collection := [] ; ()

    let collect lineno newrec =
      pb_collection := (lineno, newrec) :: !pb_collection ; ()

    let results unit =
      List.rev !pb_collection
  end
