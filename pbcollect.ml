type command =
    (** Raw commands are program names and arguments. *)
  | Raw    of string list
    (** & can be recursive, eg: { sleep 1; echo "B"; } & echo "A" *)
  | Async  of command
    (** || can be recursive, eg: { false && true; } || echo "F" *)
  | Or     of (command * command)
    (** && can be recursive, eg: { true || false; } && echo "T" *)
  | And    of (command * command)
    (** Each command in a {} grouping has its own line number. *)
  | Group  of (int * command) list
    (** Functions are usually made of a Group command. *)
  | Function of (string * command)

type pbdata =
    (** Separate assignments from commands which follow them. *)
  | Assignment  of (int * string * string list)
    (** A command. See above. *)
  | Command     of (int * command)
    (** Records the line number of syntax errors. *)
  | SyntaxError of int

module Pbcollect =
  struct
    (** The type of data we can collect on a PKGBUILD. *)
    let pb_collection : pbdata list ref = ref []

    let reset unit =
      pb_collection := [] ; ()

    let add newrec =
      pb_collection := newrec :: !pb_collection ; ()

    let results unit =
      List.rev !pb_collection
  end
