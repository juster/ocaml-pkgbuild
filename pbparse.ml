open Pbparsey
open Pbparsel

let pbparse_string pbstr =
  pbparse pblex (Lexing.from_string pbstr)

let pbparse_channel pbchan =
  pbparse pblex (Lexing.from_channel pbchan)
