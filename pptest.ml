open Pbparse
open Printf

let _ =
  try
    ignore (Parsing.set_trace true) ;
    pbparse_channel (open_in "PKGBUILD") ;
    ignore (Parsing.set_trace false) ;
  with ex -> ignore (Parsing.set_trace false) ; raise ex
      
      
  
