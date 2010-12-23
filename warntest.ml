open Pbwarn

let limit = ref Pbwarn.Style

let ourtest = Pbwarn.is_level_active !limit

let _ =
  Pbwarn.style ourtest 1 "Your shoes don't match!\n" ;
  Pbwarn.bad ourtest 2 "You really should shower.\n" ;
  Pbwarn.horrible ourtest 3 "You smell horrible!\n"
