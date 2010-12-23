open Printf

module Pbwarn =
  struct
    type warnlevel = Style | Bad | Horrible

    let is_level_active limit lvl =
        match (limit,lvl) with
        | (Style,Style) | (Style,Bad) | (Style,Horrible) -> true
        | (Bad,Style) -> false
        | (Bad,Bad) | (Bad,Horrible) -> true
        | (Horrible,Style) | (Horrible,Bad) -> false
        | (Horrible,Horrible) -> true

    let warning test lvl line msg =
      if test lvl then
        eprintf "%2d: %s" line msg
      else ()
    let style test    = warning test Style
    let bad test      = warning test Bad
    let horrible test = warning test Horrible

  end
