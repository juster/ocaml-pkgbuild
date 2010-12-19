open Printf

let string_after str idx =
  try
    String.sub str idx ((String.length str) - idx)
  with Invalid_argument(_) -> ""

let extract_ident str idx =
  if Str.string_match (Str.regexp "\\([a-zA-Z0-9]+\\)") str idx then
    begin
      printf "*DBG* found ident: %s\n" (Str.matched_group 1 str) ;
      (Str.matched_group 1 str, string_after str (Str.match_end ()))
    end
  else
    ("", str)

type param_subst =
  | Default    of string
  | AssignDef  of string
  | UnsetError of string
  | Alternate  of string

let close_bracket_idx str idx =
  let rec cbracket_idx str idx count =
    try
      let delta = match str.[idx] with
        '{' -> +1 '}' -> -1 _ -> 0 in
      if count + delta = 0 then idx
      else cbracket_idx str (idx + 1) (count + delta)
    with Invalid_argument(_) -> raise Not_found
  in cbracket_idx str idx 0

let param_expand getvalue expandme =
  let rec pebrackets expandme =
    try
      let endidx = close_bracket_idx expandme 0 in
      match expandme.[1] with
        '$' -> begin
          match pe (string_after expandme 1) with
            (name, rem) -> (getvalue name, rem)
        end
      | _ ->
          match extract_ident expandme 1 with
            (name, rem) ->
              if rem.[0] != '}' then failwith("Extra characters in ${...}")
              else (getvalue name, rem)
    with Not_found -> expandme

  and pe expandme =
    try
      let idx = String.index expandme '$' in
      printf "*DBG* idx = %d\n" idx ;
      match expandme.[idx + 1] with
      | '{' ->
          begin
            printf "*DBG* found ${}\n" ;
            pebrackets (string_after expandme (idx + 1))
          end
      | '$' ->
          begin
            printf "*DBG* found recursive parameter\n" ;
            match pe (string_after expandme (idx + 1)) with
              (name, rem) -> (getvalue name, rem)
          end
      | _   -> match extract_ident expandme (idx + 1) with
          (name, rem) -> (getvalue name, rem)
    (* The $ is at the end of the string. *)
    with Invalid_argument(_) -> (expandme, "")
    
  and peloop expandme =
    try
      match pe expandme with
        (expanded, "")  -> expanded
      | (expanded, rem) -> expanded ^ (peloop rem)
    with Not_found -> expandme

  in
  
  peloop expandme

let param_values = [ ("foo", "bar"); ("bar", "baz") ]

let _ =
  print_endline
    (param_expand (fun name ->
      try List.assoc name param_values
      with Not_found -> "<UNSET>" ) "${$foo}")
