open Expy
open Expl

module Pbexpand =
  struct
    let find_unescaped_from str idx findme =
      let rec find_from findme str idx len =
        let foundidx = String.index_from str idx findme in
        if str.[foundidx-1] != '\\' || foundidx == 0 then
          foundidx
        else if (foundidx + 1) == len then
          raise Not_found
        else
          find_from findme str (foundidx + 1) len
      in find_from findme str idx (String.length str)

    exception Unbalanced_quotes of int

    let string str =
      let rec rawexpand str =
      pbexp pbexplex (Lexing.from_string str)

      and split_and_rec str len begidx endidx action =
        let oldstr = String.sub str 0 begidx in
        let midstr = String.sub str (begidx+1) (endidx-1-begidx) in
        let remstr = (* remaining str to process *)
          if endidx == (len-1) then ""
          else String.sub str (endidx+1) (len-(endidx+1)) in
        try
          (rawexpand oldstr)
          ^ (action midstr)
          ^ (expand_from remstr 0 (String.length remstr))
        with
          Unbalanced_quotes(idx) -> raise (Unbalanced_quotes (idx+endidx+1))
              
      and expand_from str idx len =
        if idx == len then
          rawexpand str
        else try
          match str.[idx] with
        (* Expands everything except text in single-quotes *)
            '\'' ->
              let endidx = String.index_from str (idx+1) '\'' in
              split_and_rec str len idx endidx (fun str -> str)
          | '"' ->
              let endidx = find_unescaped_from str (idx+1) '"' in
              split_and_rec str len idx endidx rawexpand
          | _ ->
              expand_from str (idx+1) len
        with
          Invalid_argument(_) | Not_found -> raise (Unbalanced_quotes idx)

      in expand_from str 0 (String.length str)

    let list strlist = List.map string strlist
  end
