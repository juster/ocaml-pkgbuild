open Printf

let _ =
  let print_token tok =
    print_endline
      (match tok with
        RAWTEXT( txt ) -> ("RAWTEXT: '" ^ txt ^ "'")
      | IDENT( name ) -> ("IDENT: " ^ name)
      | SIGIL -> "SIGIL"
      | SOPEN -> "SOPEN"
      | SCLOSE -> "SCLOSE"
      | SUBDEF -> "SUBDEF"
      | SUBSET -> "SUBSET"
      | SUBERR -> "SUBERR"
      | EOF    -> "EOF") in
      
  let rec dump_tokens lexbuf =
    let tok = pbexplex lexbuf in
    if (match tok with EOF -> false | _ -> true) then
      (print_token tok; dump_tokens lexbuf)
    else () in

  let test_it msg =
    dump_tokens (Lexing.from_string msg);
    ignore (Parsing.set_trace true);
    let lbuf = Lexing.from_string msg in
    let presult = try pbexp pbexplex lbuf
    with Parsing.Parse_error -> begin
      ignore (Parsing.set_trace false);
      failwith (sprintf "Error at chars %d-%d >>> %s"
                  (Lexing.lexeme_start lbuf)
                  (Lexing.lexeme_end lbuf)
                  msg)
    end
    in
    ignore (Parsing.set_trace false);
    presult
  in
  
  test_it "${FOO:=BAR} ${BAR:?OMG where is $FOO?}"
