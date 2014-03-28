open Printf
(*open Abnf_cmd*)

let _ =
  eprintf "This isnx a program\n" ;
  let lexbuf = Lexing.from_channel (open_in "../http.abnf") in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
(*  begin match rules with
	   | h :: t -> eprintf "%s\n" h.Abnf_syntaxtree.s_name
	   | []     -> eprintf "NONE\n"
  end*)
  List.iter (fun rule -> print_endline "TEST") rules ;
