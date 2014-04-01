open Abnf_syntaxtree
open Gen_types

open Buffer


let string_of_parse_function (rds : rule_definition list) : string =
  let b = Buffer.create 32 in
  Buffer.add_string b "let parse (s : string) =\n" ;
  Buffer.contents b

let _ =
  let lexbuf = Lexing.from_channel (open_in "type.abnf") in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
  List.iter (fun rule -> print_endline (string_of_rule_definition rule)) rules ;
  print_endline (string_of_parse_function rules)
