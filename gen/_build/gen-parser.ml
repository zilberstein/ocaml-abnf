open Abnf_syntaxtree
open Gen_types

open Buffer
open Printf
open String

let indent_line (b : Buffer.t) (ind : int ref) (s : string) : unit =
  Buffer.add_string b "\n" ;
  Buffer.add_string b (String.make (!ind * 2) ' ') ;
  Buffer.add_string b s

let indent_lines (b : Buffer.t) (ind : int ref) (l : string list) : unit =
  List.iter (indent_line b ind) l

let is_concat (r : rule) : bool =
  begin match r with
        | S_concat (_, _) -> true
        | _ -> false
  end

let rec string_of_rule_parser_nested (r : rule) : string =
  begin match r with
        | S_terminal t      -> "parse_terminal st"
        | S_string   s      -> sprintf "parse_string \"%s\"" s
        | S_concat (r1, r2) -> failwith "illegal nesting"
        | S_reference s     -> sprintf "parse_%s !s" s
        | S_alt (r1, r2)    -> failwith "illegal nesting"
        | S_bracket r                -> "???"
        | S_repetition (i1, i2, r)   -> "???"
        | S_element_list (i1, i2, r) -> "???"
        | S_hex_range (i1, i2)       -> "???"
        | S_any_except (r1, r2)      -> "???"
  end


let rec string_of_rule_parser (r : rule) (out : string) (n : int) : string list =
  begin match r with
        | S_concat (r1, r2) ->
           (if has_state r1 then 
              sprintf "let a%d = %s in" n
            else
              sprintf "let _ = %s in") (string_of_rule_parser_nested r1) ::
	     (begin match r2 with
	            | S_concat (_, _) ->
		      if has_state r1 then
                        string_of_rule_parser r2 (out ^ sprintf "a%d, " n) (n + 1)
                      else
                         string_of_rule_parser r2 out n
                    | _  ->
                       sprintf "let a%d = %s in" n "???" ::
                         (out ^ sprintf "a%d)" n) :: []
	      end)
	| S_alt (r1, r2) -> string_of_rule_parser r1 "(" n
	| _ -> string_of_rule_parser_nested r :: []
  end


let string_of_rule_def_parser (rd : rule_definition) : string =
  let b = Buffer.create 32 in
  let ind = ref 0 in  (* indentation level *)
  indent_line b ind (sprintf "let parse_%s (s : string) : %s =" rd.s_name rd.s_name) ;
  ind := !ind + 1 ;
  indent_line b ind "let st = ref s in" ;
  indent_lines b ind (string_of_rule_parser rd.s_rule "(" 0) ;
  Buffer.contents b

let _ =
  let lexbuf = Lexing.from_channel (open_in "ex.abnf") in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
  List.iter (fun rule -> print_endline (string_of_rule_definition rule)) (List.rev rules) ;
  List.iter (fun rule -> print_endline (string_of_rule_def_parser rule)) rules
