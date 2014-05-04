open Abnf_syntaxtree
open Gen_types
open Gen_lexer

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

let rec name_of_terminal (t : terminal) : string =
  begin match t with
        | ALPHA    -> "ALPHA"
        | UPALPHA  -> "UALPHA"
        | LOALPHA  -> "LOALPHA"
        | DIGIT    -> "DIGIT"
        | HEXDIGIT -> "HEXDIGIT"
        | DQUOTE   -> "DQUOTE"
        | SP       -> "SP"
        | HTAB     -> "HTAB"
        | WSP      -> "WSP"
        | LWSP     -> "LWSP"
        | VCHAR    -> "VCHAR"
        | CHAR     -> "CHAR"
        | OCTET    -> "OCTET"
        | CTL      -> "CTL"
        | CR       -> "CR"
        | LF       -> "LF"
        | CRLF     -> "CRLF"
        | BIT      -> "BIT"
  end

let rec parser_of_rule_stateless (r : rule) : string =
  begin match r with
	| S_terminal t      -> String.uppercase (name_of_terminal t)
	| S_string   s      -> String.uppercase (name_of r)
	| S_concat (r1, r2) -> sprintf "%s ; %s" (parser_of_rule_stateless r1)
                                       (parser_of_rule_stateless r2)
	| S_reference s     -> s
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> type_string_of_rule r
	| S_repetition (i1, i2, r)   -> 
	   let ty = type_string_of_rule r in
	   if ty = "char" then "string" else ty
	| S_element_list (i1, i2, r) -> type_string_of_rule r
	| S_hex_range (i1, i2)       -> type_string_of_rule r
	| S_any_except (r1, r2)      -> type_string_of_rule r
  end

let rec parser_of_rule_stateful (r : rule) (name : string) (j : int) : string * string =
  begin match r with
	| S_terminal t      -> (sprintf "%s%d = %s" name j
                                        (String.uppercase (name_of_terminal t)),
                                sprintf "%s%d" name j)
	| S_string   s      -> (String.uppercase (name_of r), name ^ " = " ^ s)
	| S_concat (r1, r2) -> 
	   begin match has_state r1, has_state r2 with
	         | false, false -> (sprintf "%s ; %s" (parser_of_rule_stateless r1)
                                            (parser_of_rule_stateless r2),
                                    "")
	         | false, true  -> 
                    let (seq, state) = parser_of_rule_stateful r2 name j in
                    (sprintf "%s ; %s" (parser_of_rule_stateless r1)
                             seq,
                     state)
	         | true,  false -> 
                    let (seq, state) = parser_of_rule_stateful r1 name j in
                    (sprintf "%s ; %s" seq
                             (parser_of_rule_stateless r2),
                     state)
	         | true,  true  ->
                    let (seq1, state1) = parser_of_rule_stateful r1 name j in
                    let (seq2, state2) = parser_of_rule_stateful r2 name (j+1) in
                    (sprintf "%s ; %s" seq1 seq2,
                     sprintf "%s, %s" state1 state2)
	   end
	| S_reference s     -> (sprintf "%s%d = %s" name j s,
                                sprintf "%s%d" name j)
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> parser_of_rule_stateful r name j
	| S_repetition (i1, i2, r)   -> parser_of_rule_stateful r name j
	| S_element_list (i1, i2, r) -> parser_of_rule_stateful r name j
	| S_hex_range (i1, i2)       -> ("HEX", "RANGE")
	| S_any_except (r1, r2)      -> parser_of_rule_stateful r1 name j
   end

let rec parser_of_rule (r : rule) (name : string) (j : int) : string =
  begin match r with
	(*| S_concat (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  ->
                    begin match r2 with
                           | S_concat (r3, r4) ->
                              sprintf "%s\n%s"
                                      (parser_of_rule (S_concat (r1, r3)) name j) 
                                      (parser_of_rule r4 name (j+1))
                           | _ -> (match (parser_of_rule_stateful r "x" 0) with
                                  | seq, state ->
                                     sprintf "  | %s { %s }" seq state)
                    end
                 | true,  false -> (match (parser_of_rule_stateful r "x" 0) with
                                   | seq, state ->
                                      sprintf "  | %s { %s }" seq state) 
                 | true,  true  ->
                    sprintf "%s\n%s" (parser_of_rule r1 name j)
                            (parser_of_rule r2 name (j + 1))
           end*)
        | S_alt (r1, r2) ->
           let (seq1, state1) = parser_of_rule_stateful r1 "x" 0 in
           if is_alt r2 then
             sprintf "  | %s { %s %s }\n%s" 
                     seq1 (name_of r1) state1
                     (parser_of_rule r2 name j)
           else
             let (seq2, state2) = parser_of_rule_stateful r2 "x" 0 in
             sprintf "  | %s { %s %s }\n  | %s { %s %s }" 
                     seq1 (name_of r1) state1
                     seq2 (name_of r2) state2
        | _ -> match (parser_of_rule_stateful r "x" 0) with
                                   | seq, state ->
                                      sprintf "  | %s { %s }" seq state 
  end

let parser_of_rule_definition (rd : rule_definition) : string =
  let b = Buffer.create 16 in
  Buffer.add_string b rd.s_name ;
  Buffer.add_string b ":\n" ;
  Buffer.add_string b (parser_of_rule rd.s_rule rd.s_name 0) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b


let rule_to_file (oc : out_channel) (t : rule_definition -> string) (r : rule_definition) : unit =
  let s = t r in
  output oc s 0 (length s)

let _ =
  let lexbuf = Lexing.from_channel (open_in "ex.abnf") in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
  let types = open_out "gen/types.ml" in
  List.iter (rule_to_file types string_of_rule_definition) rules ;
  close_out types ;
  let lexer = open_out "gen/lexer.mll" in
  List.iter (rule_to_file lexer regex_of_rule_definition) rules ;
  List.iter (rule_to_file lexer lexer_of_rule_definition) rules ;
  close_out lexer ;
  let start =
    begin match rules with
          | h :: t -> h
          | [] -> failwith "no rules"
    end in
  let parser = open_out "gen/parser.mly" in
  fprintf parser "%%start <Type.%s option> %s\n" start.s_name start.s_name ;
  List.iter (rule_to_file parser parser_of_rule_definition) rules ;
  close_out parser
