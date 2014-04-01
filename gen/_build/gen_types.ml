open Abnf_syntaxtree

open Buffer
open Printf
open Str

let type_name (rd : rule_definition) : string =
  let r = Str.regexp "-" in
  Str.global_replace r "_" rd.s_name

let is_alt (r : rule) : bool =
  begin match r with
	| S_alt (_, _) -> true
	| _            -> false
  end

let rec has_state (r : rule) : bool =
  begin match r with
	| S_terminal t      -> true
	| S_string   s      -> false
	| S_concat (r1, r2) -> has_state r1 || has_state r2
	| S_reference s     -> true
	| S_alt (r1, r2)    -> true
	| S_bracket r                -> has_state r
	| S_repetition (i1, i2, r)   -> has_state r
	| S_element_list (i1, i2, r) -> has_state r
	| S_hex_range (i1, i2)       -> true
	| S_any_except (r1, r2)      -> has_state r1 || has_state r2
  end

let rec name_of (r : rule) : string = 
  begin match r with
	| S_terminal t      -> "???"
	| S_string   s      -> s
	| S_concat (r1, r2) -> "???"
	| S_reference s     -> "???"
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> "???"
	| S_repetition (i1, i2, r)   -> "???"
	| S_element_list (i1, i2, r) -> "???"
	| S_hex_range (i1, i2)       -> "???"
	| S_any_except (r1, r2)      -> "???"
  end

let type_string_of_terminal (t : terminal) : string =
  begin match t with
	| ALPHA    -> "string"
	| UPALPHA  -> "string"
	| LOALPHA  -> "string"
	| DIGIT    -> "int"
	| HEXDIGIT -> "int"
	| DQUOTE   -> "char"
	| SP       -> "char"
	| HTAB     -> "char"
	| WSP      -> "char"
	| LWSP     -> "string"
	| VCHAR    -> "string"
	| CHAR     -> "string"
	| OCTET    -> "int"
	| CTL      -> "string"
	| CR       -> "string"
	| LF       -> "string"
	| CRLF     -> "string"
	| BIT      -> "int"
  end
let rec type_string_of_rule (r : rule) : string =
  begin match r with
	| S_terminal t      -> type_string_of_terminal t
	| S_string   s      -> ""
	| S_concat (r1, r2) -> 
	   begin match has_state r1, has_state r2 with
	   | false, false -> ""
	   | false, true  -> type_string_of_rule r2
	   | true,  false -> type_string_of_rule r1
	   | true,  true  -> sprintf "%s * %s" (type_string_of_rule r1) (type_string_of_rule r2)
	   end
	| S_reference s     -> s
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> type_string_of_rule r
	| S_repetition (i1, i2, r)   -> type_string_of_rule r
	| S_element_list (i1, i2, r) -> type_string_of_rule r
	| S_hex_range (i1, i2)       -> type_string_of_rule r
	| S_any_except (r1, r2)      -> type_string_of_rule r
  end

let rec string_of_rule (r : rule) : string =
  begin match r with
	| S_alt (r1, r2) ->
	   if is_alt r2 then
	      sprintf "\n  | %s%s" (if has_state r1 then
					     sprintf "%s of %s" (name_of r1) (string_of_rule r1)
					   else
					     name_of r1) (string_of_rule r2)
	    else
	      sprintf "\n  | %s\n  | %s" (if has_state r1 then
					     sprintf "%s of %s" (name_of r1) (string_of_rule r1)
					   else
					     name_of r1)
					  (if has_state r2 then
					     sprintf "%s of %s" (name_of r2) (string_of_rule r2)
					   else
					     name_of r2)
	| _ -> type_string_of_rule r
  end

let string_of_rule_definition (rd : rule_definition) : string =
  let b = Buffer.create 16 in
  Buffer.add_string b (sprintf "type %s = " (type_name rd)) ;
  Buffer.add_string b (string_of_rule rd.s_rule) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b
  

let _ =
  let lexbuf = Lexing.from_channel (open_in "type.abnf") in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
  List.iter (fun rule -> print_endline (string_of_rule_definition rule)) rules ;

