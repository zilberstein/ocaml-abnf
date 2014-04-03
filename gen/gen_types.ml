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

let rec terminal_has_state (t : terminal) : bool =
  begin match t with
	| ALPHA    -> true
	| UPALPHA  -> true
	| LOALPHA  -> true
	| DIGIT    -> true
	| HEXDIGIT -> true
	| DQUOTE   -> false
	| SP       -> false
	| HTAB     -> false
	| WSP      -> false
	| LWSP     -> false
	| VCHAR    -> true
	| CHAR     -> true
	| OCTET    -> true
	| CTL      -> true
	| CR       -> false
	| LF       -> false
	| CRLF     -> false
	| BIT      -> true
  end

let rec has_state (r : rule) : bool =
  begin match r with
	| S_terminal t      -> terminal_has_state t
	| S_string   s      -> false
	| S_concat (r1, r2) -> has_state r1 || has_state r2
	| S_reference s     -> true
	| S_alt (r1, r2)    -> true
	| S_bracket r                -> has_state r
	| S_repetition (i1, i2, r)   -> true
	| S_element_list (i1, i2, r) -> has_state r
	| S_hex_range (i1, i2)       -> true
	| S_any_except (r1, r2)      -> has_state r1 || has_state r2
  end

let rec name_of (r : rule) : string = 
  begin match r with
	| S_terminal t      -> "???"
	| S_string   s      -> s
	| S_concat (r1, r2) -> 
	   let n1 = name_of r1 in
	   let n2 = name_of r2 in
	   if n1 = "???" then n2 else n1
	| S_reference s     -> s
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> name_of r
	| S_repetition (i1, i2, r)   -> name_of r
	| S_element_list (i1, i2, r) -> "???"
	| S_hex_range (i1, i2)       -> "???"
	| S_any_except (r1, r2)      -> "???"
  end

let type_string_of_terminal (t : terminal) : string =
  begin match t with
	| ALPHA    -> "char"
	| UPALPHA  -> "char"
	| LOALPHA  -> "char"
	| DIGIT    -> "int"
	| HEXDIGIT -> "int"
	| DQUOTE   -> "char"
	| SP       -> "char"
	| HTAB     -> "char"
	| WSP      -> "char"
	| LWSP     -> "char"
	| VCHAR    -> "char"
	| CHAR     -> "char"
	| OCTET    -> "int"
	| CTL      -> "char"
	| CR       -> "char"
	| LF       -> "char"
	| CRLF     -> "char"
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
	| S_repetition (i1, i2, r)   -> 
	   let ty = type_string_of_rule r in
	   if ty = "char" then "string" else ty
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
