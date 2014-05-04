open Abnf_syntaxtree

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
	| S_repetition (i1, i2, r)   -> has_state r
	| S_element_list (i1, i2, r) -> has_state r
	| S_hex_range (i1, i2)       -> true
	| S_any_except (r1, r2)      -> has_state r1 || has_state r2
  end

let string_of_int_option (io : int option) : string =
  match io with
  | None -> ""
  | Some i -> string_of_int i

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

let rec name_priority (r : rule) : int =
  begin match r with
	| S_terminal _      -> 1
	| S_string   _      -> 2
	| S_concat (r1, r2) -> max (name_priority r1)
                                   (name_priority r2)
	| S_reference _     -> 1
	| S_alt (_, _)      -> failwith "illegal nesting"
	| S_bracket _              -> name_priority r
	| S_repetition (i1, i2, r) -> name_priority r
	| S_element_list (i1, i2, r) -> name_priority r
	| S_hex_range (i1, i2)       -> 1
	| S_any_except (r1, r2)      -> name_priority r1
  end

let rec name_of (r : rule) : string = 
  begin match r with
	| S_terminal t      -> name_of_terminal t
	| S_string   s      -> s
	| S_concat (r1, r2) -> 
	   if name_priority r1 < name_priority r2 then
	     name_of r2
           else
             name_of r1
	| S_reference s     -> s
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> name_of r
	| S_repetition (i1, i2, r)   ->
           sprintf "%s_%s_%s" (name_of r) (string_of_int_option i1)
                   (string_of_int_option i2)
	| S_element_list (i1, i2, r) -> "???"
	| S_hex_range (i1, i2)       -> "???"
	| S_any_except (r1, r2)      -> "???"
  end
