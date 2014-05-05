open Abnf_syntaxtree

open Buffer
open Printf
open String

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
	| S_string   s      -> 
           begin match s with
                 | "," -> "comma"
                 | "." -> "period"
                 | _ ->
                    let r = Str.regexp "[^A-Za-z0-9_]" in
                    Str.global_replace r "" s
           end
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

let counter (i : int) : (unit -> int) =
  let c = ref i in
  fun () -> c := !c + 1 ; !c

let rec get_tokens (r : rule) : (string * string option) list =
  begin match r with
	| S_concat (r1, r2) ->
          get_tokens r1 @ get_tokens r2
        | S_alt (r1, r2) ->
           get_tokens r1 @ get_tokens r2
        | S_reference s -> []
        | _ -> [(String.uppercase (name_of r),
                 if has_state r then
                   Some (type_string_of_rule r)
                 else
                   None)]
  end

let tokens_to_string (rd : rule_definition) : string =
  let tokens = get_tokens rd.s_rule in
  List.fold_right (fun (name, topt) s ->
        match topt with
        | None -> sprintf "%%token %s\n%s" name s
        | Some typ -> sprintf "%%token <%s> %s\n%s" typ name s)
       tokens ""
