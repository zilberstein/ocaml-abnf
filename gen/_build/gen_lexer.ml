open Abnf_syntaxtree

open Buffer
open Printf
open Str
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

let regex_of_terminal (t : terminal) : string =
  begin match t with
	| ALPHA    -> "['A'-'Z' 'a'-'z']"
	| UPALPHA  -> "['A'-'Z']"
	| LOALPHA  -> "['a'-'z']"
	| DIGIT    -> "['0'-'9']"
	| HEXDIGIT -> "['A'-'Z' 'a'-'z' '0'-'9']"
	| DQUOTE   -> "'\"'"
	| SP       -> "' '"
	| HTAB     -> "'\t'"
	| WSP      -> "['\t' ' ']"
	| LWSP     -> "?"
	| VCHAR    -> "?"
	| CHAR     -> "?"
	| OCTET    -> "?"
	| CTL      -> "?"
	| CR       -> "'\\r'"
	| LF       -> "'\\n'"
	| CRLF     -> "\"\\r\\n\""
	| BIT      -> "['0' '1']"
  end

let rec regex_of_inner_rule (r : rule) : string =
  begin match r with
	| S_terminal t      -> regex_of_terminal t
	| S_string   s      -> sprintf "\"%s\"" s
	| S_concat (r1, r2) -> sprintf "%s %s" (regex_of_inner_rule r1) (regex_of_inner_rule r2)
	| S_reference s     -> ""
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| S_bracket r                -> regex_of_inner_rule r
	| S_repetition (io1, io2, r) -> sprintf "%s%s" (regex_of_inner_rule r)
                       (begin match io1, io2 with
                              | None, None       -> "*"
                              | None, Some i     -> sprintf "{,%d}" i
                              | Some i, None     -> if i = 1 then "+" else sprintf "{%d,}" i
                              | Some i1, Some i2 ->
                                 if i1 = i2 then 
                                   sprintf "{%d}" i1
                                 else
                                   sprintf "{%d,%d}" i1 i2
                        end)      
	| S_element_list (i1, i2, r) -> regex_of_inner_rule r
	| S_hex_range (i1, i2)       -> regex_of_inner_rule r
	| S_any_except (r1, r2)      -> regex_of_inner_rule r
  end


let rec regex_of_rule (r : rule) (name : string) (j : int) : string =
  begin match r with
	| S_concat (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  ->
                    begin match r2 with
                           | S_concat (r3, r4) -> sprintf "%s\n%s"
                                                          (regex_of_rule (S_concat (r1, r3)) name j) 
                                                          (regex_of_rule r4 name (j+1))
                           | _ -> sprintf "let %s%d = %s" name j (regex_of_inner_rule r)
                     end
                 | true,  false -> sprintf "let %s%d = %s" name j (regex_of_inner_rule r)
                 | true,  true  -> sprintf "%s\n%s" (regex_of_rule r1 name j)
                                           (regex_of_rule r2 name (j + 1))
           end
        | S_alt (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  -> regex_of_rule r2 name j 
                 | true,  false -> regex_of_rule r1 name j
                 | true,  true  -> sprintf "%s\n%s" (regex_of_rule r1 name j)
                                           (regex_of_rule r2 name (j + 1))
           end
	| S_reference s -> ""
        | _             -> sprintf "let %s%d = %s" name j (regex_of_inner_rule r)
  end

let regex_of_rule_definition (rd : rule_definition) : string =
  let b = Buffer.create 16 in
  Buffer.add_string b (regex_of_rule rd.s_rule rd.s_name 0) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b

let rec lexer_of_rule_stateless (r : rule) : string =
  "read lexbuf"

let rec lexer_of_rule_stateful (r : rule) : string =
  sprintf "%s (Lexing.lexeme lexbuf)" (String.uppercase (name_of r))

let rec lexer_of_rule (r : rule) (name : string) (j : int) : string =
  begin match r with
	| S_concat (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  ->
                    begin match r2 with
                           | S_concat (r3, r4) -> sprintf "%s\n%s"
                                                          (lexer_of_rule (S_concat (r1, r3)) name j) 
                                                          (lexer_of_rule r4 name (j+1))
                           | _ -> sprintf "%s%d\t{ %s }" name j (lexer_of_rule_stateless r)
                     end
                 | true,  false -> sprintf "%s%d\t{ %s }" name j (lexer_of_rule_stateful r)
                 | true,  true  -> sprintf "%s\n%s" (lexer_of_rule r1 name j)
                                           (lexer_of_rule r2 name (j + 1))
           end
        | S_alt (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  -> lexer_of_rule r2 name j 
                 | true,  false -> lexer_of_rule r1 name j
                 | true,  true  -> sprintf "%s\n%s" (lexer_of_rule r1 name j)
                                           (lexer_of_rule r2 name (j + 1))
           end
	| S_reference s -> ""
        | _             -> sprintf "%s%d\t{ %s }" name j (lexer_of_rule_stateful r)
  end

let lexer_of_rule_definition (rd : rule_definition) : string =
  let b = Buffer.create 16 in
  Buffer.add_string b (lexer_of_rule rd.s_rule rd.s_name 0) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b
