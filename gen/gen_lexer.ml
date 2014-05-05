open Abnf_syntaxtree

open Aux
open Buffer
open Gen_types
open Printf
open Str
open String


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
	| S_hex_range (i1, i2)       -> sprintf "[ \\%d - \\%d ]" i1 i2
	| S_any_except (r1, r2)      -> regex_of_inner_rule r1
  end


let rec regex_of_rule (r : rule) (name : string) (c : unit -> int) : string =
  begin match r with
	| S_concat (r1, r2) ->
           sprintf "%s\n%s" (regex_of_rule r1 name c)
                   (regex_of_rule r2 name c)
        | S_alt (r1, r2) ->
           sprintf "%s\n%s" (regex_of_rule r1 name c)
                   (regex_of_rule r2 name c)
	| S_reference s -> ""
        | _             -> sprintf "let %s%d = %s" name (c ())
                                   (regex_of_inner_rule r)
  end

let regex_of_rule_definition (rd : rule_definition) : string =
  let c = counter 0 in
  let b = Buffer.create 16 in
  Buffer.add_string b (regex_of_rule rd.s_rule rd.s_name c) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b

let rec lexer_of_rule_stateless (r : rule) : string =
  "read lexbuf"

let rec lexer_of_rule_stateful (r : rule) : string =
  let t = type_string_of_rule r in
  let name = String.uppercase (name_of r) in
  if t = "int" then
    sprintf "%s (int_of_string(Lexing.lexeme lexbuf))" name
  else
    sprintf "%s (Lexing.lexeme lexbuf)" name

let rec lexer_of_rule (r : rule) (name : string) (c : unit -> int) : string =
  begin match r with
	| S_concat (r1, r2) ->
           sprintf "%s\n%s" (lexer_of_rule r1 name c)
                   (lexer_of_rule r2 name c)
        | S_alt (r1, r2) ->
           begin match has_state r1, has_state r2 with
                 | false, false -> ""
                 | false, true  -> lexer_of_rule r2 name c 
                 | true,  false -> lexer_of_rule r1 name c
                 | true,  true  -> sprintf "%s\n%s" (lexer_of_rule r1 name c)
                                           (lexer_of_rule r2 name c)
           end
	| S_reference s -> ""
        | _             -> sprintf "  %s%d\t{ %s }" name (c ()) (lexer_of_rule_stateful r)
  end

let lexer_of_rule_definition (rd : rule_definition) : string =
  let c = counter 0 in
  let b = Buffer.create 16 in
  Buffer.add_string b (lexer_of_rule rd.s_rule rd.s_name c) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b
