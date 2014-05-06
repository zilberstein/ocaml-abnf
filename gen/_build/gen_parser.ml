open Abnf_syntaxtree
open Gen_types
open Gen_lexer

open Aux
open Buffer
open Printf
open String

let rec parser_of_rule_stateless (r : rule) (c : unit -> int): string =
  begin match r with
	| S_terminal t      -> 
           let _ = c () in
           String.uppercase (name_of_terminal t)
	| S_string   s      ->
           let _ = c () in
           String.uppercase (name_of r)
	| S_concat (r1, r2) ->
           sprintf "%s %s" (parser_of_rule_stateless r1 c)
                   (parser_of_rule_stateless r2 c)
	| S_reference s  ->
           let _ = c () in
           s
	| S_alt (r1, r2) -> failwith "illegal nesting"
	| _ ->
           let _ = c () in
           String.uppercase (name_of r)
  end

let rec parser_of_rule_stateful (r : rule) (name : string) (c : unit -> int) : string * string =
  begin match r with
	| S_terminal t      -> (sprintf "%s" (String.uppercase (name_of_terminal t)),
                                sprintf "$%d" (c ()))
	| S_string   s      -> (String.uppercase (name_of r), sprintf "$%d" (c ()))
	| S_concat (r1, r2) -> 
	   begin match has_state r1, has_state r2 with
	         | false, false -> (sprintf "%s %s" (parser_of_rule_stateless r1 c)
                                            (parser_of_rule_stateless r2 c),
                                    "")
	         | false, true  -> 
                    let seq1 = parser_of_rule_stateless r1 c in
                    let (seq2, state) = parser_of_rule_stateful r2 name c in
                    (sprintf "%s %s" seq1 seq2,
                     state)
	         | true,  false -> 
                    let (seq, state) = parser_of_rule_stateful r1 name c in
                    (sprintf "%s %s" seq
                             (parser_of_rule_stateless r2 c),
                     state)
	         | true,  true  ->
                    let (seq1, state1) = parser_of_rule_stateful r1 name c in
                    let (seq2, state2) = parser_of_rule_stateful r2 name c in
                    (sprintf "%s %s" seq1 seq2,
                     sprintf "%s, %s" state1 state2)
	   end
	| S_reference s     -> (sprintf "%s" s,
                                sprintf "$%d" (c ()))
	| S_alt (r1, r2)    -> failwith "illegal nesting"
	| _ -> (String.uppercase (name_of r),
                sprintf "$%d" (c ()))
  end

let parser_of_rule_constructed (r : rule) : string =
  if has_state r then
    let seq, state = parser_of_rule_stateful r "x" (counter 0) in
    sprintf "  | %s { %s (%s) }" seq (String.capitalize (name_of r)) state
  else
    sprintf "  | %s { %s }" (parser_of_rule_stateless r (counter 0))
            (String.capitalize (name_of r))

let rec parser_of_rule (r : rule) : string =
  begin match r with
        | S_alt (r1, r2) ->
           if is_alt r2 then
             sprintf "%s\n%s" 
                     (parser_of_rule_constructed r1)
                     (parser_of_rule r2)
           else
             sprintf "%s\n%s" 
                     (parser_of_rule_constructed r1)
                     (parser_of_rule_constructed r2)
        | _ ->
           if has_state r then
             let seq, state = parser_of_rule_stateful r "x" (counter 0) in
             sprintf "  | %s { %s }" seq state
           else
             sprintf "  | %s { %s }" (parser_of_rule_stateless r (counter 0))
                     (name_of r)
  end

let parser_of_rule_definition (rd : rule_definition) : string =
  let b = Buffer.create 16 in
  Buffer.add_string b rd.s_name ;
  Buffer.add_string b ":\n" ;
  Buffer.add_string b (parser_of_rule rd.s_rule) ;
  Buffer.add_string b "\n" ;
  Buffer.contents b


let rule_to_file (oc : out_channel) (t : rule_definition -> string) (r : rule_definition) : unit =
  let s = t r in
  output oc s 0 (length s)

let _ =
  let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in
  let rules = Abnf_parser.main Abnf_lexer.token lexbuf in
  let types = open_out "gen/types.ml" in
  List.iter (rule_to_file types string_of_rule_definition) (List.rev rules) ;
  close_out types ;
  let lexer = open_out "gen/lexer.mll" in
  fprintf lexer lexer_header ;
  List.iter (rule_to_file lexer regex_of_rule_definition) rules ;
  fprintf lexer "rule read = parse" ;
  List.iter (rule_to_file lexer lexer_of_rule_definition) rules ;
  close_out lexer ;
  let start =
    begin match rules with
          | h :: t -> h
          | [] -> failwith "no rules"
    end in
  let parser = open_out "gen/parser.mly" in
  fprintf parser "%%{\n  open Types\n%%}\n" ;
  List.iter (rule_to_file parser tokens_to_string) rules ;
  fprintf parser "%%start %s\n" start.s_name ;
  fprintf parser "%%type <Types.%s> %s\n%%%%\n" start.s_name start.s_name ;
  List.iter (rule_to_file parser parser_of_rule_definition) rules ;
  close_out parser
