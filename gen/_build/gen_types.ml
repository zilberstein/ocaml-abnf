open Abnf_syntaxtree

open Aux
open Buffer
open Printf
open Str

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
