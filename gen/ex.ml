type street = 
  | Rd of string
  | St of string
  | Blvd of string
  | Ave of string

type city = string

type state = string

type address = int * street * city * state

let parse_address (s : string) : address =
  let st = ref s in
  let a0 = ??? in
  let a1 = parse_terminal st in
  let a2 = parse_street !s in
  let a3 = parse_terminal st in
  let a4 = parse_city !s in
  let a5 = parse_string "," in
  let a6 = parse_terminal st in
  let a6 = ??? in
  (a0,a1,a2)

let parse_street (s : string) : street =
  let st = ref s in
  let a0 = ??? in
  let a1 = parse_terminal st in
  let a1 = ??? in
  (a0,a1,a2)

let parse_city (s : string) : city =
  let st = ref s in
  ???

let parse_state (s : string) : state =
  let st = ref s in
  ???
