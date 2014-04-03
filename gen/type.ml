type state = string

type city = string

type street = 
  | Rd of string
  | St of string
  | Blvd of string
  | Ave of string

type address = int * street * city * state
