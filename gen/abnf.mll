{
  open Lexing
  open Parser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let alpha = ['A'-'Z' 'a'-'z']
let digit = ['0'-'9']
let hexdig = digit | ['a'-'f' 'A'-'F']
let vchar = [\x21-\x7E]
let char = [\x01=\x7F]
let ctl = [\x00-\x1F] | \x7F
let bit = 0 | 1

rule read = parse
             | alpha  { ALPHA (Lexing.lexeme lexbuf) }
             | digit  { DIGIT (Lexing.lexeme lexbuf) }
             | '"'    { DQUOTE }
             | ' '    { SP }
             | '\t'   { HTAB }
             | '\r'   { CR }
             | '\n'   { LF }
             | vchar  { VCHAR (Lexing.lexeme lexbuf) }
             | ctl    { CTL (Lexing.lexeme lexbuf) }
          
