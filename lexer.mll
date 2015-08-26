{
  open Lexing
  open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }

}


let int = '-'? ['0'-'9']['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
  parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "turn"    { TURN }
  | "forward" { FORWARD }
  | "repeat"  { REPEAT  }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { EOF }
