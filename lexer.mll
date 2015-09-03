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


let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let comment = ';' [^ '\n' '\r']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id =  ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let param = ':' ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*


rule read =
  parse
  | white     { read lexbuf }
  | comment   { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }
  | float     { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"    { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | "false"   { BOOL (bool_of_string (Lexing.lexeme lexbuf)) }
  | "+"       { PLUS }
  | "-"       { MINUS }
  | "*"       { TIMES }
  | "/"       { DIVIDE }
  | "||"      { OR }
  | "&&"      { AND }
  | "!"       { NOT }
  | "<"       { LESS }
  | ">"       { GREATER }
  | "=="      { EQUAL }
  | "!="      { NOTEQUAL }
  | "<="      { LESSEQUAL }
  | ">="      { GREATEREQUAL }
  | "if"      { IF }
  | "stop"    { STOP }
  | "to"      { TO }
  | "end"     { END }
  | "forward" { FORWARD }
  | "fd"      { FORWARD }
  | "back"    { BACK }
  | "bk"      { BACK }
  | "right"   { RIGHT }
  | "rt"      { RIGHT }
  | "left"    { LEFT  }
  | "lt"      { LEFT  }
  | "repeat"  { REPEAT  }
  | "repcount" { REPCOUNT }
  | '['       { LEFT_BRACKET }
  | ']'       { RIGHT_BRACKET }
  | '('       { LEFT_PAREN }
  | ')'       { RIGHT_PAREN }
  | id        { ID (Lexing.lexeme lexbuf) }
  | param     { PARAM (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof       { EOF }
