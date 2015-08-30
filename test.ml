open Core.Std
open Lexer
open Lexing


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
     fprintf stderr "%a: %s\n" print_position lexbuf msg;
     exit (-1)
  | Parser.Error ->
     fprintf stderr "%a: syntax error\n" print_position lexbuf;
     exit (-1)

let rec parse_and_print lexbuf =
  Logoturtle.print_commands (parse_with_error lexbuf)

let rec parse_print_and_eval lexbuf =
  let ast_list = parse_with_error lexbuf in
  Logoturtle.print_commands ast_list;
  print_string "\nnow evaling\n";
  Logoturtle.eval_commands ast_list


let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_print_and_eval lexbuf;
  print_string "\n";
  In_channel.close inx

let () =
  Command.basic ~summary: "Parse and interpet Logo"
                Command.Spec.(empty +> anon ("filename" %:file))
                loop
  |> Command.run
