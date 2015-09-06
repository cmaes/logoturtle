open Lexing
open Turtlegraphics
open Logoturtle

module Html = Dom_html

let js = Js.string
let document = Html.window##document

let append_text e s = Dom.appendChild e (document##createTextNode (js s))

let replace_child p n =
  Js.Opt.iter (p##firstChild) (fun c -> Dom.removeChild p c);
  Dom.appendChild p n

exception SyntaxError of string

let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
     print_position lexbuf;
     exit (-1)
  | Parser.Error ->
     print_position lexbuf;
     exit (-1)

let rec parse_and_print lexbuf =
  Logoturtle.print_commands (parse_with_error lexbuf)

let rec parse_print_and_eval lexbuf state =
  let ast_list = parse_with_error lexbuf in
  Logoturtle.print_commands ast_list;
  print_string "\nnow evaling\n";
  Logoturtle.eval_commands_return_state state ast_list


let interpet d state str = let lexbuf = Lexing.from_string str in
                           parse_print_and_eval lexbuf state

let div = Html.createDiv document

let start d s  _ = Dom.appendChild document##body d;
                   Dom.appendChild d s.cr.cr;
                   Js._false


let _ =
  let state = Logoturtle.create_state in
  Html.window##onload <- Html.handler (start div state);
  Js.Unsafe.global##printOCAMLString <- Js.wrap_callback (fun s -> print_endline ("Hi " ^ (Js.to_string s)));
  Js.Unsafe.global##interpetLOGO <- Js.wrap_callback (fun s -> interpet div state (Js.to_string s))
