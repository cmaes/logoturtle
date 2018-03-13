open Core
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

let rec parse_print_and_eval lexbuf outfile =
  let ast_list = parse_with_error lexbuf in
  Logoturtle.print_commands ast_list;
  print_string "\nnow evaling\n";
  Logoturtle.eval_commands_to_file ast_list outfile

let basename filename =
  let lst = String.split filename ~on:'/' in
  let base = (match List.hd (List.rev lst) with
              | None -> filename
              | Some s -> s) in
  let lst2 = String.split base ~on:'.' in
  match List.hd lst2 with
  | None -> base
  | Some s -> s

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let outfile = ((basename filename) ^ ".png") in
  print_string ("Writing output to " ^ outfile ^ "\n");
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_print_and_eval lexbuf outfile;
  print_string "\n";
  In_channel.close inx

let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic_spec    ~summary: "Parse and interpet Logo"
                        ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> loop filename ())

let () =  Command.run ~version:"1.0" ~build_info:"RWO" command
