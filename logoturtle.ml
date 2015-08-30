open Cairo


type iter = int
type name = string
type param = string

type arg =
  | Number of float
  | Var    of param

type command =
   | Forward of arg
   | Right of arg
   | Left  of arg
   | Repeat of arg * command list
   | Call of name * arg list
   | Proc of name * param list * command list

exception ArgumentException of string

let square = Repeat ((Number 4.0), [Forward (Number 1.0); Right (Number 90.)]);;
let star   = Repeat ((Number 5.0), [Forward (Number 1.0); Right (Number 144.)]);;
let flower = [ Proc ("square", ["len"],
                     [ Repeat ((Number 4.0),
                               [(Forward (Var "len"));
                                Right (Number 90.)])
                    ]);
               Repeat ((Number 36.0),
                       [ Right (Number 10.);
                         Call ("square", [(Number 0.4)])])
             ];;

module StringMap = Map.Make(String)

type state = { mutable x: float;
               mutable y: float;
               mutable heading: float;
               mutable cr: Cairo.context;
               mutable symbol_table: (bytes, param list * command list) Hashtbl.t }



let create () = let surface = Cairo.Image.create Cairo.Image.ARGB32 200 200 in
                let ctx = Cairo.create surface in
                let table = Hashtbl.create 100 in
                Cairo.translate ctx 100. 100.;
                Cairo.scale ctx 100. 100.;
                Cairo.set_line_width ctx 0.01;
                Cairo.set_source_rgb ctx 0. 0. 0.;
                Cairo.set_line_join ctx JOIN_MITER;
                Cairo.set_line_cap ctx SQUARE;
                Cairo.move_to ctx 0. 0.;
                { x = 0.; y = 0.; heading = 0.; cr = ctx; symbol_table = table }

let base_state = create ();;

let turn n state = state.heading <- state.heading +. n
let pi = 4.0 *. atan(1.0)
let forward n state = let r = state.heading *. pi /. 180.0 in
                      let dx = n *. cos(r) in
                      let dy = n *. sin(r) in
                      state.x <- state.x +. dx;
                      state.y <- state.y +. dy;
                      print_string ((string_of_float state.x) ^ " " ^ (string_of_float state.y) ^ "\n");
                      Cairo.line_to state.cr state.x state.y


let write_out state = let surface = Cairo.get_target state.cr in
                      Cairo.stroke state.cr;
                      Cairo.PNG.write surface "graphics.png"


let rec getValue env = function
  | Number n -> n
  | Var name -> getValue env (StringMap.find name env)

let rec eval state env exp =
  match exp with
    | Forward arg -> forward (getValue env arg) state
    | Right arg    -> turn (getValue env arg) state
    | Left  arg    -> turn ~-.(getValue env arg) state
    | Repeat (arg, cmd) ->
       let n = int_of_float (getValue env arg) in
       for i = 1 to n do
         List.iter (eval state env) cmd
       done
    | Proc (name, ps, cmds) -> Hashtbl.add state.symbol_table name (ps, cmds)
    | Call (name, args) ->
       let ps, cmds = Hashtbl.find state.symbol_table name in
       if List.length ps <> List.length args then
         raise (ArgumentException "argument count mistmatch")
       else
         let extend_env = List.fold_left2
                            (fun env key value -> StringMap.add key value env)
                            env ps args in
         List.iter (eval state extend_env) cmds


let printArg = function
  | Number n -> string_of_float n
  | Var    name -> name

let rec print_command cmd =
  match cmd with
    | Forward n -> print_string ("Forward " ^ (printArg n) ^ " ")
    | Right   n -> print_string ("Right " ^ (printArg n) ^ " ")
    | Left    n -> print_string ("Left  " ^ (printArg n) ^ " ")
    | Repeat (n, cmd) ->
       print_string ("Repeat " ^ (printArg n) ^ " [ ");
       List.iter print_command cmd;
       print_string " ] "
    | Call (name, args) ->
       print_string (name ^ (String.concat  " " (List.map printArg args)) ^ " ")
    | Proc (name, params, cmds) ->
       print_string ("to " ^ name ^
                       (String.concat " " (List.map (fun x -> ":" ^ x) params))
                         ^ "\n");
       List.iter print_command cmds;
       print_string "\nend\n"

let rec print_commands cmds =
  match cmds with
    | [] -> ()
    | h :: t -> print_command h; print_commands t


let eval_command command = let base_env = StringMap.empty in
                           eval base_state base_env command;
                           write_out base_state

let eval_commands cmds = let base_env = StringMap.empty in
                         List.iter (eval base_state base_env) cmds;
                         write_out base_state

(*
let () = eval_commands flower
 *)

(* Uncomment to test
let () = eval base_state star;
         write_out base_state
*)
