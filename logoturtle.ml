open Cairo

type iter = int
type name = string
type param = string

type value =
  | VBool of bool
  | VFloat of float

type expr =
  | Var       of param
  | Bool      of bool
  | Number    of float
  | Plus      of expr * expr
  | Minus     of expr * expr
  | Times     of expr * expr
  | Divide    of expr * expr
  | Negate    of expr
  | Or        of expr * expr
  | And       of expr * expr
  | Not       of expr
  | Less      of expr * expr
  | Greater   of expr * expr
  | Equal     of expr * expr
  | NEqual    of expr * expr
  | LessEq    of expr * expr
  | GreaterEq of expr * expr

type command =
  | Stop
  | Forward of expr
  | Back    of expr
  | Right   of expr
  | Left    of expr
  | Repeat  of expr * command list
  | Call    of name * expr list
  | Proc    of name * param list * command list
  | If      of expr * command list

exception ArgumentException of string

let square = Repeat ((Number 4.0),
                     [Forward (Number 1.0); Right (Number 90.)]);;
let star   = Repeat ((Number 5.0),
                     [Forward (Number 1.0); Right (Number 144.)]);;
let flower = [ Proc ("square", ["len"],
                     [ Repeat ((Times (Number 2.0, Number 2.0)),
                               [(Forward (Var "len"));
                                (Right   (Number 90.))])
                    ]);
               Repeat ((Number 36.0),
                       [ Right (Number 10.);
                         Call ("square", [(Number 0.4)])])
             ];;

let tree = [ Proc ("tree", ["size"],
                   [ If ((Less (Var "size", Number 0.05)),
                         [ Forward (Var "size");
                           Back    (Var "size");
                           Stop ]);
                     Forward (Divide (Var "size", Number 3.));
                     Left    (Number 30.);
                     Call    ("tree", [(Times (Var "size", (Divide (Number 2., Number 3.))))]);
                     Right   (Number 30.);
                     Forward (Divide (Var "size", Number 6.));
                     Right   (Number 25.);
                     Call    ("tree", [(Divide (Var "size", Number 2.))]);
                     Left    (Number 25.);
                     Forward (Divide (Var "size", Number 3.));
                     Right   (Number 25.);
                     Call    ("tree", [(Divide (Var "size", Number 2.))]);
                     Left    (Number 25.);
                     Forward (Divide (Var "size", Number 6.));
                     Back    (Var "size")
                   ]);
             Call ("tree", [(Number 0.5)]);
           ];;


module StringMap = Map.Make(String)

type state = { mutable x: float;
               mutable y: float;
               mutable heading: float;
               mutable cr: Cairo.context;
               mutable symbol_table: (bytes, param list * command list) Hashtbl.t }


let pi = 4.0 *. atan(1.0);;

let create () = let surface = Cairo.Image.create Cairo.Image.ARGB32 800 800 in
                let ctx = Cairo.create surface in
                let table = Hashtbl.create 100 in
                (* paint background white *)
                Cairo.rectangle ctx 0.0 0.0 800. 800.;
                Cairo.set_source_rgb ctx 1.0 1.0 1.0;
                Cairo.fill ctx;

                (* setup turtle coordinates *)
                Cairo.translate ctx 400. 400.;
                Cairo.scale ctx 2. 2.;

                (* setup turtle line properties *)
                Cairo.set_line_width ctx 1.0;
                Cairo.set_source_rgb ctx 0. 0. 0.;
                Cairo.set_line_join ctx JOIN_MITER;
                Cairo.set_line_cap ctx SQUARE;
                Cairo.move_to ctx 0. 0.;
                { x = 0.; y = 0.; heading = 0.; cr = ctx; symbol_table = table }

let base_state = create ();;

let turn n state = state.heading <- state.heading +. n
let forward n state = let r = (state.heading *. pi /. 180.0) -. (pi /. 2.0) in
                      let dx = n *. cos(r) in
                      let dy = n *. sin(r) in
                      state.x <- state.x +. dx;
                      state.y <- state.y +. dy;
                      (* print_string ((string_of_float state.x) ^ " " ^ (string_of_float state.y) ^ "\n"); *)
                      Cairo.line_to state.cr state.x state.y


let write_out state filename = let surface = Cairo.get_target state.cr in
                               Cairo.stroke state.cr;
                               Cairo.PNG.write surface filename


(*let rec getValue env v =
  match v with
  | Bool   b -> VBool b
  | Number n -> VFloat n
  | Var name -> getValue env (StringMap.find name env)
 *)

let expr_of_value = function
  | VBool  b -> Bool b
  | VFloat f -> Number f

let float_of_value = function
  | VFloat f -> f
  | VBool  b -> failwith "Number expected"

let bool_of_value = function
  | VBool b -> b
  | VFloat f -> failwith "Boolean expected"

let rec eval_expr env = function
  | Var name           -> (StringMap.find name env)
  | Bool b             -> VBool  b
  | Number n           -> VFloat n
  | Plus  (e1, e2)     -> (match (eval_expr env e1),  (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a +. b)
                           | _,_ -> failwith "Numbers expected in addition")
  | Minus (e1, e2)     -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a -. b)
                           | _,_ -> failwith "Numbers expected in subtraction")
  | Times (e1, e2)     -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a *. b)
                           | _,_ -> failwith "Numbers expected in multiplication")
  | Divide (e1, e2)    -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat 0.0 -> failwith "Division by zero"
                           | VFloat a, VFloat b   -> VFloat (a /. b)
                           | _,_ -> failwith "Numbers expected in division")
  | Negate e           -> (match (eval_expr env e) with
                           | VFloat a -> VFloat ~-. a
                           | _ -> failwith "Numbers expected in negation")
  | Or (e1, e2)        -> (match (eval_expr env e1) with
                           | VBool true -> VBool true
                           | VBool a -> (match  (eval_expr env e2) with
                                         | VBool b -> VBool (a || b)
                                         | _ -> failwith "Boolean expected in or")
                           | _ -> failwith "Booleans expected in or")
  | And (e1, e2)       -> (match (eval_expr env e1) with
                           | VBool false -> VBool false
                           | VBool a -> (match (eval_expr env e2) with
                                         | VBool b -> VBool (a && b)
                                         | _ -> failwith "Boolean expected in and")
                           | _ -> failwith "Boolean expected in and")
  | Not e              -> (match (eval_expr env e) with
                           | VBool a -> VBool (not a)
                           | _ -> failwith "Boolean expected in not")
  | Less (e1, e2)      -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a < b)
                           | _,_ -> failwith "Number expected in < comparsion")
  | Greater (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a > b)
                           | _,_ -> failwith "Number expected in > comparison")
  | Equal   (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a = b)
                           | _,_ -> failwith "Number expected in == comparison")
  | NEqual  (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a != b)
                           | _,_ -> failwith "Number expected in != comparsion")
  | LessEq  (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a <= b)
                           | _,_ -> failwith "Number expected in <= comparison")
  | GreaterEq (e1, e2) -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a >= b)
                           | _,_ -> failwith "Number expected in >= comparison")


exception StopException

let rec eval state env inst =
  match inst with
  | Stop -> raise (StopException)
  | Forward exp -> forward (float_of_value (eval_expr env exp)) state
  | Back    exp -> forward ~-.(float_of_value (eval_expr env exp)) state
  | Right   exp -> turn (float_of_value    (eval_expr env exp)) state
  | Left    exp -> turn ~-.(float_of_value (eval_expr env exp)) state
  | Repeat (exp, cmd) ->
     let n = int_of_float (float_of_value (eval_expr env exp)) in
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
                          (fun env key exp -> StringMap.add key (eval_expr env exp) env)
                          env ps args in
       (try List.iter (eval state extend_env) cmds with
        | StopException -> ())
  | If (exp, cmds) -> if (bool_of_value (eval_expr env exp))
                      then List.iter (eval state env) cmds


(* prec  | operator  | operator
   level | character | name
   ==================================
       6 | - !       | unary minus, logical not
       5 | * /       | multiplication, division
       4 | + -       | addition, subtraction
       3 | < <= > >= | comparison operators
       2 | == !=     | equality operators
       1 | &&        | logical and
       0 | ||        | logical or

There are 8 levels of precedence.

*)



let string_of_expr e =
  let rec to_str n e =
    let (m, str) = match e with
      | Var name           -> (7, name)
      | Number n           -> (7, string_of_float n)
      | Bool  b            -> (7, string_of_bool b)
      | Negate e           -> (6, "-" ^ (to_str 0 e))
      | Not   b            -> (6, "!" ^ (to_str 0 b))
      | Times     (e1, e2) -> (5, (to_str 5 e1) ^ " * " ^ (to_str 6 e2))
      | Divide    (e1, e2) -> (5, (to_str 5 e1) ^ " / " ^ (to_str 6 e2))
      | Plus      (e1, e2) -> (4, (to_str 4 e1) ^ " + " ^ (to_str 5 e2))
      | Minus     (e1, e2) -> (4, (to_str 4 e1) ^ " - " ^ (to_str 5 e2))
      | Less      (e1, e2) -> (3, (to_str 3 e1) ^ " < " ^ (to_str 4 e2))
      | LessEq    (e1, e2) -> (3, (to_str 3 e1) ^ " <= " ^ (to_str 4 e2))
      | Greater   (e1, e2) -> (3, (to_str 3 e1) ^ " > " ^ (to_str 4 e2))
      | GreaterEq (e1, e2) -> (3, (to_str 3 e1) ^ " >= " ^ (to_str 4 e2))
      | Equal     (e1, e2) -> (2, (to_str 2 e1) ^ " == " ^ (to_str 3 e2))
      | NEqual    (e1, e2) -> (2, (to_str 2 e1) ^ " != " ^ (to_str 3 e2))
      | And       (b1, b2) -> (1, (to_str 1 b1) ^ " && " ^ (to_str 2 b2))
      | Or        (b1, b2) -> (0, (to_str 0 b1) ^ " || " ^ (to_str 1 b2))
    in
       if m < n then "(" ^ str ^ ")" else str
  in
     to_str (-1) e

let rec print_command cmd =
  match cmd with
    | Stop      -> print_string "stop"
    | Forward e -> print_string ("forward " ^ (string_of_expr e) ^ " ")
    | Back    e -> print_string ("back " ^ (string_of_expr e ) ^ " ")
    | Right   e -> print_string ("right " ^ (string_of_expr e) ^ " ")
    | Left    e -> print_string ("left  " ^ (string_of_expr e) ^ " ")
    | Repeat (e, cmd) ->
       print_string ("repeat " ^ (string_of_expr e) ^ " [ ");
       List.iter print_command cmd;
       print_string " ] "
    | Call (name, args) ->
       print_string (name ^ " " ^ (String.concat  " " (List.map string_of_expr args)) ^ " ")
    | Proc (name, params, cmds) ->
       print_string ("to " ^ name ^
                       (String.concat " " (List.map (fun x -> ":" ^ x) params))
                         ^ "\n");
       List.iter print_command cmds;
       print_string "\nend\n"
    | If (e, cmds) ->
       print_string ("if " ^ (string_of_expr e) ^ " [ ");
       List.iter print_command cmds;
       print_string  " ]\n"

let rec print_commands cmds =
  match cmds with
    | [] -> ()
    | h :: t -> print_command h; print_commands t


let eval_command command = let base_env = StringMap.empty in
                           eval base_state base_env command;
                           write_out base_state "graphics.png"

let eval_commands cmds = let base_env = StringMap.empty in
                         List.iter (eval base_state base_env) cmds;
                         write_out base_state "graphics.png"

let eval_commands_to_file cmds outfile = let base_env = StringMap.empty in
                                         List.iter (eval base_state base_env) cmds;
                                         write_out base_state outfile


(*
let () = print_commands tree;
         print_string "\n";
         eval_commands tree
*)

(* Uncomment to test
let () = eval base_state star;
         write_out base_state
*)
