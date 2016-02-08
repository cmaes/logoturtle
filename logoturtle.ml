type iter = int
type name = string
type param = string

type value =
  | VBool of bool
  | VFloat of float

type expr =
  | Var        of param
  | Bool       of bool
  | Number     of float
  | UnaryFunc  of param * expr
  | BinaryFunc of param * expr * expr
  | Plus       of expr * expr
  | Minus      of expr * expr
  | Times      of expr * expr
  | Divide     of expr * expr
  | Negate     of expr
  | Or         of expr * expr
  | And        of expr * expr
  | Not        of expr
  | Less       of expr * expr
  | Greater    of expr * expr
  | Equal      of expr * expr
  | NEqual     of expr * expr
  | LessEq     of expr * expr
  | GreaterEq  of expr * expr

type mapvalue =
  | Val       of value
  | UnaryVal  of (value -> value)
  | BinaryVal of (value -> value -> value)

type command =
  | Stop
  | Forward of expr
  | Back    of expr
  | Right   of expr
  | Left    of expr
  | Repeat  of expr * command list
  | Call    of name * expr list
  | Procdef of name * param list * command list
  | If      of expr * command list

type proc =
  | PrimitiveProc of int * (value list -> state -> unit)
  | UserProc      of param list * command list
and
  state = { mutable x: float;
            mutable y: float;
            mutable heading: float;
            mutable pendown: bool;
            mutable cr: Turtlegraphics.turtlecontext;
            mutable symbol_table: (bytes, proc) Hashtbl.t }


exception ArgumentException of string
exception RuntimeException of string

let square = Repeat ((Number 4.0),
                     [Forward (Number 1.0); Right (Number 90.)]);;
let star   = Repeat ((Number 5.0),
                     [Forward (Number 1.0); Right (Number 144.)]);;
let flower = [ Procdef ("square", ["len"],
                     [ Repeat ((Times (Number 2.0, Number 2.0)),
                               [(Forward (Var "len"));
                                (Right   (Number 90.))])
                    ]);
               Repeat ((Number 36.0),
                       [ Right (Number 10.);
                         Call ("square", [(Number 0.4)])])
             ];;

let tree = [ Procdef ("tree", ["size"],
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



let pi = 4.0 *. atan(1.0);;

type color = { r: float; g: float; b: float }

let logocolors = [| { r = 0.; g =  0.; b =  0.}; (* black *)
                    { r = 0.; g =  0.; b =  1.}; (* blue *)
                    { r = 0.; g = 1.0; b = 0.};  (* green *)
                    { r = 0.; g = 1.0; b = 1.0};  (* cyan *)
                    { r = 1.0; g = 0.0; b = 0.0};  (* red *)
                    { r = 1.; g = 0.; b = 1.};     (* magenta *)
                    { r = 1.; g = 1.; b = 0.};     (* yellow *)
                    { r = 0.; g = 0.; b = 0.};     (* white *)
                    {r = 0.64705882352; g = 0.16470588235; b = 0.16470588235}; (* brown *)
                    {r = 210. /. 255. ; g = 180. /. 255.; b = 140. /. 255. }; (* tan *)
                    {r = 0.; g = 0.50196078431; b = 0.};  (* green *)
                    {r = 0.49803921568; g = 1.0; b = 0.82745098039}; (* aqua *)
                    {r = 0.98039215686; g = 0.50196078431; b = 0.44705882352}; (* salmon *)
                    {r = 0.50196078431; g = 0.; b = 0.50196078431}; (* purple *)
                    {r = 1.; g = 0.64705882352; b = 0.}; (* orange *)
                    {r = 0.50196078431; g = 0.50196078431; b =0.50196078431} |] (* gray *)

let turn n state = state.heading <- state.heading +. n

let domove state = if state.pendown then
                     Turtlegraphics.line_to state.cr state.x state.y
                   else
                     Turtlegraphics.move_to state.cr state.x state.y

let forward n state = let r = (state.heading *. pi /. 180.0) -. (pi /. 2.0) in
                      let dx = n *. cos(r) in
                      let dy = n *. sin(r) in
                      state.x <- state.x +. dx;
                      state.y <- state.y +. dy;
                      (* print_string ((string_of_float state.x) ^ " " ^ (string_of_float state.y) ^ "\n"); *)
                      domove state

(* primitive procedures *)

let penup lst state = match lst with
    | [] -> Turtlegraphics.stroke state.cr;
            state.pendown <- false
    | _ -> raise (ArgumentException "penup expected no arguments")
let pendown lst state = match lst with
    | [] -> state.pendown <- true
    | _ -> raise (ArgumentException "pendown expected no arguments")

let setpencolor lst state = match lst with
    | [VFloat nfloat] -> let n = (int_of_float nfloat) in
                         if (n >= 0 && n < 16) then
                           let clr = logocolors.(n) in
                           Turtlegraphics.stroke state.cr;
                           Turtlegraphics.set_source_rgb state.cr clr.r clr.g clr.b;
                           Turtlegraphics.move_to state.cr state.x state.y
                         else
                           raise (ArgumentException "Invalid color specification")
    | _ -> raise (ArgumentException "setpencolor expected one numeric argument")
let setpensize lst state = match lst with
    | [VFloat size] -> Turtlegraphics.stroke state.cr;
                       Turtlegraphics.move_to state.cr state.x state.y;
                       Turtlegraphics.set_line_width state.cr size
    | _ -> raise (ArgumentException "setpensize expected one numeric argument")

let home lst state = match lst with
  | [] -> Turtlegraphics.stroke state.cr;
          state.x <- 0.;
          state.y <- 0.;
          state.heading <- 0.;
          Turtlegraphics.move_to state.cr state.x state.y
  | _ -> raise (ArgumentException "home expected no arguments")

let seth lst state = match lst with
  | [ VFloat angle ] -> state.heading <- angle
  | _ -> raise (ArgumentException "seth expected one numeric argument")

let setx lst state = match lst with
  | [ VFloat x ] -> state.x <- x;
                    domove state
  | _ -> raise (ArgumentException "setx expected one numeric argument")

let sety lst state = match lst with
  | [ VFloat y ] -> state.y <- ~-.y;
                    domove state
  | _ -> raise (ArgumentException "sety expected one numeric argument")

let setxy lst state = match lst with
  | VFloat x :: VFloat y :: [] -> state.x <- x;
                                  state.y <- ~-.y;
                                  domove state
  | _ -> raise (ArgumentException "setxy expected two numeric arguments")

let create procs names =
  let ctx = Turtlegraphics.create_context 1024 1024 in
  let table = Hashtbl.create 100 in

  (* add primitive procedures to hash table *)
  List.iter2 (fun name proc -> Hashtbl.add table name proc) names procs;

  { x = 0.; y = 0.; heading = 0.; pendown = true; cr = ctx; symbol_table = table }

let base_state = create [PrimitiveProc (0, penup);
                         PrimitiveProc (0, penup);
                         PrimitiveProc (0, pendown);
                         PrimitiveProc (0, pendown);
                         PrimitiveProc (1, setpencolor);
                         PrimitiveProc (1, setpensize);
                         PrimitiveProc (0, home);
                         PrimitiveProc (1, seth);
                         PrimitiveProc (1, seth);
                         PrimitiveProc (1, setx);
                         PrimitiveProc (1, sety);
                         PrimitiveProc (2, setxy)]
                        ["penup";
                         "pu";
                         "pendown";
                         "pd";
                         "setpencolor";
                         "setpensize";
                         "home";
                         "seth";
                         "setheading";
                         "setx";
                         "sety";
                         "setxy"];;


let write_out state filename = Turtlegraphics.write_out state.cr filename

(* primitive functions *)

let unary_float_value name g = UnaryVal
                                 (function
                                   | VFloat f ->
                                      let c = (g f) in
                                      (* print_endline (name ^ ":" ^ (string_of_float f) ^ "=" ^
                                                       (string_of_float c)); *)
                                      VFloat c
                                   | VBool b -> raise (ArgumentException (name ^ " expected number")))


let sin_val = unary_float_value "sin" (fun deg -> sin (deg *. pi /. 180.))
let cos_val = unary_float_value "cos" (fun deg -> cos (deg *. pi /. 180.))
let exp_val = unary_float_value "exp" exp
let ln_val  = unary_float_value "ln" log
let rand_val = unary_float_value "random" (fun flt -> float_of_int (Random.int (int_of_float flt)))
let power_val = BinaryVal (fun base exp -> match (base, exp) with
                                           | (VFloat a, VFloat n) -> VFloat (a ** n)
                                           | _, _ -> raise (ArgumentException "power expected two numeric arguments"))

let create_base_env funcs names = let empty = StringMap.empty in
                                  List.fold_left2 (fun env key func -> StringMap.add key func env)
                                                  empty names funcs

let base_env = create_base_env [sin_val; cos_val; exp_val; ln_val; power_val; rand_val]
                               ["sin";   "cos";   "exp";   "ln";  "power";   "random" ]

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
  | VBool  b -> raise (RuntimeException "Number expected")

let bool_of_value = function
  | VBool b -> b
  | VFloat f -> raise (RuntimeException "Boolean expected")

let value_of_mapval = function
  | Val v -> v
  | _ -> raise (RuntimeException "Value expected")

let unaryval_of_mapval = function
  | UnaryVal v -> v
  | _ -> raise (RuntimeException "Function of 1 argument expected")

let binaryval_of_mapval = function
  | BinaryVal v -> v
  | _ -> raise (RuntimeException "Function of 2 arguments expected")

let lookup_var name env = try (StringMap.find name env) with
                           | Not_found -> raise (RuntimeException ("Variable " ^ name ^ " not found"))

let rec eval_expr env = function
  | Var name           -> value_of_mapval (lookup_var name env)
  | Bool b             -> VBool  b
  | Number n           -> VFloat n
  | UnaryFunc (name, e) -> let myfunc = unaryval_of_mapval (StringMap.find name env) in
                           myfunc (eval_expr env e)
  | BinaryFunc (name,e1, e2) -> let myfunc = binaryval_of_mapval (StringMap.find name env) in
                                myfunc (eval_expr env e1) (eval_expr env e2)
  | Plus  (e1, e2)     -> (match (eval_expr env e1),  (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a +. b)
                           | _,_ -> raise (RuntimeException "Numbers expected in addition"))
  | Minus (e1, e2)     -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a -. b)
                           | _,_ -> raise (RuntimeException "Numbers expected in subtraction"))
  | Times (e1, e2)     -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VFloat (a *. b)
                           | _,_ -> raise (RuntimeException "Numbers expected in multiplication"))
  | Divide (e1, e2)    -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat 0.0 -> raise (RuntimeException "Division by zero")
                           | VFloat a, VFloat b   -> VFloat (a /. b)
                           | _,_ -> raise (RuntimeException "Numbers expected in division"))
  | Negate e           -> (match (eval_expr env e) with
                           | VFloat a -> VFloat ~-. a
                           | _ -> raise (RuntimeException "Number expected in negation"))
  | Or (e1, e2)        -> (match (eval_expr env e1) with
                           | VBool true -> VBool true
                           | VBool a -> (match  (eval_expr env e2) with
                                         | VBool b -> VBool (a || b)
                                         | _ -> raise (RuntimeException "Boolean expected in or"))
                           | _ -> raise (RuntimeException "Booleans expected in or"))
  | And (e1, e2)       -> (match (eval_expr env e1) with
                           | VBool false -> VBool false
                           | VBool a -> (match (eval_expr env e2) with
                                         | VBool b -> VBool (a && b)
                                         | _ -> raise (RuntimeException "Boolean expected in and"))
                           | _ -> raise (RuntimeException "Boolean expected in and"))
  | Not e              -> (match (eval_expr env e) with
                           | VBool a -> VBool (not a)
                           | _ -> raise (RuntimeException "Boolean expected in not"))
  | Less (e1, e2)      -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a < b)
                           | _,_ -> raise (RuntimeException "Number expected in < comparsion"))
  | Greater (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a > b)
                           | _,_ -> raise (RuntimeException "Number expected in > comparison"))
  | Equal   (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a = b)
                           | _,_ -> raise (RuntimeException "Number expected in == comparison"))
  | NEqual  (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a != b)
                           | _,_ -> raise (RuntimeException "Number expected in != comparsion"))
  | LessEq  (e1, e2)   -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a <= b)
                           | _,_ -> raise (RuntimeException "Number expected in <= comparison"))
  | GreaterEq (e1, e2) -> (match (eval_expr env e1), (eval_expr env e2) with
                           | VFloat a, VFloat b -> VBool (a >= b)
                           | _,_ -> raise (RuntimeException "Number expected in >= comparison"))


exception StopException

let lookup_proc symtable name = try Hashtbl.find symtable name with
                                | Not_found -> raise (RuntimeException ("Procedure " ^ name ^ " not found"))

let rec eval state env inst =
  match inst with
  | Stop        -> raise (StopException)
  | Forward exp -> forward (float_of_value (eval_expr env exp)) state
  | Back    exp -> forward ~-.(float_of_value (eval_expr env exp)) state
  | Right   exp -> turn (float_of_value    (eval_expr env exp)) state
  | Left    exp -> turn ~-.(float_of_value (eval_expr env exp)) state
  | Repeat (exp, cmd) ->
     let n = int_of_float (float_of_value (eval_expr env exp)) in
     for i = 1 to n do
       let extend_env = StringMap.add "repcount" (Val (VFloat (float_of_int i))) env in
       List.iter (eval state extend_env) cmd
     done
  | Procdef (name, ps, cmds) -> Hashtbl.add state.symbol_table name (UserProc (ps, cmds))
  | Call (name, args) ->
     let procedure  = lookup_proc state.symbol_table name in
     (match procedure with
       | UserProc (ps, cmds) -> (
         if List.length ps <> List.length args then
           raise (ArgumentException "argument count mismatch")
         else
           let extend_env = List.fold_left2
                              (fun env key exp -> StringMap.add key (Val (eval_expr env exp)) env)
                              env ps args in
           (try List.iter (eval state extend_env) cmds with
            | StopException -> ())
       )
       | PrimitiveProc (n, primproc) -> if n <> List.length args then
                                          raise (ArgumentException "argument count mismatch")
                                        else
                                          primproc (List.map (eval_expr env) args) state
     )
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
      | UnaryFunc (name, e) -> (7, name ^ "(" ^ (to_str 0 e) ^ ")")
      | BinaryFunc (name, e1, e2) -> (7, name ^ "(" ^ (to_str 0 e1) ^ " , " ^ (to_str 0 e2) ^ ")")
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
    | Stop          -> print_string "stop "
    | Forward e     -> print_string ("forward " ^ (string_of_expr e) ^ " ")
    | Back    e     -> print_string ("back " ^ (string_of_expr e ) ^ " ")
    | Right   e     -> print_string ("right " ^ (string_of_expr e) ^ " ")
    | Left    e     -> print_string ("left  " ^ (string_of_expr e) ^ " ")
    | Repeat (e, cmd) ->
       print_string ("repeat " ^ (string_of_expr e) ^ " [ ");
       List.iter print_command cmd;
       print_string " ] "
    | Call (name, args) ->
       print_string (name ^ " " ^ (String.concat  " " (List.map string_of_expr args)) ^ " ")
    | Procdef (name, params, cmds) ->
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


let eval_command command = eval base_state base_env command;
                           write_out base_state "graphics.png"

let eval_commands cmds = List.iter (eval base_state base_env) cmds;
                         write_out base_state "graphics.png"

let eval_commands_to_file cmds outfile = List.iter (eval base_state base_env) cmds;
                                         write_out base_state outfile

let create_state  = base_state

let eval_commands_return_state state cmds = Turtlegraphics.remove_turtle state.cr;
                                            List.iter (eval state base_env) cmds;
                                            write_out state "dummy.png";
                                            Turtlegraphics.draw_turtle state.cr state.x state.y
                                                                       (state.heading *. pi /. 180.);
                                            state


(*
let () = print_commands tree;
         print_string "\n";
         eval_commands tree
*)

(* Uncomment to test
let () = eval base_state star;
         write_out base_state
*)
