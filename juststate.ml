(* This file contains a simple implementation that
   just modifies the state of turtle. It does not
   draw anything *)


type arg = float
type iter = int

type command =
   | Forward of arg
   | Turn of arg
   | Repeat of iter * command list


let sample = Repeat (10, [Forward 1.0; Turn 20.]);;


type state = { mutable x: float;
               mutable y: float;
               mutable heading: float }




let create () = { x = 0.; y = 0.; heading = 0. }

let test = create ();;


let turn n state = state.heading <- state.heading +. n
let pi = 4.0 *. atan(1.0)
let forward n state = let r = state.heading *. pi /. 180.0 in
                      let dx = n *. cos(r) in
                      let dy = n *. sin(r) in
                      state.x <- state.x +. dx;
                      state.y <- state.y +. dy


let rec eval state exp =
  match exp with
    | Forward n -> forward n state
    | Turn n    -> turn n state
    | Repeat (n, cmd) ->
       for i = 1 to n do
         List.iter (eval state) cmd
       done
