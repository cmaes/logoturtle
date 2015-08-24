open Cairo

type arg = float
type iter = int

type command =
   | Forward of arg
   | Turn of arg
   | Repeat of iter * command list


let square = Repeat (4, [Forward 1.0; Turn 90.]);;
let star   = Repeat (5, [Forward 1.0; Turn 144.]);;


type state = { mutable x: float;
               mutable y: float;
               mutable heading: float;
               mutable cr: Cairo.context }




let create () = let surface = Cairo.Image.create Cairo.Image.ARGB32 200 200 in
                let ctx = Cairo.create surface in
                Cairo.translate ctx 100. 100.;
                Cairo.scale ctx 100. 100.;
                Cairo.set_line_width ctx 0.1;
                Cairo.set_source_rgb ctx 0. 0. 0.;
                Cairo.set_line_join ctx JOIN_MITER;
                Cairo.set_line_cap ctx SQUARE;
                Cairo.move_to ctx 0. 0.;
                { x = 0.; y = 0.; heading = 0.; cr = ctx }

let test = create ();;


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

let rec eval state exp =
  match exp with
    | Forward n -> forward n state
    | Turn n    -> turn n state
    | Repeat (n, cmd) ->
       for i = 1 to n do
         List.iter (eval state) cmd
       done


let () = eval test star;
         write_out test
