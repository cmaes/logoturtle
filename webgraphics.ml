open Lwt
open Js
module Html = Dom_html

let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f

type turtlecontext = { mutable cr : Html.canvasElement Js.t;
                       mutable ctx : Html.canvasRenderingContext2D Js.t;
                       mutable backup: Html.imageData Js.t;
                       mutable saved: bool;
                       mutable w : float;
                       mutable h : float;
                       mutable x : float;
                       mutable y : float;}

let document = Html.window##document


let stroke c  = print_endline "stroke";
                c.ctx##stroke ()
let move_to c x y = print_endline ("moveTo " ^ (string_of_float x) ^ " " ^ (string_of_float y));
                    c.x <- x;
                    c.y <- y;
                    c.ctx##moveTo (x, y)
let line_to c x y = print_endline ("lineTo " ^ (string_of_float x) ^ " " ^ (string_of_float y));
                    c.x <- x;
                    c.y <- y;
                    c.ctx##lineTo (x, y)
let set_line_width c w = c.ctx##stroke ();
                         c.ctx##beginPath ();
                         c.ctx##moveTo (c.x, c.y);
                         c.ctx##lineWidth <- w

let set_source_rgb c r g b = let rn = int_of_float (255.0 *. r) in
                               let gn = int_of_float (255.0 *. g) in
                               let bn = int_of_float (255.0 *. b) in
                               c.ctx##stroke ();
                               c.ctx##beginPath ();
                               c.ctx##moveTo (c.x, c.y);
                               c.ctx##strokeStyle <- Js.string ("rgb(" ^ string_of_int rn ^ "," ^
                                                                  string_of_int gn ^ "," ^
                                                                    string_of_int bn ^ ")")
let write_out c filename = stroke c;
                           print_endline ("NOT IMPLEMENTED: write_out " ^ filename )

let draw_turtle c x y h =
  let wf = c.w in
  let hf = c.h in
  let img = Opt.get
              (Opt.bind (document##getElementById (Js.string "turtleimg"))
                        Html.CoerceTo.img)
              (fun () -> error "can't find img element turtleimg") in
  stroke c;
  (* save to backup canvas *)
  c.backup <- c.ctx##getImageData (0., 0., wf, hf);
  c.saved <- true;

  (* draw turtle *)
  c.ctx##save ();
  c.ctx##translate ( x, y);
  c.ctx##rotate (h);
  c.ctx##drawImage_withSize (img, -25., -50. , 50., 50.);
  c.ctx##restore ()

let remove_turtle c = let saved = c.saved in
                      if saved then
                        (c.ctx##putImageData (c.backup, 0., 0.);
                         c.saved <- false;)

let create_context w h = let c = Html.createCanvas document in
                         c##width <- w;
                         c##height <- h;
                         let ctx = c##getContext (Html._2d_) in
                         let wf = float_of_int w in
                         let hf = float_of_int h in
                         ctx##translate (wf /. 2.0 +. 0.5, hf /. 2.0 +. 0.5);
                         ctx##beginPath ();
                         ctx##moveTo (0., 0.);
                         { cr = c;  ctx = ctx; backup = ctx##getImageData (0., 0., wf, hf); saved = false; w = wf; h = hf; x = 0.; y = 0.;}
