open Cairo
type turtlecontext = { mutable cr: Cairo.context; }

let stroke ctx = Cairo.stroke ctx.cr
let set_source_rgb ctx = Cairo.set_source_rgb ctx.cr
let move_to ctx = Cairo.move_to ctx.cr
let line_to ctx = Cairo.line_to ctx.cr
let set_line_width ctx = Cairo.set_line_width ctx.cr
let create_context w h = let surface = Cairo.Image.create Cairo.Image.ARGB32 w h in
                         let ctx = Cairo.create surface in
                         let wf = (float_of_int w) in
                         let hf = (float_of_int h) in
                         (* paint background white *)
                         Cairo.rectangle ctx 0.0 0.0 wf hf;
                         Cairo.set_source_rgb ctx 1.0 1.0 1.0;
                         Cairo.fill ctx;

                         (* setup turtle coordinates *)
                         Cairo.translate ctx (wf /. 2.0) (hf /. 2.0);
                         Cairo.scale ctx 2. 2.;

                         (* setup turtle line properties *)
                         Cairo.set_line_width ctx 1.0;
                         Cairo.set_source_rgb ctx 0. 0. 0.;
                         Cairo.set_line_join ctx JOIN_MITER;
                         Cairo.set_line_cap ctx SQUARE;
                         Cairo.move_to ctx 0. 0.;
                         { cr = ctx; }

let write_out ctx filename = let surface = Cairo.get_target ctx.cr in
                             Cairo.stroke ctx.cr;
                             Cairo.PNG.write surface filename
