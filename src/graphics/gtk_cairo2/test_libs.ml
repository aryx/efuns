(* Yoann Padioleau
 * 
 * Copyright (C) 2015 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(* floats are the norm in cairo *)
open Common2_.ArithFloatInfix

(*****************************************************************************)
(* Test cairo/gtk *)
(*****************************************************************************)

let width = 500
let height = 500

let test_draw_cairo cr =
  (* [0,0][1,1] world scaled to a width x height screen *)
  Cairo.scale cr (float_of_int width) (float_of_int height);

  Cairo.set_source_rgba cr 0.5 0.5 0.5  0.5;
  Cairo.set_line_width cr 0.001;

  Cairo.move_to cr 0.5 0.5;
  Cairo.line_to cr 0.6 0.6;
  Cairo.stroke cr;

(*
  Cairo.select_font_face cr "monospace"
    Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_BOLD;
*)
  Cairo.select_font_face cr "fixed"
    ~weight:Cairo.Normal;
  Cairo.set_font_size cr 0.05;

  let _extent = Cairo.text_extents cr "peh" in
  (* WEIRD: if Cairo.text_extents cr "d" create an Out_of_memory exn *)
  (* related? https://github.com/diagrams/diagrams-cairo/issues/43
  *)


  Cairo.move_to cr 0.1 0.1;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.move_to cr 0.1 0.2;
  Cairo.show_text cr "THIS IS SOME TEXT";
  Cairo.set_font_size cr 0.05;
  Cairo.move_to cr 0.1 0.3;
  Cairo.show_text cr "THIS IS SOME TEXT";

  Cairo.set_source_rgb cr 0.1 0.1 0.1;
  Cairo.move_to cr 0.1 0.1;
  Cairo.line_to cr 0.1 0.2;
  Cairo.stroke cr;

  let start = ref 0.0 in

  for _i = 0 to 3 do
    let end_ = !start +. 0.5 in
    Cairo.arc cr 0.5 0.5 ~r:0.3 ~a1:!start ~a2:end_;
    Cairo.stroke cr;
    start := end_;
  done;
  ()

let test_draw_pango cr =

  let layout = Cairo_pango.create_layout cr in
  let ctx = Cairo_pango.Font_map.create_context 
    (Cairo_pango.Font_map.get_default ()) in


  Pango.Layout.set_text layout "WWWWW let x = 1 in main () for x = 1 to 3!";
  let desc = Pango.Font.from_string 
(*
    "Monospace 18"
    "Fixed Bold 32"
    "b&h-Luxi Bold 23"
    "Arial Bold 30" 
    "Menlo 18"
*)
    "Monospace 18"

  in
  Pango.Context.set_font_description ctx desc;
  Pango.Layout.set_font_description layout desc;
  Cairo_pango.update_layout cr layout;

  Cairo.move_to cr 0. 0.;
  Cairo_pango.show_layout cr layout;
  Logs.debug (fun m -> m "font = %s" (Pango.Font.to_string desc));


  let metrics = 
    Pango.Context.get_metrics ctx 
      (Pango.Context.get_font_description ctx) None in
  let w = 
    float_of_int (Pango.Font.get_approximate_char_width metrics) / 1024. in
  let descent = 
    float_of_int (Pango.Font.get_descent metrics) / 1024. in
  let ascent = 
    float_of_int (Pango.Font.get_ascent metrics) / 1024. in
  let h = ascent + descent in
  Logs.debug (fun m -> m "w h ascent descent %s "
        (Dumper.dump (w, h, ascent, descent)));

  Cairo.move_to cr 0. h;
  (* The 'i' should align with the 'W' above if the font is monospace *)
  Pango.Layout.set_text layout "iiiii let x = 1 in main () for x = 1 to 3!";
  Cairo_pango.show_layout cr layout;

  ()



let test_cairo () =
  let _locale = GtkMain.Main.init () in
  let w = GWindow.window ~title:"test" () in
  (w#connect#destroy GMain.quit) |> ignore;
  let px = GDraw.pixmap ~width ~height ~window:w () in
  px#set_foreground `WHITE;
  px#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  let cr = Cairo_gtk.create px#pixmap in
  test_draw_pango cr;
  (GMisc.pixmap px ~packing:w#add ()) |> ignore;
  w#show ();
  GMain.main()
