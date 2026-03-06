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
module Color = Simple_color

(* floats are the norm in cairo *)
open Common2.ArithFloatInfix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to Cairo functions.
 * 
 * Most of the code comes pfff/code_map/cairo_helpers.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* was in pfff/.../cairo_helpers.ml *)
let set_source_color ?(alpha=1.) ~cr ~color () = 
  (let (r,g,b) = color |> Color.rgbf_of_string in
  Cairo.set_source_rgba cr r g b alpha;
  )

(*let re_space = Str.regexp "^[ ]+$"*)

(* The code below is necessary when using the (limited/buggy) toy cairo text
 * API. It seems also necessary when using Pango as I get some
 * Pango warnings about UTF-8 characters and some ugly display.
 *)
let prepare_string s = 
  let buf = Bytes.of_string s in
(*  if s ==~ re_space then  s ^ s (* double it *) else  *)
  for i = 0 to String.length s -.. 1 do
    let c = String.get s i in
    let final_c =
      match c with
      | _ when int_of_char c >= 128 -> 'Z'
      | '\t'-> ' '
      | _ -> c
    in
    Bytes.set buf i final_c
  done;
  Bytes.to_string buf
(* TODO use charreprs instead? *)


let fill_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.fill cr;
  ()

let draw_rectangle_xywh ?alpha ~cr ~x ~y ~w ~h ~color () = 
  set_source_color ?alpha ~cr ~color ();
  
  Cairo.move_to cr x y;
  Cairo.line_to cr (x+w) y;
  Cairo.line_to cr (x+w) (y+h);
  Cairo.line_to cr x (y+h);
  Cairo.stroke cr;
  ()

(* see http://cairographics.org/FAQ/#clear_a_surface *)
let clear cr =
  Cairo.set_source_rgba cr 0. 0. 0.   0.;
  Cairo.set_operator cr Cairo.SOURCE;
  Cairo.paint cr;
  Cairo.set_operator cr Cairo.OVER;
  ()

(*****************************************************************************)
(* Pango fonts *)
(*****************************************************************************)

let pango_layout cr desc =
  let layout = Cairo_pango.create_layout cr in
  Pango.Layout.set_font_description layout desc;
  Cairo_pango.update_layout cr layout;
  layout
[@@profiling]

let pango_show_text ly cr str =
  Pango.Layout.set_text ly  (prepare_string str);
  Cairo_pango.update_layout cr ly;
  Cairo_pango.show_layout cr ly
[@@profiling]
 
