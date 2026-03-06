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
open World
open Efuns

module CH = Cairo_helpers
(* floats are the norm in cairo *)
open Common2.ArithFloatInfix

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Minimap a la Code Thumbnails (or Sublime text).
 *
 * Many regular apps like powerpoint, Preview, etc. have thumbnails preview.
 *
 * Focus+context!
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let draw_minimap w =

  let cr = Cairo.create w.base in
  Cairo.translate cr (w.metrics.main_width + w.metrics.margin_width *. 2.) 0.0;

  CH.fill_rectangle_xywh ~cr ~x:0. ~y:0. 
    ~w:w.metrics.mini_width ~h:w.metrics.main_height
    ~color:"grey22" ();

  Cairo.scale cr (1. / w.metrics.mini_factor) (1. / w.metrics.mini_factor);

  let frame = w.edt.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in
  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in
  let startline = startpage *.. w.metrics.linemax in
  let endline = 
    min (startline +.. w.metrics.linemax) (Text.nbr_lines text -.. 1) in


  for i = startline to endline do
    let line = Text.compute_representation text buf.buf_charreprs i in
    let repr_str = line.Text.repr_string in
    line.Text.boxes |> List.rev |> List.iter (fun box ->
      (* opti: no need to spend time on long lines, their tail is not
       * displayed. Useful in eshell buffers with long compilation lines.
       *)
      if box.Text.box_col >= 100
      then () 
      else  begin
      let h = w.metrics.font_height in
      let x = float_of_int box.Text.box_pos_repr * w.metrics.font_width in
      let line_in_page = i mod w.metrics.linemax in
      let y = (float_of_int line_in_page * h) + h * 0.1 in
      Cairo.move_to cr x y;
      let attr = box.Text.box_attr in

      let str = Bytes.sub_string repr_str 
                box.Text.box_pos_repr box.Text.box_size in

      let fgcolor = 
        let idx = attr land 255 in
        w.edt.edt_colors_names.(idx)
      in
      let fontsize = (attr lsr 16) land 255 in
      CH.set_source_color ~cr ~color:fgcolor ();

      let ly = 
        if fontsize =|= 0 
        then w.ly
        else begin
          (* def of func is fontsize 3 *)
          let size = 30 +.. 20 *.. fontsize in
          (* Does not have to be "fixed ..." here, we want to optimize for
           * readability. Actually we can move to the bol
           * so preceding long tokens like \subsubsection do not push
           * us too much to the right.
           *)
          Cairo.move_to cr 0. y;
          let desc = Pango.Font.from_string (spf "Serif %d" size) in
          CH.pango_layout cr desc
        end
      in
      CH.pango_show_text ly cr str

(*
  this generate some out_of_memory error when run directly efuns
  on lexer_nw.mll. weird, but Cairo text api is known to be buggy.
*)
(*
      Cairo.select_font_face cr "serif"
        Cairo.FONT_SLANT_NORMAL Cairo.FONT_WEIGHT_NORMAL;
      Cairo.set_font_size cr (38. + 25. * (float_of_int fontsize));
      Cairo.show_text cr (prepare_string str);
*)
      end
    )
  done;
  ()
[@@profiling]

let draw_minimap_overlay w =

  let cr = Cairo.create w.overlay in
  CH.clear cr;

  Cairo.translate cr (w.metrics.main_width + w.metrics.margin_width * 2.) 0.0;
  Cairo.scale cr (1. / w.metrics.mini_factor) (1. / w.metrics.mini_factor);

  let frame = w.edt.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in

  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in
  let startline = startpage *.. w.metrics.linemax in

  let line = line -.. startline in

  let x = 0. in
  let y = (float_of_int line) * w.metrics.font_height in
  let h = (float_of_int w.edt.edt_height) * w.metrics.font_height in

  Cairo.set_line_width cr ((Cairo.get_line_width cr) * w.metrics.mini_factor);
  CH.fill_rectangle_xywh ~alpha:0.2 ~cr ~x ~y ~w:w.metrics.main_width ~h
    ~color:"white" ();
  ()
[@@profiling]


(* opti to avoid recompute/redraw expensive minimap *)
let active_frame_info w =
  let frame = w.edt.top_windows |> List.hd |> (fun tw -> tw.top_active_frame) in
  let buf = frame.frm_buffer in
  let text = buf.buf_text in

  let line = Text.point_line text frame.frm_start in
  (* ex: linemax = 400, line = 10 => startpage = 0; line = 410 => page = 1 *)
  let startpage = line /.. w.metrics.linemax in

  buf.buf_name, Text.version text, startpage

let idle_minimap = ref None

let draw_minimap_when_idle w win =
  !idle_minimap |> Option.iter (fun x ->
    GMain.Timeout.remove x;
  );
  idle_minimap := 
    Some (GMain.Timeout.add ~ms:50 ~callback:(fun () ->

      (* opti: avoid redrawing if nothing was modified and we just moved
       * the cursor 
       *)
      let active_frame = active_frame_info w in
      if active_frame <> w.last_top_frame_info
      then begin 
        (* todo: do in a thread when idle *)
        draw_minimap w;
        w.last_top_frame_info <- active_frame;
      end;
      draw_minimap_overlay w;

      (* this will trigger the expose event *)
      GtkBase.Widget.queue_draw win#as_widget;
      (* avoid double Idle.remove *)
      idle_minimap := None;
      false
    ))
