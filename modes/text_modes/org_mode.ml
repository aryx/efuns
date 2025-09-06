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
open Efuns
open Options

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Poor's man port of Emacs Org mode to Efuns.
 * 
 * alt: could use the highlighter from pfff in lang_text/org_mode.ml there
 * but it adds a dependency to pfff and this file anyway is not that big.
 * We can reproduce its functionality here and tailor it more for Efuns.
 *
 * todo:
 *  - C-M-up and down, to move around sections
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

(* copy of highlight_code.ml for the CommentSectionxxx stuff *)

(* map length( '*' ) -> color *)
let colors = [|
  "impossible_color_0_indexed";

  "coral";
  "orange";
  "LimeGreen";
  "LightBlue3";
  "gray";
  |]


let color_buffer_and_set_outlines buf =
  let text = buf.buf_text in
  let re = Str.regexp "^\\([*]+\\)" in

  (* for outline *)
  let outline_points = ref [] in

  Text.with_new_point text (fun point ->
    Text.set_position text point 0;
    try 
      while true do
        let len_stars = Text.search_forward text re point in
        let fontsize = max 0 (5 - len_stars) in
        let attr = 
          Text.make_attr (Attr.get_color (colors.(len_stars))) 1 fontsize false
        in
        Text.set_attrs text point (Text.point_to_eol text point) attr;

        (* outlines *)
        let lvl = len_stars in
        Stack_.push (lvl, Text.dup_point text point) outline_points;

        (* just highlight the last star, like in Org mode *)
        for _i = 0 to len_stars - 2 do
          let attr = Text.make_attr (Attr.get_color "black") 1 0 false in
          Text.set_attr text point attr;
          Text.fmove text point 1;
        done;
        Text.fmove text point 1;
        (* old: Text.fmove text point (len_stars+1); 
         * but already did some fmove before so just have to fmove 1
         *)
      done

    with Not_found -> ()
  );
  (* copy paste of pfff_modes.ml *)
  let outline_points = 
    match List.rev !outline_points with
    (* to avoid some index out of bounds create at least one point *)
    | [] -> [0, Text.new_point text]
    | xs -> xs
  in
  Var.set_local buf Outline_mode.outline_var outline_points;

  (* for other non-outline elements *)
  Color.color buf (Str.regexp "^#.*$") false
    (Text.make_attr (Attr.get_color !!Pl_colors.syncweb_comment_color) 1 0 false);
  Color.color buf (Str.regexp "^#[a-zA-Z-_]+:") false
    (Text.make_attr (Attr.get_color !!Pl_colors.section_comment_color) 1 0 false);
  ()

(*****************************************************************************)
(* Installation *)
(*****************************************************************************)

let mode =  Ebuffer.new_major_mode "Org" (Some (fun buf ->
  color_buffer_and_set_outlines buf;
  (*
    let tbl = Ebuffer.create_syntax_table () in
    buf.buf_syntax_table <- tbl;
    tbl.(Char.code '_') <- true;
  *)
  ()
))

let org_mode frame = 
  Ebuffer.set_major_mode frame.frm_buffer mode

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist [".*\\.\\(txt\\|org\\)", mode];
    Action.define_action "org_mode" org_mode;
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer_and_set_outlines::(Var.get_global Ebuffer.saved_buffer_hooks));
  )
