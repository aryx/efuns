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
open Efuns
open Options
module PH = Parse_and_highlight

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Using the TeX/Noweb parser and highlighters in pfff (used for codemap)
 * for efuns.
 *
 * todo: look at tex_mode.mll? some good ideas? useful keys?
 *)

(*****************************************************************************)
(* Pfff specifics *)
(*****************************************************************************)

let funcs = { PH.
  parse = (fun file ->
    let (ast2, _stat) = Parse_nw.parse file in
    ast2
  );
  highlight = (fun ~tag_hook prefs file (ast, toks) -> 
    Highlight_nw.visit_program ~tag_hook prefs file (ast, toks)
  );
  info_of_tok = (fun _ -> failwith "not needed");
  }

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  UTmp.with_temp_file ~contents:s ~suffix:"c" (fun file ->
    Pfff_modes.colorize_and_set_outlines funcs buf file
  );
  (* overcome some of the parsing limitations like the lack of structure
   * in comments.
   *)
  Color.color buf (Str.regexp "^%[a-zA-Z-]+:") false
    (Text.make_attr (Attr.get_color !!Pl_colors.section_comment_color) 1 0 false);
  ()


(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "noweb_mode_hook"

let mode = Ebuffer.new_major_mode "Noweb(Pfff)" (Some (fun buf ->
  color_buffer buf; 

  let tbl = Ebuffer.create_syntax_table () in
  buf.buf_syntax_table <- tbl;
  (* true in code usually, not necesseraly for tex itself, but have
   * to pick one
   *)
  tbl.(Char.code '_') <- true;

  Minor_modes.toggle_minor_on_buf Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hooks.exec_hooks hooks buf;
))

let noweb_mode = 
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist [".*\\.nw$", mode];

    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
    Var.set_global hooks [];
  )
