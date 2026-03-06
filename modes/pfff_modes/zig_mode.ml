(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Tree-sitter-based Zig major mode.
 *
 * This module provides a major mode to edit Zig files by using
 * the Zig parser and highlighter in respectively Semgrep and Codemap.
 *
 * todo:
 *  - indentation?
 *)

(*****************************************************************************)
(* Colors *)
(*****************************************************************************)

let color_buffer buf =
  let s = Text.to_string buf.buf_text in
  UTmp.with_temp_file ~contents:s ~suffix:"zig" (fun file ->
    Pfff_modes.colorize_and_set_outlines Parse_and_highlight.zig buf file
  )

(*****************************************************************************)
(* The mode *)
(*****************************************************************************)
let hooks = Store.create_abstr "zig_mode_hook"

let mode = Ebuffer.new_major_mode "Zig(Semgrep)" (Some (fun buf ->
  color_buffer buf;

  buf.buf_syntax_table.(Char.code '_') <- true;

  Minor_modes.toggle_minor_on_buf Paren_mode.mode buf;

  let hooks = Var.get_var buf hooks in
  Hooks.exec_hooks hooks buf;
))

let zig_mode =
  Major_modes.enable_major_mode mode
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    Var.add_global Ebuffer.modes_alist
      [".*\\.zig$",mode];

    (* recolor at save time *)
    Var.set_major_var mode Ebuffer.saved_buffer_hooks
      (color_buffer::(Var.get_global Ebuffer.saved_buffer_hooks));
    Var.set_global hooks [];
  )
