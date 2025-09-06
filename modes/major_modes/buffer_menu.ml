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
open Options

open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A port of my (colorized) buffer-menu from emacs to efuns.
 *
 * todo:
 *  - D, X to delete buffers and execute plan
 *)

let buflist_name = "*Buffer List*"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* less: could be factorized with Multi_buffers.prev_buffers,
 * but right now change_buffer_hook is more reliable than the unchecked
 * invariant that every call to Frame.change_buffer is preceded by
 * Multi_buffers.set_previous_frame
 *)
let list = ref []

(* ?? -> <> (as change_buffer_hook) *)
let change_buffer_record frame =
  let name = frame.frm_buffer.buf_name in
  if name <> buflist_name
  then list := name :: Utils.list_removeq !list name

let buflist_array = Store.create_abstr "buffer_menu_buflist"

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let mode =  Ebuffer.new_major_mode "Buffer List" None


(* bounded to C-M-Tab in std_efunsrc.ml  *)
let menu frame =

  let buf = Ebuffer.default buflist_name in
  let text = buf.buf_text in

  Ebuffer.set_major_mode buf mode;

  let str = " MR Buffer             Size  Mode         File\n" in
  (* M = Modified, R = Read-only? *)
  Text.update text str;
  Text.insert_at_end text " -- ------             ----  ----         ----\n";

  let current = frame.frm_buffer.buf_name in
  let all = Multi_buffers.buffer_list () in
  let hall = all |> Hashtbl_.hashset_of_list in
  list := !list |> List.filter (fun str -> Hashtbl.mem hall str);
  let history = !list in

  let list = Utils.list_removeq history current in
  let list =
    if current <> buflist_name
    then current :: list
    else list
  in
  let list =
    list @ 
    (all |> List_.exclude (fun str -> List.mem str list))
  in

  Var.set_local buf buflist_array (Array.of_list list);

  list |> List.iter (fun name ->
    let buf = Ebuffer.default name in
    Text.insert_at_end text 
      (Common.spf " %s  %-17s%6d  %-13s%s\n"
         (if (buf.buf_last_saved = Text.version buf.buf_text) then " " else "*")
         buf.buf_name
         (Text.size buf.buf_text)
         buf.buf_major_mode.maj_name
         (match buf.buf_filename with
         | None -> ""
         | Some s -> String.sub s 0 (min (String.length s -1) 36)
         ));
  );
  Dircolors.colorize_buffer buf;
  (*  Text.toggle_readonly text; *)
  Text.goto_line text buf.buf_point 2;
  Multi_buffers.set_previous_frame frame;
  Frame.change_buffer frame.frm_window "*Buffer List*";

  ()


let key_return frame =
  let (buf, text, point) = Frame.buf_text_point frame in
  let arr = Var.get_local buf buflist_array in
  let line = Text.point_line text point in
  try 
    (* -2 because of header *)
    let buf_name = arr.(line - 2) in
    Frame.change_buffer frame.frm_window buf_name
  with exn ->
    Message.message frame
      (Common.spf "not valid entry in buffer list, exn = %s" 
          (Printexc.to_string exn))


(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ = 
  Hooks.add_start_hook (fun () ->
    Action.define_action "buffer_menu_change_buffer_record" 
      change_buffer_record;
     Frame.change_buffer_hooks =:=
      ("buffer_menu_change_buffer_record" :: !!Frame.change_buffer_hooks);

    let map = mode.maj_map in
    Keymap.add_binding map [(NormalMap, XK.xk_Return)] key_return;
  )
