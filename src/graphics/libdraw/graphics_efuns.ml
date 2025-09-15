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

module Color = Simple_color
open Efuns

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Draw API *)
(*****************************************************************************)
open Xdraw (* for the fields *)

let clear_eol col line len =
  let loc = Globals.location () in
  if !Globals.debug_graphics
  then Logs.debug (fun m -> m "clear_eol: %d %d %d" col line len);
  let (r,g,b) = Color.rgb_of_string loc.loc_colors_names.(1) in
  Draw.set_bg_color 1 r g b;
  Draw.clear_eol col line len

let draw_string col line  str  offset len   attr =
  let loc = Globals.location () in
  if !Globals.debug_graphics
  then Logs.debug (fun m -> m "draw_string %d %d %s %d %d %d"
         col line str offset len attr);

  let bgcolor_idx = 
    let idx = (attr lsr 8) land 255 in
    if attr land Text.highlight_bit > 0
    then 2
    else idx
  in
  let bgcolor = loc.loc_colors_names.(bgcolor_idx) in
  let (r,g,b) = Color.rgb_of_string bgcolor in
  Draw.set_bg_color bgcolor_idx r g b;
  Draw.clear_eol col line len;

  let fgcolor_idx = (attr) land 255 in
  let fgcolor = loc.loc_colors_names.(fgcolor_idx) in
  let (r,g,b) = Color.rgb_of_string fgcolor in
  Draw.set_fg_color fgcolor_idx r g b;
  Draw.draw_string col line (String.sub str offset len);
  ()

let update_display () =
  if !Globals.debug_graphics
  then Logs.debug (fun m -> m "update_displays")

let backend = { 
  clear_eol = clear_eol; 
  draw_string = draw_string; 
  update_display = update_display;
  update_window_title = (fun _ -> ());
}

(*****************************************************************************)
(* paint/configure/expose *)
(*****************************************************************************)

(*****************************************************************************)
(* The main UI *)
(*****************************************************************************)

let init init_files = 

  (*-------------------------------------------------------------------*)
  (* Graphics initialisation *)
  (*-------------------------------------------------------------------*)
  Draw.initdraw None "test_draw_ml";

  (*-------------------------------------------------------------------*)
  (* Window creation *)
  (*-------------------------------------------------------------------*)

  (*-------------------------------------------------------------------*)
  (* Creation of core DS of Efuns (buffers, frames, top_window) *)
  (*-------------------------------------------------------------------*)

  (* loc.loc_height <- 45; *)
  (* will boostrap and use a newly created *help* buffer *)
  let top_window = Top_window.create () in
  (* the *bindings* buffer *)
  Interactive.create_bindings () |> ignore;
  (* open the first buffers *)
  init_files |> List.iter (fun name ->
    Frame.load_file top_window.window name |> ignore
  );

  (*-------------------------------------------------------------------*)
  (* Layout *)
  (*-------------------------------------------------------------------*)

    (*-------------------------------------------------------------------*)
    (* main view *)
    (*-------------------------------------------------------------------*)

  top_window.graphics <- Some backend;
  Top_window.update_display ();

  (*-------------------------------------------------------------------*)
  (* Events *)
  (*-------------------------------------------------------------------*)

  let escape = ref false in

  (* Main loop *)
  while true do
    let code = Draw.ekbd () in
    (*Printf.printf "key = %d" code;*)

    let modifiers, code = 
      match code with
      (* control-j *)
      | 10 -> 0, XK.xk_Return
      | 8 | 9 | 13  -> 0, code
      | _ when code >= 1 && code <= 26 -> 
          Xtypes.controlMask, code - 1 + Char.code 'a'
      (* escape key *)
      | 27 -> 0, 27

      (* see keyboard.h *)
      | 63488 -> 0, XK.xk_Down
      | 61454 -> 0, XK.xk_Up
      | 61457 -> 0, XK.xk_Left
      | 61458 -> 0, XK.xk_Right

      | _ -> 0, code
    in
    if code = 27
    then escape := true
    else begin
      let modifiers = modifiers lor (if !escape then Xtypes.mod1Mask else 0) in
      escape := false;
      let evt = Xtypes.XTKeyPress (modifiers, spf "%d" code, code) in
      Top_window.handler top_window evt
    end
  done;
  ()

  (*-------------------------------------------------------------------*)
  (* End *)
  (*-------------------------------------------------------------------*)
