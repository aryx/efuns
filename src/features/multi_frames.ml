(*s: features/multi_frames.ml *)
(*s: copyright header2 *)
(***********************************************************************)
(*                                                                     *)
(*                           xlib for Ocaml                            *)
(*                                                                     *)
(*       Fabrice Le Fessant, projet Para/SOR, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)
(*e: copyright header2 *)
open Efuns
  
(*s: function [[Multi_frames.cut_frame]] *)
let cut_frame frame =
  let window = frame.frm_window in
  if window.win_height > 3 then
    let h = window.win_height / 2 in
    let w1 = Window.create false
        (Window window) window.win_xpos window.win_ypos
        window.win_width h in
    let w2 = Window.create false (Window window) window.win_xpos 
        (window.win_ypos + h) 
      window.win_width (window.win_height - h) in
    window.win_down <- VComb (w1,w2);
    Frame.install w1 frame;
    w2 
  else
    window
(*e: function [[Multi_frames.cut_frame]] *)

(*s: function [[Multi_frames.remove_frame]] *)
let remove_frame frame =
  if frame.frm_mini_buffer = None then
    let window = frame.frm_window in
    match window.win_up with
    | TopWindow _ -> ()
    | Window upwin ->
        Window.prev (Frame.install upwin) window
(*e: function [[Multi_frames.remove_frame]] *)

(*s: function [[Multi_frames.v_cut_frame]] *)
let vertical_cut_frame frame =
  if frame.frm_mini_buffer = None 
  then Frame.create frame.caps (cut_frame frame) None frame.frm_buffer |> ignore
[@@interactive]
(*e: function [[Multi_frames.v_cut_frame]] *)

(*s: function [[Multi_frames.h_cut_frame]] *)
let horizontal_cut_frame (frame : Frame.t) =
  if frame.frm_mini_buffer = None 
  then
    let window = frame.frm_window in
    if window.win_width > 10 then begin
      let wi = window.win_width / 2 in
      let w1 = Window.create false
                 (Window window) window.win_xpos window.win_ypos
                 wi window.win_height in
      let w2 = Window.create false (Window window) (window.win_xpos + wi) 
                 window.win_ypos
                 (window.win_width - wi) window.win_height in
      window.win_down <- HComb (w1,w2);
      Frame.install w1 frame;
      Frame.create frame.caps w2 None frame.frm_buffer |> ignore
    end
[@@interactive]
(*e: function [[Multi_frames.h_cut_frame]] *)

(*s: function [[Multi_frames.delete_frame]] *)
let delete_frame frame =
  if frame.frm_mini_buffer = None 
  then
    let window = frame.frm_window in
    match window.win_up with
    | TopWindow _ -> ()
    | Window upwin ->
        Frame.install upwin frame;
        Frame.active frame
[@@interactive]
(*e: function [[Multi_frames.delete_frame]] *)

(*s: function [[Multi_frames.one_frame]] *)
let one_frame frame =
  if frame.frm_mini_buffer = None 
  then
    let window = frame.frm_window in
    let top_window = Window.top window in
    if not (top_window.window == window) then
      begin
        Frame.install top_window.window frame;
        Frame.active frame
      end
[@@interactive]
(*e: function [[Multi_frames.one_frame]] *)
      
      
(*s: function [[Multi_frames.next_frame]] *)
let next_frame frame =
  let window = frame.frm_window in
  Window.next Frame.active window
[@@interactive]
(*e: function [[Multi_frames.next_frame]] *)
(*e: features/multi_frames.ml *)
