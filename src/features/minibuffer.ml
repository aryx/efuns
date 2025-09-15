(*s: features/minibuffer.ml *)
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

(*s: constant [[Minibuffer.charreprs]] *)
let charreprs = Array.init 256 (fun i -> String.make 1 (Char.chr i))
(*e: constant [[Minibuffer.charreprs]] *)
(*s: toplevel [[Minibuffer._1]] *)
let _ =
  charreprs.(9) <- String.make !Ebuffer.tab_size ' '
(*e: toplevel [[Minibuffer._1]] *)

(*s: function [[Minibuffer.buf_create]] *)
let buf_create text local_map =
  { 
    buf_text = text;

    buf_name = "*Minibuffer*";
    buf_filename = None; (* no connected file! *)

    buf_point = Text.new_point text;
    buf_start = Text.new_point text;

    buf_last_saved = Text.version text;
    buf_modified = 0;


    buf_map = local_map;
    buf_map_partial = true;
    buf_charreprs = charreprs;
    buf_syntax_table = Ebuffer.default_syntax_table;
    buf_vars = Store.new_store ();
    buf_major_mode = Ebuffer.fondamental__mode;
    buf_minor_modes = [];

    buf_sync = false;
    buf_mark = None;
    buf_shared = 0;
    buf_finalizers = [];
    buf_history_pos = [||];
  }
(*e: function [[Minibuffer.buf_create]] *)

(*s: function [[Minibuffer.kill]] *)
let kill mini_frame old_frame =
  let window = mini_frame.frm_window in
  let top_window = Window.top window in
  Top_window.clear_message top_window;
  top_window.top_mini_buffers <- List.tl top_window.top_mini_buffers;
  if old_frame.frm_killed 
  then Frame.unkill window old_frame;
  top_window.top_active_frame <- old_frame;
  Frame.kill mini_frame
(*e: function [[Minibuffer.kill]] *)

(*s: function [[Minibuffer.return]] *)
let return action old_frame = fun mini_frame ->
  let repstr = Text.to_string mini_frame.frm_buffer.buf_text in
  kill mini_frame old_frame;
  action old_frame repstr
(*e: function [[Minibuffer.return]] *)

(*s: function [[Minibuffer.create]] *)
let create (frame : Frame.t) (local_map : Keymap.t) (request : string) : Frame.t =
  let window = frame.frm_window in
  let top_window = Window.top window in

  let qlen = String.length request in
  let request = 
     if qlen < 50 
     then request 
     else String.sub request 0 47 ^ "..."
  in

  let mini_text = 
    Text.create "" in
  let mini_buf = 
    buf_create mini_text local_map in
  let mini_window = 
    Window.create true (*mini*) (TopWindow top_window) 
      qlen (top_window.top_height - 1) (top_window.top_width - qlen) 1 in
  let mini_frame = 
    Frame.create frame.caps mini_window (Some request)(*mini*) mini_buf in    

  mini_frame.frm_cutline <- max_int;
  mini_frame.frm_has_status_line <- 0;

  top_window.top_mini_buffers <- mini_frame :: top_window.top_mini_buffers;

  Keymap.add_binding local_map [ControlMap, Char.code 'g']
    (fun mini_frame -> kill mini_frame frame);
  mini_frame
(*e: function [[Minibuffer.create]] *)

(*s: function [[Minibuffer.create_return]] *)
let create_return frame local_map request default action =
  let mini_frame = create frame local_map request in
  Edit.insert_string mini_frame default;
  Keymap.add_binding local_map [NormalMap, XK.xk_Return] 
    (return action frame);
  mini_frame
(*e: function [[Minibuffer.create_return]] *)

(*s: function [[Minibuffer.update_request]] *)
let update_request frame request =
  let qlen = String.length request in
  let window = frame.frm_window in
  let top_window = Window.top window in
  let _mini_window = Window.create true window.win_up
      qlen window.win_ypos
      (top_window.top_width - qlen) 1
  in
  frame.frm_width <- top_window.top_width - qlen;
  frame.frm_xpos <- qlen;
  frame.frm_mini_buffer <- Some request;
  frame.frm_redraw <- true
(*e: function [[Minibuffer.update_request]] *)
  
(*e: features/minibuffer.ml *)
