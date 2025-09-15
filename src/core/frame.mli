(* see Efuns.frame and Efuns.status types *)
type t = Efuns.frame

(* creation *)

(* this will also make the new frame the top active frame, and
 * kill all the previous frames that were in the window.
 *)
val create : 
  <Efuns.frame_caps; .. > ->
  Efuns.window -> string option -> Efuns.buffer -> t

(* without_top means it does not make the frame the top active frame *)
val create_without_top :
  <Efuns.frame_caps; .. > ->
  Efuns.window -> string option -> Efuns.buffer -> t
(* alias for create_without_top *)
val create_inactive : 
  <Efuns.frame_caps; .. > ->
  Efuns.window -> Efuns.buffer -> t

(* load/save/kill *)

val load_file : 
  <Efuns.frame_caps; .. > ->
  Efuns.window -> string (* filename *) -> t

(* ugly: you may want to use Multi_buffers.set_previous_frame just before.
 * note: you can't maintain a reference to the old frame as in
 *   let frm = ... in 
 *    Frame.change_buffer frm.window buf; 
 *    frm.frm_buffer... <- ...
 * because the old frm reference is now dead. If you want to capture
 * back the frame you'll have to grab the (new) top active frame.
 *)
val change_buffer : 
  <Efuns.frame_caps; .. > ->
  Efuns.window -> string -> unit

val change_buffer_hooks: Efuns.action_name list Options.t

val save_buffer : t -> unit

val kill : t -> unit

exception BufferKilled
val unkill : Efuns.window -> t -> unit

(* find *)

val find_buffer_frame : Efuns.buffer -> t

(* display *)

val display : Efuns.top_window -> t -> unit

(* prepare the frame for display, setup cutline, frm_table, etc *)
val install : Efuns.window -> t -> unit

(* status line *)

val status_format : (Efuns.status_info * (int * int)) list ref
val status_print : Efuns.status -> string -> Efuns.status_info -> unit
val status_modified : t -> bool -> unit
val status_col : t -> int -> unit
val status_major_mode : t  -> unit
val status_line : t -> int -> unit
val status_name : t -> string -> unit

(* cursor *)

val set_cursor : t -> unit

val point_to_x_when_no_cutline : Efuns.buffer -> Text.point -> int
val cursor_to_coord : t -> int -> int -> Text.coord

(* x y screen coordinates *)
val move_point : t -> Text.point -> int -> int -> unit

(* getters/setters *)

val current_dir : t -> string

(* dumpers *)

(* helpers *)

val buf_text_point: Efuns.frame -> (Efuns.buffer * Text.t * Text.point)
val to_frame : (Efuns.buffer -> Text.point -> 'a) -> t -> 'a

(* navigation *)

val active : t -> unit

(* misc *)

val editname : string
exception FoundFrame of t

val help_bindings : Efuns.action

(*
val display_line : Efuns.top_window -> t -> string -> int -> unit
val update_table : 'a -> t -> unit
val resize : t -> unit 
*)
