(* see Efuns.top_window type *)
type t = Efuns.top_window

(* creation *)

val create : < Efuns.frame_caps; ..> -> t

(* display *)

(* see also Efuns.backend : t -> Xdraw.graphics_backend *)
val update_display : unit -> unit

(* event handler *)

val handler : t -> Xtypes.event -> unit

(* keyboard event *)

(* internal: handle_key() *)
val handle_key_start_hook : (Efuns.frame -> unit) list Var.t
val handle_key_end_hook : (unit -> unit) list Var.t

(* used by self_insert_command *)
val keypressed : Efuns.keySym ref

(* for macros.ml *)
val in_start_macro: bool ref
val in_call_macro: bool ref
(* internal *)
val recorded_keys: (int * Efuns.keySym) list ref

(* mouse event *)

val mouse_set_active : t -> Efuns.frame

(* menus *)

val file_menu : (string * string) list Options.t
val edit_menu : (string * string) list Options.t
val help_menu : (string * Efuns.action) array ref
val buffers_menu : (t -> unit -> unit -> unit) ref

(* message *)

(* This is the low-level API for messages. You should probably use
 * Message.message instead; it handles *Messages* and takes a frame 
 *)
val message: t -> string -> unit
val clear_message : t -> unit

val mini_message : Efuns.frame -> string -> unit

(* cursor *)
(* used by cursor thread *)
val cursor_on: t -> unit
val cursor_off: t -> unit


(* misc *)
val wrap : t -> (t -> unit) -> unit -> unit
