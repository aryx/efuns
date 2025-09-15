
(* there are other globals: Action.actions, Hook.start_hooks, etc. *)

val editor : unit -> Efuns.editor
(* should be set once in main() *)
val global_editor : Efuns.editor option ref

val check : bool ref
val debug : bool ref
val debug_graphics : bool ref
val debug_display : bool ref

(* for dynamic loading of OCaml code? *)
val path : string list ref
val load_path : string list Options.t
val efuns_path : string list

val font : string Options.t

(* for X11 only? *)
val displayname: string ref

val with_lock : (unit -> 'a) -> 'a
