
type command =
  | LoadFile of string (* filename *) * int (* pos *) * int (* line *) * string

(* assumes Globals.editor has been set *)
val start: <Efuns.frame_caps; ..> -> unit

val server_start: Efuns.action

