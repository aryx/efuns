
(* M-! *)
val shell_command : Efuns.action

(* = Efuns.frame_caps *)
type caps = < Cap.fork; Cap.exec; Cap.wait; Cap.chdir >

type end_action = (Efuns.buffer -> int -> unit)

val system : 
  <caps; .. > -> string (* a dir *) -> string -> string -> end_action -> 
  Efuns.buffer
val start_command : 
  <caps; .. > -> string (* a dir *) -> string -> Efuns.window -> string -> end_action option -> 
  Efuns.frame

(* used by shell_mode *)
val open_process : 
  < caps ; .. > ->
  string (* a dir *) -> string -> int * in_channel * out_channel

(*
val shell_hist : string list ref
*)
