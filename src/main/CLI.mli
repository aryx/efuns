type caps = < Cap.no_caps >

(* entry point (can also raise Exit.ExitCode) *)
val main : < caps ; .. > -> string array -> Exit.t
