(*s: CLI.mli *)
(*s: type [[CLI.caps]] *)
type caps = < Cap.no_caps >
(*e: type [[CLI.caps]] *)
(*s: signature [[CLI.main]] *)
(* entry point (can also raise Exit.ExitCode) *)
val main : < caps ; .. > -> string array -> Exit.t
(*e: signature [[CLI.main]] *)
(*e: CLI.mli *)
