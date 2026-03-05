
(* Entry point: M-x magit_status *)
val magit_status : Efuns.action

(* Internals exposed for testing *)

type line_info =
  | Header
  | SectionHead of string
  | FileHead of { staged : bool; file : string }
  | HunkHead of { staged : bool; file : string; hunk_header : string;
                  hunk_start : int; hunk_len : int }
  | HunkLine of { staged : bool; file : string; hunk_header : string;
                  hunk_start : int; hunk_len : int }
  | Blank

val git_lines : < Efuns.frame_caps; .. > -> string -> string -> string list
val find_repo_root : < Efuns.frame_caps; .. > -> string -> string option

val parse_diff : string list -> (string * (string * string list) list) list
val parse_hunk_range : string -> int * int
val build_hunk_patch : string list -> string -> string
