(* Claude Code
 *
 * Copyright (C) 2026 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 * Inspired by Magit for Emacs:
 *   Copyright (C) 2008-2026 The Magit Project Contributors
 *   Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
 *   Original creator: Marius Vollmer
 *   https://github.com/magit/magit
 *   SPDX-License-Identifier: GPL-3.0-or-later
 *)
open Common
open Efuns

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A minimal magit-like mode for Efuns.
 *
 * Supports:
 *  - M-x magit_status: show git status with diff hunks
 *  - s/u: stage/unstage file or hunk at point
 *  - c: commit (prompts for message)
 *  - p: push to remote
 *  - g: refresh status
 *  - Tab: toggle section visibility
 *  - q: quit magit buffer
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* claude: each line in the status buffer maps to one of these *)
type line_info =
  | Header
  | SectionHead of string (* "Unstaged" or "Staged" *)
  | FileHead of { staged : bool; file : string }
  | HunkHead of { staged : bool; file : string; hunk_header : string;
                  hunk_start : int; hunk_len : int }
  | HunkLine of { staged : bool; file : string; hunk_header : string;
                  hunk_start : int; hunk_len : int }
  | Blank

(*****************************************************************************)
(* Variables and constants *)
(*****************************************************************************)

let magit_buf_name = "*magit-status*"

(* claude: maps 0-based line number -> line_info *)
let line_map_var = Store.create_abstr "Magit.line_map"
(* claude: tracks which sections are collapsed *)
let collapsed_var = Store.create_abstr "Magit.collapsed"
(* claude: the git repo root *)
let repo_root_var = Store.create_string "Magit.repo_root"

let commit_hist = ref []

(*****************************************************************************)
(* Git helpers *)
(*****************************************************************************)

(* claude: run a git command and return stdout lines, using capabilities *)
let git_lines (caps : < Efuns.frame_caps; .. >) root args =
  let cmd = spf "cd %s && git %s" (Filename.quote root) args in
  CapExec.cmd_to_list caps#exec cmd

let git_string caps root args =
  String.concat "\n" (git_lines caps root args)

(* claude: find the repo root from a directory *)
let find_repo_root (caps : < Efuns.frame_caps; .. >) dir =
  let cmd = spf "cd %s && git rev-parse --show-toplevel 2>/dev/null"
      (Filename.quote dir) in
  CapExec.with_open_process_in caps#exec cmd (fun ic ->
    try Some (input_line ic) with End_of_file -> None
  )

(*****************************************************************************)
(* Rendering *)
(*****************************************************************************)

(* claude: parse diff output into (file, hunk list) pairs.
 * Each hunk is (header_line, body_lines). *)
let parse_diff lines =
  let files = ref [] in
  let current_file = ref "" in
  let current_hunks = ref [] in
  let current_hunk_header = ref "" in
  let current_hunk_lines = ref [] in
  let flush_hunk () =
    if !current_hunk_header <> "" then begin
      current_hunks :=
        (!current_hunk_header, List.rev !current_hunk_lines) :: !current_hunks;
      current_hunk_header := "";
      current_hunk_lines := []
    end
  in
  let flush_file () =
    flush_hunk ();
    if !current_file <> "" then begin
      files := (!current_file, List.rev !current_hunks) :: !files;
      current_file := "";
      current_hunks := []
    end
  in
  lines |> List.iter (fun line ->
    if String.length line >= 6 &&
       String.sub line 0 6 = "diff -" then begin
      flush_file ()
    end
    else if String.length line >= 4 &&
            String.sub line 0 4 = "+++ " then begin
      (* +++ b/filename or +++ /dev/null *)
      let name =
        if String.length line > 6 && String.sub line 4 2 = "b/"
        then String.sub line 6 (String.length line - 6)
        else
          (* for deleted files use --- a/filename *)
          !current_file
      in
      current_file := name
    end
    else if String.length line >= 4 &&
            String.sub line 0 4 = "--- " then begin
      if !current_file = "" then begin
        let name =
          if String.length line > 6 && String.sub line 4 2 = "a/"
          then String.sub line 6 (String.length line - 6)
          else ""
        in
        current_file := name
      end
    end
    else if String.length line >= 3 &&
            String.sub line 0 3 = "@@ " then begin
      flush_hunk ();
      current_hunk_header := line
    end
    else if !current_hunk_header <> "" then
      current_hunk_lines := line :: !current_hunk_lines
  );
  flush_file ();
  List.rev !files

(* claude: extract the hunk range start and length from header like
 * "@@ -10,5 +10,7 @@ ..." -> (10, 7) for the + side *)
let parse_hunk_range header =
  try
    let re = Str.regexp "@@ [^ ]+ \\+\\([0-9]+\\),\\([0-9]+\\)" in
    if Str.string_match re header 0 then
      (int_of_string (Str.matched_group 1 header),
       int_of_string (Str.matched_group 2 header))
    else
      let re2 = Str.regexp "@@ [^ ]+ \\+\\([0-9]+\\) " in
      if Str.string_match re2 header 0 then
        (int_of_string (Str.matched_group 1 header), 1)
      else (0, 0)
  with _ -> (0, 0)

let rec render_status (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let caps = frame.caps in
  let root = Var.get_local buf repo_root_var in
  let collapsed = Var.get_local buf collapsed_var in
  let text = buf.buf_text in

  (* claude: remember cursor line to restore position *)
  let old_line = Text.point_line text frame.frm_point in

  let line_map = ref [] in
  let lineno = ref 0 in
  let out = Buffer.create 4096 in

  let add_line info s =
    Buffer.add_string out s;
    Buffer.add_char out '\n';
    line_map := (!lineno, info) :: !line_map;
    incr lineno
  in

  (* Header *)
  let branch =
    try List.hd (git_lines caps root "rev-parse --abbrev-ref HEAD")
    with _ -> "unknown"
  in
  add_line Header (spf "Head: %s" branch);
  add_line Blank "";

  (* Unstaged changes *)
  let unstaged_diff = git_lines caps root "diff" in
  let unstaged_files = parse_diff unstaged_diff in

  if unstaged_files <> [] then begin
    let section_collapsed = Hashtbl.mem collapsed "Unstaged" in
    add_line (SectionHead "Unstaged")
      (spf "%s Unstaged changes (%d)"
         (if section_collapsed then ">" else "v")
         (List.length unstaged_files));
    if not section_collapsed then
      unstaged_files |> List.iter (fun (file, hunks) ->
        let file_key = "U:" ^ file in
        let file_collapsed = Hashtbl.mem collapsed file_key in
        add_line (FileHead { staged = false; file })
          (spf "  %s modified   %s"
             (if file_collapsed then ">" else "v") file);
        if not file_collapsed then
          hunks |> List.iter (fun (hdr, lines) ->
            let (hs, hl) = parse_hunk_range hdr in
            add_line (HunkHead { staged = false; file; hunk_header = hdr;
                                 hunk_start = hs; hunk_len = hl })
              (spf "    %s" hdr);
            lines |> List.iter (fun l ->
              add_line (HunkLine { staged = false; file; hunk_header = hdr;
                                   hunk_start = hs; hunk_len = hl })
                (spf "    %s" l)
            )
          )
      );
    add_line Blank ""
  end;

  (* Staged changes *)
  let staged_diff = git_lines caps root "diff --cached" in
  let staged_files = parse_diff staged_diff in

  if staged_files <> [] then begin
    let section_collapsed = Hashtbl.mem collapsed "Staged" in
    add_line (SectionHead "Staged")
      (spf "%s Staged changes (%d)"
         (if section_collapsed then ">" else "v")
         (List.length staged_files));
    if not section_collapsed then
      staged_files |> List.iter (fun (file, hunks) ->
        let file_key = "S:" ^ file in
        let file_collapsed = Hashtbl.mem collapsed file_key in
        add_line (FileHead { staged = true; file })
          (spf "  %s modified   %s"
             (if file_collapsed then ">" else "v") file);
        if not file_collapsed then
          hunks |> List.iter (fun (hdr, lines) ->
            let (hs, hl) = parse_hunk_range hdr in
            add_line (HunkHead { staged = true; file; hunk_header = hdr;
                                 hunk_start = hs; hunk_len = hl })
              (spf "    %s" hdr);
            lines |> List.iter (fun l ->
              add_line (HunkLine { staged = true; file; hunk_header = hdr;
                                   hunk_start = hs; hunk_len = hl })
                (spf "    %s" l)
            )
          )
      );
    add_line Blank ""
  end;

  (* Untracked files *)
  let untracked = git_lines caps root "ls-files --others --exclude-standard" in
  if untracked <> [] then begin
    let section_collapsed = Hashtbl.mem collapsed "Untracked" in
    add_line (SectionHead "Untracked")
      (spf "%s Untracked files (%d)"
         (if section_collapsed then ">" else "v")
         (List.length untracked));
    if not section_collapsed then
      untracked |> List.iter (fun file ->
        add_line (FileHead { staged = false; file })
          (spf "  %s" file)
      );
    add_line Blank ""
  end;

  if unstaged_files =*= [] && staged_files =*= [] && untracked =*= [] then
    add_line Header "Nothing to commit, working tree clean";

  (* Update buffer content *)
  Text.update text (Buffer.contents out);
  let arr = Array.make !lineno Blank in
  !line_map |> List.iter (fun (i, info) -> arr.(i) <- info);
  Var.set_local buf line_map_var arr;

  (* Colorize *)
  colorize_status buf;

  buf.buf_last_saved <- Text.version text;
  (* Restore cursor position *)
  let max_line = Text.nbr_lines text - 1 in
  let target_line = min old_line max_line in
  Text.goto_line text frame.frm_point (max 0 target_line)

and colorize_status buf =
  let green_attr = Text.make_attr (Attr.get_color "green3") 1 0 false in
  let red_attr = Text.make_attr (Attr.get_color "IndianRed") 1 0 false in
  let cyan_attr = Text.make_attr (Attr.get_color "cyan3") 1 0 false in
  let yellow_attr = Text.make_attr (Attr.get_color "yellow3") 1 0 false in
  Color.color buf (Str.regexp "^Head:.*$") false cyan_attr;
  Color.color buf (Str.regexp "^[v>] .*$") false yellow_attr;
  Color.color buf (Str.regexp "^    \\+.*$") false green_attr;
  Color.color buf (Str.regexp "^    -.*$") false red_attr;
  Color.color buf (Str.regexp "^    @@.*$") false cyan_attr;
  ()

(*****************************************************************************)
(* Actions *)
(*****************************************************************************)

let get_line_info (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let text = buf.buf_text in
  let line = Text.point_line text frame.frm_point in
  let arr = Var.get_local buf line_map_var in
  if line >= 0 && line < Array.length arr
  then arr.(line)
  else Blank

(* claude: stage a file or hunk *)
let rec magit_stage (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let caps = frame.caps in
  let root = Var.get_local buf repo_root_var in
  let info = get_line_info frame in
  (match info with
  | FileHead { staged = false; file } ->
      git_string caps root (spf "add %s" (Filename.quote file)) |> ignore;
  | HunkHead { staged = false; file; hunk_header; _ }
  | HunkLine { staged = false; file; hunk_header; _ } ->
      stage_hunk caps root file hunk_header
  | SectionHead "Untracked" | SectionHead "Unstaged" ->
      git_string caps root "add -u" |> ignore
  | _ ->
      Message.message frame "Nothing to stage here"
  );
  render_status frame
[@@interactive]

and stage_hunk caps root file hunk_header =
  (* claude: generate a patch for just this hunk using git diff and filtering *)
  let diff_lines = git_lines caps root
      (spf "diff %s" (Filename.quote file)) in
  let patch = build_hunk_patch diff_lines hunk_header in
  if patch <> "" then begin
    let tmpfile = Filename.temp_file "magit" ".patch" in
    let oc = open_out tmpfile in
    output_string oc patch;
    close_out oc;
    git_string caps root
      (spf "apply --cached %s" (Filename.quote tmpfile)) |> ignore;
    Sys.remove tmpfile
  end

and build_hunk_patch diff_lines target_hunk_header =
  (* claude: reconstruct a valid patch containing only the target hunk *)
  let buf = Buffer.create 2048 in
  let in_header = ref true in
  let header_lines = ref [] in
  let found = ref false in
  let done_ = ref false in
  diff_lines |> List.iter (fun line ->
    if not !done_ then begin
      if !in_header then begin
        header_lines := line :: !header_lines;
        if String.length line >= 3 && String.sub line 0 3 = "+++" then
          in_header := false
      end
      else if String.length line >= 3 && String.sub line 0 3 = "@@ " then begin
        if !found then
          done_ := true
        else if line = target_hunk_header then begin
          (* emit header *)
          List.rev !header_lines |> List.iter (fun h ->
            Buffer.add_string buf h;
            Buffer.add_char buf '\n'
          );
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          found := true
        end
      end
      else if !found then begin
        Buffer.add_string buf line;
        Buffer.add_char buf '\n'
      end
    end
  );
  Buffer.contents buf

(* claude: unstage a file or hunk *)
let rec magit_unstage (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let caps = frame.caps in
  let root = Var.get_local buf repo_root_var in
  let info = get_line_info frame in
  (match info with
  | FileHead { staged = true; file } ->
      git_string caps root (spf "reset HEAD %s" (Filename.quote file))
      |> ignore
  | HunkHead { staged = true; file; hunk_header; _ }
  | HunkLine { staged = true; file; hunk_header; _ } ->
      unstage_hunk caps root file hunk_header
  | SectionHead "Staged" ->
      git_string caps root "reset HEAD" |> ignore
  | _ ->
      Message.message frame "Nothing to unstage here"
  );
  render_status frame
[@@interactive]

and unstage_hunk caps root file hunk_header =
  let diff_lines = git_lines caps root
      (spf "diff --cached %s" (Filename.quote file)) in
  let patch = build_hunk_patch diff_lines hunk_header in
  if patch <> "" then begin
    let tmpfile = Filename.temp_file "magit" ".patch" in
    let oc = open_out tmpfile in
    output_string oc patch;
    close_out oc;
    git_string caps root
      (spf "apply --cached --reverse %s" (Filename.quote tmpfile))
    |> ignore;
    Sys.remove tmpfile
  end

(* claude: toggle collapse of section/file at point *)
let magit_toggle (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let collapsed = Var.get_local buf collapsed_var in
  let info = get_line_info frame in
  let key =
    match info with
    | SectionHead s -> Some s
    | FileHead { staged; file } ->
        Some ((if staged then "S:" else "U:") ^ file)
    | _ -> None
  in
  (match key with
  | Some k ->
      if Hashtbl.mem collapsed k
      then Hashtbl.remove collapsed k
      else Hashtbl.replace collapsed k true;
      render_status frame
  | None -> Message.message frame "Nothing to toggle here")
[@@interactive]

(* claude: refresh the status buffer *)
let magit_refresh (frame : Frame.t) =
  render_status frame
[@@interactive]

(* claude: quit magit buffer, go back to previous buffer *)
let magit_quit (frame : Frame.t) =
  Multi_buffers.kill_buffer frame
[@@interactive]

(*****************************************************************************)
(* Commit *)
(*****************************************************************************)

let magit_commit (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let caps = frame.caps in
  let root = Var.get_local buf repo_root_var in
  (* claude: check there are staged changes *)
  let staged = git_lines caps root "diff --cached --name-only" in
  if staged =*= [] then
    Message.message frame "Nothing staged to commit"
  else
    Select.select_string frame "Commit message: " commit_hist ""
      (fun msg ->
         let escaped = Filename.quote msg in
         let output = git_string caps root (spf "commit -m %s" escaped) in
         Message.message frame output;
         render_status frame)
[@@interactive]

(*****************************************************************************)
(* Push *)
(*****************************************************************************)

let magit_push (frame : Frame.t) =
  let buf = frame.frm_buffer in
  let caps = frame.caps in
  let root = Var.get_local buf repo_root_var in
  Message.message frame "Pushing...";
  (* claude: run push in a thread to not block the UI *)
  let edt = Globals.editor () in
  Thread.create (fun () ->
    let output = git_string caps root "push" in
    Mutex.lock edt.edt_mutex;
    Message.message frame (if output = "" then "Push complete" else output);
    Mutex.unlock edt.edt_mutex;
    Top_window.update_display ()
  ) () |> ignore
[@@interactive]

(*****************************************************************************)
(* Mode and entry point *)
(*****************************************************************************)

let install _buf =
  ()

let mode = Ebuffer.new_major_mode "Magit" (Some install)

let magit_status (frame : Frame.t) =
  let edt = Globals.editor () in
  let dir = edt.edt_dirname in
  match find_repo_root frame.caps dir with
  | None ->
      Message.message frame (spf "Not a git repository: %s" dir)
  | Some root ->
      let buf =
        match Ebuffer.find_buffer_opt magit_buf_name with
        | Some buf ->
            Var.set_local buf repo_root_var root;
            buf
        | None ->
            let text = Text.create "" in
            let buf = Ebuffer.create magit_buf_name None text
                (Keymap.create ()) in
            Ebuffer.set_major_mode buf mode;
            Var.set_local buf repo_root_var root;
            Var.set_local buf collapsed_var (Hashtbl.create 16);
            Var.set_local buf line_map_var (Array.make 0 Blank);
            buf
      in
      Multi_buffers.set_previous_frame frame;
      Frame.change_buffer frame.caps frame.frm_window buf.buf_name;
      let top_window = Window.top frame.frm_window in
      let frame = top_window.top_active_frame in
      render_status frame
[@@interactive]

(*****************************************************************************)
(* Setup *)
(*****************************************************************************)

let _ =
  Hooks.add_start_hook (fun () ->
    [
      [(NormalMap, Char.code 's')], magit_stage;
      [(NormalMap, Char.code 'u')], magit_unstage;
      [(NormalMap, Char.code 'c')], magit_commit;
      [(NormalMap, Char.code 'p')], magit_push;
      [(NormalMap, Char.code 'g')], magit_refresh;
      [(NormalMap, Char.code 'q')], magit_quit;
      [(NormalMap, XK.xk_Tab)], magit_toggle;
      [(NormalMap, XK.xk_Return)], magit_toggle;
    ] |> List.iter (fun (key, action) ->
      Keymap.add_major_key mode key action)
  )
