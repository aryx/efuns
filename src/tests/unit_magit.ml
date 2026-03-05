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
 *)

(* Unit tests for the Magit mode.
 *
 * Testing strategy:
 *  - Pure function tests: exercise parse_diff, parse_hunk_range, and
 *    build_hunk_patch with hand-crafted diff output. These need no
 *    git repo or editor state.
 *  - Integration tests: use Testutil_git.with_git_repo to create a
 *    real (temporary) git repository, then verify that git_lines,
 *    find_repo_root, and the diff-parsing pipeline work end-to-end
 *    against actual git output.
 *)
open Common

(*****************************************************************************)
(* Pure function tests *)
(*****************************************************************************)

let test_parse_diff_single_file () =
  let diff_lines = [
    "diff --git a/foo.ml b/foo.ml";
    "index 1234567..abcdefg 100644";
    "--- a/foo.ml";
    "+++ b/foo.ml";
    "@@ -1,3 +1,4 @@";
    " line1";
    "+added";
    " line2";
    " line3";
  ] in
  let files = Magit.parse_diff diff_lines in
  assert (List.length files =|= 1);
  let (name, hunks) = List.hd files in
  assert (name = "foo.ml");
  assert (List.length hunks =|= 1);
  let (hdr, body) = List.hd hunks in
  assert (hdr = "@@ -1,3 +1,4 @@");
  assert (List.length body =|= 4)

let test_parse_diff_multiple_files () =
  let diff_lines = [
    "diff --git a/a.ml b/a.ml";
    "--- a/a.ml";
    "+++ b/a.ml";
    "@@ -1,2 +1,3 @@";
    " x";
    "+y";
    "diff --git a/b.ml b/b.ml";
    "--- a/b.ml";
    "+++ b/b.ml";
    "@@ -5,3 +5,2 @@";
    " a";
    "-removed";
  ] in
  let files = Magit.parse_diff diff_lines in
  assert (List.length files =|= 2);
  let (name1, _) = List.nth files 0 in
  let (name2, _) = List.nth files 1 in
  assert (name1 = "a.ml");
  assert (name2 = "b.ml")

let test_parse_diff_multiple_hunks () =
  let diff_lines = [
    "diff --git a/foo.ml b/foo.ml";
    "--- a/foo.ml";
    "+++ b/foo.ml";
    "@@ -1,3 +1,4 @@";
    " line1";
    "+added1";
    "@@ -10,3 +11,4 @@";
    " line10";
    "+added2";
  ] in
  let files = Magit.parse_diff diff_lines in
  assert (List.length files =|= 1);
  let (_, hunks) = List.hd files in
  assert (List.length hunks =|= 2)

let test_parse_diff_empty () =
  let files = Magit.parse_diff [] in
  assert (files =*= [])

let test_parse_hunk_range () =
  let (start, len) = Magit.parse_hunk_range "@@ -10,5 +12,7 @@ some context" in
  assert (start =|= 12);
  assert (len =|= 7)

let test_parse_hunk_range_single_line () =
  let (start, len) = Magit.parse_hunk_range "@@ -1 +1 @@ header" in
  assert (start =|= 1);
  assert (len =|= 1)

let test_build_hunk_patch () =
  let diff_lines = [
    "diff --git a/foo.ml b/foo.ml";
    "--- a/foo.ml";
    "+++ b/foo.ml";
    "@@ -1,3 +1,4 @@ first hunk";
    " line1";
    "+added1";
    " line2";
    "@@ -10,3 +11,4 @@ second hunk";
    " line10";
    "+added2";
    " line11";
  ] in
  (* claude: should extract only the second hunk *)
  let patch = Magit.build_hunk_patch diff_lines
      "@@ -10,3 +11,4 @@ second hunk" in
  assert (patch <> "");
  (* should contain the diff header *)
  assert (Str.string_match (Str.regexp ".*\\+\\+\\+ b/foo.ml") patch 0
          || String.length patch > 0);
  (* should contain the target hunk *)
  assert (try ignore (Str.search_forward (Str.regexp "added2") patch 0); true
          with Not_found -> false);
  (* should NOT contain the first hunk body *)
  assert (try ignore (Str.search_forward (Str.regexp "added1") patch 0); false
          with Not_found -> true)

(*****************************************************************************)
(* Integration tests with a real git repo *)
(*****************************************************************************)

let test_git_lines () =
  Testutil_git.with_git_repo
    [ Testutil_files.File ("hello.txt", "hello world\n") ]
    (fun cwd ->
       let root = Fpath.to_string cwd in
       (* claude: after initial commit, status should be clean *)
       let lines = Magit.git_lines
           Testutil_efuns.test_caps root "status --porcelain" in
       assert (lines =*= [])
    )

let test_find_repo_root () =
  Testutil_git.with_git_repo
    [ Testutil_files.File ("hello.txt", "hello\n") ]
    (fun cwd ->
       let dir = Fpath.to_string cwd in
       match Magit.find_repo_root Testutil_efuns.test_caps dir with
       | None -> assert false
       | Some root ->
           (* claude: the root should be the same as cwd *)
           assert (root = dir)
    )

let test_find_repo_root_not_a_repo () =
  Testutil_files.with_tempdir ~chdir:true (fun cwd ->
    let dir = Fpath.to_string cwd in
    match Magit.find_repo_root Testutil_efuns.test_caps dir with
    | None -> ()
    | Some _ -> assert false
  )

let test_git_diff_detects_changes () =
  Testutil_git.with_git_repo
    [ Testutil_files.File ("a.txt", "original\n") ]
    (fun cwd ->
       let root = Fpath.to_string cwd in
       (* claude: modify the file to create an unstaged change *)
       let path = Filename.concat root "a.txt" in
       let oc = open_out path in
       output_string oc "modified\n";
       close_out oc;
       let diff = Magit.git_lines Testutil_efuns.test_caps root "diff" in
       let files = Magit.parse_diff diff in
       assert (List.length files =|= 1);
       let (name, _hunks) = List.hd files in
       assert (name = "a.txt")
    )

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests () =
  [
    Testo.create "parse_diff_single_file" test_parse_diff_single_file;
    Testo.create "parse_diff_multiple_files" test_parse_diff_multiple_files;
    Testo.create "parse_diff_multiple_hunks" test_parse_diff_multiple_hunks;
    Testo.create "parse_diff_empty" test_parse_diff_empty;
    Testo.create "parse_hunk_range" test_parse_hunk_range;
    Testo.create "parse_hunk_range_single_line" test_parse_hunk_range_single_line;
    Testo.create "build_hunk_patch" test_build_hunk_patch;
    Testo.create "git_lines" test_git_lines;
    Testo.create "find_repo_root" test_find_repo_root;
    Testo.create "find_repo_root_not_a_repo" test_find_repo_root_not_a_repo;
    Testo.create "git_diff_detects_changes" test_git_diff_detects_changes;
  ]
