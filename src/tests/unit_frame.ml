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

(* Unit tests for Frame operations and cursor bug regressions *)

(* Bug 1 regression: Frame.set_cursor should not raise OOB when cursor_y
   exceeds frm_table length (happens with long lines that wrap).

   Scenario: short lines fill most of the frame, then a long line starts
   near the bottom. After display populates frm_table, we move the cursor
   to the end of the long line. set_cursor calls point_to_xy_opt which
   finds the long line at row i near the bottom, computes
   y = i + (x_nocut-1)/frm_cutline, and y can exceed frm_height - 1.
   The subsequent frm_table.(y) in set_cursor crashes with OOB. *)
let test_set_cursor_long_line_no_oob () =
  let width = 20 in
  let height = 12 in
  (* 7 short lines, then a very long line that will wrap many times *)
  let short_lines = String.concat "\n"
    ["line 1"; "line 2"; "line 3"; "line 4";
     "line 5"; "line 6"; "line 7"] in
  let long_line = String.make 200 'x' in
  let content = short_lines ^ "\n" ^ long_line in
  let (top, frame, text, point) =
    Testutil_efuns.make_frame ~width ~height content
  in
  (* First display: populates frm_table with point at beginning.
     The long line starts near the bottom of the frame. *)
  Frame.display top frame;
  (* Now move the cursor to the end of the long line.
     The table is still populated from the first display —
     set_cursor will be called without display recentering first. *)
  Text.set_position text point (Text.size text);
  (* This call should not raise Invalid_argument "index out of bounds" *)
  Frame.set_cursor frame

(* Bug 2 regression: Minibuffer.kill should clear top_second_cursor.

   Scenario: isearch sets top_second_cursor <- Some frame, then creates
   a minibuffer. When the user cancels with C-g, Minibuffer.kill is called
   but does NOT clear top_second_cursor. This leaves a stale reference
   that causes "weird, second cursor" errors on the next display cycle. *)
let test_minibuffer_kill_clears_second_cursor () =
  let (_top, (frame : Efuns.frame), _text, _point) =
    Testutil_efuns.make_frame ~width:80 ~height:25 "test content"
  in
  let (top : Efuns.top_window) = Window.top frame.frm_window in
  (* Simulate what isearch does: set second cursor before creating minibuffer *)
  top.top_second_cursor <- Some frame;
  (* Create a minibuffer (like C-s does) *)
  let local_map = Keymap.create () in
  let mini_frame = Minibuffer.create frame local_map "I-search: " in
  (* Simulate C-g: kill the minibuffer (this is what the C-g binding does) *)
  Minibuffer.kill mini_frame frame;
  (* After killing the minibuffer, top_second_cursor should be None.
     Currently it is NOT cleared by Minibuffer.kill — this is the bug. *)
  assert (top.top_second_cursor = None)

let tests () =
  [
    Testo.create "set_cursor_long_line_no_oob"
      test_set_cursor_long_line_no_oob;
    Testo.create "minibuffer_kill_clears_second_cursor"
      test_minibuffer_kill_clears_second_cursor;
  ]
