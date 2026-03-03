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

(* Unit tests for Text.t (gap buffer) operations *)

let test_insert () =
  Testutil_efuns.init_editor ();
  let text = Text.create "" in
  let point = Text.new_point text in
  Text.insert text point "hello";
  let content = Text.to_string text in
  assert (content = "hello");
  Text.remove_point text point

let test_insert_multiple () =
  Testutil_efuns.init_editor ();
  let text = Text.create "" in
  let point = Text.new_point text in
  Text.insert text point "hello";
  (* point stays at position 0, so we must move forward to append *)
  Text.fmove text point 5;
  Text.insert text point " world";
  let content = Text.to_string text in
  assert (content = "hello world");
  Text.remove_point text point

let test_delete () =
  Testutil_efuns.init_editor ();
  let text = Text.create "hello world" in
  let point = Text.new_point text in
  (* Move to position 5 ("hello| world") *)
  Text.fmove text point 5;
  (* Delete " " (1 char forward) *)
  Text.delete text point 1;
  let content = Text.to_string text in
  assert (content = "helloworld");
  Text.remove_point text point

let test_point_movement () =
  Testutil_efuns.init_editor ();
  let text = Text.create "abcdef" in
  let point = Text.new_point text in
  (* Move forward 3 *)
  Text.fmove text point 3;
  assert (Text.point_col text point = 3);
  (* Move backward 1 *)
  Text.bmove text point 1;
  assert (Text.point_col text point = 2);
  Text.remove_point text point

let test_multiline () =
  Testutil_efuns.init_editor ();
  let text = Text.create "line1\nline2\nline3" in
  let point = Text.new_point text in
  assert (Text.point_line text point = 0);
  (* Move past first newline *)
  Text.fmove text point 6;
  assert (Text.point_line text point = 1);
  assert (Text.point_col text point = 0);
  Text.remove_point text point

let tests () =
  [
    Testo.create "text_insert" test_insert;
    Testo.create "text_insert_multiple" test_insert_multiple;
    Testo.create "text_delete" test_delete;
    Testo.create "text_point_movement" test_point_movement;
    Testo.create "text_multiline" test_multiline;
  ]
