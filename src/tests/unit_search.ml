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

(* Unit tests for Search operations *)

(* claude: helper to simulate a keypress via the public handler API *)
let send_key top modifiers keysym =
  Top_window.handler top (Xtypes.XTKeyPress (modifiers, "", keysym))

(* Regression: isearch should not raise Not_found when the search string
   has no match in the buffer. Previously, Text.search_forward raised
   Not_found which propagated up to handle_key as an uncaught exception.
   Now isearch_s catches Not_found and shows "Failing I-search". *)
let test_isearch_no_match_no_exception () =
  let (top, frame, _text, _point) =
    Testutil_efuns.make_frame ~width:80 ~height:25 "hello world"
  in
  (* Display once so frm_table is populated *)
  Frame.display top frame;
  (* Send C-s to start isearch forward *)
  send_key top Xtypes.controlMask (Char.code 's');
  (* Now we're in the minibuffer with isearch active.
     Send 'z' — no match in "hello world", should NOT raise Not_found *)
  send_key top 0 (Char.code 'z')

(* Regression: isearch backward (C-r) should not raise Not_found when
   the point is at position 0 (beginning of buffer). Previously,
   Text.search_backward explicitly raised Not_found for pos 0. *)
let test_isearch_backward_at_bob_no_exception () =
  let (top, frame, _text, _point) =
    Testutil_efuns.make_frame ~width:80 ~height:25 "hello world"
  in
  Frame.display top frame;
  (* Send C-s to start isearch, then C-r to switch to backward *)
  send_key top Xtypes.controlMask (Char.code 's');
  (* Type 'h' which matches at pos 0 *)
  send_key top 0 (Char.code 'h');
  (* Switch to backward search with C-r — point is near beginning,
     should NOT raise Not_found *)
  send_key top Xtypes.controlMask (Char.code 'r')

let tests () =
  [
    Testo.create "isearch_no_match_no_exception"
      test_isearch_no_match_no_exception;
    Testo.create "isearch_backward_at_bob_no_exception"
      test_isearch_backward_at_bob_no_exception;
  ]
