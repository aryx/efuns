# Bug: Array Out of Bounds on Long Wrapped Lines

**Root cause**: `Frame.point_to_xy_opt` computes a `y` coordinate that can
exceed the frame table size when a long line wraps past the bottom of the
visible frame. `Frame.set_cursor` then uses this unchecked `y` to index into
`frm_table`, causing an array out of bounds exception.

**Steps to reproduce**:

1. Open `tests/bugfixes/frame_cursor_oob/bug_repro.txt` in efuns
2. Resize the window to be **small vertically** (~15-20 lines tall)
3. Use `M->` (Alt+Shift+>) or `C-End` to go to the end of the buffer
4. Use `C-v` / `M-v` (Page Down/Up) to scroll so that line 40 (the very
   long "LONGLINE: aaaa..." line) is near the **bottom** of the visible frame
5. Use `C-s` (isearch forward) and type `THE_END` to position the cursor
   at the end of the long line
6. Alternatively: use `C-e` (end-of-line) while on line 40

**What happens**: The long line wraps across multiple display rows. When the
cursor is at the end of the line, `point_to_xy_opt` computes `y = i + (x_nocut-1)/frm_cutline`.
If `i` is near the bottom of the frame and the line wraps 3-4 times, `y`
can exceed `frm_height - 1`. The subsequent `frm_table.(y)` access in
`set_cursor` crashes with `Invalid_argument "index out of bounds"`.

**Expected**: The cursor should be clamped or the frame should scroll so
the cursor position is visible, not crash.

**Regression test**: `src/tests/unit_frame.ml` — `test_point_to_xy_opt_long_line_no_oob`
