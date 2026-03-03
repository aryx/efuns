# Bug: "Second Cursor" Error After Cancelling Incremental Search

**Root cause**: `Select.incremental_mini_buffer` sets `top_second_cursor <- Some frame`
before creating the minibuffer. When the user cancels with `C-g`, `Minibuffer.kill`
is called but it does NOT clear `top_second_cursor`. Only the normal "Return"
path (the `end_action` callback) clears it. This leaves a stale second cursor
reference. On the next display cycle, `Top_window.clean_display` detects the
inconsistency (second cursor exists but no minibuffer) and reports the error.

**Steps to reproduce**:

1. Open any file in efuns
2. Press `C-s` to start an incremental forward search
3. Type a few characters (e.g., `short`)
4. Press `C-g` to cancel the search
5. Now type any regular key or perform any action

**What happens**: You see an error message like:
  `clean_display: weird, second cursor for <buffer_name> but no minibuffer`

The error is reported by `Top_window.clean_display` (top_window.ml:170-174).

**Expected**: Cancelling the search should cleanly reset all state including
the second cursor.

**Note**: The same bug can be triggered with `query-replace` (`M-%`):
1. Press `M-%` (or however query-replace is bound)
2. Enter the search string and replacement string
3. When prompted "Replace? (y/n)", press `C-g` to cancel
4. The second cursor error appears on subsequent actions.

## Variant: isearch C-g specifically

The `isearch` function in `search.ml` adds its own `C-g` binding (line 321-325)
that calls `Minibuffer.kill` directly WITHOUT clearing `top_second_cursor`.
The `incremental_mini_buffer` only clears it in the `end_action` callback
(which runs on Return, not on C-g).

**Steps**:
1. `C-s` (start incremental search)
2. Type anything
3. `C-g` (cancel)
4. Any subsequent keypress triggers the "weird, second cursor" error

**Regression test**: `src/tests/unit_frame.ml` — `test_second_cursor_cleared_on_frame_kill`
