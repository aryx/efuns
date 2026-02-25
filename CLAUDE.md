# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Efuns

Efuns is an Emacs clone written in OCaml, originally by Fabrice Le Fessant (INRIA, 1999) and maintained/extended by Yoann Padioleau since 2015. It uses GTK/Cairo for rendering.

## Build Commands

```sh
# Build the main efuns and efuns_client binaries
make
# or equivalently:
dune build _build/install/default/bin/{efuns,efuns_client}

# Build everything (including all libraries)
make all

# Run tests
make test
# or:
dune runtest -f

# Clean
make clean

# Install
make install

# Lint with semgrep
make check

# Docker build
make build-docker
```

Before building, run `./configure` to set up OPAM dependencies. See `Dockerfile` for the full dependency chain (requires semgrep-libs and codemap to be installed first).

## Architecture

### Core Data Model (`src/core/efuns.ml`)

All fundamental types are defined in one large mutually-recursive type definition in `src/core/efuns.ml`:

- **`editor`** — global singleton holding all buffers (`edt_buffers`, `edt_files`), top windows, colors, fonts, and the global keymap
- **`buffer`** — file content (`buf_text : Text.t`), its major/minor modes, buffer-local variables (`buf_vars : Store.t`), keymap, and point/mark positions
- **`frame`** — a view of a buffer within a window; holds display state (cursor position, screen lines, etc.)
- **`top_window`** — a GUI window containing a tree of frames via `window` nodes
- **`window`** — tree node that is either a `WFrame`, `HComb`, or `VComb` (horizontal/vertical split)
- **`major_mode`** / **`minor_mode`** — modes with their own keymap, hooks, and buffer-local vars

### Directory Structure

```
src/core/        Core types and modules (efuns.ml, text.ml, keymap.ml, frame.ml, etc.)
src/features/    Editor features: search, move, edit, highlight, indent, minibuffer, etc.
src/graphics/    Graphics backends: gtk_cairo2/ is the main GTK+Cairo backend;
                 xdraw.ml defines the abstract graphics_backend record
src/ipc/         Server/client IPC: server.ml listens on a Unix socket,
                 efuns_client.ml sends commands
src/main/        Entry point: Main.ml → CLI.ml → Graphics_efuns.init
libs/commons/    Utility libs: options.ml (config), store.ml (typed vars), concur.ml, etc.
modes/major_modes/   Buffer menu, dired, shell, outline
modes/minor_modes/   Abbrevs, fill, paren matching, tab
modes/prog_modes/    OCaml, C, Lisp, Makefile modes (with ocamllex lexers)
modes/text_modes/    Plain text modes
modes/pfff_modes/    pfff program analysis integration (for smart navigation)
ppx_interactive/ Custom PPX rewriter (see below)
external/        Symlinks to external OPAM packages (semgrep-libs, codemap, etc.)
tests/           Test files by language (ml/, c/, lisp/, etc.) used for indent/highlight tests
```

### `[@@interactive]` PPX

The `ppx_interactive` rewriter automatically registers editor actions. Any function annotated with `[@@interactive]` is transformed so it calls `Action.define_action` at module load time:

```ocaml
let forward_char frm = ... [@@interactive]
(* becomes: *)
let forward_char frm = ...
let _ = Action.define_action "forward_char" forward_char
```

This is why the main executable uses `-linkall` — all modules must be linked to execute their top-level registrations.

### Key Conventions

- **Capability-based security**: `Cap.*` types (e.g., `Cap.forkew`, `Cap.env`) are threaded through functions that need OS access. `CLI.ml` defines the top-level capability set.
- **Buffer-local variables**: Use `Store.t` (via `Var` module) for per-buffer and per-mode state instead of global refs.
- **Hooks**: `Hooks.start_hooks` collects mode registrations run at startup after the editor is initialized.
- **Configuration**: User config lives in `~/.efunsrc` (loaded via `Options` module). Options are defined with `Options.define_option`.
- **Graphics abstraction**: `Xdraw.graphics_backend` is a record of functions; the GTK/Cairo implementation is in `src/graphics/gtk_cairo2/graphics_efuns.ml`.
- **IPC**: `efuns_client` sends `LoadFile` commands over a Unix socket at `/tmp/efuns-server.<user>.<display>:0`.
