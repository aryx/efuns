#TODO: switch to principia/docs/latest/mkcommon ?

SYNCFLAGS=-md5sum_in_auxfile -less_marks -lang ocaml

VERSION=6
DOC=Editor

SRC_ORIG=Editor.nw Editor_extra.nw

TOP=../

#TODO: to lpize at some point:
# dircolors.ml, shell.ml, outline_mode.ml, prog_modes/*, etc

#pad: I've indented some of the files I created and put them under the
# file from which they come from in the original Efuns from LeFessant.
SRC= \
  libs/commons/store.ml\
  libs/commons/store.mli\
  \
  src/core/efuns.ml\
   src/core/var.ml\
   src/core/parameter_option.ml\
   src/core/attr.ml\
   src/core/hooks.ml\
   src/core/action.ml\
   src/core/globals.ml\
   src/core/error.ml\
  src/core/text.ml\
  src/core/ebuffer.ml\
  src/core/keymap.ml\
  src/core/window.ml\
  src/core/frame.ml\
  src/core/top_window.ml\
  \
  src/features/misc_features.ml\
   src/features/edit.ml\
   src/features/move.ml\
   src/features/copy_paste.ml\
   src/features/scroll.ml\
   src/features/mouse.ml\
   src/features/highlight.ml\
   src/features/indent.ml\
   src/features/color.ml\
   src/features/structure.ml\
   src/features/electric.ml\
   src/features/transform.ml\
  src/features/minibuffer.ml\
  src/features/system.ml\
  src/features/select.ml\
  src/features/search.ml\
  src/features/interactive.ml\
  src/features/multi_buffers.ml\
  src/features/multi_frames.ml\
  src/features/abbrevs.ml\
  src/features/compil.ml\
  \
  config/default_config.ml\
  src/main/CLI.ml\
  src/main/Main.ml\
  \
  modes/major_modes/dired.ml\
  \
  modes/minor_modes/minor_mode_sample.ml\
  modes/minor_modes/abbrevs_mode.ml\
  modes/minor_modes/TODO_more/accents_mode.ml\
  modes/minor_modes/fill_mode.ml\
  modes/minor_modes/paren_mode.ml\
  modes/minor_modes/tab_mode.ml\
  \
  modes/prog_modes/makefile_mode.ml\
  modes/prog_modes/TODO_more/ocaml_toplevel.ml\
  \
  src/ipc/server.ml\
  src/ipc/efuns_client.ml\
  \
  todo/dynamic/eval.ml\
  todo/misc/efuns_filebrowser.ml\
  todo/misc/efuns_texbrowser.ml\

SRC_VIEWS=$(SRC:%=$(TOP)/%)

include ~/github/semgrep-internals/Makefile.common

index:
	$(INDEXER) -lang cmt $(TOP) \
    | grep -v 'TODO_NO_SOURCE_FOUND' \
    | grep -v '_build/default' \
    | sed -e 's#src/core/#core/#g' \
    > defs_and_uses.list
