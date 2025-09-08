#############################################################################
# Configuration section
#############################################################################
< mkconfig

#############################################################################
# Variables
#############################################################################

LEXERS= \
 modes/prog_modes/c_lexer.ml modes/prog_modes/lisp_lexer.ml modes/prog_modes/ocaml_lexer.ml \
 modes/text_modes/tex_mode.ml modes/text_modes/html_mode.ml

#alt: build separate lib.cma and have split mkfile instead of a single one
# but nothing depends on efuns (yet) so simpler to have a single mkfile
# and a big SRC
SRC=\
 \
 libs/commons/utils.ml libs/commons/str2.ml\
 libs/commons/log.ml\
 libs/commons/options.ml\
 libs/commons/store.ml\
 libs/commons/concur.ml\
 \
 src/graphics/xtypes.ml\
 src/graphics/xdraw.ml\
 src/graphics/xK.ml\
 \
 src/core/error.ml\
 src/core/text.ml\
 src/core/efuns.ml\
 src/core/globals.ml\
 src/core/var.ml\
 src/core/parameter_option.ml\
 src/core/attr.ml\
 src/core/action.ml\
 src/core/hooks.ml\
 src/core/keymap.ml\
 src/core/ebuffer.ml\
 src/core/window.ml\
 src/core/frame.ml\
 src/core/top_window.ml\
 \
 src/features/highlight.ml\
 src/features/message.ml\
 src/features/move.ml\
 src/features/copy_paste.ml\
 src/features/edit.ml\
 src/features/electric.ml\
 src/features/mouse.ml\
 src/features/indent.ml\
 src/features/structure.ml\
 src/features/minibuffer.ml\
 src/features/multi_frames.ml\
 src/features/scroll.ml\
 src/features/select.ml\
 src/features/interactive.ml\
 src/features/multi_buffers.ml\
 src/features/abbrevs.ml\
 src/features/system.ml\
 src/features/color.ml\
 src/features/dircolors.ml\
 src/features/compil.ml\
 src/features/search.ml\
 src/features/transform.ml\
 src/features/rectangle.ml\
 src/features/macros.ml\
 src/features/misc_features.ml\
 \
 modes/minor_modes/minor_modes.ml\
 modes/minor_modes/minor_mode_sample.ml\
 modes/minor_modes/paren_mode.ml\
 modes/minor_modes/abbrevs_mode.ml\
 modes/minor_modes/fill_mode.ml\
 modes/minor_modes/tab_mode.ml\
 \
 modes/major_modes/major_modes.ml\
 modes/major_modes/dired.ml\
 modes/major_modes/buffer_menu.ml\
 modes/major_modes/shell.ml\
 modes/major_modes/outline_mode.ml\
 \
 modes/prog_modes/pl_colors.ml\
 modes/prog_modes/common_lexer.ml\
 modes/prog_modes/common_indenter.ml\
 $LEXERS \
 modes/prog_modes/makefile_mode.ml\
 modes/prog_modes/ocaml_mode.ml\
 modes/prog_modes/lisp_mode.ml\
 modes/prog_modes/c_mode.ml\
 modes/text_modes/org_mode.ml\
 \
 src/ipc/server.ml \
 \
 config/default_config.ml\
 config/pad.ml\

# std_efunsrc.ml\
# main.ml
# graphics/libdraw/draw.ml \
# graphics/libdraw/graphics_efuns.ml \
#COBJS=commons/realpath.$O graphics/libdraw/draw.$O

# we need xix-libs when we don't use semgrep-libs
INCLUDES=\
 -I $XIX/lib_core/collections -I $XIX/lib_core/commons \
 -I libs/commons \
 -I src/graphics \
 -I src/core -I src/features -I src/ipc \
 -I modes/minor_modes -I modes/major_modes -I modes/prog_modes \
 -I $EXTERNAL_DIRS
# -I $BACKENDDIR

#LESS: factorize XIX_LIBS=lib_core/collections lib_core_commons
LIBS=$XIX/lib_core/collections/lib.cma $XIX/lib_core/commons/lib.cma

SYSLIBS=str.cma unix.cma  threads.cma

##############################################################################
# Generic variables
##############################################################################

OBJS=${SRC:%.ml=%.cmo}
CMIS=${OBJS:%.cmo=%.cmi}

##############################################################################
# Top rules
##############################################################################

all:V: efuns.byte

efuns.byte: $OBJS $COBJS
	$OCAMLC $INCLUDES $LINKFLAGS $SYSLIBS $EXTERNAL_LIBS $SYSCLIBS $LIBS $COBJS $OBJS -o $target

clean:V:
    rm -f $OBJS $CMIS $COBJS
    rm -f *.[5678vij] *.byte

nuke:V: clean
    rm -f $LEXERS

beforedepend:VQ: $LEXERS

#ugly: factorize with % rule below but then get vacuous node detected
# in mk hence the duplication for now
#%.ml: %.mll
#	$OCAMLLEX $prereq

modes/prog_modes/ocaml_lexer.ml: modes/prog_modes/ocaml_lexer.mll
	$OCAMLLEX $prereq
modes/prog_modes/c_lexer.ml: modes/prog_modes/c_lexer.mll
	$OCAMLLEX $prereq
modes/prog_modes/lisp_lexer.ml: modes/prog_modes/lisp_lexer.mll
	$OCAMLLEX $prereq

modes/text_modes/tex_mode.ml: modes/text_modes/tex_mode.mll
	$OCAMLLEX $prereq
modes/text_modes/html_mode.ml: modes/text_modes/html_mode.mll
	$OCAMLLEX $prereq

MLIS=${SRC:%.ml=%.mli}

depend:V: beforedepend
	$OCAMLDEP $INCLUDES $SRC $MLIS > .depend
#  | grep -v -e '.* :$' > .depend

##############################################################################
# Generic rules
##############################################################################

# do not use $prereq or it will include also the .cmi in the command line
# because of the .depend file that also define some rules
%.cmo: %.ml
	$OCAMLC $INCLUDES $COMPFLAGS -c $stem.ml

%.cmi: %.mli
	$OCAMLC $INCLUDES -c $stem.mli

#%.$O: %.c
#	$CC $CFLAGS -v -c $stem.c -o $stem.$O

##############################################################################
# Automatic dependencies (ocamldep)
##############################################################################

<.depend
