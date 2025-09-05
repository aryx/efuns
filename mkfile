#############################################################################
# Configuration section
#############################################################################
< mkconfig

#TODO: to port to ocaml-light (and plan9) you need to handle ppx [@@interactive]!
# maybe simpler to begin with consider them as attribute
# and generate a big file from Linux that store all the code
# by those interactive and use this file for ocaml-light
#</$objtype/mkfile

#############################################################################
# Variables
#############################################################################

#BACKENDDIR=graphics/libdraw

# commons/common.ml commons/file_type.ml commons/simple_color.ml \

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

XXX=\
 src/core/text.ml\
 src/core/efuns.ml\
 src/core/globals.ml\
 src/core/var.ml\
 src/core/attr.ml\
 src/core/action.ml\
 src/core/hooks.ml\
 src/core/keymap.ml\
 src/core/ebuffer.ml\
 src/core/window.ml\
 src/core/frame.ml\
 src/core/top_window.ml\

SRC2=\
 \
 src/features/simple.ml\
 src/features/mouse.ml\
 src/features/highlight.ml\
 src/features/parameter.ml\
 src/features/indent.ml\
 src/features/structure.ml\
 src/features/minibuffer.ml\
 src/features/multi_frames.ml\
 src/features/select.ml\
 src/features/interactive.ml\
 src/features/multi_buffers.ml\
 src/features/complexe.ml\
 src/features/abbrevs.ml\
 src/features/system.ml\
 src/features/dircolors.ml\
 src/features/compil.ml\
 src/features/search.ml\
 \
 modes/minor_modes/minor_mode_sample.ml\
 modes/minor_modes/paren_mode.ml\
 modes/minor_modes/abbrevs_mode.ml\
 modes/minor_modes/fill_mode.ml\
 modes/minor_modes/tab_mode.ml\
 \
 modes/major_modes/dired.ml\
 modes/major_modes/buffer_menu.ml\
 modes/major_modes/shell.ml\
 modes/major_modes/outline_mode.ml\
 \
 modes/prog_modes/pl_colors.ml\
 modes/prog_modes/makefile_mode.ml\
 modes/prog_modes/ocaml_mode.ml\
 modes/prog_modes/lisp_mode.ml\
 modes/prog_modes/c_mode.ml\
 \
 modes/text_modes/org_mode.ml\
 modes/text_modes/tex_mode.ml\
 modes/text_modes/html_mode.ml\
 \
 src/ipc/server.ml \
 \
 std_efunsrc.ml\
 pad.ml\
 graphics/libdraw/draw.ml \
 graphics/libdraw/graphics_efuns.ml \
 main.ml
#alt: build separate libs and have split mkfile instead of a single one
# but nothing depends on efuns (yet) so simpler to have a single mkfile

#COBJS=commons/realpath.$O graphics/libdraw/draw.$O


INCLUDES=\
 -I $XIX/lib_core/collections \
 -I $XIX/lib_core/commons \
 -I libs/commons \
 -I src/graphics \
 -I src/core \

#TODO: factorize XIX_LIBS=lib_core/collections lib_core_commons
LIBS=$XIX/lib_core/collections/lib.cma $XIX/lib_core/commons/lib.cma

# -I $BACKENDDIR

SYSLIBS=str.cma unix.cma  threads.cma

##############################################################################
# Generic variables
##############################################################################


OBJS=${SRC:%.ml=%.cmo}
CMIS=${OBJS:%.cmo=%.cmi}
#SYSCLIBS=${SYSLIBS:%.cma=$LIBDIR/lib%.a}

#CC=pcc
#LD=pcc
#CINCLUDES= -I$LIBDIR
# -B to disable the check for missing return, which is flagged
# because of CAMLReturn
#CFLAGS=-FVB -D_POSIX_SOURCE -D_BSD_EXTENSION -D_PLAN9_SOURCE $CINCLUDES

##############################################################################
# Top rules
##############################################################################

all:V: efuns.byte

# currently pcc does not accept -L so I replaced -cclib -unix by
# the more explicit /usr/local/lib/ocaml/libunix.a
#old:$OCAMLC str.cma unix.cma threads.cma  -custom -cclib -lstr -cclib -lunix -cclib -lthreads $COBJS  $OBJS -o $target

efuns.byte: $OBJS $COBJS
	$OCAMLC $LINKFLAGS -custom $SYSLIBS $SYSCLIBS $LIBS $COBJS $OBJS -o $target

clean:V:
    rm -f $OBJS $CMIS $COBJS
    rm -f *.[58] *.byte


MODES= \
 prog_modes/ocaml_mode.ml prog_modes/c_mode.ml prog_modes/lisp_mode.ml \
 text_modes/tex_mode.ml text_modes/html_mode.ml

#beforedepend: $MODES
beforedepend:VQ:
	echo nothing
prog_modes/ocaml_mode.ml: prog_modes/ocaml_mode.mll
	$OCAMLLEX $prereq
prog_modes/c_mode.ml: prog_modes/c_mode.mll
	$OCAMLLEX $prereq
prog_modes/lisp_mode.ml: prog_modes/lisp_mode.mll
	$OCAMLLEX $prereq
text_modes/tex_mode.ml: text_modes/tex_mode.mll
	$OCAMLLEX $prereq
text_modes/html_mode.ml: text_modes/html_mode.mll
	$OCAMLLEX $prereq


MLIS=${SRC:%.ml=%.mli}

depend:V: beforedepend
	$OCAMLDEP $INCLUDES $SRC $MLIS > .depend
#  | grep -v -e '.* :$' > .depend

##############################################################################
# Generic rules
##############################################################################

# do not use prereq or it will include also the .cmi in the command line
# because of the .depend file that also define some rules
%.cmo: %.ml
	$OCAMLC $INCLUDES $COMPFLAGS -c $stem.ml

%.cmi: %.mli
	$OCAMLC $INCLUDES -c $stem.mli

#%.$O: %.c
#	$CC $CFLAGS -v -c $stem.c -o $stem.$O


<.depend
