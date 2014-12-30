#   makefile for MUD library.
#  $Log: makefile,v $
#  Revision 1.2  2010/08/25 23:52:46  asnd
#  Fix up builds on different platforms, including mac, windows, 64bit Linux, new Linux
#  dstros without termcap (readline is not used by default in mud_util for easy make
#  without configure).
#
#  Revision 1.1  2001/09/13 04:59:42  asnd
#  DA - Fixup make files (now separate, with overall Makefile)
#

ifndef LIB_DIR
LIB_DIR    :=.
endif
ifndef INSTALL_DIR
INSTALL_DIR := ../lib
endif

#  Specify intended Fortran compiler compatibility
#  (see cfortran.h.  g77 is our addition)

FORT = g77
#  FORT = f2cFortran
#  FORT = AbsoftUNIXFortran
#  FORT = AbsoftProFortran
#  etc.  Many compilers would need no definition.

CC    = gcc
CC_SWITCHES =  -O
DEBUG =
MFLAG =
# linux... add -m64 or -m32 flag like "make MFLAG=-m32"
CFLAGS = -I.
LIB  =  $(LIB_DIR)/libmud.a
OBJS =  mud.o mud_misc.o mud_all.o mud_new.o mud_gen.o \
        mud_tri_ti.o mud_encode.o \
        mud_friendly.o fmud_friendly.o mud_fort.o 

ifdef FORT
FDEF = -D$(FORT)
else
FDEF =
endif

%.o: %.c mud.h
	$(CC) $(MFLAG) $(DEBUG) $(CFLAGS) -c $(CC_SWITCHES) $(FDEF) $<

all: $(LIB)

install: $(LIB)
	cp $(LIB) $(INSTALL_DIR)

$(LIB): $(OBJS)
	rm -f $@
	ar r $@ $(OBJS)
	ranlib $@

clean:
	rm -f $(LIB) $(OBJS)

fmud_friendly.o : cfortran.h
