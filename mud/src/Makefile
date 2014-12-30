#   makefile for MUD library.
#  $Log: makefile,v $
#  Revision 1.1  2001/09/13 04:59:42  asnd
#  DA - Fixup make files (now separate, with overall Makefile)
#

## ifndef LIB_DIR
LIB_DIR    := ../../lib
## endif

#  Specify intended Fortran compiler compatibility
#  (see cfortran.h.  g77 is our addition)

FORT = g77
#  FORT = f2cFortran
#  FORT = AbsoftUNIXFortran
#  FORT = AbsoftProFortran
#  etc.  Many compilers would need no definition.

CC    = gcc
CC_SWITCHES = -O
DEBUG =

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
	$(CC) $(DEBUG) $(CFLAGS) -c $(CC_SWITCHES) $(FDEF) $<

all: $(LIB)

$(LIB): $(OBJS)
	rm -f $@
	ar r $@ $(OBJS)
	ranlib $@

clean:
	rm -f $(LIB) $(OBJS)

fmud_friendly.o : cfortran.h
