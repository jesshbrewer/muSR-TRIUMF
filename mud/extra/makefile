#
#  Makefile for change_titles and other extra utilities.
#  mud.h comes from ../src, executable written in  ../bin
# $Log: makefile,v $
# Revision 1.5  2010/08/25 23:52:46  asnd
# Fix up builds on different platforms, including mac, windows, 64bit Linux, new Linux
# dstros without termcap (readline is not used by default in mud_util for easy make
# without configure).
#
# Revision 1.4  2007/06/21 03:13:20  asnd
# Added removerf program to extras
#
# Revision 1.3  2005/06/21 00:49:03  asnd
# make make clean clean
#
# Revision 1.2  2005/06/18 06:19:47  asnd
# Contribute badbin-fixing program
#
# Revision 1.1  2005/06/14 01:38:31  asnd
# Create extra utilities dir, containing change_titles initially
#
# Default actions; override them on the "make" command line 
# (eg, make DEBUG=-g MFLAG=-m64 ARCH=Darwin) or change them here.

OPT   = -O3
DEBUG =
MFLAG =
ARCH  = LINUX
LIB_DIR = ../lib
SRC_DIR = ../src
INSTALL_DIR = ../bin

LIBMUD  = $(LIB_DIR)/libmud.a

TARGETS = change_titles badbin removerf

CC =gcc
# get mud.h from src; for mac/fink search /sw/ area
CFLAGS    = -I. -I$(SRC_DIR) -I/sw/include
LINKFLAGS = -L$(LIB_DIR) -lmud -L/sw/lib -lm

HEADERS = $(SRC_DIR)/mud.h

all : $(TARGETS)

clean : 
	rm -f $(TARGETS)

install :
	cp $(TARGETS) $(INSTALL_DIR)

change_titles: $(HEADERS) change_titles.c
	$(CC) $(MFLAG) $(DEBUG) -o $@ change_titles.c $(CFLAGS) $(LINKFLAGS)

badbin: $(HEADERS) badbin.c
	$(CC) $(MFLAG) $(DEBUG) -o $@ badbin.c $(CFLAGS) $(LINKFLAGS)

removerf: $(HEADERS) removerf.c
	$(CC) $(MFLAG) $(DEBUG) -o $@ removerf.c $(CFLAGS) $(LINKFLAGS)
