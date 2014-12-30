#
#  Makefile for mud_util
#
#
#    Copyright (C) 1994-2010 TRIUMF (Vancouver, Canada)
#    T. Whidden, D. Arseneau, S. Daviel
#    
#    Released under the GNU LGPL - see http://www.gnu.org/licenses
#
#    This program is free software; you can distribute it and/or modify it under 
#    the terms of the Lesser GNU General Public License as published by the Free 
#    Software Foundation; either version 2 of the License, or any later version. 
#    Accordingly, this program is distributed in the hope that it will be useful, 
#    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
#    or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License 
#    for more details.
#
#  mud.h comes from ../src , executable written in  ../bin
#
# $Log: makefile,v $
# Revision 1.8  2010/08/25 23:52:46  asnd
# Fix up builds on different platforms, including mac, windows, 64bit Linux, new Linux
# dstros without termcap (readline is not used by default in mud_util for easy make
# without configure).
#
# Revision 1.7  2007/02/03 05:42:38  asnd
# Incorporate ral2mud and psi2mud for (old) RAL and PSI data files
#
# Revision 1.6  2005/05/06 00:43:39  asnd
# Fix ARCH test for termcap inclusion
#
# Revision 1.5  2005/03/22 02:02:39  asnd
# Update for compiling on Mac osx Darwin
#
# Revision 1.4  2001/09/26 04:40:53  asnd
# Link to termcap (for readline 4)
#
# Revision 1.2  2001/07/04 04:52:00  asnd
# Add combine command to add/subtract/concatenate runs
#
# Revision 1.1  2001/04/03 17:29:24  suz
# original
# 
# Default actions; override them on the "make" command line 
# (eg, make DEBUG=-g ARCH=Darwin MFLAG=-m32) or change them here.
#
# The readline library (for command-line editing) is not used
# by default, use    make READLINE="-lreadline -lhistory"
# or similar to enable. The combination of libraries varies by
# system, and may include -ltermcap.
#
# When getopt is not available in the local C library, or an old
# version is privided, then use the one given here with 
# "make GETOPT=getopt.o".

OPT   = -O3
DEBUG =
MFLAG =
ARCH  = LINUX
LIB_DIR = ../lib
BIN_DIR = .
SRC_DIR = ../src
INSTALL_DIR = ../bin
READLINE =
GETOPT = 
# or GETOPT = getopt.o

TARGET  = $(BIN_DIR)/mud_util
LIBMUD  = $(LIB_DIR)/libmud.a

CC =gcc
# get mud.h from src; for mac/fink search /sw/ area

CFLAGS    = -I. -I$(SRC_DIR) -I/sw/include 
LINKFLAGS = -lm -L$(LIB_DIR) -lmud -L/sw/lib

# Maybe use readline 
ifneq ($(READLINE),)
CFLAGS  += -Dusereadline
LINKFLAGS += $(READLINE)
endif

ifeq ($(ARCH),Darwin)
GETOPT=getopt.o
endif

OBJS = mud_util.o $(GETOPT) tri2mud.o mud2tri.o dat2mud.o mud2dat.o \
	mud2txt.o tri2txt.o combine.o ral2mud.o psi2mud.o

HEADERS = mud_util.h $(SRC_DIR)/mud.h

all : $(TARGET)

install : $(TARGET)
	cp $(TARGET) $(INSTALL_DIR)

clean : 
	rm -f $(OBJS) $(TARGET)

%.o : %.c mud_util.h $(SRC_DIR)/mud.h 
	$(CC) $(MFLAG) -c $(OPT) $(DEBUG) $(CFLAGS) $<

$(TARGET) : $(OBJS) $(LIBMUD)
	$(CC) $(MFLAG) -o $@ $(OBJS) $(DEBUG) $(LINKFLAGS)

getopt.o : tailor.h getopt.h
mud_util.o : getopt.h
mud2dat.o : trii_fmt.h
dat2mud.o : trii_fmt.h
mud2tri.o : trid_fmt.h
tri2mud.o : trid_fmt.h
ral2mud.o : ral_fmt.h
psi2mud.o : psi_fmt.h
