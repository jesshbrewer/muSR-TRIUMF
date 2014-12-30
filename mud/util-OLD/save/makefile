#
#  Makefile for mud_util
#  mud.h comes from ../src , executable written in  ../bin
# $Log: makefile,v $
# Revision 1.4  2001/09/26 04:40:53  asnd
# Link to termcap (for readline 4)
#
# Revision 1.2  2001/07/04 04:52:00  asnd
# Add combine command to add/subtract/concatenate runs
#
# Revision 1.1  2001/04/03 17:29:24  suz
# original
# 
ARCH  = LINUX
OPT   = -O3

ifndef LIB_DIR
LIB_DIR = ../lib
endif
ifndef BIN_DIR
BIN_DIR = .
endif
ifndef SRC_DIR
SRC_DIR = ../src
endif
ifndef INSTALL_DIR
INSTALL_DIR = ../bin
endif

TARGET  = $(BIN_DIR)/mud_util
LIBMUD  = $(LIB_DIR)/libmud.a

CC =gcc
# get mud.h from src ; use readline (history) library
CFLAGS    = -I. -I$(SRC_DIR) -Dusereadline
LINKFLAGS = -L$(LIB_DIR) -lmud -lm -lreadline -lhistory -ltermcap

OBJS = mud_util.o getopt.o tri2mud.o mud2tri.o dat2mud.o mud2dat.o \
	mud2txt.o tri2txt.o combine.o

HEADERS = mud_util.h $(SRC_DIR)/mud.h

all : $(TARGET)

install : $(TARGET)
	cp $(TARGET) $(INSTALL_DIR)

clean : 
	rm $(OBJS) $(TARGET)

%.o : %.c mud_util.h $(SRC_DIR)/mud.h 
	$(CC) -c $(OPT) $(DEBUG) $(CFLAGS) $<

$(TARGET) : $(OBJS) $(LIBMUD)
	$(CC) -o $@ $(OBJS) $(DEBUG) $(LINKFLAGS)

getopt.o : tailor.h getopt.h
mud_util.o : getopt.h
mud2dat.o : trii_fmt.h
dat2mud.o : trii_fmt.h
mud2tri.o : trid_fmt.h
tri2mud.o : trid_fmt.h
