#
# Makefile for mud
#
# Examples:
# 	make
#	make clean
#	make tidy
#	make util
# 	make DEBUG="-g -DDEBUG"
#	make all MFLAG=-m64
#	make install INSTALL_DIR=~/
#	make dist
#       make ARCH=Darwin READLINE="-lreadline -lhistory"
#       ...
#
# Note that libc_tw is not used.  Equivalent functions or macros
# are defined in mud.h, mud.c, mud_util.c
#
#  $Log: Makefile,v $
#  Revision 1.8  2010/08/25 23:52:45  asnd
#  Fix up builds on different platforms, including mac, windows, 64bit Linux, new Linux
#  dstros without termcap (readline is not used by default in mud_util for easy make
#  without configure).
#
#  Revision 1.7  2005/06/21 01:27:01  asnd
#  Update global makefiles
#
#  Revision 1.6  2003/11/25 16:17:37  asnd
#  Add mud_friendly.html
#
#  Revision 1.5  2002/10/18 05:16:05  asnd
#  Add "make dist" to produce mud.zip and mud.tar.gz
#
#  Revision 1.4  2001/09/13 05:06:50  asnd
#  DA - Fixup make files (now separate, with overall Makefile)
#
#  Revision 1.3  2000/10/11 01:23:35  midas
#  - TW from linux to lib
#
#  Revision 1.2  2000/10/11 00:48:41  midas
#  re-arranged for CVS
#
#
ARCH = LINUX
SRC_DIR    := $(CURDIR)/src
UTIL_DIR   := $(CURDIR)/util
EXTRA_DIR  := $(CURDIR)/extra
LIB_DIR    := $(CURDIR)/lib
BIN_DIR    := $(CURDIR)/bin
INSTALL_DIR = /usr/local
#
# Targets
#
MUD_LIB  = $(LIB_DIR)/libmud.a
MUD_UTIL = $(BIN_DIR)/mud_util
CHTITLES = $(BIN_DIR)/change_titles
BADBIN   = $(BIN_DIR)/badbin

MAKEFILES = mud/descrip.mms mud/Makefile mud/makefile.dos mud/makefile.vms mud/Makefile.win 

export

all :   $(BIN_DIR) $(LIB_DIR) $(MUD_LIB) $(MUD_UTIL) extra

#####################################################################

$(LIB_DIR):
	@if [ ! -d  $(LIB_DIR) ] ; then \
	 @echo "Making directory $(LIB_DIR)" ; \
         mkdir $(LIB_DIR); \
	fi;

$(BIN_DIR):
	@if [ ! -d  $(BIN_DIR) ] ; then \
	 @echo "Making directory $(BIN_DIR)" ; \
         mkdir $(BIN_DIR); \
	fi;

$(MUD_LIB): FORCED
	cd $(SRC_DIR) && $(MAKE)

$(MUD_UTIL): $(MUD_LIB) FORCED
	cd $(UTIL_DIR) && $(MAKE) && $(MAKE) install

extra : $(MUD_LIB) FORCED
	cd $(EXTRA_DIR) && $(MAKE) && $(MAKE) install

install: $(MUD_LIB) $(MUD_UTIL) $(BADBIN) $(CHTITLES) FORCED
	cp $(MUD_LIB)  $(INSTALL_DIR)/lib
	cp $(MUD_UTIL) $(INSTALL_DIR)/bin 
	cp $(CHTITLES) $(BADBIN) $(INSTALL_DIR)/bin

clean : tidy FORCED
	rm -f $(LIB_DIR)/libmud.a \
	      $(UTIL_DIR)/mud_util $(BIN_DIR)/mud_util $(SRC_DIR)/libmud.a \
	      $(BIN_DIR)/badbin $(BIN_DIR)/change_titles $(BIN_DIR)/removerf \
	      $(CURDIR)/mud.tar.gz $(CURDIR)/mud.zip

tidy : 
	rm -f $(BIN_DIR)/*.o $(LIB_DIR)/*.o $(SRC_DIR)/*.o $(UTIL_DIR)/*.o \
	  && cd $(EXTRA_DIR) && $(MAKE) clean && cd $(CURDIR)

dist: clean
	rm -f mud.tar.gz mud.zip
	( cd .. ; \
	  tar zcvf mud/mud.tar.gz --exclude CVS $(MAKEFILES) mud/readme mud/mud_friendly.html mud/src mud/util mud/extra ; \
	  zip -r mud/mud $(MAKEFILES) mud/readme mud/mud_friendly.html mud/src mud/util mud/extra -x \*CVS\* ; \
	  cd $(CURDIR) )

lib : $(MUD_LIB)
util : $(MUD_UTIL)
FORCED:
