#
# Makefile for mud
#
# Examples:
# 	make
#	make clean
#	make tidy
#	make util
# 	make lib 'DEBUG=-g'
#	make install 'INSTALL_DIR=~/'
#       ...
#
# Note that libc_tw is not used.  Equivalent functions or macros
# are defined in mud.h, mud.c, mud_util.c
#
#  $Log: Makefile,v $
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
LIB_DIR    := ../lib
BIN_DIR    := ../bin
## INSTALL_DIR = /usr/local
INSTALL_DIR = ..
#
# Targets
#
MUD_LIB  = $(LIB_DIR)/libmud.a
MUD_UTIL = $(BIN_DIR)/mud_util
export

all :   $(BIN_DIR) $(LIB_DIR) $(MUD_LIB) $(MUD_UTIL)

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
	cd $(UTIL_DIR) && $(MAKE)

install: $(MUD_LIB) $(MUD_UTIL) FORCED
	cp $(MUD_LIB)  $(INSTALL_DIR)/lib
	cp $(MUD_UTIL) $(INSTALL_DIR)/bin 

clean : tidy FORCED
	rm -f $(BIN_DIR)/mud_util $(LIB_DIR)/libmud.a \
	      $(UTIL_DIR)/mud_util $(SRC_DIR)/libmud.a 

tidy :
	rm -f $(BIN_DIR)/*.o $(LIB_DIR)/*.o $(SRC_DIR)/*.o $(UTIL_DIR)/*.o 

lib : $(MUD_LIB)
util : $(MUD_UTIL)
FORCED:
