#
#  Makefile for mud_util (aka convert and filewrite and ifilewrite)
#

INSTALL_DIR =	mud:
LIB_DIR = 	sys$disk:[-.lib]
CFLAGS =	$(CFLAGS)/include=($(LIB_DIR))/warn=(noinfo)
LINKFLAGS =	$(LINKFLAGS)

.IFDEF AXP
CRTL = 
.ELSE
CRTL =		,sys$disk:[]vaxcrtl/opt
.ENDIF

all : mud_util.exe
   @ continue

install : $(INSTALL_DIR)mud_util.exe
   @ continue

$(INSTALL_DIR)mud_util.exe : mud_util.exe
    - if(f$sea("$@").nes."") then delete/nolog $@;*
    copy/nolog $< $@

MUD_OBJS =	mud_util.obj,\
		getopt.obj,\
		tri2mud.obj,\
		mud2tri.obj,\
		dat2mud.obj,\
		mud2dat.obj,\
                mud2txt.obj,\
		tri2txt.obj,\
		combine.obj

mud_util.exe :  $(MUD_OBJS), $(LIB_DIR)mud.olb
    $(LINK)$(LINKFLAGS)/exe=$@ $(MUD_OBJS), $(LIB_DIR)mud/lib $(CRTL)

COMMON_HEADS =	$(LIB_DIR)mud.h

mud_util.obj : $(COMMON_HEADS)
tri2mud.obj : $(COMMON_HEADS), trid_fmt.h
mud2tri.obj : $(COMMON_HEADS), trid_fmt.h
dat2mud.obj : $(COMMON_HEADS), trii_fmt.h
mud2dat.obj : $(COMMON_HEADS), trii_fmt.h
mud2txt.obj : $(COMMON_HEADS)
combine.obj : $(COMMON_HEADS)

