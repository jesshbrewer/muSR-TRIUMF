include ../../Makefile.inc

LIBDIR=../../lib
UTLIB=$(LIBDIR)/libjhbutil.a

LIBRARY = \
	$(UTLIB)(avg.o) \
	$(UTLIB)(cfwhm.o) \
	$(UTLIB)(cpeaks.o) \
	$(UTLIB)(euler.o) \
	$(UTLIB)(invavg.o) \
	$(UTLIB)(linfit.o) \
	$(UTLIB)(load_value.o) \
	$(UTLIB)(lsqfit.o) \
	$(UTLIB)(matrixsubs.o) \
	$(UTLIB)(minfmt.o) \
	$(UTLIB)(minmax.o) \
	$(UTLIB)(neighb.o) \
	$(UTLIB)(order.o) \
	$(UTLIB)(peaks.o) \
	$(UTLIB)(plotsubs.o) \
	$(UTLIB)(randsubs.o) \
	$(UTLIB)(spinsubs.o) \
	$(UTLIB)(stdlat.o) \
	$(UTLIB)(transpose.o) \
	$(UTLIB)(vecsubs.o) \
	$(UTLIB)(wtdavg.o) \
	$(UTLIB)(xtalsubs.o) 

LIB = 	-L$(LIBDIR) -ljhbutil \
	-L$(LIBDIR) -ldoplot \
	-L$(LIBDIR) -lask \
	-L$(LIBDIR) -leval \
        -lreadline -ltermcap -lc \
	-L/usr/local/pgplot -lpgplot \
	-L/usr/X11R6/lib -lX11 \
	-L/usr/local/cern/lib -lmathlib -lpacklib

all: ../../bin/avg ../../bin/invavg 
words: ../../bin/words

../../bin/avg: avg.o $(LIBRARY)
	g77 -g -o $@ $< $(OBJ) $(LIB) -static

../../bin/invavg: invavg.o $(LIBRARY)
	g77 -g -o $@ $< $(OBJ) $(LIB) -static

../../bin/words: words.o $(LIBRARY)
	g77 -g -o $@ $< $(OBJ) $(LIB) -static

