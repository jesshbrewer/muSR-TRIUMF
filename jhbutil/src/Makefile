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
	$(UTLIB)(xtalsubs.o) \
	$(UTLIB)(xyreaddb.o) 

LIB = 	-L$(LIBDIR) -ljhbutil \
	-L$(LIBDIR) -ldoplot \
	-L$(LIBDIR) -lask \
	-L$(LIBDIR) -leval \
        -L/lib64 -lreadline -lc -lX11 \
	-L/usr/lib64 -lpgplot \
	-L/usr/lib64/cernlib/2006-g77/lib/ -lmathlib -lpacklib

all: ../../bin/avg ../../bin/invavg ../../bin/words

../../bin/avg: avg.o $(LIBRARY)
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB) 

../../bin/invavg: invavg.o $(LIBRARY)
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB) 

../../bin/words: words.o $(LIBRARY)
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB) 

tidy:	
	rm *.o
