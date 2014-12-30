include ../../Makefile.inc

LIBDIR=../../lib
ASKLIB=$(LIBDIR)/libask.a

LIBRARY = \
	$(ASKLIB)(ask_noisy.o)		\
	$(ASKLIB)(ask51_noisy.o)	\
	$(ASKLIB)(askcom.o)		\
	$(ASKLIB)(askfile.o)		\
	$(ASKLIB)(askif_noisy.o)	\
	$(ASKLIB)(batchUNX.o)		\
	$(ASKLIB)(btd_noisy.o)		\
	$(ASKLIB)(cmli_noisy.o)		\
	$(ASKLIB)(cmli_50_noisy.o)	\
	$(ASKLIB)(convert_noisy.o)	\
	$(ASKLIB)(encdx.o)		\
	$(ASKLIB)(helpUNX.o)		\
	$(ASKLIB)(interactUNX.o)	\
	$(ASKLIB)(movec.o)		\
	$(ASKLIB)(parse_noisy.o)	\
	$(ASKLIB)(promptUNX.o)		\
	$(ASKLIB)(realtoch.o)		\
	$(ASKLIB)(replace_noisy.o)	\
	$(ASKLIB)(setc.o)		\
	$(ASKLIB)(sleepUNX.o)		\
	$(ASKLIB)(strlen.o)		\
	$(ASKLIB)(systemUNX.o)		\
	$(ASKLIB)(tell_noisy.o)		\
	$(ASKLIB)(trimblank_noisy.o)	\
	$(ASKLIB)(trim_zero.o)		\

LIB =	-L$(LIBDIR) -lask \
	-L$(LIBDIR) -leval \
        -L/lib64 -lreadline -lc 
##	-L$(LIBDIR) -lreadline

all:	../bin/test ../bin/testutil

../bin/test:	test.o $(LIBRARY) 
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB)

../bin/testutil:	testutil.o $(LIBRARY) 
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB)

tidy:	
	rm *.o
