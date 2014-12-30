include ../../Makefile.inc

LIBDIR=../../lib
EVALLIB=$(LIBDIR)/libeval.a

LIBRARY = \
	$(EVALLIB)(eval.o)	\
	$(EVALLIB)(evalfun.o)	\
	$(EVALLIB)(evaltab.o)	\
	$(EVALLIB)(evaltok.o)	\
	$(EVALLIB)(upper.o)	

LIB =	-L$(LIBDIR) -leval 

all:	../bin/test  ../bin/eval

tidy:	
	rm *.o

../bin/test:	test.o $(LIBRARY) 
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB)

../bin/eval:	evaluate.o $(LIBRARY) 
	$(FORTRAN) -o $@ $< $(OBJ) $(LIB)
