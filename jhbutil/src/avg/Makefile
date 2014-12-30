include ../../../Makefile.inc

LIBDIR=../../../lib

LIB = 	-L$(LIBDIR) -ljhbutil \
	-L$(LIBDIR) -lask \
	-L$(LIBDIR) -leval \
        -L/lib64 -lreadline -lc -lX11 \

all: ../../../bin/avg ../../../bin/invavg 

../../../bin/avg: avg.o 
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB) 

../../../bin/invavg: invavg.o 
	$(FORTRAN) $(FORT_SWITCHES) $(DEBUG) -o $@ $< $(OBJ) $(LIB) 

tidy:	
	rm *.o
