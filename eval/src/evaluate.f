	logical OK, LOC_EVAL
	real*8 VAL
	character argv*32
	character*255 STR
	integer i, iargc, n

ccc FAILS!	call EVALLOAD('/home/jess/CVS/eval/src/consts.def')
	STR = ' '
	n = iargc()
	do i = 1, n
	   argv = ' '
           call getarg( i, argv )
cc           write( *, '( i2, 1x, a )' ) i, argv
ccc(gfortran!)	   STR = trim(STR)//argv
	end do
ccc(gfortran!)	   	OK = LOC_EVAL( trim(STR), VAL )
   	OK = LOC_EVAL( STR, VAL )
	if ( OK ) then
		write (*,2222) STR,VAL
ccc(gfortran!)		write (*,2222) trim(STR),VAL
 2222		format ( A,' = ',G16.9 )
	else
ccc(gfortran!)      write (*,3333) trim(STR)
	   write (*,3333) STR
 3333		format ( 1X,'LOC_EVAL cannot evaluate "',A,'"' )
	endif
	call exit
	end
