	logical OK, LOC_EVAL
	real*8 VAL
	character*7 STR
	data STR /'sqrt(2)'/
	OK = LOC_EVAL( STR, VAL )
	if ( OK ) then
		write (*,1111) STR, VAL
 1111		format ( 1X,'"',A,'" returns ',G12.5 )
	else
		write (*,2222) STR
 2222		format ( 1X,'LOC_EVAL cannot evaluate "',A,'"' )
	endif
	call exit
	end
