	subroutine MINMAX (N, X, XMIN, XMAX)
C
C...Find the MINimum and MAXiumum of the real*4 array X.
C
	real*4 X(*), XMIN, XMAX
	integer*4 N, i
C
	XMIN = X(1) 
	XMAX = X(1) 
	do i=1,N
		XMIN = AMIN1(XMIN, X(i))
		XMAX = AMAX1(XMAX, X(i))
	end do
	return 
	end 
