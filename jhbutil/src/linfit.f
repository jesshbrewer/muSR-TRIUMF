	SUBROUTINE LIN_FIT (N, X, DX, X0, DX0, SLOPE, DSLOPE, STDDEV)
C-------------------------------------------------------------------------------
C...Make a linear least-squares fit to a collection of points (X,DX) 
C   and return their zero-intercept and slope (in "per bin" units): 
C   POINTS WITH ZERO ERRORS ARE IGNORED!
C-------------------------------------------------------------------------------
	DIMENSION X(*), DX(*)
C===============================================================================
	STDDEV = 0.0
	X0 = X(1)
	DX0 = DX(1)
	SLOPE = 0.0
	DSLOPE = 0.0
	IF (N .LT. 2) RETURN
C
C...WEIGHTED fit:
C
	SUMW = 0.0
	SUMT = 0.0
	SUMTT = 0.0
	SUMX = 0.0
	SUMTX = 0.0
	ANNN = 0.0
C
	DO 2050 I=1,N
	IF (DX(I) .EQ. 0.0) GO TO 2050
	ANNN = ANNN + 1.0
	T = FLOAT(I)
	WT = 1.0/(DX(I)*DX(I))
	SUMW = SUMW + WT
	SUMT = SUMT + T*WT
	SUMTT = SUMTT + T*T*WT
	SUMX = SUMX + X(I)*WT
	SUMTX = SUMTX + T*X(I)*WT
 2050	CONTINUE
C
	IF (ANNN .EQ. 0.0) RETURN
C
	SUMW = SUMW/ANNN
	SUMT = SUMT/ANNN
	SUMX = SUMX/ANNN
	SUMTT = SUMTT/ANNN
	SUMTX = SUMTX/ANNN
C
	DENOM = SUMT*SUMT - SUMW*SUMTT
	X0 = (SUMT*SUMTX - SUMX*SUMTT)/DENOM
	SLOPE = (SUMT*SUMX - SUMW*SUMTX)/DENOM
	DX0 = 1.0/SQRT(2.0*ANNN*SUMW)
	DSLOPE = 1.0/SQRT(2.0*ANNN*SUMTT)
C
C...NOW FIND CHISQUARED AND CHECK FOR STDDEV TROUBLE...
	STDDEV = 0.
	DO 200 I=1,N
	IF (DX(I) .EQ. 0.) GO TO 200
	STDDEV = STDDEV + 
     >		(X0+SLOPE*FLOAT(I) - X(I))**2/(DX(I)*DX(I))
  200	CONTINUE
	STDDEV = SQRT(STDDEV)
	IF (NN .LT. 2) RETURN
	STDDEV = STDDEV/SQRT(FLOAT(NN-1))
C
CCCCCCC	IF (STDDEV .GT. 1.) DX0 = DX0*STDDEV
C
	RETURN
	END
