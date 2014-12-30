C...AVG...
C
C...General-purpose Weighted-Average Calculator:
C
	DIMENSION X(20), DX(20)
C
	WRITE (*,1111)
 1111	FORMAT (/,' Weighted Average calculator:',/)
 1000	NPTS = 0
	SXW = 0.
	SW = 0.
  100	WRITE (*,111) NPTS
  111	FORMAT (1X,I3,' pts so far.',$)
	CALL ASK2 ('  Next (X,dX) (0 if done) > ', 
     >		'R',X(NPTS+1), 'R',DX(NPTS+1))
	IF (X(NPTS+1) .EQ. 0.0  .AND.  
     >		DX(NPTS+1) .EQ. 0.0) GO TO 2000
	NPTS = NPTS + 1
	IF (DX(NPTS) .EQ. 0.0) DX(NPTS) = 1.0E-10
	GO TO 100
 2000	CALL WTDAVG (NPTS, X, DX, XBAR, DXBAR, STDDEV)
	WRITE (*,777) NPTS, XBAR, DXBAR, STDDEV
  777	FORMAT (/,' Weighted Average of',I3,' points gives',/,
     >   ' X =',G16.7,'  +/-  dX =',G16.7,/,
     >   ' with Std Dev of',G11.4,/)
	GO TO 1000
	END
