C...INVAVG...
C
C...General-Purpose Weighted Average of INVERSES of values...
C
	DIMENSION X(20), DX(20)
C
	WRITE (*,1111)
 1111	FORMAT (//,' Reciprocals Weighted Average calculator:',//)
 1000	NPTS = 0
	SXW = 0.
	SW = 0.
  100	WRITE (*,111) NPTS
  111	FORMAT (1X,I3,' Points so far.',$)
	CALL ASK2 ('  Next (X,dX) [0 if done] > ', 
     .		'R',X(NPTS+1), 'R',DX(NPTS+1))
	IF (DX(NPTS+1) .EQ. 0.) GO TO 2000
	NPTS = NPTS + 1
	XINV = 1.0/X(NPTS)
	XMIN = X(NPTS) - DX(NPTS)
C...(TAKE THE WORST CASE ERROR)
	DX(NPTS) = XINV
	IF (XMIN .LE. 0.) GO TO 110
	DX(NPTS) = 1.0/XMIN - XINV
  110	X(NPTS) = XINV
	GO TO 100
 2000	CALL WTDAVG (NPTS, X, DX, XBAR, DXBAR, STDDEV)
	WRITE (*,777) NPTS, XBAR, DXBAR, STDDEV
  777	FORMAT (//,' Weighted Average of ',I3,'  points gives ',/,
     1   ' 1/X =',G16.7,',  d(1/X) =',G16.7,/,
     2   ' with Std Dev of ',F11.2,/)
	GO TO 1000
	END
