	SUBROUTINE DASHLN (X0, Y0, DX, DY, DASH)
C
	BLANK = 0.5*DASH	
	RANGE = SQRT(DX*DX + DY*DY)
	NDASH = (RANGE-DASH)/(BLANK+DASH)
	IF (NDASH .LT. 2) RETURN
	DDX = DX/(3.0*FLOAT(NDASH) + 2.0)
	DDY = DY/(3.0*FLOAT(NDASH) + 2.0)
C
	XD = X0
	YD = Y0
	DO 100 I=1,NDASH	
	CALL PLOT_R (XD, YD, 3)
	XD = XD + 2.0*DDX
	YD = YD + 2.0*DDY
	CALL PLOT_R (XD, YD, 2)
	XD = XD + DDX
	YD = YD + DDY
  100	CONTINUE	
	CALL PLOT_R (XD, YD, 3)
	CALL PLOT_R (XD+2.0*DDX, YD+2.0*DDY, 2)
	CALL PLOT_R (XD+2.0*DDX, YD+2.0*DDY, 3)
C
	RETURN
	END
C============================================================================
	SUBROUTINE DRAW_LINE (X, Y, N, I)
C
C...Implementation of the old MTS "LINE" routine.  Connects a series of points.
C============================================================================
	REAL*4 X(N), Y(N)
	IF (N .LT. 1) RETURN
	CALL PLOT_R (X(1), Y(1), 3)
	IF (N .LT. 2) RETURN
	DO 10 J=2,N
   10	CALL PLOT_R (X(J), Y(J), 2)
	CALL PLOT_R (X(N), Y(N), 3)
	RETURN
	END
C============================================================================
	SUBROUTINE Jess_SYMBOL (X, Y, SIZE, JSYM, ANGLE, III)
C
C...An alternative to Kost's SYMBOL routine.  ANGLE and III are ignored.
C============================================================================
C
	DATA PI /3.14159/
cccc	DATA RADIAN /57.29578/
C
	HLF = 0.5*SIZE
	if (jsym .gt. 6) go to 6000
c
	JSYMP1 = JSYM + 1
C
	GO TO (1010,1000,2000,3000,4000,5000,6000), JSYMP1
C
C...Square point: type 0
C
 1010	HLF = 0.9*HLF
	CALL PLOT_R (X-HLF,Y-HLF,3)
	CALL PLOT_R (X-HLF,Y+HLF,2)
	CALL PLOT_R (X+HLF,Y+HLF,2)
	CALL PLOT_R (X+HLF,Y-HLF,2)
	CALL PLOT_R (X-HLF,Y-HLF,2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C...Octagonal point: type 1
C
 1000	OCT = 0.375*HLF
	CALL PLOT_R (X-HLF,Y-OCT,3)
	CALL PLOT_R (X-HLF,Y+OCT,2)
	CALL PLOT_R (X-OCT,Y+HLF,2)
	CALL PLOT_R (X+OCT,Y+HLF,2)
	CALL PLOT_R (X+HLF,Y+OCT,2)
	CALL PLOT_R (X+HLF,Y-OCT,2)
	CALL PLOT_R (X+OCT,Y-HLF,2)
	CALL PLOT_R (X-OCT,Y-HLF,2)
	CALL PLOT_R (X-HLF,Y-OCT,2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C...Triangular point: type 2
C
 2000	QTR = 0.5*HLF
	CALL PLOT_R (X-HLF, Y-HLF+0.5*QTR, 3)
	CALL PLOT_R (X, Y+HLF, 2)
	CALL PLOT_R (X+HLF, Y-HLF+0.5*QTR, 2)
	CALL PLOT_R (X-HLF, Y-HLF+0.5*QTR, 2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C...Triangular point: type 3
C
 3000	QTR = 0.5*HLF
	CALL PLOT_R (X-HLF, Y+HLF-0.5*QTR, 3)
	CALL PLOT_R (X, Y-HLF, 2)
	CALL PLOT_R (X+HLF, Y+HLF-0.5*QTR, 2)
	CALL PLOT_R (X-HLF, Y+HLF-0.5*QTR, 2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C...Diamond point: type 4
C
 4000	DIA = 1.2*HLF
	CALL PLOT_R (X, Y-DIA, 3)
	CALL PLOT_R (X-DIA, Y, 2)
	CALL PLOT_R (X, Y+DIA, 2)
	CALL PLOT_R (X+DIA, Y, 2)
	CALL PLOT_R (X, Y-DIA, 2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C...."Star" point: type 5
C
 5000	HLF = 1.25*HLF
	TH1 = PI/2.5
	TH2 = 2.0*TH1
	BX = HLF*SIN(TH2)
	BY = HLF*COS(TH2)
	CX = - HLF*SIN(TH1)
	CY = HLF*COS(TH1)
	CALL PLOT_R (X, Y+HLF, 3)
	CALL PLOT_R (X+BX, Y+BY, 2)
	CALL PLOT_R (X+CX, Y+CY, 2)
	CALL PLOT_R (X-CX, Y+CY, 2)
	CALL PLOT_R (X-BX, Y+BY, 2)
	CALL PLOT_R (X, Y+HLF, 2)
	CALL PLOT_R (X, Y, 3)
	RETURN
C
C..."Cross" point: type 6 (and 7-11 when combined with others)
C
 6000	CALL PLOT_R (X-HLF, Y-HLF, 3)
	CALL PLOT_R (X+HLF, Y+HLF, 2)
	CALL PLOT_R (X-HLF, Y+HLF, 3)
	CALL PLOT_R (X+HLF, Y-HLF, 2)
	CALL PLOT_R (X, Y, 3)
	IF (JSYM .EQ. 6) RETURN
C
C...Superimpose other point type for 7-11:
C
	JSYMM6 = JSYM - 6
	GO TO (1010,1000,2000,3000,4000,5000), JSYMM6
C
	END
