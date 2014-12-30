C...ENCDX package -- Intelligent and Aesthetic Formatter.
C 
C 
      SUBROUTINE ENCDX (X, LINE, MIN, MAX, LR, NSIG)
C 
C...Pack a pleasing code for X into LINE, 
C   using NSIG (< MAX) significant digits.
C   Note that NSIG is both input and output parameter --
C   on entry, |NSIG| is Roundoff criterion (for NSIG=3 set 1.999=2.0);
C   on exit, NSIG = # signif. dig. in the result.
C   For NSIG < 0, treat zeroes as signif. digits.  
C   E format for NLOG > MIN, where NLOG = ALOG10(ABS(X)).
C   Left-justified for LR < 0, Right-justified for LR > 0,
C            Centered for LR = 0.
C   For LR > 1, do NOT shave off "dangling decimal point."
C 
      INTEGER*4 NSIG, LR, MIN, MAX
      REAL*4 X
      CHARACTER*1 LINE(*)

      CHARACTER*1 DIG(10), DOT, DASH, ECH, BLNK
C...Numbers rounded off to 5 places, so don't use big values of MAX.
      LOGICAL SIGZER
      INTEGER NSIG0, J, N, NLOG, NPWR, IX, NSH, ILOG, NSG, I
      INTEGER NXTR, IT
      REAL*8 AX, XX, XXX, TEN, SMALL, HALF
      DATA TEN /10.0D0/, HALF /0.5D0/
      DATA DIG/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'/ 
      DATA DOT/'.'/, DASH/'-'/, ECH/'E'/, BLNK/' '/
      DATA NPWR /6/
C===============================================================================
	SIGZER = (NSIG .LT. 0)
	NSIG = IABS(NSIG)
	NSIG0 = NSIG
      CALL SETC (MAX, LINE, BLNK)
      IF (X .NE. 0.0) GO TO 10
      NSIG = 1 
      IF (LR) 7, 8, 9
   7  LINE(1) = DIG(1)
      RETURN 
   8  LINE(1+MAX/2) = DIG(1)
      RETURN 
   9  LINE(MAX) = DIG(1)
      RETURN 
C 
  10  AX = ABS(X) 
      NLOG = DLOG10(AX) + HALF
      NSIG = MIN0(MAX-2,MAX0(MIN,NSIG))
      XX = AX*TEN**(NSIG - NLOG)
      N = XX + HALF
      SMALL = TEN**(-MIN0(6,NSIG))
      IF (DABS(XX-N)/XX .LT. SMALL) XX = N 
      XXX = XX*TEN**(NPWR - NSIG)
      IX = XXX + HALF
C...Note: XX .LE. 10.0, XXX .LE. 10**6, 
C   so IX .LT. 10**7 (no overflow).
C 
      CALL BTD (IX, LINE, MAX, NSIG, BLNK)
C 
C..Now got Mantissa to 6 places.  Shift to Left-justified to fiddle.
      NLOG = NLOG + NSIG - (NPWR + 1)
      IF (NSIG .GE. MAX) GO TO 900
      NSH = MAX - NSIG
      CALL MOVEC (NSIG, LINE(NSH+1), LINE(1))
      CALL SETC (NSH, LINE(NSIG+1), BLNK)
C 
C...Insert Decimal Point:
  900 IF (NSIG .EQ. MAX) NSIG = NSIG - 1
      CALL RSHC (1, NSIG, LINE)
      LINE(2) = DOT
CCCCC	call tell(' LINE=', '10S', LINE)
      NSIG = NSIG + 1
C...(Treat Decimal Point as a Significant Digit.)
      IF (NLOG .LT. 2-MAX) GO TO 2020
      IF (NLOG .LT. -3) GO TO 2020
      IF (NLOG .GT. MIN) GO TO 2020
C...F format:
      IF (NLOG .EQ. 0) GO TO 220
      IF (NLOG .LT. 0) GO TO 210
C...Shift Decimal Point Right: 
      LINE(2) = LINE(1)
      NLOG = NLOG + 1
      CALL MOVEC (NLOG, LINE(2), LINE(1))
      LINE(NLOG+1) = DOT
      GO TO 220 
C...Shift Decimal Point Left and add Zeros:
  210 IF (NLOG .EQ. -1) GO TO 230
      ILOG = IABS(NLOG)
      NSH = ILOG - 1
      NSIG = MIN0(NSIG+NSH, MAX)
      CALL RSHC (NSH, NSIG-NSH-2, LINE(3))
      LINE(NSH+2) = LINE(1)
      LINE(1) = DOT
      CALL SETC (NSH, LINE(2), DIG(1))
      GO TO 220 
  230 LINE(2) = LINE(1)
      LINE(1) = DOT
C...Remove Trailing Zeros, if any:
  220	NSG = NSIG 
      DO 100 I=1,NSG
      J = NSG + 1 - I
      IF (LINE(J).NE.DIG(1)) GO TO 221
	IF (SIGZER  .AND.  NSIG .LE. NSIG0) GO TO 221
      LINE(J) = BLNK
  100 NSIG = NSIG - 1
C 
  221	IF (LINE(1).NE.DOT) GO TO 222
	CALL RSHC (1, NSIG, LINE)
	LINE(1) = DIG(1)
	NSIG = NSIG + 1
  222	IF (X .GE. 0) GO TO 240
C...Insert Minus Sign: 
      IF (NSIG .EQ. MAX) NSIG = NSIG - 1
      CALL RSHC (1, NSIG, LINE)
      NSIG = NSIG + 1
      LINE(1) = DASH
  240 IF (LINE(NSIG).NE.DOT) GO TO 4000
      IF (LR .GT. 1) GO TO 4000
C...Cleave off the final Decimal Point: 
      LINE(NSIG) = BLNK
      NSIG = NSIG - 1
      GO TO 4000 
C 
C...E format -- 
C   Remove Trailing Zeroes, if any:
 2020 NSG = NSIG 
      DO 200 I=1,NSG
      J = NSG + 1 - I
      IF (LINE(J).NE.DIG(1)) GO TO 2030
      LINE(J) = BLNK
  200 NSIG = NSIG - 1
 2030 IF (X .GE. 0) GO TO 2040
C...Insert Minus Sign: 
      IF (NSIG .EQ. MAX) NSIG = NSIG - 1
      CALL RSHC (1, NSIG, LINE)
      NSIG = NSIG + 1
      LINE(1) = DASH
C 
 2040 IF (NLOG .EQ. 0)GO TO 4000
      NXTR = 2 
      IF (NLOG .LT. 0) NXTR = 3
      ILOG = IABS(NLOG)
      IF (ILOG .GT. 9) NXTR = NXTR+1
      NSIG = MIN0(NSIG+NXTR, MAX)
      IF (ILOG .GT. 9) GO TO 3000
      LINE(NSIG) = DIG(ILOG+1)
      LINE(NSIG-1) = ECH
      IF (NLOG .GE. 0) GO TO 4000
      LINE(NSIG-1) = DASH
      LINE(NSIG-2) = ECH
      GO TO 4000 
 3000 LINE(NSIG) = DIG(1+MOD(ILOG,10))
      IT = ILOG/10 
      LINE(NSIG-1) = DIG(1+IT)
      LINE(NSIG-2) = ECH
      IF (NLOG .GE. 0) GO TO 4000
      LINE(NSIG-2) = DASH
      LINE(NSIG-3) = ECH
C 
 4000 IF (NSIG .GE. MAX) RETURN
C 
C...Leave Left-Justified if LR < 0:
      NSH = MAX - NSIG
      IF (LR) 7000, 5000, 6000
C...Center up on MAX digits for LR = 0:
 5000 NSH = NSH/2 
      IF (NSH .LT. 1) RETURN
C...Shift to Right-justified if LR > 0:
 6000 CALL RSHC (NSH, NSIG, LINE)
      CALL SETC (NSH, LINE, BLNK)
 7000 RETURN 
      END 
      SUBROUTINE RSHC (N, NCH, LINE)
C  RIGHT-SHIFT NCH CHARACTERS N TIMES IN THE ARRAY LINE.
C  (DO NOT USE MOVEC, FOR OBVIOUS REASONS.)
C  BEWARE OF EXCEEDING THE BOUNDARIES OF LINE.  DO YOUR
C  BOOKKEEPING EXTERNALLY.
      CHARACTER*1 LINE(1)
      INTEGER N, NCH

      INTEGER I,J
C 
      DO 100 I=1,NCH
      J = NCH + 1 - I
  100 LINE(J+N) = LINE(J)
      RETURN 
      END 
