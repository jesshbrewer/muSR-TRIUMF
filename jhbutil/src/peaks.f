	SUBROUTINE PEAKS (NDAT, SPECT, NPK, LOCPK, PEAK, IHWFM)
C===============================================================================
C...Search the NDAT-long (input) spectrum SPECT(i) for the (output) NPK peaks 
C   PEAK(j) located at positions LOCPK(j) in the original spectrum.  Thus 
C   PEAK(j) = SPECT(LOCPK(j)).   A "peak" is anywhere the slope changes 
C   from positive to negative.  The resultant list of peaks is 
C   *NOT* SORTed into order of decreasing height!! 
C   IHWFM(j) is the estimated (leading edge) HWFM 
C   of the jth peak, in "# of bins" units.  
C   The arrays PEAK(j) and WIDTH(j) are only written 
C   when includes in the argument list.   
C								-- Jess Brewer
C		Latest revision:	Sept. 1986
C-------------------------------------------------------------------------------
	REAL*4 SPECT(1), PEAK(1)
	INTEGER*4 LOCPK(1), IHWFM(1)
	LOGICAL GOING_UP, WR_PEAK, WR_WIDTH
C===============================================================================
	NPK = 0
	IF (NDAT .LT. 3) RETURN
c	N_ARG = LOC_NARGS(DUM)
c	WR_PEAK = (N_ARG .GE. 5)
c	WR_WIDTH = (N_ARG .GE. 6)
	NDM1 = NDAT - 1
	IW = 1
	DO 100 I=2,NDM1
	XXX = SPECT(I)
	IF (SPECT(I-1) .LT. XXX) THEN
		GOING_UP = .TRUE.
		IF (SPECT(I+1) .LT. XXX) THEN		! (Down next bin)
			NPK = NPK + 1
			LOCPK(NPK) = I
c			IF (WR_PEAK) PEAK(NPK) = XXX
			PEAK(NPK) = XXX
		END IF
c		IF (WR_WIDTH) IW = IW + 1
		IW = IW + 1
	ELSE
c		IF (WR_WIDTH) THEN
			IF (GOING_UP .AND. NPK .GT. 0) 
     >				IHWFM(NPK) = IW
			IW = 0
c		END IF
		GOING_UP = .FALSE.
	END IF
  100	CONTINUE
	RETURN
	END
