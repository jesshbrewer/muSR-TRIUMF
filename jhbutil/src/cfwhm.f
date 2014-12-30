	SUBROUTINE CFWHM (JPK, N, C, FWHM, HWM, HWP, *)
C
C...General-purpose routine to find the FWHM of the absolute
C   magnitude of a peak at bin JPK in the N-bin complex array
C   C.  FWHM is in units of "bins," whatever those are.
C   (Multiply by the bin size externally.)
C
	COMPLEX C(N)
C==============================================================================
	PEAK = CABS(C(JPK))
	HALF = 0.5*PEAK
	HWP = 0.0
	HWM = 0.0
	FWHM = 0.0
	IF (PEAK .EQ. 0.0) RETURN 1
	IF (N .LT. 3) RETURN 1
	IF (JPK .LT. 1  .OR.  JPK .GT. N) RETURN 1
C
C...First search forward:
C
	LOC = JPK - 1
 1000	LOC = LOC + 1
	IF (LOC .GT. N) GO TO 1900
	BIN = CABS(C(LOC))
	IF (BIN .LT. HALF) GO TO 1100
	HWP = HWP + 1.0
	GO TO 1000
 1100	IF (LOC .EQ. N) GO TO 1900
	BINNXT = CABS(C(LOC+1))
	HWP = HWP + (BIN - HALF)/(BIN - BINNXT)
C
C...Then search backward:
C
 1900	LOC = JPK + 1
 2000	LOC = LOC - 1
	IF (LOC .LT. 1) GO TO 9000
	BIN = CABS(C(LOC))
	IF (BIN .LT. HALF) GO TO 2200
	HWM = HWM + 1.0
	GO TO 2000
 2200	IF (LOC .EQ. 1) GO TO 9000
	BINNXT = CABS(C(LOC-1))
	HWM = HWM + (BIN - HALF)/(BIN - BINNXT)
C
C...Now add halfwidths together:
C
 9000	FWHM = HWM + HWP
C
	RETURN
	END
