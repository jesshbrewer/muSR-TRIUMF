	SUBROUTINE CPEAKS (NDAT, SPECT, MXPK, CRIT, NPK, LOCPK)
C===============================================================================
C...Search the NDAT-long (input) spectrum SPECT(i) for the (output) NPK peaks 
C   PEAK(j) located at positions LOCPK(j) in the original spectrum.  Thus 
C   PEAK(j) = SPECT(LOCPK(j)).   A "peak" is anywhere the slope changes 
C   from positive to negative.  The resultant list of peaks is 
C   *NOT* SORTed into order of decreasing height!! 
C								-- Jess Brewer
C		Latest revision:	May 1998
C-------------------------------------------------------------------------------
	implicit none

	integer*4 NDAT, MXPK, NPK, LOCPK(*)
	real*4 CRIT
	complex SPECT(*)

	integer*4 i, nDm1
	real*4 xnow, xlst, xnxt
C===============================================================================
	NPK = 0
	if (NDAT .lt. 3) return
	nDm1 = NDAT - 1
	do i=2,nDm1
	   xlst = cabs(SPECT(i-1))
	   xnow = cabs(SPECT(i))
	   xnxt = cabs(SPECT(i+1))
	   if (xlst .lt. xnow) then            ! Going up.  Is next one down?
	      if (xnxt .lt. xnow) then         ! Yes.  This is a peak!
		 NPK = NPK + 1
		 LOCPK(NPK) = i
		 if (NPK .ge. MXPK) return     ! No more than MXPK allowed.
	      end if
	   end if
	end do
	return
	end
