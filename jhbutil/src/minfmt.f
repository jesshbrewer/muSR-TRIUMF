C...MIN_FMT...  	 
C 
C...General-purpose MINUIT FORMAT Input Line writer, using ENCODX 
C   to get around the problem of 10-character G-format ==> 3-digit	
C   accuracy.  (ENCODX is available in [JESS.UTIL]USERS.OLB)
C 
	SUBROUTINE MIN_FMT (LUN,IX,R8NAM,X,DX,XMIN,XMAX,REMARK)
C
	REAL*8 R8NAM, REMARK(8)
	LOGICAL*1 LINE(40), PART(10,4)
	EQUIVALENCE (LINE(1), PART(1,1))
	character*64 crmk
	logical*1 rmk(64)
	equivalence (rmk,crmk)
	logical*1 LZ
	byte BZ
	equivalence (LZ,BZ)
	logical*1 blnk
	data blnk /1H /, BZ /0/
C======================================================================
	DO 100 I=1,4 
 100	PART(1,I) = blnk
C 
	NSIG = 7 	
	CALL ENCDX (X, PART(2,1), 6, 9, 2, NSIG) 
	NSIG = 7 	
	CALL ENCDX (DX, PART(2,2), 6, 9, 2, NSIG) 
	NSIG = 7 	
	CALL ENCDX (XMIN, PART(2,3), 6, 9, 2, NSIG) 
	NSIG = 7 	
	CALL ENCDX (XMAX, PART(2,4), 6, 9, 2, NSIG) 
C
	NCH = 64
	CALL TRIM_ZERO (NCH, REMARK)
	CALL TRIM_BLNK (NCH, REMARK)
	if (NCH .eq. 0) then	! Don't print blank REMARK.
	   crmk = ' '
	else
	   call MOVEC (NCH, REMARK, rmk)
	   if (NCH .lt. 64) rmk(NCH+1) = LZ
	   if (NCH .lt. 63) rmk(NCH+2) = LZ
	end if
C
	WRITE (LUN,1111) IX, R8NAM, LINE, crmk
 1111	FORMAT (I10,2X,A8,40A1,2X,A)	
C
	RETURN 	 
	END 
