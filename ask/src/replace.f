	subroutine REPLACE (MESG, XOLD, XNEW, LTYPE)

C...Deliver MESG and the value of XOLD and the question
C   "New value? " and get back from user (on same line, via ASK)
C   the value of XNEW.  
C			-- Jess Brewer, Nov 1983

	character MESG*(*)
	character*1 LTYPE, STRING(12)
	real*4 XOLD1
	integer*4 IOLD1
	equivalence (XOLD1, IOLD1)
C=================================================
	write (*,1110) MESG
 1110	format (1X,A,$)

	if (LTYPE .eq. 'I') then
		XOLD1 = XOLD	! I*4 Variable.  "-1234567890 " or smaller.
		write (*,1111) IOLD1
 1111		format (I12,$)
		go to 2000
	else if (LTYPE .eq. 'R') then 
		NSIG = 6	! Real*4 Variable. "-1.23456E-12 " or smaller.
		call ENCDX (XOLD, STRING, 1, 12, 0, NSIG)
		write (*,1112) STRING
 1112		format (12A1,$)
	else
		write (*,1119) LTYPE
 1119		format ('*** UNDEFINED TYPE "',A1,'" ***')
		XNEW = XOLD
		return
	end if
C
 2000	call ASK1 (' -- New value? ', LTYP, XNEW)
	return
	end
