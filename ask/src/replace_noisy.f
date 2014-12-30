C====================================================

	subroutine REPLACE (MESG, IOLD, INEW, LTYPE)

C====================================================
C...Deliver MESG and the value of IOLD and the question
C   "New value? " and get back from user (on same line, via ASK)
C   the value of INEW.  IOLD/INEW can be integers (*4) or reals (*4)
C   as indicated by the LTYPE 'I' or 'R'.
C			-- Jess Brewer, Nov 1983

	implicit none
c...	passed variables
	character MESG*(*)
	integer*4 IOLD, INEW
	character*1 LTYPE

c...    function variable (minimum) width string for real
	character*32 realtoch_v
	external     realtoch_v
	integer      nospaces
	external     nospaces

c...	local variables
	character*32 STRING
	real*4 XOLD1
	integer*4 IOLD1
	integer iost, ls
	equivalence (XOLD1, IOLD1)
C=================================================
	write (*,1110) MESG
 1110	format (1X,A,$)

	IOLD1 = IOLD  !  Assigns XOLD1 too

	if (LTYPE .eq. 'I') then
	   write (STRING,1111,iostat=iost) IOLD1
 1111	   format (I14)
	else if (LTYPE .eq. 'R') then 
	   STRING = realtoch_v( XOLD1, 11, 7 )
	else
	   write (*,1119) LTYPE
 1119	   format ('*** UNDEFINED TYPE "',A1,'" ***')
	   INEW = IOLD
	   return
	end if
	ls = nospaces(STRING)
	write (*,1200,iostat=iost) STRING(1:ls)
 1200	format (1X,'[',A,']',$)
C
 2000	call ASK1 (': ', LTYPE, INEW)
	return
	end


C=================================================

	subroutine CREPLACE (MESG, COLD, CNEW)

C=================================================

C...Deliver MESG and the value of character string COLD to prompt
C   for a new value.  Get back from user (on same line, via ASK)
C   the string CNEW.  
C			-- Donald Arseneau  01-Mar-2001

	implicit none
c...	passed variables
	character*(*) MESG, COLD, CNEW

c...	functions
        integer chtrim
        external chtrim

c...	local
	character*80  prompt
	character*4   ltype
        character*128 replacement  !  Allow for COLD and CNEW same
	integer iost,ls
C=================================================

	write (prompt,1111,iostat=iost) 
     >        MESG(1:chtrim(MESG)),   COLD(1:chtrim(COLD))
 1111	format (A,' [',A,'] >')

	write (ltype,1114,iostat=iost) len(CNEW)
 1114	format( I3.3,'A' )

        ls = min( len(cold), len(replacement) )
 	call ASK1S (prompt(1:chtrim(prompt)), ltype, replacement, ls)
	if (ls.gt.0) then
           CNEW = replacement
        else
           CNEW = COLD
        endif

	return
	end

