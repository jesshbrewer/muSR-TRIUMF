	SUBROUTINE GET_CONDS (NCHL, LINE, NCHC, COND, NCOND, MAXCOND)
C==============================================================================
C...Parse NCHL-character LINE for 
C   (a) "IF" indicating conditional expressions to follow; 
C   (b) up to MAXCOND NCHC-character (blank-filled) expressions COND 
C       -- delimited by blanks!
C------------------------------------------------------------------------------
	CHARACTER*1 LINE(*)
	CHARACTER*1 COND(*)
C==============================================================================
	NCOND = 0				! Reset # of conditions.
	CALL SETC (NCHC*MAXCOND, COND, ' ')	! Blank out expressions.
	IF (NCHL .LT. 4) RETURN			! LINE too short for anything?
C
C...Check for "IF ..."
C
	IF (LINE(1) .NE. 'I') RETURN
	IF (LINE(2) .NE. 'F') RETURN
	IF (LINE(3) .NE. ' ') RETURN
C
C...Command line includes Condition Expression(s): 
C
	CALL SPARSE (NCHL-3, LINE(4), NCHC, COND, NCOND, MAXCOND)
C
	RETURN
	END
