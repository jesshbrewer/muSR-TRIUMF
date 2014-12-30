	SUBROUTINE SPARSE (NCHL, LINE, NCHSTR, STR, NSTR, MAXSTR)
C==============================================================================
C...Parse NCHL-character LINE for up to MAXSTR NCHSTR-character strings STR 
C   (delimited by BLANKS); results are left-justified and blank-filled. 
C------------------------------------------------------------------------------
	CHARACTER*1 LINE(*), STR(*)
C==============================================================================
	NSTR = 0
	NCH = NCHL
	CALL TRIM_BLNK (NCHL,LINE)
	IF (NCH .EQ. 0) RETURN
C
	LOCL = 1
	LOCS = 1
C
 1234	IF (NSTR .GE. MAXSTR) RETURN
	NSTR = NSTR + 1
	CALL SETC (NCHSTR, STR(LOCS), ' ')
	NCH = NCHL - LOCL + 1			! (Remainder of LINE)
	CALL TRIM_BLNK (NCH, LINE(LOCL))	! Anything left in LINE?
	IF (NCH .EQ. 0) THEN
		NSTR = NSTR - 1			! Nope.
		RETURN
	END IF					! Yes: 
 1000	IF (LINE(LOCL) .EQ. ' ') THEN		! Skip over any leading blanks.
		LOCL = LOCL + 1
		NCH = NCH - 1
		GO TO 1000
	END IF
	CALL MOVEC (NCH, LINE(LOCL), STR(LOCS))
	LOCS = LOCS + NCHSTR
	LOCL = LOCL + NCH + 1
	IF (LOCL .LE. NCHL) GO TO 1234
C
	RETURN
	END
