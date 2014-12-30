	LOGICAL FUNCTION IF_TRUE (EXPR)
C------------------------------------------------------------
	CHARACTER EXPR*(*)
	REAL*8 VALUE, VV
	LOGICAL LOC_EVAL 
C=======================================================================
	IF_TRUE = .FALSE.
	LEN = INDEX(EXPR, '=')		! How far to "="? 
	IF (LEN .EQ. 1) RETURN		! "=" 1st char?  Bad syntax.  
	IF (LEN .EQ. 0) THEN		! No equality check.  Positive value?
		IF (.NOT. LOC_EVAL (EXPR, VALUE)) RETURN
		IF (VALUE .GT. 0.0) IF_TRUE = .TRUE.
		RETURN
	ELSE				! Compare both sides for equality:
		LAST = LEN - 1
		NEXT = LEN + 1
		IF (.NOT. LOC_EVAL (EXPR(1:LAST), VALUE)) RETURN
		IF (.NOT. LOC_EVAL (EXPR(NEXT:), VV)) RETURN
		IF (VV .EQ. VALUE) IF_TRUE = .TRUE.
		RETURN
	END IF
	END
