	LOGICAL FUNCTION IF_NUMERIC (NCH, STRING, VALUE)
C------------------------------------------------------------
	INTEGER NCH
	CHARACTER*1 STRING(NCH)
	REAL*8 VALUE
C=======================================================================
	NNCH = NCH
	CALL TRIM_BLNK (NNCH, STRING)
	READ (STRING,1111,ERR=2000) VALUE
 1111	FORMAT (F10.0)
	IF_NUMERIC = .TRUE.
	RETURN
 2000	VALUE = 0.0
	IF_NUMERIC = .FALSE.
	RETURN
	END
