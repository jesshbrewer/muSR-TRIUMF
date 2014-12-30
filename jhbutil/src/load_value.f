	LOGICAL FUNCTION LOAD_VALUE (VNAME, VALUE)
c-----------------------------------------------------------------
	real*8 VALUE
	character VNAME*(*), DUM*20, EXPR*100
	logical LOC_EVAL
c-----------------------------------------------------------------
	call SETC (20,DUM,' ')
	write (DUM,1111) VALUE
 1111	format ('==',G16.9)
	EXPR = VNAME//DUM
	LOAD_VALUE = LOC_EVAL (EXPR, VALUE)
	RETURN
	END
