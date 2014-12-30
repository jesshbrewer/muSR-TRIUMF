C...INTEGRATE...
C===============================================================================
C...Integrate a function from an expression entered on-line, using 
C   LOC_EVAL to evaluate the expression and (IMSL) DCADRE to integrate it.
C		-- Jess Brewer, TRIUMF/UBC 
C		Latest revision	November 1986.
C===============================================================================
	COMMON /EXPRESSION/ EQN
	CHARACTER LINE*132, DUM*80, EQN*80
	LOGICAL*1 SLINE(132), SEQN(80)
	EQUIVALENCE (LINE,SLINE), (EQN,SEQN)
	LOGICAL ASK_IF
	EXTERNAL FCN_EVAL
C===============================================================================
 1234	CALL ASK ('Enter function of X to be integrated:  ', 
     >			'80S',SEQN)
	CALL ASK ('Range of X over which to integrate:  ', 
     >			'R',X1, 'R',X2)
	XRANGE = X2 - X1
	CALL ASK ('Convergence criterion:  ', 'R',CRIT)
C-------------------------------------------------------------------------------
	Y = DCADRE (FCN_EVAL, X1,X2, 0.0, CRIT, ERR, IER) 
C-------------------------------------------------------------------------------
	CALL TELL ('Integral, Error(est.) = ', 'R',Y, 'R',ERR) 
	IF (IER .GT. 66) CALL TELL ('DCADRE Error Code ', 'I',IER) 
	IF (ASK_IF ('Want to try another? ')) GO TO 1234
	CALL EXIT
C
	END
C==============================================================================
	FUNCTION FCN_EVAL (X)
C-------------------------------------------------------------------------------
	COMMON /EXPRESSION/ EQN
	CHARACTER EQN*80
	REAL*8 VALUE
	LOGICAL LOC_EVAL
C-------------------------------------------------------------------------------
	IF (.NOT. LOAD_SYM ('X       ', X)) GO TO 9876
	IF (.NOT. LOC_EVAL (EQN, VALUE)) GO TO 9876
	FCN_EVAL = VALUE
	RETURN
C-------------------------------------------------------------------------------
 9876	CALL ASK ('****  LOC_EVAL  Error. ****') 
	RETURN
C
	END
C==============================================================================
	LOGICAL FUNCTION LOAD_SYM (SNAME, SVALUE)
C------------------------------------------------------------------------------
C...Load the value SVALUE into the symbol name SNAME (8 characters) 
C   via LOC_EVAL.
C
	REAL*8 SNAME, VALUE
	REAL*4 SVALUE
	CHARACTER CHSTR*24
	LOGICAL*1 STRING(24)
	EQUIVALENCE (CHSTR,STRING(1))
	LOGICAL LOC_EVAL
C------------------------------------------------------------------------------
	CALL SETC (24,STRING,' ')
	ENCODE (23,1111,STRING) SNAME, SVALUE
 1111	FORMAT (A8,'=',G14.7)
	LOAD_SYM = LOC_EVAL (CHSTR, VALUE)		! Load value
	RETURN
	END
