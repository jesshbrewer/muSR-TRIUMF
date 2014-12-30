C Q: using value.ne.0 to convert to logical --- allow for +/- epsilon?
C-----------------------------------------------------------------------
C...String Parsing including Expression Evaluation via    LOC_EVAL
C
C   Call PARSE to set the input line
C
C   Call PARSE_ADD_DELIM, PARSE_DELETE_DELIM to add/delete one delimiter
C   Call PARSE_SET_DELIMS, PARSE_PRINT_DELIMS to set/display all delimiters
C   Call PARSE_PUSH_DELIMS, PARSE_POP_DELIMS for one level of save/restore
C
C   Call PARSEVALUE for each input variable, with a type specifier
C   giving the storage class and the number of elements.
C
C	'D' (Real*8)		'R' (Real*4)
C	'I' (Integer*4)		'M' (Integer*2)
C	'S' (character String)  'L' (Logical*4)
C       'C' (Complex*8)         'K' (Logical*2)
C
C   Thus (e.g.), '80S' calls for a string of 80 characters and 
C   '3R' calls for an array of 3 Real*4 variables, while 
C   'D' calls for a single Real*8 variable.  
C------------------------------------------------------------------------------
	SUBROUTINE PARSE(CLINE)
	IMPLICIT NONE
	! Possible parameters for all entrys
	CHARACTER CLINE*(*), CH*1
	CHARACTER*1 TYPESTR(*)
	REAL*4 VAR(*)
	LOGICAL ERR

	! External functions
	LOGICAL LOC_EVAL

	! Input line data
	CHARACTER LINE*512
	CHARACTER*1 SLINE(512)
	EQUIVALENCE (LINE,SLINE)
	INTEGER LOC, NCH
	SAVE LINE, LOC, NCH

	! Parsing variables
	REAL*8 ANSWER
	INTEGER TYPECODE, KELEM, NELEM, TOKBEG, TOKEND, I

	! Delimiter data
	INTEGER NDELIM, SAVE_NDELIM
	CHARACTER DELIM*10, SAVE_DELIM*10
        SAVE DELIM, NDELIM, SAVE_DELIM, SAVE_NDELIM
	DATA DELIM /','/, NDELIM/1/

C============================================================================
C	start of SUBROUTINE PARSE(CLINE)
C============================================================================
	LINE = CLINE
	NCH = LEN(LINE)
	LOC = 1
c	write(*,*) 'Delim=[', DELIM(1:NDELIM), ']'
c	write(*,*) 'Line=', LINE, '(', LOC, ',', NCH, ')'
        RETURN

C============================================================================
	ENTRY PARSE_ADD_DELIM(CH)
C============================================================================
        DO I=1,NDELIM		! find out if delim is aready in
	   IF (DELIM(I:I) .EQ. CH) RETURN
	END DO
	NDELIM = MIN(LEN(DELIM),NDELIM+1)
	DELIM(NDELIM:NDELIM) = CH
	RETURN

C============================================================================
	ENTRY PARSE_DELETE_DELIM(CH)
C============================================================================
	IF (NDELIM .EQ. 1) RETURN   ! Jess says we need at least one
        DO I=1,NDELIM		! find delim
	   IF (DELIM(I:I) .EQ. CH) GOTO 100
	END DO
	RETURN			! delim not found
 100	DELIM(I:) = DELIM(I+1:) ! remove delim
	NDELIM = NDELIM - 1
	RETURN

C============================================================================
	ENTRY PARSE_SET_DELIMS(CLINE)
C============================================================================
c	IF (LEN(CLINE) .EQ. 0) RETURN ! Need at least one delim
	DELIM = CLINE
	NDELIM = MIN(LEN(DELIM),LEN(CLINE))
	RETURN

C============================================================================
	ENTRY PARSE_PUSH_DELIMS(CLINE)
C============================================================================
c	IF (LEN(CLINE) .EQ. 0) RETURN ! Need at least one delim
	SAVE_DELIM = DELIM
	SAVE_NDELIM = NDELIM
	DELIM = CLINE
	NDELIM = MIN(LEN(DELIM),LEN(CLINE))
	RETURN

C============================================================================
	ENTRY PARSE_POP_DELIMS
C============================================================================
c	IF (LEN(CLINE) .EQ. 0) RETURN ! Need at least one delim
	DELIM = SAVE_DELIM
	NDELIM = SAVE_NDELIM
	RETURN

C============================================================================
	ENTRY PARSE_WRITE_DELIMS
C============================================================================
	write(*,*) 'ASK delimiters are now [', DELIM(1:NDELIM), ']'
	RETURN

C============================================================================
	ENTRY PARSEVALUE(TYPESTR,VAR,ERR)
C============================================================================
c	write(*,*) 'Parse value'
	ERR = .FALSE.
        CALL GETTYPE (TYPESTR, TYPECODE, NELEM)

	IF (TYPECODE .EQ. 1) THEN	! String
c	   write(*,*) 'Grabbing token'
	   CALL GETTOKEN(SLINE,NCH,DELIM(1:NDELIM),LOC,TOKBEG,TOKEND)
c	   write(*,*) 'Grabbed ', LINE(tokbeg:tokend),
c     >          ' at (', tokbeg, ',', tokend, ')'
	   KELEM=1
	   DO WHILE (TOKBEG .LE. TOKEND) ! Copy token, with '' -> '
	      IF (SLINE(TOKBEG).EQ.'''' .AND.
     >            SLINE(TOKBEG+1).EQ.'''') TOKBEG = TOKBEG+1
	      CALL SETVALS(SLINE(TOKBEG), VAR, KELEM)
	      KELEM = KELEM+1
	      TOKBEG = TOKBEG+1
	   END DO
	   DO KELEM=KELEM,NELEM	! extend with blanks
	      CALL SETVALS(' ', VAR, KELEM)
	   END DO
	ELSE			! Expression list
	   DO KELEM=1,NELEM
	      CALL GETTOKEN(SLINE,NCH,DELIM(1:NDELIM),LOC,TOKBEG,TOKEND)
c	      write (*,*) 'Token=', LINE(TOKBEG:TOKEND)
	      IF (TOKEND-TOKBEG .LT. 0) THEN
		 CALL SETVALUE(0.0D0, VAR, TYPECODE, KELEM)
	      ELSE IF (LOC_EVAL(LINE(TOKBEG:TOKEND), ANSWER)) THEN
c		 write (*,*) 'Answer=', ANSWER
		 CALL SETVALUE(ANSWER, VAR, TYPECODE, KELEM)
	      ELSE
		 ERR = .TRUE.
	      ENDIF
	   END DO
	ENDIF
	RETURN
	END


C============================================================================
	SUBROUTINE GETTOKEN(LINE,NCH,DELIM,POS,TOKBEG,TOKEND)
C============================================================================
c       Given line and delim, shift POS to the start of the
c	next token and set (START:STOP) to be the token range
	IMPLICIT NONE
	CHARACTER*1 LINE(*)
	CHARACTER DELIM*(*)
	INTEGER NCH, POS, TOKBEG, TOKEND

c	INTEGER I
c	write (*,*) 'Parse start ', POS, ' :', (LINE(I), I=POS,NCH)

c       Skip initial blanks on line
 	DO WHILE (POS.LE.NCH .AND. LINE(POS).EQ.' ')
	   POS = POS+1
	END DO
c	write (*,*) 'Skipped blanks to ', POS

c       Skip to the end of line or next delimiter, remembering
c       the location of the last non-blank character. If token
c       starts with ' match the closing ' but ignore '' inside.
        IF (POS.LE.NCH .AND. LINE(POS).EQ.'''') THEN
	   POS = POS+1
	   TOKBEG=POS		! skip opening character
	   DO WHILE (POS.LE.NCH .AND. LINE(POS).NE.'\0')
	      ! accept '' in the string, but somebody else must do '' -> '
	      IF (LINE(POS).EQ.'''') THEN
		 IF (POS.LT.NCH .AND. LINE(POS+1).NE.'''') GO TO 100
		 POS=POS+1
	      ENDIF
	      POS = POS+1
	   END DO
 100	   TOKEND = POS-1
	   IF (POS.LE.NCH .AND. LINE(POS).NE.'\0') POS = POS+1
	   DO WHILE (POS.LE.NCH .AND. LINE(POS).EQ.' ')
	      POS = POS + 1
	   END DO
	ELSE
	   TOKBEG=POS
	   TOKEND=-1
	   DO WHILE (POS.LE.NCH .AND. LINE(POS).NE.'\0' .AND.
     >		INDEX(DELIM, LINE(POS)).EQ.0)
	      IF (LINE(POS).NE.' ') TOKEND = POS
	      POS = POS+1
	   END DO
	   IF (TOKEND .LT. 0) TOKEND = POS - 1
	ENDIF

c	Skip the delimiter if we are sitting on one
 	IF (POS .LE. NCH .AND. INDEX(DELIM,LINE(POS)).NE.0) POS=POS+1

c	write (*,*) 'Parse to ', POS, ' with (',TOKBEG,',',TOKEND,')'
	
        RETURN
	END


C============================================================================
	SUBROUTINE GETTYPE (SPEC, TYPE, COUNT)
C============================================================================
c Convert 'nnnX' specification into a type and a count using
c the following type codes.
C	'S' 1 (character String)
C       'K' 2 (Logical*2)
C       'M' 3 (Integer*2)
C       'L' 4 (Logical*4)
C	'I' 5 (Integer*4)
C       'R' 6 (Real*4)
C	'D' 7 (Real*8)
C       'C' 8 (Complex)
	IMPLICIT NONE
	CHARACTER*1 SPEC(*)
	INTEGER TYPE, COUNT
	INTEGER POS, DIGIT

c 100	FORMAT (A, $)
c 101	FORMAT (A)

c	write(*,100) 'skip['
c	Skip leading blanks
	POS = 1
	DO WHILE (SPEC(POS) .EQ. ' ')
c	   write(*,100) spec(pos)
	   POS = POS + 1
	END DO

c       Determine vector size
c	write(*,100) ']count['
	COUNT = 0
	DIGIT = 0
	DO WHILE (DIGIT .GE. 0 .AND. DIGIT .LE. 9)
c	   write(*,100) spec(pos)
	   COUNT = COUNT*10+DIGIT
	   DIGIT = ICHAR(SPEC(POS)) - ICHAR('0')
	   POS = POS+1
	END DO
	POS = POS-1

c	Determine vector type
c	Write(*,100) ']type['
c	write(*,100) spec(pos)
c	write(*,101) ']'
	TYPE = INDEX('SKMLIRDC', SPEC(POS))
	IF (TYPE .EQ. 0) TYPE = INDEX('SKMLIRDC', SPEC(POS))
	IF (POS.EQ.1) COUNT = 1
c	write (*,*) ' count=', count, ' type=', SPEC(POS)
	RETURN
	END

	
C============================================================================
	SUBROUTINE SETVALUE (VALUE, VAR, TYPE, ELEMENT)
C============================================================================
C This is the naughty subroutine which finally converts from a real
C maps elemens of the
c return vector with the value of the appropriate type. It assumes 
c (since it can't do otherwise) that the user handed in a variable 
c with enough space to accept all the return values. 
c
c See the GETTYPE function for an explanation of the types.
c
	IMPLICIT NONE
	REAL*8 VALUE
	REAL*4 VAR(*)
	INTEGER TYPE, ELEMENT
	
	LOGICAL*2 K
	INTEGER*2 M
	LOGICAL*4 L
	INTEGER*4 I
	REAL*4 R
	REAL*8 D
	COMPLEX*8 C

	GO TO  (4010,4020,4030,4040,4050,4060,4070,4080), TYPE
 4010	RETURN ! Can't get here because never called with strings

 4020	K = VALUE.NE.0.0D0
	CALL SETVALK(K, VAR, ELEMENT)
	RETURN

 4030	M = NINT(VALUE)
	CALL SETVALM(M, VAR, ELEMENT)
	RETURN

 4040	L = VALUE.NE.0.0D0
	CALL SETVALL(L, VAR, ELEMENT)
	RETURN

 4050	I = NINT(VALUE)
	CALL SETVALI(I, VAR, ELEMENT)
	RETURN

 4060	R = VALUE
	CALL SETVALR(R, VAR, ELEMENT)
	RETURN

 4070	D = VALUE
	CALL SETVALD(D, VAR, ELEMENT)
	RETURN

 4080	C = 666.0
	CALL SETVALC(C, VAR, ELEMENT)
	RETURN
	END
