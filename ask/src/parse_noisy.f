C Q: using value.ne.0 to convert to logical --- allow for +/- epsilon?
c	Test for lowercase type has been corrected. TR July 5, 2000
C-----------------------------------------------------------------------
C...String Parsing including Expression Evaluation via    LOC_EVAL
C
C   Call PARSE to set the input line
C
C   Call PARSE_ADD_DELIM, PARSE_DELETE_DELIM to add/delete one delimiter
C   Call PARSE_SET_DELIMS, PARSE_PRINT_DELIMS to set/display all delimiters
C   Call PARSE_PUSH_DELIMS, PARSE_POP_DELIMS for one level of save/restore
C
C	Ignored characters [,] within numerical fields, but not strings.
C   Call PARSE_ADD_IGNORE, PARSE_DELETE_IGNORE to add/delete one ignored char
C   Call PARSE_SET_IGNORE, PARSE_PRINT_IGNORE to set/display all ignored chars.
C   Call PARSE_PUSH_IGNORE, PARSE_POP_IGNORE for one level of save/restore
C
C   Call PARSEVALUE for each input variable, with a type specifier
C   giving the storage class and the number of elements.
C
C	'D' (Real*8)		'R' (Real*4)
C	'I' (Integer*4)		'M' (Integer*2)
C	'S' (character Array)   'L' (Logical*4)
C       'C' (Complex*8)         'K' (Logical*2)
C       'A' (character String)  'W' ("Word" space-delimited String)
C
C   Thus (e.g.)
C       '3R' calls for an array of 3 Real*4 variables
C       'D' calls for a single Real*8 variable.
C       '80S' calls for a character array of 80 characters [character*1 str(80)]
C       '80A' calls for a character string of 80 characters [character str*(80)]
C------------------------------------------------------------------------------
	SUBROUTINE PARSE(CLINE)
	IMPLICIT NONE
	! Possible parameters for all entrys
	CHARACTER CLINE*(*), CH*1
	CHARACTER*1 TYPESTR(*)
	REAL*4 VAR(*)
	LOGICAL ERR, USE_DEF

	! External functions
	LOGICAL LOC_EVAL

	! Input line data
cTR	CHARACTER ALINE*8192
	CHARACTER LINE*8192
	CHARACTER*1 SLINE(8192)
	EQUIVALENCE (LINE,SLINE)
	INTEGER LOC, NCH
	SAVE LINE, LOC, NCH

	! Parsing variables
	REAL*8 ANSWER, DEFAULT_VAL, DV, RV
	INTEGER TYPECODE, KELEM, NELEM, TOKBEG, TOKEND, I

	! Delimiter data
	INTEGER NDELIM, SAVE_NDELIM, TEMP_NDELIM
	CHARACTER DELIM*10, SAVE_DELIM*10, TEMP_DELIM*10

	! Ignored characters for numerical fields, but not character fields.
	INTEGER NIGNORE, SAVE_NIGNORE
	CHARACTER IGNORE*10, SAVE_IGNORE*10

	CHARACTER IGNORENULL*10
	INTEGER NIGNORENULL

        SAVE DELIM, NDELIM, SAVE_DELIM, SAVE_NDELIM
        SAVE IGNORE, NIGNORE, SAVE_IGNORE, SAVE_NIGNORE
	SAVE IGNORENULL, NIGNORENULL
	SAVE DEFAULT_VAL

	DATA DELIM /','/, NDELIM/1/
	DATA IGNORE /'[]'/, NIGNORE/2/
	DATA NIGNORENULL /0/
	DATA DEFAULT_VAL /0.0d0/

	character*32 dtypestr ! debug
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
	ENTRY PARSE_SET_DEFAULT(RV)
	DEFAULT_VAL = DBLE(RV)
	RETURN

C============================================================================
	ENTRY PARSE_SET_DEFAULTD(DV)
	DEFAULT_VAL = DV
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
c	write(*,*) 'ASK delimiters are now [', DELIM(1:NDELIM), ']'
	RETURN

C============================================================================
	ENTRY PARSE_ADD_IGNORE(CH)
C============================================================================
        DO I=1,NIGNORE		! find out if IGNORE is aready in
	   IF (IGNORE(I:I) .EQ. CH) RETURN
	END DO
	NIGNORE = MIN(LEN(IGNORE),NIGNORE+1)
	IGNORE(NIGNORE:NIGNORE) = CH
	RETURN

C============================================================================
	ENTRY PARSE_DELETE_IGNORE(CH)
C============================================================================
	IF (NIGNORE .EQ. 0) RETURN   
        DO I=1,NIGNORE		! find IGNORE
	   IF (IGNORE(I:I) .EQ. CH) GOTO 113
	END DO
	RETURN			! IGNORE not found
 113	IGNORE(I:) = IGNORE(I+1:) ! remove IGNORED character
	NIGNORE = NIGNORE - 1
	RETURN

C============================================================================
	ENTRY PARSE_SET_IGNORE(CLINE)
C============================================================================
	IGNORE = CLINE
	NIGNORE = MIN(LEN(IGNORE),LEN(CLINE))
	RETURN

C============================================================================
	ENTRY PARSE_PUSH_IGNORE(CLINE)
C============================================================================
	SAVE_IGNORE = IGNORE
	SAVE_NIGNORE = NIGNORE
	IGNORE = CLINE
	NIGNORE = MIN(LEN(IGNORE),LEN(CLINE))
	RETURN

C============================================================================
	ENTRY PARSE_POP_IGNORE
C============================================================================
	IGNORE = SAVE_IGNORE
	NIGNORE = SAVE_NIGNORE
	RETURN

C============================================================================
	ENTRY PARSE_WRITE_IGNORE
C============================================================================
	write(*,*) 'ASK ignored characters are now [', 
     >             IGNORE(1:NIGNORE), ']'
	RETURN


C============================================================================
	ENTRY PARSEVALUE(TYPESTR,VAR,USE_DEF,ERR)
C============================================================================
	ERR = .FALSE.
        CALL GETTYPE (TYPESTR, TYPECODE, NELEM)
	IF (TYPECODE .EQ. 10) THEN	! character "Word"
	   TEMP_NDELIM = NDELIM
	   TEMP_DELIM = DELIM
	   DELIM = ' '
	   NDELIM = 1
	   TYPECODE = 2
	ELSE
	   TEMP_NDELIM = 0
	ENDIF

	IF (TYPECODE .EQ. 1) THEN	! character array
	   CALL GETTOKEN(SLINE,NCH,DELIM(1:NDELIM),
     >          IGNORENULL(1:NIGNORENULL),
     >		LOC,TOKBEG,TOKEND)
c	     write(*,*) 'PARSEVALUE: Grabbed...', LINE(tokbeg:tokend), !debug
c     >          '... at (', tokbeg, ',', tokend, ')' !debug
	   IF ((TOKBEG .GT. TOKEND) .AND. USE_DEF) then
C		! Use default string, since string in LINE is empty.
	   ELSE
	     KELEM=1
	     DO WHILE (TOKBEG .LE. TOKEND) ! Copy token, with '' -> '
	      IF (SLINE(TOKBEG).EQ.'''' .AND.
     >            SLINE(TOKBEG+1).EQ.'''') TOKBEG = TOKBEG+1
cc	        call tell1('parsevalue: SETVALS ', 'S', SLINE(TOKBEG)) 
	        CALL SETVALS(SLINE(TOKBEG), VAR, KELEM)
cDEBUG		        call tell1('parsevalue: SETVALS ', 'S', SLINE(TOKBEG)) 
	      KELEM = KELEM+1
	      TOKBEG = TOKBEG+1
	     END DO
	     DO KELEM=KELEM,NELEM	! extend with blanks
	       CALL SETVALS(' ', VAR, KELEM)
	     END DO
	   ENDIF
	ELSE IF (TYPECODE .EQ. 2) THEN	! character string
c	   write(*,*) 'Grabbing token'
	   CALL GETTOKEN(SLINE,NCH,DELIM(1:NDELIM),
     >          IGNORENULL(1:NIGNORENULL),
     >		LOC,TOKBEG,TOKEND)
c	     write(*,*) 'PARSEVALUE: Grabbed...', LINE(tokbeg:tokend),
c     >          '... at (', tokbeg, ',', tokend, ')'
	   
	   IF ((TOKBEG .GT. TOKEND) .AND. USE_DEF) then
C		! Use default string, since string in LINE is empty.
	   ELSE
	     KELEM=1
	     DO WHILE (TOKBEG .LE. TOKEND) ! Copy token, with '' -> '
	      IF (LINE(TOKBEG:TOKBEG).EQ.'''' .AND.
     >            LINE(TOKBEG+1:TOKBEG+1).EQ.'''') TOKBEG = TOKBEG+1
c	      IF (.NOT. USE_DEF) THEN
	        CALL SETVALA(LINE(TOKBEG:TOKBEG), VAR, KELEM)
cDEBUG		        call tell1('parsevalue: SETVALA ', 
cDEBUG	     &			'A', LINE(TOKBEG:TOKBEG)) 
c	      ENDIF
	      KELEM = KELEM+1
	      TOKBEG = TOKBEG+1
	     END DO
	     DO KELEM=KELEM,NELEM	! extend with blanks
	       CALL SETVALA(' ', VAR, KELEM)
	     END DO
	   ENDIF
	ELSE			! Expression list
	   DO KELEM=1,NELEM
	      CALL GETTOKEN(SLINE,NCH,DELIM(1:NDELIM),IGNORE(1:NIGNORE),
     >		LOC,TOKBEG,TOKEND)
c	      write (*,*) 'Token=', LINE(TOKBEG:TOKEND)
	      IF (TOKEND-TOKBEG .LT. 0) THEN
		 IF (.NOT. USE_DEF) THEN
		   CALL SETVALUE(DEFAULT_VAL, VAR, TYPECODE, KELEM)
		 ENDIF
	      ELSE IF (LOC_EVAL(LINE(TOKBEG:TOKEND), ANSWER)) THEN
c		 write (*,*) 'Answer=', ANSWER
cJul		 IF (.NOT.(USE_DEF .AND. (ANSWER .EQ. 0.0D0))) THEN
cJul		   CALL SETVALUE(ANSWER, VAR, TYPECODE, KELEM)
cJul		 ENDIF

		 CALL SETVALUE(ANSWER, VAR, TYPECODE, KELEM)
	      ELSE
		 ERR = .TRUE.
	      ENDIF
	   END DO
	ENDIF

C       Restore delim if changed for "W" type
	IF (TEMP_NDELIM .gt. 0) THEN
	   DELIM = TEMP_DELIM
	   NDELIM = TEMP_NDELIM
	ENDIF

	RETURN
	END


C============================================================================
	SUBROUTINE GETTOKEN(LINE,NCH,DELIM,IGNORE,POS,TOKBEG,TOKEND)
C============================================================================
c       Given line and delim, shift POS to the start of the
c	next token and set (START:STOP) to be the token range
	IMPLICIT NONE
	CHARACTER*1 LINE(*)
	CHARACTER DELIM*(*)
	CHARACTER IGNORE*(*)
	INTEGER NCH, POS, TOKBEG, TOKEND

c	INTEGER I
c	write (*,*) 'Parse start ', POS, ' :', (LINE(I), I=POS,NCH)

c       Skip initial blanks on line
	      ! Replaced ignored characters by space.
	IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
 	DO WHILE (POS.LE.NCH .AND. LINE(POS).EQ.' ')
	   POS = POS+1
	      ! Replaced ignored characters by space.
	   IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
	END DO
c	write (*,*) 'Skipped blanks to ', POS

c       Skip to the end of line or next delimiter, remembering
c       the location of the last non-blank character. If token
c       starts with ' match the closing ' but ignore '' inside.
	      ! Replaced ignored characters by space.
	IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
        IF (POS.LE.NCH .AND. LINE(POS).EQ.'''') THEN
	   POS = POS+1
	   TOKBEG=POS		! skip opening character
	   DO WHILE (POS.LE.NCH .AND. ICHAR(LINE(POS)).NE.0)
	      ! Replaced ignored characters by space.
	      IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
	      ! accept '' in the string, but somebody else must do '' -> '
	      IF (LINE(POS).EQ.'''') THEN
		 IF (POS.LT.NCH .AND. LINE(POS+1).NE.'''') GO TO 100
		 POS=POS+1
	      ENDIF
	      POS = POS+1
	   END DO
 100	   TOKEND = POS-1
	   ! Replaced ignored characters by space.
	   IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
	   IF (POS.LE.NCH .AND. ICHAR(LINE(POS)).NE.0) POS = POS+1
	   DO WHILE (POS.LE.NCH .AND. LINE(POS).EQ.' ')
	      POS = POS + 1
	   END DO
	ELSE        ! Not a quoted string
	   TOKBEG=POS
	   TOKEND=-1
	   DO WHILE (POS.LE.NCH .AND. ICHAR(LINE(POS)).NE.0 .AND.
     >		INDEX(DELIM, LINE(POS)).EQ.0)
	      ! Replaced ignored characters by space.
	      IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
	      IF (LINE(POS).NE.' ') TOKEND = POS
	      POS = POS+1
	   END DO
	   IF (TOKEND .LT. 0) TOKEND = POS - 1
	ENDIF

c       DA Addition: April 2001.  If we stopped on a space, skip spaces
c       until we hit some other character (or end)
	IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
	DO WHILE (POS.LE.NCH .AND. LINE(POS).EQ.' ')
	   POS = POS + 1
	END DO

c	Skip the delimiter if we are sitting on one
	! Replaced ignored characters by space.
	IF (INDEX(IGNORE, LINE(POS)) .NE. 0) LINE(POS) = ' '
 	IF (POS .LE. NCH .AND. INDEX(DELIM,LINE(POS)).NE.0) POS=POS+1

c	write (*,*) 'Parse to ', POS, ' with (',TOKBEG,',',TOKEND,')'
	
        RETURN
	END


C============================================================================
	SUBROUTINE GETTYPE (SPEC, TYPE, COUNT)
C============================================================================
c Convert 'nnnX' specification into a type and a count using
c the following type codes.
C	'S' 1 (character array, the original string option)
C	'A' 2 (character String)
C       'K' 3 (Logical*2)
C       'M' 4 (Integer*2)
C       'L' 5 (Logical*4)
C	'I' 6 (Integer*4)
C       'R' 7 (Real*4)
C	'D' 8 (Real*8)
C       'C' 9 (Complex)
C	'W'10 (Word: Character String, with space as delimiter)
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
	TYPE = INDEX('SAKMLIRDCW', SPEC(POS))
	IF (TYPE .EQ. 0) TYPE = INDEX('sakmlirdcw', SPEC(POS))  
	IF (POS.EQ.1) COUNT = 1
c	write (*,*) ' GETTYPE: count=', count, ' type=', SPEC(POS) !debug
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

	GO TO  (4010,4015,4020,4030,4040,4050,4060,4070,4080), TYPE
 4010	RETURN ! Can't get here because never called with strings
 4015	RETURN ! Can't get here because never called with strings

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

