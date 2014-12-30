C Q: Need BTD to make encodx work and remove hacks.
C
C...TELL...
C
C...Format-Free Output to SYS$OUTPUT:  Deliver a one-line message 
C   stored in STORY, followed by up to 10 fields of variables.
C
C		-- Jess Brewer (August 1983)
C
C   Each Field is specified as either a single variable or an array; 
C   Put variable value(s) on the same line if there is room.   
C   Each Field may be: 
C
C       'S': character string [character*1] 
C	'K': Logical*2			'L': Logical*4 
C	'M': Integer*2			'I': Integer*4 
C	'R': Real*4 			'D': Real*8 
C or	'C': Complex [2*4=8 bytes] 
C
c	TR Jul 24, 2000. Changed how prompts written out, so that
c	**ALL** prompts end with a single blank. This is due 
c	to copying to NEWPROMPT, to help with ASK*D() routines.
c
C   Note that in order for this to work properly, you must make sure
C   your input string is of the correct length. The following example
c   doesn't do the right thing:
c        character*80 story
c        story='you are number '
c        call tell1(story, 'I', 1)
c   but the next one does:
c        character*80 story
c        story='you are number '
c        call tell1(story(1:strlen(story)+1), 'I', 1)
c   Note that strlen returns the position of the last non-blank character
c   in the string, so to get a space after the prompt we must put it in
c   by hand. Fortunately, the following is correct:
c        call tell1('you are number ', 'I', 1)
c
c   Now introduced TELL*D
c	call tell1D(storey, 'format', var, storeydef)
c   which returns the output string in storeydef rather than printing it
c   and also puts in the current values of var between {}.
c   Useful for creating prompts including the default values, in ASK.
c	
C----------------------------------------------------------------------
	SUBROUTINE TELLBASE
C----------------------------------------------------------------------
	IMPLICIT NONE
	CHARACTER STORY*(*), STOREYDEF*(*)
	CHARACTER*1 L1(*), L2(*), L3(*), L4(*), L5(*), L6(*),
     .		  L7(*), L8(*), L9(*), L10(*)
	REAL*4  A1(*), A2(*), A3(*), A4(*), A5(*), A6(*), A7(*),
     .		A8(*), A9(*), A10(*)

	INTEGER POS
C	TR Jul 24, 2000 Increase size of line. Check for overflow.
	CHARACTER LINE*131
C	CHARACTER LINE*79
	LOGICAL USE_DEF

	INTEGER NFIELDS

	SAVE USE_DEF
	SAVE LINE
	SAVE POS

C============================================================================
	ENTRY TELL0 (STORY)
	NFIELDS = 0
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL1 (STORY, L1,A1)
	NFIELDS = 1
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL2 (STORY, L1,A1, L2,A2)
	NFIELDS = 2
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL3 (STORY, L1,A1, L2,A2, L3,A3)
	NFIELDS = 3
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL4 (STORY, L1,A1, L2,A2, L3,A3, L4,A4)
	NFIELDS = 4
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL5 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >    L5,A5)
	NFIELDS = 5
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL6 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6)
	NFIELDS = 6
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY TELL7 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7)
	NFIELDS = 7
	USE_DEF = .FALSE.
	GOTO 1
      
	ENTRY TELL8 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8)
	NFIELDS = 8
	USE_DEF = .FALSE.
	GOTO 1
      
	ENTRY TELL9 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8, L9,A9)
	NFIELDS = 9
	USE_DEF = .FALSE.
	GOTO 1
      
	ENTRY TELL10 (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10)
	NFIELDS = 10
	USE_DEF = .FALSE.
	GOTO 1
      
C======================================================================

	ENTRY TELL1D (STORY, L1,A1, STOREYDEF)
	NFIELDS = 1
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL2D (STORY, L1,A1, L2,A2, STOREYDEF)
	NFIELDS = 2
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL3D (STORY, L1,A1, L2,A2, L3,A3, STOREYDEF)
	NFIELDS = 3
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL4D (STORY, L1,A1, L2,A2, L3,A3, L4,A4, STOREYDEF)
	NFIELDS = 4
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL5D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >    L5,A5, STOREYDEF)
	NFIELDS = 5
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL6D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, STOREYDEF)
	NFIELDS = 6
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY TELL7D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, STOREYDEF)
	NFIELDS = 7
	USE_DEF = .TRUE.
	GOTO 1
      
	ENTRY TELL8D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8, STOREYDEF)
	NFIELDS = 8
	USE_DEF = .TRUE.
	GOTO 1
      
	ENTRY TELL9D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, STOREYDEF)
	NFIELDS = 9
	USE_DEF = .TRUE.
	GOTO 1
      
	ENTRY TELL10D (STORY, L1,A1, L2,A2, L3,A3, L4,A4,
     >     L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, STOREYDEF)
	NFIELDS = 10
	USE_DEF = .TRUE.
	GOTO 1
      
C======================================================================
 1	NFIELDS = MIN0(NFIELDS,10)
C
	LINE = STORY
	POS = LEN(STORY)+1

	IF (NFIELDS .LT. 1) THEN
C	  WRITE (*,*) 'TELL: (NFIELDS .LT. 1) ', NFIELDS
	  GO TO 4000
	ENDIF

	IF (USE_DEF) THEN
c	   write (*,*) 'tell*d: writing { ', pos
	  if (pos+1 .gt. len(line)) goto 4000
	  LINE(POS:POS) = '{'
	  POS = POS+1
	ENDIF

	CALL TELL_ARR_OUT (L1, A1, LINE, POS)
	IF (NFIELDS .LT. 2) GO TO 3000

	if (pos+2 .gt. len(line)) THEN
	  WRITE (*,*) 'TELL: (pos+2 .gt. len(line))', pos, len(line)
	  goto 4000
	ENDIF

	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L2, A2, LINE, POS)
	IF (NFIELDS .LT. 3) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L3, A3, LINE, POS)
	IF (NFIELDS .LT. 4) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L4, A4, LINE, POS)
	IF (NFIELDS .LT. 5) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L5, A5, LINE, POS)
	IF (NFIELDS .LT. 6) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L6, A6, LINE, POS)
	IF (NFIELDS .LT. 7) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L7, A7, LINE, POS)
	IF (NFIELDS .LT. 8) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L8, A8, LINE, POS)
	IF (NFIELDS .LT. 9) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L9, A9, LINE, POS)
	IF (NFIELDS .LT. 10) GO TO 3000

	if (pos+2 .gt. len(line)) goto 4000
	LINE(POS:POS+2) = ', '
	POS = POS+2
	CALL TELL_ARR_OUT (L10, A10, LINE, POS)
C
 3000	CONTINUE
	IF (USE_DEF) THEN
c	  write (*,*) 'tell*d: writing } ', pos 
	  if (pos+1 .gt. len(line)) goto 4000
	  LINE(POS:POS) = '}'
	  POS = POS+1
C	  STOREYDEF = LINE
	ENDIF

 4000   CONTINUE
	IF (USE_DEF) THEN
	  STOREYDEF = LINE
	ENDIF
        IF ((POS.GT.1) .and. .not. USE_DEF) then
	  WRITE (*,*) ' ', LINE(1:POS)
c        else
c	  write (*,*) 'tell*d: line=', LINE(1:POS)
	endif
c	write(*,*) 'TELL: pos=', pos, ':', line(1:pos)
	RETURN
	END

C========================================================
	SUBROUTINE TELL_ARR_OUT (TYPESTR, VAR, LINE, POS)
C--------------------------------------------------------
	IMPLICIT NONE
	CHARACTER LINE*(*)
	CHARACTER*1 TYPESTR(*)
	REAL*4 VAR(*)
	INTEGER POS

	INTEGER SETSTR
	CHARACTER CODE*32
	CHARACTER*1 CH
	INTEGER TYPECODE, J, N, LENGTH
C========================================================================
C-------------------------------------------------------- Interpret type
	CALL GETTYPE(TYPESTR,TYPECODE,N)
	IF (TYPECODE .EQ. 0) THEN
	   WRITE (*,9999)   
 9999	   FORMAT (/,'  *** TELL:  Type unrecognizable! ****',/) 
	   RETURN
	ENDIF

C-------------------------------------------------------- Output a string:

 1111	FORMAT (A1,$)
	IF (TYPECODE .EQ. 1) THEN ! String array
c            ! Unitialized strings are filled with zeros, convert to spaces.
           DO J = 1, N
                CALL TRIMZERO_VALS(' ', VAR, J)
           ENDDO
	   DO N=N,1,-1
	      CALL GETVALS(CH, VAR, N)
	      IF (CH .NE. ' ') GOTO 200
	   END DO
 200	   IF (POS+N+1 .GT. LEN(LINE)) THEN
	      WRITE(*,*) ' ', LINE(1:POS)
	      POS = 1
	   ENDIF
	   DO J=1,N
	      CALL GETVALS(CH, VAR, J)
	      LINE(POS:POS) = CH
	      POS = POS+1
	   END DO


	ELSE IF (TYPECODE .EQ. 2) THEN ! String string
c            ! Unitialized strings are filled with zeros, convert to spaces.
           DO J = 1, N
                CALL TRIMZERO_VALA(' ', VAR, J)
           ENDDO
	   DO N=N,1,-1
	      CALL GETVALA(CH, VAR, N)
	      IF (CH .NE. ' ') GOTO 300
	   END DO
 300	   IF (POS+N+1 .GT. LEN(LINE)) THEN
	      WRITE(*,*) ' ', LINE(1:POS)
	      POS = 1
	   ENDIF
	   DO J=1,N
	      CALL GETVALA(CH, VAR, J)
	      LINE(POS:POS) = CH
	      POS = POS+1
	   END DO

C-------------------------------------------------------- Output an array:
	ELSE                      ! Array i.e, [x1, x2, ..., xn] or x1
	   DO J=1,N
	      LENGTH = SETSTR(VAR, CODE, TYPECODE, J)
c	      write (*,*) 'Appending ', CODE
	      IF (POS+LENGTH+2 .GT. LEN(LINE)) THEN
		 WRITE(*,*) ' ', LINE(1:POS)
		 POS = 1
	      ENDIF
	      IF (N.GT.1 .AND. J.EQ.1) THEN 
		 LINE(POS:POS) = '['
		 POS = POS+1
	      ENDIF
	      LINE(POS:) = CODE(1:LENGTH)
	      POS = POS + LENGTH
	      IF (N.GT.1) THEN
		 IF (J.NE.N) THEN
		    LINE(POS:POS+1) = ', '
		    POS = POS+2
		 ELSE
		    LINE(POS:POS) = ']'
		    POS = POS+1
		 ENDIF
	      ENDIF
	   END DO
	ENDIF
	RETURN
	END

C============================================================================
	INTEGER FUNCTION SETSTR (VAR, STR, TYPE, ELEMENT)
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
	REAL*4 VAR(*)
	CHARACTER STR*(*)
	INTEGER TYPE, ELEMENT

	INTEGER SETSTR_L, SETSTR_I, SETSTR_R, SETSTR_C

	CHARACTER*1 S
	CHARACTER   A*(1)
	LOGICAL*2 K
	INTEGER*2 M
	LOGICAL*4 L
	INTEGER*4 I
	REAL*4 R
	REAL*8 D
	COMPLEX*8 C

	CALL SETC (LEN(STR),STR,' ')
	GO TO  (4010,4015,4020,4030,4040,4050,4060,4070,4080), TYPE

 4010	CALL GETVALS(S, VAR, ELEMENT)
	STR(1:1) = S
	SETSTR = 1
	RETURN

 4015	CALL GETVALA(A, VAR, ELEMENT)
	STR(1:1) = A
	SETSTR = 1
	RETURN

 4020	CALL GETVALK(K, VAR, ELEMENT)
	L = K
	SETSTR = SETSTR_L(STR, L)
	RETURN

 4030	CALL GETVALM(M, VAR, ELEMENT)
	I = M
	SETSTR = SETSTR_I(STR, I)
	RETURN

 4040	CALL GETVALL(L, VAR, ELEMENT)
	SETSTR = SETSTR_L(STR, L)
	RETURN

 4050	CALL GETVALI(I, VAR, ELEMENT)
	SETSTR = SETSTR_I(STR, I)
	RETURN

 4060	CALL GETVALR(R, VAR, ELEMENT)
	SETSTR = SETSTR_R(STR, R, 6)
	RETURN

 4070	CALL GETVALD(D, VAR, ELEMENT)
	R = D
	SETSTR = SETSTR_R(STR, R, 8)
	RETURN

 4080	CALL GETVALC(C, VAR, ELEMENT)
	SETSTR = SETSTR_C(STR, C, 5)
	RETURN

	END


C============================================================================
	INTEGER FUNCTION SETSTR_I(STR, I)
C============================================================================
        IMPLICIT NONE
	INTEGER*4 I
	CHARACTER STR*(*)

	INTEGER J, K

	WRITE (STR,1000) I
 1000	FORMAT (I32)
	DO 10 J = 1, LEN(STR)
	   IF (STR(J:J) .NE. ' ') GOTO 11
 10	CONTINUE
 11	DO 12 K = 1, LEN(STR)-J+1
	   STR(K:K) = STR(K+J-1:K+J-1)
 	   IF (STR(K:K) .EQ. ' ') GOTO 21
 12	CONTINUE
 21	SETSTR_I = K-1
	RETURN
	END

C============================================================================
	INTEGER FUNCTION SETSTR_L(STR, L)
C============================================================================
	LOGICAL*4 L
	CHARACTER STR*(*)

	STR(1:1) = 'F'
	IF (L) STR(1:1) = 'T'
	SETSTR_L = 1
	RETURN
	END

C============================================================================
	INTEGER FUNCTION SETSTR_R(STR, R, NSIG)
C============================================================================
	IMPLICIT NONE
	REAL*4 R
	CHARACTER*(*) STR
	CHARACTER*1 LINE(12)
	INTEGER NSIG


	INTEGER SIG, I

	SIG = NSIG	! ENCDX updates SIG with number actually printed
	CALL ENCDX (R, LINE, 1, 12, -1, SIG)
	DO I=1,12
	   STR(I:I) = LINE(I)
	   IF (STR(I:I) .EQ. ' ') GOTO 100
	END DO
	I = I+1
 100	SETSTR_R = I-1
	RETURN
	END

C============================================================================
	INTEGER FUNCTION SETSTR_C(STR, C, NSIG)
C============================================================================
	COMPLEX*8 C
	CHARACTER*(*) STR
	INTEGER NSIG

        INTEGER SETSTR_R
	INTEGER OFFSET

	STR = '('
	OFFSET = SETSTR_R(STR(2:), REAL(C), NSIG) + 2
	STR(OFFSET:OFFSET) = ','
	OFFSET = SETSTR_R(STR(OFFSET+1:), AIMAG(C), NSIG) + 2
	STR(OFFSET:OFFSET) = ')'
	SETSTR_C = OFFSET+1
	RETURN
	END
