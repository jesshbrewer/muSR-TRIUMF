      CHARACTER*1 FUNCTION EVALTOKEN(LINE, POS, TOKBEG, TOKEND)
      IMPLICIT NONE
C     !! constants
      CHARACTER*1 T_NAME, T_NUMBER, T_ASSIGN, T_ADD, 
     >     T_SUBTRACT, T_MULTIPLY, T_DIVIDE, T_POWER, 
     >     T_OPEN, T_CLOSE, T_LIST, T_GT, T_LT,
     >     T_GE, T_LE, T_EQ, T_AND, T_OR, T_NOT, 
     >     T_EOF, T_ERROR, T_NE, T_TRUE, T_FALSE
      PARAMETER (T_NAME='$', T_NUMBER='#', T_ASSIGN=':', T_ADD='+', 
     >     T_SUBTRACT='-', T_MULTIPLY='*', T_DIVIDE='/', T_POWER='^',
     >     T_OPEN='(', T_CLOSE=')', T_LIST=',', T_GT='>', T_LT='<',
     >     T_GE='}', T_LE='{', T_EQ='=', T_AND='&', T_OR='|', T_NOT='!',
     >     T_EOF='.', T_ERROR='?', T_NE='~', T_TRUE='T', T_FALSE='F')

C     !! parameters
      CHARACTER LINE*(*)
      INTEGER POS, TOKBEG, TOKEND

C     !! local variables
      INTEGER NCH
      CHARACTER WORD*20

      CHARACTER DIGIT*10, FIRST*54, REST*64
c      SAVE DIGIT, FIRST, REST
      PARAMETER (DIGIT='0123456789',
     >  FIRST='abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$',
     >  REST=FIRST//DIGIT)

c	EOF:
c	ASSIGN:	  =
c	EQ:       == | .EQ.
c       NE:       != | .NE.
c	GT:       >  | .GT.
c	GE:       >= | .GE.
c	LT:       <  | .LT.
c	LE:       <= | .LE.
c	AND:      &  | .AND.
c	OR:       |  | .OR.
c	NOT:      !  | .NOT.
c       TRUE:     .TRUE.
c       FALSE:    .FALSE.
c	POWER:    **
c	MULTIPLY: *
c	DIVIDE:   /
c	ADD:      +
c	SUBTRACT: -
c	OPEN:     (
c	CLOSE:    )
c	LIST:     ,
c	NAME:	  [a-zA-Z_$][a-zA-Z_$0-9]*
c	NUMBER:		unsigned fraction exponent
c	fraction	[.] digit*
c	exponent 	[eE] signed
c	signed		[+]unsigned | [-]unsigned | unsigned
c	unsigned	digit digit*
c	digit		[0-9]

c     write (*,*) 'Parse start at ', POS
c     Skip initial blanks on line
      NCH=LEN(LINE)
      DO WHILE (POS.LE.NCH .AND. LINE(POS:POS).EQ.' ')
         POS = POS+1
      END DO
      TOKBEG = POS
      TOKEND = POS

C     Check for the end of string
      IF (POS.GT.NCH) THEN
         EVALTOKEN = T_EOF
         RETURN
      ENDIF

c     Decode the operators
      IF (LINE(POS:POS) .EQ. '=') THEN
         IF (LINE(POS+1:POS+1) .EQ. '=') THEN
            POS = POS+2
            TOKEND = TOKBEG+1
            EVALTOKEN = T_EQ
            RETURN
         ELSE
            POS = POS + 1
            EVALTOKEN = T_ASSIGN
            RETURN
         ENDIF
      ENDIF

      IF (LINE(POS:POS) .EQ. '>') THEN
         IF (LINE(POS+1:POS+1) .EQ. '=') THEN
            POS = POS+2
            TOKEND = TOKBEG+1
            EVALTOKEN = T_GE
            RETURN
         ELSE
            POS = POS + 1
            EVALTOKEN = T_GT
            RETURN
         ENDIF
      ENDIF

      IF (LINE(POS:POS) .EQ. '<') THEN
         IF (LINE(POS+1:POS+1) .EQ. '=') THEN
            POS = POS+2
            TOKEND = TOKBEG+1
            EVALTOKEN = T_LE
            RETURN
         ELSE
            POS = POS + 1
            EVALTOKEN = T_LT
            RETURN
         ENDIF
      ENDIF

      IF (LINE(POS:POS) .EQ. '&') THEN
         POS = POS + 1
         EVALTOKEN = T_AND
         RETURN
      END IF

      IF (LINE(POS:POS) .EQ. '|') THEN
         POS = POS + 1
         EVALTOKEN = T_OR
         RETURN
      END IF

      IF (LINE(POS:POS) .EQ. '!') THEN
         POS = POS + 1
         EVALTOKEN = T_NOT
         RETURN
      END IF

      IF (LINE(POS:POS) .EQ. '*') THEN
         IF (LINE(POS+1:POS+1) .EQ. '*') THEN
            POS = POS+2
            TOKEND = TOKBEG+1
            EVALTOKEN = T_POWER
            RETURN
         ELSE
            POS = POS + 1
            EVALTOKEN = T_MULTIPLY
            RETURN
         ENDIF
      ENDIF

      IF (LINE(POS:POS) .EQ. '/') THEN
         POS = POS + 1
         EVALTOKEN = T_DIVIDE
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. '+') THEN
         POS = POS + 1
         EVALTOKEN = T_ADD
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. '-') THEN
         POS = POS + 1
         EVALTOKEN = T_SUBTRACT
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. '(') THEN
         POS = POS + 1
         EVALTOKEN = T_OPEN
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. ')') THEN
         POS = POS + 1
         EVALTOKEN = T_CLOSE
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. ',') THEN
         POS = POS + 1
         EVALTOKEN = T_LIST
         RETURN
      ENDIF

      IF (LINE(POS:POS) .EQ. '.') THEN
C        ! convert to uppercase and compare with .xxx.
         WORD=LINE(POS:POS+7)
         CALL STR_UPPER(WORD)
         IF (WORD(1:4) .EQ. '.EQ.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_EQ
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.NE.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_NE
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.LE.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_LE
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.GE.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_GE
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.LT.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_LT
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.GT.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_GT
            RETURN
         ENDIF
         IF (WORD(1:4) .EQ. '.OR.') THEN
            POS = POS + 4
            TOKEND = POS-1
            EVALTOKEN = T_OR
            RETURN
         ENDIF
         IF (WORD(1:5) .EQ. '.AND.') THEN
            POS = POS + 5
            TOKEND = POS-1
            EVALTOKEN = T_AND
            RETURN
         ENDIF
         IF (WORD(1:5) .EQ. '.NOT.') THEN
            POS = POS + 5
            TOKEND = POS-1
            EVALTOKEN = T_NOT
            RETURN
         ENDIF
         IF (WORD(1:6) .EQ. '.TRUE.') THEN
            POS = POS + 6
            TOKEND = POS-1
            EVALTOKEN = T_TRUE
            RETURN
         ENDIF
         IF (WORD(1:7) .EQ. '.FALSE.') THEN
            POS = POS + 7
            TOKEND = POS-1
            EVALTOKEN = T_FALSE
            RETURN
         ENDIF
      ENDIF


c     NAME:	  [a-zA-Z._$][a-zA-Z._$0-9]*
      IF (INDEX(FIRST,LINE(POS:POS)) .GT. 0) THEN
         POS=POS+1
         DO WHILE(INDEX(REST,LINE(POS:POS)) .GT. 0)
            POS=POS+1
         END DO
         TOKEND = POS-1
         EVALTOKEN = T_NAME
         RETURN
      ENDIF

c	NUMBER:		unsigned fraction exponent
c	fraction	[.] digit*
c	exponent 	[eE] signed
c	signed		[+]unsigned | [-]unsigned | unsigned
c	unsigned	digit digit*
c	digit		[0-9]
      DO WHILE(INDEX(DIGIT,LINE(POS:POS)).GT.0)
         POS = POS+1
      END DO
      IF (LINE(POS:POS).EQ.'.') POS = POS+1
      DO WHILE(INDEX(DIGIT,LINE(POS:POS)) .GT. 0)
         POS = POS+1
      END DO
      IF (LINE(TOKBEG:POS-1).EQ.'.') POS=POS-1
      IF (POS.GT.TOKBEG .AND. INDEX('eE', LINE(POS:POS)).GT.0) THEN
         TOKEND = POS   ! temporary hold: number must end with digit
         POS=POS+1
         IF (INDEX('+-',LINE(POS:POS)).GT.0) POS=POS+1
         IF (INDEX(DIGIT,LINE(POS:POS)).EQ.0) POS=TOKEND
         DO WHILE(INDEX(DIGIT,LINE(POS:POS)).GT.0)
            POS=POS+1
         END DO
      ENDIF
      IF (POS.GT.TOKBEG) THEN
         TOKEND=POS-1
         EVALTOKEN = T_NUMBER
         RETURN
      ENDIF

      EVALTOKEN = T_ERROR
      END
