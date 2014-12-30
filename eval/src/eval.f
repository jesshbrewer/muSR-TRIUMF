C The EVAL suite provides a set of calculator functions. They have no 
C external dependencies, but the calling sequence is 
C
C EVAL(STRING,VALUE)->TRUE when VALUE
C EVALDEFINE(NAME,VALUE)->TRUE if associate VALUE with case-sensitive NAME
C EVALLOOKUP(NAME,VALUE)->TRUE if can find VALUE associated with NAME
C EVALCLEAR(NAME,VALUE)->TRUE if can remove NAME->VALUE association
C EVALLOAD(FILENAME) grabs variable definitions from FILENAME
C EVALSAVE(FILENAME) writes variable definitions to FILENAME
C
C LOC_EVAL is a synonym for EVAL and LOAD_SYM is a synonym for EVALDEFINE
C
C Note: pointers to functions don't seem to be possible, but you can extend
C beyond the intrinsic fortran functions by modifying EVALISFUN and EVALDOFUN
C
C Q: currently not case-sensitive
C Q: currently use = for assignment and == for comparison
C    should we swap these for LOC_EVAL compatibility?
C Q: currently only 0.0 is FALSE
C    should a range +/- epsilon be false?
C Q: currently 2.le.3 doesn't work since 2. is a number and .3 is a number
C    does anyone care?
C Q: currently a missing operator defaults to multiply [e.g. 2 sin (2 pi)]
C    is this a good plan?
C Q: currently missing parenthesis on functions are assumed [e.g sin pi]
C    is this a good plan
C Q: currently function has tightest binding [e.g. sin 2 pi == sin(2)*pi]
C    should assumed multiplication be tighter [e.g. sin 2 pi == sin(2*pi)]
C Q: currently prints errors --- a bad thing in batch mode
C    should we return an error code or an error string instead? how?
C Q: currently use &,| for and/or?
C    should we use &&,|| or allow both?
c Q: currently use ** for exponentiation
C    should we use ^ instead, or allow both?
C Q: should we provide C ?: operator   [e.g. (x>5 ? 1 : -1)]
C    implementation note: we lose info when converting numbers to T/F before
C       evaluating condition --- instead use truth(tok,val) returns T/F
C    implementation note: token T_ASSIGN and T_ERROR will need new symbols
C Q: should we provide min(a,b,c,...), max(a,b,c,...)
C    implementation note: 
C         ! before collapse parenthesis
C         collapse T_NUMBER T_LIST T_NUMBER to T_NUMBER T_NUMBER
C         ! replace collapse parenthesis with
C         if (open_expr > 2 && tok(open_expr-2)==T_NAME)
C            if not T_NUMBER T_NUMBER ... then error
C            tok(open_expr-2) = T_NUMBER
C            num(open_expr-2) =
C              evaldofun(fun(open_expr-2),new_close-open_expr+1,num(open_expr))
C            open_expr = open_expr - 1
C         else 
C            if (new_close != open_expr) then error
C            tok(open_expr-1) = T_NUMBER
C            num(open_expr-1) = num(open_expr)
C         shift tail from close_expr+1 to open_expr
C Q: should we provide user defined functions f(x)=expr
C    implementation note:
C         create evalfun(proto,expr) to store fun [e.g. evalfun("f(x,y)","x y")]
C         create evalpush(name,value) to place new entry at end of var table
C         create evalpop(name) to clear last entry from table
C         modify evallookup(name,value) to search back from the end of table
C         modify evaldofun to push all arg-value pairs, tokenize expr and
C           replace the function call tokens T_NAME T_OPEN T_NUM...T_CLOSE
C           with the tokenized expr
C Q: should we use a recursive descent parser
C         It would be faster since we wouldn't have to keep collapsing 
C         and expanding the token string. It could be done in C since
C         some fortrans have difficulty with recursion. Calling C from 
C         fortran isn't as scary as I thought it would be, or I would
C         have done this from the start.
C Q: evaltok should use a character class table rather than INDEX

C     Aliases for compatibility with existing code.
      LOGICAL FUNCTION LOC_EVAL(STRING, VALUE)
      CHARACTER STRING*(*)
      REAL*8 VALUE
      LOGICAL EVAL
      LOC_EVAL = EVAL(STRING, VALUE)
      RETURN
      END

      LOGICAL FUNCTION LOAD_SYM(NAME, VALUE)
      CHARACTER NAME*(*)
      REAL VALUE
      LOGICAL EVALDEFINE
      LOAD_SYM = EVALDEFINE(NAME, dble(VALUE))
      RETURN
      END


      LOGICAL FUNCTION EVAL(STRING, VALUE)
      IMPLICIT NONE
C     !! constants
      INTEGER MAXTOK
      PARAMETER (MAXTOK=256)

c     Note: since tokens are mnemonic, can interpret tok() by printing it
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
      CHARACTER STRING*(*)
      REAL*8 VALUE

C     !! functions
      CHARACTER*1 EVALTOKEN
      LOGICAL EVALLOOKUP, EVALDEFINE, EVALISFUN
      REAL*8 EVALDOFUN

C     !! data
      INTEGER POS, FROM, TO, NTOK
      INTEGER EXPR_OPEN, EXPR_CLOSE, NEW_CLOSE, I, J
      INTEGER VARFROM, VARTO
      LOGICAL ASSIGN
      CHARACTER*1 TOKEN, TOK(MAXTOK)
      REAL*8 NUM(MAXTOK)
      INTEGER FUN(MAXTOK)
      CHARACTER ERR*80

C initialize predefined variables and functions
      CALL EVALINIT

C remove "var=" from the front of the expression and remember if it occurs
      ASSIGN = .FALSE.
      POS = 1
      TOKEN = EVALTOKEN(STRING, POS, FROM, TO)
      IF (TOKEN .EQ. T_NAME) THEN
         VARFROM = FROM
         VARTO = TO
         TOKEN = EVALTOKEN(STRING, POS, FROM, TO)
         IF (TOKEN .EQ. T_ASSIGN) THEN
            ASSIGN = .TRUE.
         ELSE
            POS = 1
         ENDIF
      ELSE
         POS = 1
      ENDIF
      
C wrap the expression with () to eliminate special cases in the evaluation.
      NTOK = 1
      TOK(1) = T_OPEN
      TOKEN = T_OPEN

C tokenize and lookup variables
      DO WHILE (TOKEN .NE. T_EOF)
         TOKEN = EVALTOKEN(STRING, POS, FROM, TO)
c        write(*,*) 'Got token "', TOKEN, '" from ', STRING(FROM:TO)
         NTOK = NTOK+1
         IF (NTOK .EQ. MAXTOK) THEN
            ERR='expression is too long'
            GO TO 666
         END IF
         IF (TOKEN .EQ. T_NAME) THEN
            IF (EVALLOOKUP(STRING(FROM:TO),NUM(NTOK))) THEN
               TOK(NTOK) = T_NUMBER
            ELSE IF (EVALISFUN(STRING(FROM:TO), FUN(NTOK))) THEN
               TOK(NTOK) = T_NAME
            ELSE 
               ERR=STRING(FROM:TO) // ' is not defined'
               GO TO 666
            ENDIF
         ELSE IF (TOKEN .EQ. T_ERROR) THEN
            ERR='invalid text starting ' // STRING(POS:POS+10)
            GO TO 666
         ELSE IF (TOKEN .EQ. T_NUMBER) THEN
            TOK(NTOK) = T_NUMBER
            READ (STRING(FROM:TO),*,ERR=50) NUM(NTOK)
            GO TO 60
 50         ERR=STRING(FROM:TO) // ' is not a valid number'
            GO TO 666
 60         CONTINUE
         ELSE IF (TOKEN .NE. T_EOF) THEN
            TOK(NTOK) = TOKEN
         ENDIF
      ENDDO
      TOK(NTOK) = T_CLOSE


c systematically replace all nested parentheses with their valuations,
c eventually leaving only the valuation of the outermost parenthesis.
c Within each  group, first replace the most tightly bound, and proceed
c toward the most loosely bound. Conceptually this is simple, but it is
c a bit of a trick to make the comression work in place without too much
c extra copying. Note particularly that j (the new end of expression) moves
c much more slowly through the string than i. Further note that the
c results of one compression can be used in the evaluation of the next, so
c we must compare the new j with the old i+1 and i+2. It's a bit wierd. Also
c notice that since transcription is occurring in place with compression, we
c must go all the way to the end of the expression even though it couldn't
c possibly be a target for compression (due to the lack of tokens).

      DO WHILE (TOK(1) .EQ. T_OPEN)
c        write (*,*) 'Expression: ', (TOK(I), I=1,NTOK)
c     find a subexpression with no nested parentheses to evaluate
         EXPR_OPEN = 2
         I = EXPR_OPEN
         DO WHILE (I .LE. NTOK .AND. TOK(I).NE.T_CLOSE)
            IF (TOK(I) .EQ. T_OPEN) EXPR_OPEN = I+1
            I = I+1
         END DO
         EXPR_CLOSE = I-1
         NEW_CLOSE = EXPR_CLOSE
         IF (I .GT. NTOK) THEN
            ERR='mismatched parenthesis'
            GO TO 666
         ENDIF
c        write (*,*) 'sub-exp: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     evaluate function calls
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ. T_NAME .AND. TOK(I+1).EQ.T_NUMBER) THEN
               NUM(J) = EVALDOFUN(FUN(J),NUM(I+1))
               TOK(J) = T_NUMBER
               I = I+1
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'function: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)

c     implied multiplication 
c     make "2 sin(2 pi x)" do the right thing
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ. T_NUMBER .AND. TOK(I+1).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)*NUM(I+1)
               I = I+1
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'multiply: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     exponentiation
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ. T_NUMBER .AND. TOK(I+1).EQ.T_POWER .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)**NUM(I+2)
               I = I+2
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'power: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     multiplication and division
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_MULTIPLY .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)*NUM(I+2)
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_DIVIDE .AND.
     >              TOK(I+2).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)/NUM(I+2)
               I = I+2
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'product: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     addition and subtraction
         I = EXPR_OPEN
         J = I
         IF (TOK(I).EQ.T_SUBTRACT .AND. TOK(I+1).EQ.T_NUMBER) THEN
            TOK(J) = T_NUMBER
            NUM(J) = -NUM(I+1)
            I = I+1
         ENDIF
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ. T_NUMBER .AND. TOK(I+1).EQ.T_SUBTRACT .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)-NUM(I+2)
               I = I+2
            ELSE IF (TOK(J).EQ. T_NUMBER .AND. TOK(I+1).EQ.T_ADD .AND.
     >              TOK(I+2).EQ.T_NUMBER) THEN
               NUM(J) = NUM(J)+NUM(I+2)
               I = I+2
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'sum: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     comparisons
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_GT .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .GT. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_GE .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .GE. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_EQ .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .EQ. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_LE .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .LE. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_LT .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .LT. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE IF (TOK(J).EQ.T_NUMBER .AND. TOK(I+1).EQ.T_NE .AND.
     >           TOK(I+2).EQ.T_NUMBER) THEN
               IF (NUM(J) .NE. NUM(I+2)) THEN
                  TOK(J) = T_TRUE
               ELSE
                  TOK(J) = T_FALSE
               ENDIF
               I = I+2
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'compare: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
         
c     convert numbers to TRUE/FALSE
c     TRUE <=> num != 0.0, but since real, should we approx. 0.0?
         I = EXPR_OPEN
         DO WHILE (I .LE. NEW_CLOSE .AND. TOK(I).NE.T_AND .AND.
     >        TOK(I).NE.T_OR .AND. TOK(I).NE.T_NOT)
            I = I + 1
         END DO
         IF (I .LE. NEW_CLOSE) THEN
            I = EXPR_OPEN
            DO WHILE (I .LE. NEW_CLOSE)
               IF (TOK(I).EQ.T_NUMBER) THEN
                  IF (NUM(I) .NE. 0.0D0) THEN
                     TOK(I) = T_TRUE
                  ELSE
                     TOK(I) = T_FALSE
                  ENDIF
               ENDIF
               I = I+1
            END DO
         END IF
c        write (*,*) '#->T/F: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     logical negation
c     ! num <=> num != 0, but since real, should we approx. 0.0?
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ. T_NOT .AND. TOK(I+1).EQ.T_TRUE) THEN
               TOK(J) = T_FALSE
               I = I+1
            ELSE IF (TOK(J).EQ. T_NOT .AND. TOK(I+1).EQ.T_FALSE) THEN
               TOK(J) = T_TRUE
               I = I+1
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'not: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)
         
c     and/or
         I = EXPR_OPEN
         J = I
         DO WHILE (I .LE. NEW_CLOSE)
            IF (TOK(J).EQ.T_TRUE .AND. TOK(I+1).EQ.T_AND .AND.
     >           TOK(I+2).EQ.T_TRUE) THEN
               TOK(J) = T_TRUE
               I=I+2
            ELSE IF (TOK(J).EQ.T_TRUE .AND. TOK(I+1).EQ.T_AND .AND.
     >              TOK(I+2).EQ.T_FALSE) THEN
               TOK(J) = T_FALSE
               I=I+2
            ELSE IF (TOK(J).EQ.T_FALSE .AND. TOK(I+1).EQ.T_AND .AND.
     >              TOK(I+2).EQ.T_TRUE) THEN
               TOK(J) = T_FALSE
               I=I+2
            ELSE IF (TOK(J).EQ.T_FALSE .AND. TOK(I+1).EQ.T_AND .AND.
     >              TOK(I+2).EQ.T_FALSE) THEN
               TOK(J) = T_FALSE
               I=I+2
            ELSE IF (TOK(J).EQ.T_TRUE .AND. TOK(I+1).EQ.T_OR .AND.
     >              TOK(I+2).EQ.T_TRUE) THEN
               TOK(J) = T_TRUE
               I=I+2
            ELSE IF (TOK(J).EQ.T_TRUE .AND. TOK(I+1).EQ.T_OR .AND.
     >              TOK(I+2).EQ.T_FALSE) THEN
               TOK(J) = T_TRUE
               I=I+2
            ELSE IF (TOK(J).EQ.T_FALSE .AND. TOK(I+1).EQ.T_OR .AND.
     >              TOK(I+2).EQ.T_TRUE) THEN
               TOK(J) = T_TRUE
               I=I+2
            ELSE IF (TOK(J).EQ.T_FALSE .AND. TOK(I+1).EQ.T_OR .AND.
     >              TOK(I+2).EQ.T_FALSE) THEN
               TOK(J) = T_FALSE
               I=I+2
            ELSE
               I = I+1
               J = J+1
               TOK(J) = TOK(I)
               IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
               IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            ENDIF
         ENDDO
         NEW_CLOSE = J-1
c        write (*,*) 'and/or: ', (TOK(I), I=EXPR_OPEN,NEW_CLOSE)

c     finally, collapse the parenthesis
c     convert TRUE/FALSE to numbers
         IF (NEW_CLOSE .NE. EXPR_OPEN) THEN
            ERR='invalid expression'
            GO TO 666
         ELSE IF (TOK(EXPR_OPEN).EQ. T_TRUE) THEN
            TOK(EXPR_OPEN) = T_NUMBER
            NUM(EXPR_OPEN) = 1.0D0
         ELSE IF (TOK(EXPR_OPEN).EQ. T_FALSE) THEN
            TOK(EXPR_OPEN) = T_NUMBER
            NUM(EXPR_OPEN) = 0.0D0
         ELSE IF (TOK(EXPR_OPEN).NE. T_NUMBER) THEN
            ERR='invalid expression'
            GO TO 666
         ENDIF
            
         
         TOK(EXPR_OPEN-1) = TOK(EXPR_OPEN)
         NUM(EXPR_OPEN-1) = NUM(EXPR_OPEN)
         I=EXPR_CLOSE+2
         J=EXPR_OPEN
         DO WHILE (I .LE. NTOK)
            TOK(J) = TOK(I)
            IF (TOK(I).EQ.T_NUMBER) NUM(J) = NUM(I)
            IF (TOK(I).EQ.T_NAME) FUN(J) = FUN(I)
            I = I+1
            J = J+1
         END DO
         NTOK = J-1
         
      END DO

c     check if we completed successfully
      IF (TOK(1).EQ.T_NUMBER .AND. NTOK.NE.1) THEN
         ERR='mismatched parenthesis'
         GO TO 666
      END IF
      EVAL = .TRUE.
      IF (TOK(1) .EQ. T_TRUE) THEN
         VALUE = 1.0D0
      ELSE IF (TOK(1) .EQ. T_FALSE) THEN
         VALUE = 0.0D0
      ELSE
         VALUE = NUM(1)
      ENDIF
      IF (ASSIGN .AND. .NOT. 
     >     EVALDEFINE(STRING(VARFROM:VARTO),VALUE)) THEN
         ERR='could not define ' // STRING(VARFROM:VARTO)
         GOTO 666
      ENDIF
      RETURN

 666  EVAL = .FALSE.
      WRITE (*,*) ERR
      END
      
