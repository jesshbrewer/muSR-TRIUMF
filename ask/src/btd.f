      SUBROUTINE BTD(NUMB,CH,NCH,NSIG,FILL,*)
C
C     LIBRARY-ROUTINE
C
C                                                29/JULY/1980
C                                                C.J. KOST SIN
C
C     reqd. routines - NONE
C
C================================================================
C================================================================
C==                                                            ==
C==   BTD: ("BINARY TO DECIMAL"), CONVERTS FORTRAN INTEGER     ==
C==        NUMBERS INTO NUMERIC CHARACTER STRINGS.             ==
C==                                                            ==
C==   THIS ROUTINE IS EQUIVALENT TO THE UBC CHARACTER ROUTINE: ==
C==   "BTD".                                                   ==
C==                                                            ==
C==   WRITTEN BY ARTHUR HAYNES, TRIUMF U.B.C., APRIL 17, 1979. ==
C==                                                            ==
C==   INPUT  PARAMETERS: NUMB,NCH, (I*4); FILL (L*1).          ==
C==                                                            ==
C==   OUTPUT PARAMETERS: CH(NCH), (L*1); NSIG (I*4).           ==
C==                                                            ==
C==   HOW TO USE:                                              ==
C==                                                            ==
C==   CALL BTD(NUMB,CH,NCH,NSIG,FILL,&S)                       ==
C==                                                            ==
C==   WHERE:                                                   ==
C==                                                            ==
C==   NUMB: IS AN INTEGER EXPRESSION GIVING THE NUMBER TO BE   ==
C==         CONVERTED.                                         ==
C==                                                            ==
C==   CH  : IS AN ARRAY OF "NCH" CHARACTERS WHERE THE CHARACTER==
C==         REPRESENTATION OF "NUMB" IS TO BE STORED.          ==
C==                                                            ==
C==   NCH : IS THE NUMBER OF CHARACTERS DESIRED IN "CH".       ==
C==         "NCH" SHOULD BE <= 12 AND => 0. IF "NCH" <= 0, THEN==
C==         THE NUMBER OF CHARACTERS WILL BE TAKEN AS THE      ==
C==         NUMBER OF SIGNIFICANT DIGITS IN "NUMB" PLUS ONE FOR==
C==         THE SIGN IF "NUMB" IS NEGATIVE. IF "NCH">12, THE   ==
C==         CHARACTERS WILL BE RIGHT JUSTIFIED IN THE 12       ==
C==         POSITIONS STARTING WITH "CH" AND A RETURN 1 TAKEN. ==
C==                                                            ==
C==   NSIG: WILL BE SET TO THE NUMBER OF SIGNIFICANT DIGITS    ==
C==         IN 'NUMB', (PLUS 1 IF THE SIGN IS NEGATIVE).       ==
C==                                                            ==
C==   FILL: IS A CHARACTER WHICH WILL BE USED TO REPLACE       ==
C==         LEADING ZEROS IN THE STRING.                       ==
C==                                                            ==
C==   &S  : (RETURN1) IS THE NUMBER OF A FORTRAN STATEMENT TO  ==
C==         WHICH CONTROL WILL BE TRANSFERRED IF "NCH" > 12.   ==
C==                                                            ==
C==   COMMENTS:                                                ==
C==                                                            ==
C==   AFTER A CALL TO BTD, "NSIG" > "NCH" IMPLIES A LOSS OF    ==
C==   SIGNIFICANT DIGITS IN THE CONVERSION.                    ==
C==                                                            ==
C==   IF "NUMB" EQUALS ZERO, THEN THE ENTIRE FIELD OF "NCH"    ==
C==   CHARACTERS IN "CH" WILL CONSIST OF "FILL" CHARACTERS.    ==
C==                                                            ==
C================================================================
C================================================================
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   Modified by J.Chuma, 7Mar97, for LINUX g77
C   ADD1 and NFIRST were BYTE

      CHARACTER*1 CH(1),FILL,MINUS,DIGIT(10)
      LOGICAL ADD1, NFIRST
      DATA MINUS/'-'/,
     * DIGIT/'0','1','2','3','4','5','6','7','8','9'/
      INUMB=IABS(NUMB)
C================================================================
C==   HANDLE THE SPECIAL CASE WHEN "NUMB"= -2**31 = -2147483648==
C==   WHICH IS THE LARGEST NEGATIVE 2'S COMPLEMENT INTEGER     ==
C==   THAT CAN BE STORED IN 32 BITS.                           ==
C==   THIS CASE IS INDICATED WHEN IABS(NUMB) IS NEGATIVE, I.E. ==
C==   WHEN ADD1=INUMB.LT.0 IS .TRUE.                           ==
C==   IN THIS CASE WE INITIALLY SET INUMB=IABS(NUMB+1) WHICH IS==
C==   EQUAL TO |-2147483647| = 2147483647 AND THEN CONVERT     ==
C==   INUMB=2147483647 TO CHARACTERS. AFTER THE CONVERSION WE  ==
C==   THEN CHANGE THE LAST CHARACTER FROM "7" TO "8" = DIGIT(9).=
C================================================================
      ADD1=INUMB.LT.0
      IF(ADD1)INUMB=IABS(NUMB+1)
C================================================================
C==   USING INUMB2=INUMB DETERMINE "NSIG", THE NUMBER OF       ==
C==   SIGNIFICANT DIGITS IN NUMB, BY REPEATEDLY DIVIDING INUMB2==
C==   BY 10 UNTIL IT EQUALS 0. NSIG = THEN NUMBER OF           ==
C==   SIGNIFCANT DIGITS PLUS ONE IF NUMB IS NEGATIVE.          ==
C================================================================
      INUMB2=INUMB
      NSIG=0
      IF(NUMB.LT.0)NSIG=1
10    IF(INUMB2.EQ.0)GO TO 20
      NSIG=NSIG+1
      INUMB2=INUMB2/10
      GO TO 10
C================================================================
C==   NCHAR IS THE NUMBER OF CHARACTERS TO BE PLACED IN "CH".  ==
C================================================================
20    NCHAR=NCH
      IF(NCH.LE.0)NCHAR=NSIG
      IF(NCHAR.GT.12)NCHAR=12
C================================================================
C==   NFIRST IS A FLAG WHICH IS SET TO .TRUE. AS SOON AS INUMB ==
C==   BECOMES ZERO IN THE FOLLOWING DO LOOP.                   ==
C================================================================
      NFIRST=.FALSE.
C================================================================
C==   THE FOLLOWING DO LOOP EXTRACTS THE DIGITS FROM INUMB     ==
C==   ONE BY ONE MOVING RIGHT TO LEFT BY FIRST TAKING          ==
C==   MOD(INUMB,10), WHICH IS THE RIGHT MOST DIGIT OF INUMB,   ==
C==   AND SECONDLY DIVIDING INUMB BY 10 WHICH SHIFTS THE NEXT  ==
C==   DIGIT INTO THE RIGHT MOST POSITION IN INUMB.             ==
C==   WHEN INUMB BECOMES 0 THEN THE LEADING CHARACTERS IN CH   ==
C==   ARE REPLACED BY THE FILL CHARACTER AND A MINUS SIGN IF   ==
C==   NECESSARY.                                               ==
C================================================================
      DO 40 I=1,NCHAR
      INDEX=NCHAR-I+1
      CH(INDEX)=FILL
      IF(INUMB.EQ.0)GO TO 30
      CH(INDEX)=DIGIT(MOD(INUMB,10)+1)
      INUMB=INUMB/10
      GO TO 40
30    IF(NFIRST)GO TO 40
      NFIRST=.TRUE.
C================================================================
C==   HERE IS WHERE WE PLACE THE MINUS SIGN IN CH, IF NUMB < 0.==
C================================================================
      IF(NUMB.LT.0)CH(INDEX)=MINUS
40    CONTINUE
C================================================================
C==   HERE IS WHERE WE CHANGE THE LAST DIGIT IN CH FROM "7" TO ==
C==   "8" FOR THE SPECIAL CASE WHEN ADD1=.TRUE., I.E. WHEN     ==
C==   NUMB = -2**31 = -2147483648 (SEE ABOVE).                 ==
C================================================================
      IF(ADD1)CH(NCHAR)=DIGIT(9)
c     write (*,*) 'btd: final :', (ch(i), i=1,nch), ':'
      IF(NCH.GT.12)RETURN1
      RETURN
      END
