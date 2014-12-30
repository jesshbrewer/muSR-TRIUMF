cc	Date: Wed, 02 Jun 1999 18:13:56 +0000
cc	From: TANYA MARIA RISEMAN <tmr@thsun7.ph.bham.ac.uk>
cc	Organization: University of Birmingham(UK)
cc	Subject: [.ut]evalfcn.for.  Now with abs() too

      LOGICAL FUNCTION EVALISFUN(NAME, NUMBER)
      CHARACTER NAME*(*)
      INTEGER NUMBER

      INTEGER NFUN
      PARAMETER (NFUN=18)
      CHARACTER LIST(NFUN)*8
      DATA LIST /'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
     >     'sinh', 'cosh', 'tanh', 'exp', 'log', 'log10', 'sqrt',
     >     'aint', 'anint', 'min', 'max', 'abs'/
c      DATA LIST /'sin', 'cos', 'tan', 'acos', 'asin', 'atan',
c     >     'sinh', 'cosh', 'tanh', 'exp', 'log', 'log10', 'sqrt',
c     >     'aint', 'anint', 'min', 'max'/

      EVALISFUN = .TRUE.
      DO NUMBER=1,NFUN
         IF (NAME .EQ. LIST(NUMBER)) RETURN
      END DO
      EVALISFUN = .FALSE.
      END

      REAL*8 FUNCTION EVALDOFUN(NUMBER, VALUE)
      INTEGER NUMBER
      REAL*8 VALUE

      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), NUMBER
      EVALDOFUN = VALUE
      RETURN
 1    EVALDOFUN = SIN(VALUE)
      RETURN
 2    EVALDOFUN = COS(VALUE)
      RETURN
 3    EVALDOFUN = TAN(VALUE)
      RETURN
 4    EVALDOFUN = ASIN(VALUE)
      RETURN
 5    EVALDOFUN = ACOS(VALUE)
      RETURN
 6    EVALDOFUN = ATAN(VALUE)
      RETURN
 7    EVALDOFUN = SINH(VALUE)
      RETURN
 8    EVALDOFUN = COSH(VALUE)
      RETURN
 9    EVALDOFUN = TANH(VALUE)
      RETURN
 10   EVALDOFUN = EXP(VALUE)
      RETURN
 11   EVALDOFUN = LOG(VALUE)
      RETURN
 12   EVALDOFUN = LOG10(VALUE)
      RETURN
 13   EVALDOFUN = SQRT(VALUE)
      RETURN
 14   EVALDOFUN = AINT(VALUE)
      RETURN
 15   EVALDOFUN = ANINT(VALUE)
      RETURN
 16   EVALDOFUN = VALUE ! min
      RETURN
 17   EVALDOFUN = VALUE ! max
      RETURN
 18   EVALDOFUN = ABS(VALUE)
      RETURN
      END
