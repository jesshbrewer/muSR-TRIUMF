C ======================================================================
      BLOCK DATA EVALTABLES
C ======================================================================
      INCLUDE 'evaltab.inc'
      DATA NVAR /0/
      END

C ======================================================================
      SUBROUTINE EVALINIT()
C ======================================================================
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      LOGICAL EVALDEFINE
      LOGICAL DONE
      SAVE DONE
      DATA DONE /.FALSE./

      IF (DONE) RETURN
      DONE = .TRUE.
      IF (EVALDEFINE('e',EXP(1.0D0))) CONTINUE
      IF (EVALDEFINE('pi',4.0D0*ATAN(1.0D0))) CONTINUE
      if (evaldefine('c',2.99792458D08)) continue
      if (evaldefine('qe',1.60217653D-19)) continue
      if (evaldefine('h',6.6260693D-34)) continue
      if (evaldefine('hbar',1.05457168D-34)) continue
      if (evaldefine('tauMu',2.19703D-6)) continue
      if (evaldefine('tauPi',26.030D-9)) continue
      if (evaldefine('tauPi0',0.828D-16)) continue
      if (evaldefine('tauN',917.0D0)) continue
      if (evaldefine('alpha',1.0D0/137.03599911D0)) continue
      if (evaldefine('rEl',2.817940325D-15)) continue
      if (evaldefine('GG',6.6742D-11)) continue
      if (evaldefine('g',9.80665D0)) continue
      if (evaldefine('mEarth',5.9742D24)) continue
      if (evaldefine('rEarth',6.378164D6)) continue
      if (evaldefine('rEM',3.84399D8)) continue
      if (evaldefine('rSun',6.9599D8)) continue
      if (evaldefine('mSun',1.989D30)) continue
      if (evaldefine('SolConst',1.353D3)) continue
      if (evaldefine('AU',1.4959787D11)) continue
      if (evaldefine('LY',9.46053D15)) continue
      if (evaldefine('N0',6.0221415D23)) continue
      if (evaldefine('kB',1.3806505D-23)) continue
      if (evaldefine('gmu',0.013553879D0)) continue
      if (evaldefine('HFMu',4463.302D0)) continue
      if (evaldefine('FLUXQ',2.06783461D-7)) continue
      if (evaldefine('dt0BNC',48.848125D-3)) continue
      END

C ======================================================================
      SUBROUTINE EVALLOAD(FILENAME)
C ======================================================================
C Evaluate each line in the user's evalrc resource file
C Lines will be of the form y=expr, so the side effect will
C be to define all the variables
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      LOGICAL LOC_EVAL
      CHARACTER FILENAME*(*)
      INTEGER HANDLE
      REAL*8 VALUE
      CHARACTER LINE*128
      LOGICAL IN_USE

C     ! Find an available handle
      HANDLE = 0
      IN_USE = .TRUE.
      DO WHILE (HANDLE .LT. 20 .AND. IN_USE)
         HANDLE = HANDLE + 1
         INQUIRE (UNIT=HANDLE, OPENED=IN_USE)
      END DO
      IF (HANDLE .EQ. 20) THEN
         WRITE (*,*) 'No available handles --- can''t load variables'
         RETURN
      ENDIF

C     ! Process all lines in the file
      OPEN (UNIT=HANDLE, FILE=FILENAME, STATUS='OLD', ERR=110)
      DO WHILE (.TRUE.)
         READ (*,*,END=100, ERR=110) LINE
         IF (.NOT. LOC_EVAL(LINE, VALUE)) GO TO 110
      END DO
 100  CONTINUE 
      CLOSE (UNIT=HANDLE)
      RETURN
 110  CONTINUE
      CLOSE (UNIT=HANDLE)
      WRITE (*,*) 'Error in variable definition file ', FILENAME
      END


C ======================================================================
      SUBROUTINE EVALSAVE(FILENAME)
C ======================================================================
C Evaluate each line in the user's evalrc resource file
C Lines will be of the form y=expr, so the side effect will
C be to define all the variables
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      CHARACTER FILENAME*(*)
      INTEGER HANDLE, I
      LOGICAL IN_USE

C     ! Find an available handle
      HANDLE = 0
      IN_USE = .TRUE.
      DO WHILE (HANDLE .LT. 20 .AND. IN_USE)
         HANDLE = HANDLE + 1
         INQUIRE (UNIT=HANDLE, OPENED=IN_USE)
      END DO
      IF (HANDLE .EQ. 20) THEN
         WRITE (*,*) 'No available handles --- can''t save variables'
         RETURN
      ENDIF

C     ! Process all lines in the file
      OPEN (HANDLE, FILE=FILENAME, STATUS='NEW', ERR=110)
      DO I=1,NVAR
         WRITE (*,FMT=90,ERR=110) VAR(I), VAL(I)
 90      FORMAT (1A,'=',1G25.17)
      END DO
      CLOSE (UNIT=HANDLE)
      RETURN
 110  CONTINUE
      CLOSE (UNIT=HANDLE)
      WRITE (*,*) 'Error in variable definition file ', FILENAME
      END

C ======================================================================
      LOGICAL FUNCTION EVALDEFINE(NAME, VALUE)
C ======================================================================
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      INTEGER I
      REAL*8 VALUE
      CHARACTER NAME*(*)
      CHARACTER*(NAMELEN) TARGET

c     write (*,*) 'Defining ', name, ' = ', value
      TARGET = NAME
      CALL STR_UPPER(TARGET)
      DO I=1,NVAR
         IF (TARGET .EQ. VAR(I)) THEN
            VAL(I) = VALUE
            EVALDEFINE = .TRUE.
            RETURN
         ENDIF
      END DO
      IF (NVAR .GE. MAXVAR) THEN
         EVALDEFINE = .FALSE.
      ELSE
         EVALDEFINE = .TRUE.
         NVAR = NVAR + 1
         VAR(I) = TARGET
         VAL(I) = VALUE
      ENDIF
      RETURN
      END

C ======================================================================
      LOGICAL FUNCTION EVALLOOKUP(NAME, VALUE)
C ======================================================================
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      REAL*8 VALUE
      CHARACTER NAME*(*)

      INTEGER I
      CHARACTER*(NAMELEN) TARGET

c     write (*,*) 'Looking for ', name
      TARGET = NAME
      CALL STR_UPPER(TARGET)
      DO I=1,NVAR
         IF (TARGET .EQ. VAR(I)) THEN
            VALUE = VAL(I)
            EVALLOOKUP = .TRUE.
c           write(*,*) '... found ', value
            RETURN
         ENDIF
      END DO
      EVALLOOKUP = .FALSE.
      RETURN
      END

C ======================================================================
      LOGICAL FUNCTION EVALCLEAR(NAME)
C ======================================================================
      EXTERNAL EVALTABLES
      INCLUDE 'evaltab.inc'
      INTEGER I,J
      CHARACTER NAME*(*)

c     write (*,*) 'Clearing ', name
      J=1
      DO I=1,NVAR
         IF (NAME .NE. VAR(I)) THEN
            IF (I .NE. J) THEN
               VAR(J) = VAR(I)
               VAL(J) = VAL(I)
            ENDIF
            J = J+1
         ENDIF
      END DO
      EVALCLEAR = I .NE. J
      RETURN
      END

