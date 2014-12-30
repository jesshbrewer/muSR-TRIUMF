      LOGICAL FUNCTION TT_INPUT(DUM)
C======================================================================C
C
C  Tries to determine if the input stream is a terminal.
C  If it is, returns TRUE, otherwise FALSE.
C
C  Modified 11-JUL-91 by FWJ: to handle the case of
C     CREATE/TERMINAL command_string, where
C     SYS$COMMAND = __node$TWAxx but SYS$INPUT = _TWAxx
C  Modified 05-AUG-93 by FWJ: UNIX variant added using ISATTY.
C  Modified 19-JUL-93 by FWJ: UNIX: added check for uninitialized
C     unit number, which could happen if CLEAR_PLOT is not loaded.
C  Modified 24-Jul-2000 BY TMR: Added IMPLICIT NONE.
c
C======================================================================C
      IMPLICIT NONE
c...	passed variables
      REAL DUM

C...	functions
      INTEGER LIB$SYS_TRNLOG
      
c...	local variables
      INTEGER IINS
      COMMON /PLOT_INPUT_UNIT/ IINS

      CHARACTER*256 SYSCOM,SYSINP
      LOGICAL IBATCH
      INTEGER*4 NCH
      INTEGER ISTAT

      IF(IBATCH())THEN
        TT_INPUT=.FALSE.
        RETURN
      ENDIF

      ISTAT = LIB$SYS_TRNLOG('SYS$COMMAND',,SYSCOM)
      ISTAT = LIB$SYS_TRNLOG('SYS$INPUT',,SYSINP)
      NCH = LEN(SYSINP)
      CALL TRIM_BLNK(NCH, SYSINP)
      TT_INPUT = (INDEX(SYSCOM,SYSINP(2:NCH)).NE.0)
      RETURN
      END
