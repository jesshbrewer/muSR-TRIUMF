      SUBROUTINE SYSMSG(ISTAT,LUN)
C======================================================================C
C                                                                      C
C  PUT_SYSMSG                                      F.W. Jones, TRIUMF  C
C                                                                      C
C  Puts out the system message for status code ISTAT.                  C
C                                                                      C
C  Input:                                                              C
C                                                                      C
C    ISTAT   System status code (e.g. return value from Run-Time       C
C            Library routine or System Service)                        C
C                                                                      C
C    LUN   Optional argument specifying a logical unit number for      C
C          message output.  If LUN=0 or LUN is omitted, the message    C
C          is put on SYS$OUTPUT.  If LUN>0, the message is put out     C
C          on logical unit LUN.                                        C
C                                                                      C
C  Example:                                                            C
C                                                                      C
C    CHARACTER*10 FOO                                                  C
C    FOO=')JUNK'                                                       C
C    ISTAT=LOK_IOFAST$OPENR(1,FOO)                                     C
C    IF(.NOT.ISTAT)THEN                                                C
C      WRITE(*,*)'Error opening file ',FOO                             C
C      CALL PUT_SYSMSG(ISTAT)                                          C
C    ENDIF                                                             C
C                                                                      C
C  Produces the following output on SYS$OUTPUT:                        C
C                                                                      C
C    Error opening file )JUNK                                          C
C    %RMS-F-SYN, file specification syntax error                       C
C    %FOR-F-FILNAMSPE, file name specification error                   C
C                                                                      C
C======================================================================C
c	Added implicit none. TR July 2000.

      IMPLICIT NONE
c...	passed variables
      INTEGER ISTAT,LUN

c...	functions
      LOGICAL LIB$SYS_GETMSG

c...	local variables
      CHARACTER*80 MSG
      LOGICAL IST
      INTEGER L, IEX
C
C Get the message:
      MSG=' '
      IST=LIB$SYS_GETMSG(ISTAT,L,MSG)
      IF(.NOT.IST)RETURN      !failed to get a message ... do nothing.
C
C Chop off any (empty) diagnostic fields:
      IEX=INDEX(MSG,'!')
      IF(IEX.GT.0)L=IEX-1
C
C Put the message to the relevant destination:
      IF(LUN.GT.0)THEN
        WRITE(LUN,10)
10      FORMAT(' ')
        WRITE(LUN,*)MSG(1:L)
      ELSE
        WRITE(*,10)
        WRITE(*,*)MSG(1:L)
      ENDIF      
C
      RETURN
      END
