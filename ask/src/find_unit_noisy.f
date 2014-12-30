      SUBROUTINE FIND_UNIT(LUNZ)
C***********************************************************************
C                                                                      *
C    Reqd. routines - NONE                                             *
C                                                                      *
C  Find a free unit number between 30 and 99                           *
C  Output: LUNZ (the first free unit number found)                     *
C                                                                      *
C                                           Author:  J Chuma           *
C                                           Date:    April 19, 1984    *
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C	Added implicit none and changed WRITE(6,*) to WRITE(*,*). TR July 2000.
      IMPLICIT NONE
C...	passed variables      
      LOGICAL LUOPEN
c...	local variables
      INTEGER LUNZ
      DO 100 LUNZ = 30, 99
        INQUIRE(UNIT=LUNZ,OPENED=LUOPEN,ERR=100)
        IF(.NOT. LUOPEN)RETURN
100   CONTINUE
      WRITE(*,*)' *** UNABLE TO FIND AN UN-OPENED UNIT ***'
      LUNZ = -1
      RETURN
      END
