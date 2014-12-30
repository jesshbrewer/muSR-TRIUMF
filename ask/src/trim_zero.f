C============================================================================
      SUBROUTINE TRIM_ZERO (N, STR)
C============================================================================
      IMPLICIT NONE
      INTEGER N
      CHARACTER*1 STR(*), CZ
      integer*1 BZ
      equivalence (BZ,CZ)
      data BZ /0/

      DO WHILE (N .GE. 1 .AND. STR(N) .EQ. CZ)
         N = N - 1
      END DO
      END
