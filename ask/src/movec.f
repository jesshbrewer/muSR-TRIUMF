C============================================================================
      SUBROUTINE MOVEC(N, SOURCE, DEST)
C============================================================================
      CHARACTER*1 SOURCE(*), DEST(*)
      INTEGER N,I
 
      DO 100 I=1,N
         DEST(I) = SOURCE(I)
 100  CONTINUE
      END
 
