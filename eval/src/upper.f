C============================================================================
      SUBROUTINE CASE_UPPER (N, A_STR)
C============================================================================
      IMPLICIT NONE
      INTEGER N
      CHARACTER*1 A_STR(N)
      CHARACTER STR*(*)
      INTEGER I
      LOGICAL FIRST, ISSTR
      CHARACTER*26 LOWER, UPPER
      CHARACTER*1 TABLE(256)
      SAVE FIRST, TABLE
      DATA FIRST/.TRUE./, LOWER/'abcdefghijklmnopqrstuvwxyz'/,
     >     UPPER/'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/

      ISSTR = .FALSE.
      GOTO 1
C============================================================================
      ENTRY STR_UPPER (STR)
C============================================================================
      ISSTR = .TRUE.

 1    IF (FIRST) THEN
         DO I=1,256
            TABLE(I) = CHAR(I-1)
         END DO
         DO I=1,26
            TABLE(ICHAR(LOWER(I:I))+1) = UPPER(I:I)
         END DO

      END IF

      IF (ISSTR) THEN
         DO I=1,LEN(STR)
            STR(I:I)=TABLE(ICHAR(STR(I:I))+1)
         END DO
      ELSE
         DO I=1,N
            A_STR(I)=TABLE(ICHAR(A_STR(I))+1)
         END DO
      END IF
      RETURN
      END
