C============================================================================
      INTEGER FUNCTION STRLEN(STR)
C============================================================================
      IMPLICIT NONE
      CHARACTER STR*(*)

      DO STRLEN=LEN(STR),1,-1
         IF (STR(STRLEN:STRLEN).NE.' ') RETURN
      END DO
      STRLEN = 0
      RETURN
      END
