C     ! constants
      INTEGER MAXVAR, NAMELEN
      PARAMETER (MAXVAR=256, NAMELEN=32)

C     ! common block data
      INTEGER NVAR
      REAL*8 VAL(MAXVAR)
      CHARACTER*(NAMELEN) VAR(MAXVAR)

c     ! common block
      COMMON /EVALTAB/ VAL, NVAR, VAR
