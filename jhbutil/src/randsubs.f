C...RANDOM...
C
C...A package of Random-Number Generators obeying the
C   protocols of the MTS package.
C
	FUNCTION FRANDE (DUMM)
C
 1000	X = RAN(IDUM)
	IF (X .LE. 0.0) GO TO 1000
	FRANDE = - ALOG(X)
C
	RETURN
	END
	FUNCTION FRANDN (DUMM)
C
C...Generate Random Numbers with a Normal Distribution (Mean=0, Std Dev=1)
C   using Marsaglia's Polar Method (see Knuth, Vol. II, pp. 103-113):
C
CCCC	LOGICAL EVEN
CCCC	DATA EVEN /.FALSE./
C
CCCC	IF (EVEN) GO TO 2000
C
 1000	U1 = 2.0*RAN(IDUM) - 1.0
	U2 = 2.0*RAN(IDUM) - 1.0
	S = U1*U1 + U2*U2
	IF (S .GE. 1.0) GO TO 1000
C
	FS = SQRT(-2.0*ALOG(S)/S)
	FRANDN = U1*FS
CCCC	SPARE = U2*FS
C
CCCC	EVEN = .TRUE.
	RETURN
C
CCCC 2000	FRANDN = SPARE
C
CCCC	EVEN = .FALSE.
CCCC	RETURN
	END
	FUNCTION SRANDE (DUMM)
C
C...Symmetric (about zero) Exponentially-Distributed Random Number Generator:
C
	SRANDE = FRANDE(0.)
	IF (RAN(IDUM) .LT. 0.5) SRANDE = - SRANDE
C
	RETURN
	END
	FUNCTION RANDLR (DUMM)
C
C...Lorentzian-Distributed Random Number Generator:
C
 1000	ARG = 1.57079633*RAN(IDUM)
	IF (ARG .GT. 1.57078) GO TO 1000
	RANDLR = TAN(ARG)
	IF (RAN(IDUM) .LT. 0.5) RANDLR = - RANDLR
C
	RETURN
	END
