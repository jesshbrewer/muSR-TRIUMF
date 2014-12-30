C...MATRIXSUBS...
C
C...PACKAGE OF GENERAL-PURPOSE MATRIX-MANIPULATION SUBROUTINES
C   SUPPLEMENTING SYSTEM LIBRARY ROUTINES...
C
C------------------JESS BREWER, ID "JESS" -----MAY 1978
C
	SUBROUTINE CMATM (IADD, NDIM, CONST, A, B, C)
C
COMPLEX MATRIX MULTIPLICATION --
C  FOR IADD = 0, C = CONST*(A*B)
C  FOR IADD > 0, C = C + CONST*(A*B)
C  ALL MATRICES ARE NDIM X NDIM.
C  EVERYTHING IS COMPLEX.
C
	COMPLEX CONST, A(*), B(*), C(*)
	COMPLEX ELEM
C
	DO 100 J=1,NDIM
	IXTR = (J-1)*NDIM
	DO 100 I=1,NDIM
	LOC = I + IXTR
	ELEM = 0.0
	DO 200 K=1,NDIM
	IKLOC = I + (K-1)*NDIM
	KJLOC = K + IXTR
  200	ELEM = ELEM + CONST*A(IKLOC)*B(KJLOC)
	IF (IADD .EQ. 0) GO TO 99
	C(LOC) = C(LOC) + ELEM
	GO TO 100
   99	C(LOC) = ELEM
  100	CONTINUE
C
	RETURN
	END
C======================================================================
	SUBROUTINE CSTMAT (IADD, N, C, A, B)
C
C...MULTIPLY THE COMPLEX MATRIX "A" BY THE COMPLEX CONSTANT "C"
C   AND RETURN THE RESULTANT COMPLEX MATRIX "B".
C   ALL MATRICES ARE   N-BY-N .
C...FOR IADD = 0, B = C*A.   FOR IADD > 0, B = B + C*A.
C
	COMPLEX C, A(*), B(*)
	COMPLEX CTEMP
C
	DO 100 J=1,N
	IXTR = (J-1)*N
	DO 100 I=1,N
	K = I + IXTR
	CTEMP = A(K)
	CTEMP = CTEMP*C
	IF (IADD .EQ. 0) GO TO 97
	B(K) = B(K) + CTEMP
	GO TO 100
   97	B(K) = CTEMP
  100	CONTINUE
	RETURN
	END
C======================================================================
	SUBROUTINE CBLKUD (IFUP, NDIM, MDIM, B, A)
C
CONSTRUCT THE NDIM X NDIM MATRIX A(IA,JA) OUT OF THE (NBLKS*NBLKS)
C	    MDIM X MDIM BLOCKS B(IB,JB,KB,LB) -- WHERE THE TWO LAST INDIC
C	    ON B SPECIFY THE BLOCK POSITION AND THE FIRST TWO THE
C	    POSITION WITHIN THAT BLOCK.  TO KEEP IT GENERAL, USE
C	    ONE-DIMENSIONAL ARRAYS AND CALCULATE THE INDEX.
CONSTRUCT A OUT OF B WHEN IFUP = 1
CONSTRUCT B OUT OF A WHEN IFUP = -1
C  ADD CONVERTED B INTO A WHEN IFUP = 2
C  ADD CONVERTED A INTO B WHEN IFUP = -2
C	   SET A AND EACH B EQUAL TO UNIT MATRICES WHEN IFUP = 0.
COMPLEX A AND B ONLY.
C
	COMPLEX A(*), B(*)
C
	NBLKS = NDIM/MDIM
	IF (NBLKS*MDIM .NE. NDIM) GO TO 9999
	MSQ = MDIM*MDIM
C
	DO 100 LB=1,NBLKS
	LBM1 = LB - 1
	KBEXTR = LBM1*NBLKS
	DO 100 KB=1,NBLKS
	KBM1 = KB - 1
	LKEXTR = (KBEXTR + KBM1)*MSQ
	DO 100 JB=1,MDIM
	JBM1 = JB - 1
	LKJXTR = LKEXTR + JBM1*MDIM
	DO 100 IB=1,MDIM
	KKB = IB + LKJXTR
	IA = IB + KBM1*MDIM
	JA = JB + LBM1*MDIM
	KA = IA + (JA - 1)*NDIM
	IF (IFUP .GT. 0) GO TO 98
	IF (IFUP .LT. 0) GO TO 99
C  SET A AND EACH B = IDENTITY MATRICES.
	A(KA) = 0.0
	B(KKB) = 0.0
	IF (IA .EQ. JA) A(KA) = 1.0
	IF (IB .EQ. JB) B(KKB) = 1.0
	GO TO 100
   98	IF (IFUP .GT. 1) GO TO 981
C  SET A = CONVERTED B (BLOCK UP).
	A(KA) = B(KKB)
	GO TO 100
C  SET A = A + CONVERTED B (BLOCK UP AND ADD).
  981	A(KA) = A(KA) + B(KKB)
	GO TO 100
   99	IF (IFUP .LT. -1) GO TO 991
C  SET B = CONVERTED A (BLOCK DOWN).
	B(KKB) = A(KA)
	GO TO 100
C  SET B = B + CONVERTED A (BLOCK DOWN AND ADD).
  991	B(KKB) = B(KKB) + A(KA)
  100	CONTINUE
C
	RETURN
C
C  YOU BLEW IT.  NDIM IS NOT EVENLY DIVISIBLE BY MDIM.
 9999	WRITE (*,999) NDIM, MDIM
  999	FORMAT (10(/),10X,'-------- YOU TRIED TO BREAK UP A',I3,
     1     ' - DIMENSIONAL MATRIX INTO',I3,' - DIMENSIONAL BLOCKS.')
	STOP
	END
C======================================================================
	SUBROUTINE PRCMAT (LUN, N, A)
C
C...PRINT OUT THE N-BY-N MATRIX "A(I,J)" ON LOGICAL UNIT "LUN"...
C
	COMPLEX A(*)
	DIMENSION TEMP(32)
C
C...AESTHETICALLY PROPORTIONED FOR 4-BY-4.  32 MAX...
C
	IF (N .GT. 32) RETURN
C
	WRITE (LUN, 111) N, N
  111	FORMAT (//,20X,'COMPLEX',I3,'-BY-',I1,'  MATRIX:',/)
C
	DO 100 IPASS=1,2
	GO TO (1,2), IPASS
    1	WRITE (LUN,112)
  112	FORMAT (10X,'REAL PART:',/)
	GO TO 3
    2	WRITE (LUN,113)
  113	FORMAT (10X,'IMAG PART:',/)
    3	DO 100 I=1,N
	DO 200 J=1,N
	K = I + (J-1)*N
	GO TO (198,199), IPASS
  198	TEMP(J) = REAL(A(K))
	GO TO 200
  199	TEMP(J) = AIMAG(A(K))
  200	CONTINUE
	WRITE (LUN, 114) (TEMP(M), M=1,N)
  114	FORMAT (/,4(2X,G15.7), 7(/,4X,4(2X,G15.7)))
  100	CONTINUE
C
	RETURN
	END
C======================================================================
	COMPLEX FUNCTION ELMTRX (N, VF, A, VI)
C
C...GENERAL-PURPOSE MATRIX-ELEMENT CALCULATOR...
C
C...ELMTRX = MATRIX ELEMENT <VF|A|VI>, WHERE |VI> AND |VF> ARE
C   N-DIMENSIONAL COMPLEX VECTORS AND  A  IS AN N-BY-N COMPLEX MATRIX...
C
	COMPLEX VF(*), A(*), VI(*)
C
	ELMTRX = 0.0
C
	DO 100 J=1,N
	K = N*(J-1)
	DO 100 I=1,N
C
  100	ELMTRX = ELMTRX + CONJG(VF(I))*A(K+I)*VI(J)
C
C...NOTE! THE MATRIX A IS ASSUMED TO BE OF CONVENTIONAL FORM
C...THAT IS, "COLUMN" J AND "ROW" I FOR A(I,J)...
C
	RETURN
	END
C======================================================================
	COMPLEX FUNCTION DOTPRD (N, VF, VI)
C
C...INNER-PRODUCT of Complex State Vectors:
C
C...DOTPRD = <VF|VI>, where |VI> and |VF> are 
C   N-dimensional complex State Vectors.
C
	COMPLEX VF(*), VI(*)
C----------------------------------------------------------------------
	DOTPRD = 0.0
C
	DO 100 I=1,N
  100	DOTPRD = DOTPRD + CONJG(VF(I))*VI(I)
C
	RETURN
	END

