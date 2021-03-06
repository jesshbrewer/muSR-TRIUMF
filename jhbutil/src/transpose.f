* ======================================================================
* NIST Guide to Available Math Software.
* Fullsource for module 513 from package TOMS.
* Retrieved from NETLIB on Thu Apr  9 18:24:36 1998.
* ======================================================================
      SUBROUTINE TRANSPOSE(A, M, N, MOVE, IWRK)
C *****
C  ALGORITHM 380 - REVISED
C *****
C  A IS A ONE-DIMENSIONAL ARRAY OF LENGTH MN=M*N, WHICH
C  CONTAINS THE MXN MATRIX TO BE TRANSPOSED (STORED
C  COLUMWISE). 
C  that is, M is runlength, N is number of runs
C  MOVE IS A ONE-DIMENSIONAL ARRAY OF LENGTH IWRK
C  USED TO STORE INFORMATION TO SPEED UP THE PROCESS.  THE
C  VALUE IWRK=(M+N)/2 IS RECOMMENDED.
      DIMENSION A(M*N), MOVE(IWRK)
      REAL A
      INTEGER M,N,MOVE, IWRK
C CHECK ARGUMENTS AND INITIALIZE.
      IF (M.LT.2 .OR. N.LT.2) GO TO 120
      IF (M.EQ.N) GO TO 130
      NCOUNT = 2
      K = M*N - 1
      DO 10 I=1,IWRK
        MOVE(I) = 0
   10 CONTINUE
      IF (M.LT.3 .OR. N.LT.3) GO TO 30
C CALCULATE THE NUMBER OF FIXED POINTS, EUCLIDS ALGORITHM
C FOR GCD(M-1,N-1).
      IR2 = M - 1
      IR1 = N - 1
   20 IR0 = MOD(IR2,IR1)
      IR2 = IR1
      IR1 = IR0
      IF (IR0.NE.0) GO TO 20
      NCOUNT = NCOUNT + IR2 - 1
C SET INITIAL VALUES FOR SEARCH
   30 I = 1
      IM = M
C AT LEAST ONE LOOP MUST BE RE-ARRANGED
      GO TO 80
C SEARCH FOR LOOPS TO REARRANGE
   40 MAX = K - I
      I = I + 1
      IF (I.GT.MAX) stop
      IM = IM + M
      IF (IM.GT.K) IM = IM - K
      I2 = IM
      IF (I.EQ.I2) GO TO 40
      IF (I.GT.IWRK) GO TO 60
      IF (MOVE(I).EQ.0) GO TO 80
      GO TO 40
   50 I2 = M*I1 - K*(I1/N)
   60 IF (I2.LE.I .OR. I2.GE.MAX) GO TO 70
      I1 = I2
      GO TO 50
   70 IF (I2.NE.I) GO TO 40
C REARRANGE THE ELEMENTS OF A LOOP AND ITS COMPANION LOOP
   80 I1 = I
      KMI = K - I
      B = A(I1+1)
      I1C = KMI
      C = A(I1C+1)
   90 I2 = M*I1 - K*(I1/N)
      I2C = K - I2
      IF (I1.LE.IWRK) MOVE(I1) = 2
      IF (I1C.LE.IWRK) MOVE(I1C) = 2
      NCOUNT = NCOUNT + 2
      IF (I2.EQ.I) GO TO 110
      IF (I2.EQ.KMI) GO TO 100
      A(I1+1) = A(I2+1)
      A(I1C+1) = A(I2C+1)
      I1 = I2
      I1C = I2C
      GO TO 90
C FINAL STORE AND TEST FOR FINISHED
  100 D = B
      B = C
      C = D
  110 A(I1+1) = B
      A(I1C+1) = C
      IF (NCOUNT.LT.M*N) GO TO 40
C NORMAL RETURN
  120 RETURN
C IF MATRIX IS SQUARE,EXCHANGE ELEMENTS A(I,J) AND A(J,I).
  130 N1 = N - 1
      DO 150 I=1,N1
        J1 = I + 1
        DO 140 J=J1,N
          I1 = I + (J-1)*N
          I2 = J + (I-1)*M
          B = A(I1)
          A(I1) = A(I2)
          A(I2) = B
  140   CONTINUE
  150 CONTINUE
      GO TO 120
      END
