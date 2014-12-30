C...VECSUBS...
C
C...Vector Manipulation Routines:
C
	SUBROUTINE CVMUL (N, C, V, W)
C
C...Set W = C*V.  (V and W can be same N-dim array OK)
C
	DIMENSION V(N), W(N)
C
	DO 100 I=1,N
  100	W(I) = C*V(I)
	RETURN
	END
C
	SUBROUTINE AVMUL (N, A, V, W)
C
C...Set W = A*V where A is a Matrix.  (V and W can be same N-dim array OK)
C
	DIMENSION V(N), W(N), A(1), WORK(1000)
C
	DO 100 K=1,N
	WORK(K) = 0.0
	DO 100 I=1,N
	J = K + N*(I-1)		! Math notation A(K,I) = Aki assumed.
  100	WORK(K) = WORK(K) + A(J)*V(I)
	DO 200 K=1,N
  200	W(K) = WORK(K)
	RETURN
	END
C
	SUBROUTINE VADD (N, V1, V2, W)
C
C...Set W = V1+V2.  (Vi and W can be same N-dim array OK)
C
	DIMENSION V1(N), V2(N), W(N)
C
	DO 100 I=1,N
  100	W(I) = V1(I) + V2(I)
	RETURN
	END
C
	SUBROUTINE VSUB (N, V1, V2, W)
C
C...Set W = V1-V2.  (Vi and W can be same N-dim array OK)
C
	DIMENSION V1(N), V2(N), W(N)
C
	DO 100 I=1,N
  100	W(I) = V1(I) - V2(I)
	RETURN
	END
C
	SUBROUTINE CVADD (N, V1, C, V2, W)
C
C...Set W = V1+C*V2.  (Vi and W can be same N-dim array OK)
C
	DIMENSION V1(N), V2(N), W(N)
C
	DO 100 I=1,N
  100	W(I) = V1(I) + C*V2(I)
	RETURN
	END
C
	FUNCTION VDOT (N, V, W)
C
C...Set VDOT = V*W   (V and W can be same N-dim array OK)
C
	DIMENSION V(N), W(N)
C
	VDOT = 0.0
	DO 100 I=1,N
  100	VDOT = VDOT + V(I)*W(I)
	RETURN
	END
C
	SUBROUTINE VCROSS (N, V1, V2, W)
C
C...Set W = V1xV2.  (V1, V2 or W can be same N-dim array.)
C
	DIMENSION V1(N), V2(N), W(N), WORK(1000)
C
	WORK(1) = V1(2)*V2(3) - V1(3)*V2(2)
	WORK(2) = V1(3)*V2(1) - V1(1)*V2(3)
	WORK(3) = V1(1)*V2(2) - V1(2)*V2(1)
	DO 100 K=1,N
  100	W(K) = WORK(K)
	RETURN
	END
	SUBROUTINE UNITV (N, V, VVV, UV)
C
C...Form Magnitude VVV and Unit Vector UV from parent vector V:
C
	DIMENSION V(N), UV(N)
C
	VVV = SQRT(VDOT(N,V,V))
	DO 200 I=1,N
  200	UV(I) = 0.0
C
	IF (VVV .EQ. 0.0) RETURN
C
	DO 300 I=1,N
  300	UV(I) = V(I)/VVV
C
	RETURN
	END
	SUBROUTINE TRANSPOSE (N, A, B)
C
C...Transpose NxN matrix A and load into B.  (Can be same matrix.)
C
	DIMENSION A(1), B(1)
C
	DO 100 J=1,N
	K = N*(J-1)
	DO 100 I=1,N
	BJI = A(K+I)
	L = N*(I-1) + J
	B(K+I) = A(L)
  100	B(L) = BJI
C
	RETURN
	END
