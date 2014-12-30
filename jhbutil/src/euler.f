C...EULER...
C
C...Generate the 3-D rotation matrix ROTMAT
C   specified by the conventional Euler angles
C   A (alpha), B (beta) and G (gamma). 
C
	SUBROUTINE EULER (A, B, G, ROTMAT)
C
	REAL*4 A, B, G, ROTMAT(3,3)
C===========================================================
	CA = COS(A)
	SA = SIN(A)
	CB = COS(B)
	SB = SIN(B)
	CG = COS(G)
	SG = SIN(G)
	ROTMAT(1,1) = CA*CB*CG - SA*SG
	ROTMAT(2,1) = SA*CB*CG + CA*SG
	ROTMAT(3,1) = - SB*CG 
	ROTMAT(1,2) = - CA*CB*SG - SA*CG
	ROTMAT(2,2) = - SA*CB*SG + CA*CG
	ROTMAT(3,2) = SB*SG
	ROTMAT(1,3) = CA*SB
	ROTMAT(2,3) = SA*SB
	ROTMAT(3,3) = CB
C
	RETURN
	END
