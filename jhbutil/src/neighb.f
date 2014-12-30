C...NEIGHB...
C
C...Analyze the Nearest, Next-Nearest, etc., Neighbor Shells for a
C   given lattice with a Primitive Basis consisting of NAPB atoms at
C   locations specified by RPB and Primitive Lattice Vectors PLV.
C   The jPB'th atom of the basis has coordinates RPB(k,jPB) in
C   units of the primitive lattice vectors V(k), which in turn
C   have Cartesian components PLV(i,k) where i = x,y,z.  Got that?
C   Please note that you must stick strictly to this notation!
C   (You can load in the common lattices using "STDLAT" subroutine.)
C
C   A maximum of NSITES sites will be searched,
C   starting more or less with the closest,
C   and the resultant sites' positions stored in the
C   arrays INDEXS and RSITE, each of which must be dimensioned
C   at least as large as NSITES.  RSITE(j) contains the distance
C   (in whatever "physical" units you have expressed distances in)
C   from the "origin" to the jth site, and the coordinates (I,J,K)
C   the jth site (in units of Primitive Lattice Vectors)
C   are packed (for economy) into the fullword integer variable
C   INDEXS(j) = (I+20) + 100*(J+20) + 10000*(K+20) + 1000000*JPB.
C   (Use the Subroutine "DCINDX" to decode these if you like.)
C   Note that I am assuming that no one in his right mind will
C   ever look at lattice sites more than 19 sites away from
C   the origin using this program.  This seems a safe bet.
C   Here JPB .LE. NAPB is the number of the Primitive Basis atom
C   for the current site.  If you want to get the (x,y,z)
C   coordinates of that site, use the "SITE" subroutine.
C   Now, here's the good part:
C
C   INDEXS and RSITE are   O R D E R E D   in order of INcreasing
C   RSITE!  Thus the nearest neighbors are the first in the list,
C   and so on, regardless of their "coordinates" or identity.
C
C   In addition, the number of distinct shells found (including
C   the "zero" shell if there is an atom at the origin) is returned
C   in NSHELL, and the number of sites in the jth shell is returned
C   in NPERSH(j), which should be dimensioned 50 or so for safety.
C   In RSHELL(j) is the common value of RSITE for all in Shell j.
C   This should save you some accounting.
C
C   Please note that ALL the possible sites contributing to
C   the last one or two shells (most distant) are unlikely to be
C   included.  If you really want a COMPLETE list of that shell,
C   make sure NSITES is big enough to get several shells beyond it.
C   This can of course get expensive.  As a general rule of thumb,
C   including all of the nth shell (counting the origin as shell 1)
C   will cost you about (2n+1)**3 calculations and arrays should
C   be big enough to hold NSITES = (2n+1)**3 sites.
C
C
	SUBROUTINE NEIGHB (NAPB, PLV, RPB, NSITES,
     >		INDEXS, RSITE,  NSHELL, NPERSH, RSHELL)
C
	REAL*4 PLV(3,3), RPB(4,20)
	REAL*4 RSITE(NSITES), RSHELL(50)
	INTEGER INDEXS(NSITES), NPERSH(50)
	REAL*4 RABS(3)
C====================================================================
	ISITE = 0
C
	IJK = (FLOAT(NSITES+1)/FLOAT(NAPB))**(1.0/3.0) + 0.5
	IJKBY2 = 0.5*FLOAT(IJK + 1)
C
	DO 1000 I1=1,IJK
	I = I1 - IJKBY2
C
	DO 1000 I2=1,IJK
	J = I2 - IJKBY2
C
	DO 1000 I3=1,IJK
	K = I3 - IJKBY2
C
	J1 = I + 20
	J2 = J + 20
	J3 = K + 20
C
	DO 1000 JPB=1,NAPB
C
	ISITE = ISITE + 1
	IF (ISITE .GT. NSITES) GO TO 5000
C
	CALL SITE (I, J, K, JPB, PLV, RPB, RABS)
C
	RSITE(ISITE) = SQRT(RABS(1)**2 + RABS(2)**2 + RABS(3)**2)
 1000	INDEXS(ISITE) = J1 + 100*J2 + 10000*J3 + 1000000*JPB
C
 5000	NSITES = MIN0(ISITE, NSITES)
C
	CALL ORDER (1,2,NSITES,RSITE,INDEXS,INDEXS,INDEXS)
C
	NSHELL = 0
	RADIUS = RSITE(1)
	ISITE = 0
	NTHIS = 0
C
 6000	ISITE = ISITE + 1
	IF (ISITE .GT. NSITES) GO TO 7000
	IF (RADIUS .GT. 0.0) GO TO 6100
	IF (RSITE(ISITE) .GT. 0.0) GO TO 7000
	GO TO 6200
 6100	FDIFF = (RSITE(ISITE) - RADIUS)/RADIUS
	IF (FDIFF .GT. 0.00001) GO TO 7000
 6200	NTHIS = NTHIS + 1
	GO TO 6000
 7000	NSHELL = NSHELL + 1
	NPERSH(NSHELL) = NTHIS
	RSHELL(NSHELL) = RADIUS
	NTHIS = 1
	RADIUS = RSITE(ISITE)
C
	IF (ISITE .GT. NSITES) RETURN
	IF (NSHELL .GE. 50) RETURN
C
	GO TO 6000
C
	END
	SUBROUTINE SITE (I1, I2, I3, JPB, PLV, RPB, RABS)
C
C...Calculate the (x,y,z) coordinates RABS(i) of a lattice site
C   at the JPBth atom of the Primitive Basis associated with
C   a position (I1,I2,I3) Primitive Lattice Vectors from the
C   "origin."  The Primitive Lattice Vectors are provided in
C   PLV and the relative positions of the Primitive Basis atoms
C   (in terms of primitive lattice vectors) are provided in RPB.
C   That is, the kth primitive lattice vector has (x,y,z) components
C   PLV(i,k) and the jth atom of the primitive basis is located
C   RPB(k,j) primitive-lattice-vectors from the lattice site.
C
	REAL*4 PLV(3,3), RPB(4,20), RABS(3)
C
	DO 100 K=1,3
	RABS(K) = FLOAT(I1)*PLV(K,1)
     >	  + FLOAT(I2)*PLV(K,2)
     >	  + FLOAT(I3)*PLV(K,3)
  100	CONTINUE
C
	DO 200 K=1,3
	DO 200 I=1,3
  200	RABS(K) = RABS(K) + RPB(I,JPB)*PLV(K,I)
C
	RETURN
	END
	SUBROUTINE DCINDX (INDEX, I, J, K, L)
C
C...Unpack I, J, K and L from INDEX coded as follows:
C   INDEX = (I+20) + 100*(J+20) + 10000*(K+20) + 1000000*L.
C
	M = INDEX
	L = M/1000000
	M = M - L*1000000
	K = M/10000
	M = M - K*10000
	K = K - 20
	J = M/100
	I = M - J*100
	J = J - 20
	I = I - 20
C
	RETURN
	END
