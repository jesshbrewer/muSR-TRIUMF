C...STDLAT
C
C...Look up lattice with 8-char name "R8NAME" (RETURN 1 on failure),
C   use A,B,C as lattice constants (B is now redundant, but...),
C   and generate the 3 primitive lattice vectors PLV in terms of
C   cartesian (x,y,z).  Supply number of atoms in the primitive
C   basis, NAPB < 6, and the location of each in terms of the
C   primitive lattice vectors (NOT x,y,z): RPB.  The fourth
C   "component" of RPB is an "attribute" for that atom of the basis;
C   e.g., the charge of the ion.
C
	SUBROUTINE STDLAT (R8NAME, A, B, C, PLV, NAPB, RPB, *)
C
	REAL*4 A, B, C, PLV(3,3), RPB(4,20)
	REAL*4 STDPLV(3,3,9), STDRPB(4,4,9)
	character*8 R8NAME, STDNAM(2,9)
	INTEGER NPBSTD(9)
C
	DATA NTYPE /9/
C
	DATA STDNAM /'sc      ','SC      ',
     >  'fcc     ','FCC     ',
     >  'bcc     ','BCC     ',
     >  'hcp     ','HCP     ',
     >  'diamond ','DIAMOND ',
     >  'NaCl    ','NACL    ',
     >  'CsCl    ','CSCL    ',
     >  'ZnS     ','ZNS     ',
     >  'ortho   ','ORTHO   '/
C
	DATA STDPLV /1.,0.,0.,  0.,1.,0.,  0.,0.,1.,
     >  .5,.5,0.,  0.,.5,.5,  .5,0.,.5,
     >  .5,.5,-.5, -.5,.5,.5, .5,-.5,.5,
     >  .8660254,.5,0., -.8660254,.5,0.,  0.,0.,1.,
     >  .5,.5,0.,  0.,.5,.5,  .5,0.,.5,
     >  .5,.5,0.,  0.,.5,.5,  .5,0.,.5,
     >  1.,0.,0.,  0.,1.,0.,  0.,0.,1.,
     >  .5,.5,0.,  0.,.5,.5,  .5,0.,.5,
     >  1.,0.,0.,  0.,1.,0.,  0.,0.,1. /
C
	DATA NPBSTD /1, 1, 1, 2, 2, 2, 2, 2, 1/
C
	DATA STDRPB /
     >	0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,
     >	0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,
     >	0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,
     >  0.,0.,0.,1., .666666666666666667,.33333333333333333,.5,1.,
     >	0.,0.,0.,1.,  0.,0.,0.,1.,
     >  0.,0.,0.,1., .25,.25,.25,1., 0.,0.,0.,1., 0.,0.,0.,1.,
     >  0.,0.,0.,1., .5,.5,.5,-1., 0.,0.,0.,1., 0.,0.,0.,1.,
     >  0.,0.,0.,1., .5,.5,.5,-1., 0.,0.,0.,1., 0.,0.,0.,1.,
     >  0.,0.,0.,1., .25,.25,.25,-1., 0.,0.,0.,1., 0.,0.,0.,1.,
     >	0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1.,  0.,0.,0.,1./
C========================================================================<
	DO 100 ITYP=1,NTYPE
	DO 100 J=1,2
	IF (R8NAME .EQ. STDNAM(J,ITYP)) GO TO 1000
  100	CONTINUE
C
	RETURN 1
C
 1000	NAPB = NPBSTD(ITYP)
C
	AA = A
	BB = A
	CC = A
	IF (ITYP .EQ. 4) CC = A*SQRT(8.0/3.0)
	IF (ITYP .EQ. 9) BB = B
	IF (ITYP .EQ. 9) CC = C
C
	DO 300 JLV=1,3
	PLV(1,JLV) = AA*STDPLV(1,JLV,ITYP)
	PLV(2,JLV) = BB*STDPLV(2,JLV,ITYP)
  300	PLV(3,JLV) = CC*STDPLV(3,JLV,ITYP)
C
	DO 400 J=1,4
	DO 400 K=1,4
  400	RPB(K,J) = STDRPB(K,J,ITYP)
C
	RETURN
	END
