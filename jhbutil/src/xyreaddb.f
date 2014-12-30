	SUBROUTINE XYREAD_DB 
C===============================================================================
C..."*.DB" File Data Readin subroutine serving XYFIT via XYREAD:
C								-- Jess Brewer
C		Latest revision:	July 1992
C-------------------------------------------------------------------------------
	include 'xy_data.cmn'
c
	COMMON /FILEIO/ LFREAD, LFPRNT, ITTY, LFDAT, LFFIT
	COMMON /TEXTS/ TITLE, COMENT, NXL, XLABEL, NYL, YLABEL
	LOGICAL*1 CHAR, TITLE(80), COMENT(80), XLABEL(40), YLABEL(40)
	real*4 CMNT4
	equivalence (CMNT4,COMENT(1))
C===============================================================================
	COMMON /DB_DATA/ MAX_DB, NDIM, NAME(2,20), NDATA !!, DB(27000)
	COMMON /DB_TEXT/ TITL(20), COMMENT(20,10), ABSTRACT(20,100), 
     >			NCHLAB(20), LABEL(20,20), REMARK(10,1000)
	REAL*8 DAME(20), WD(4,19), VALUE, TMP8
	EQUIVALENCE (DAME(1),NAME(1,1))
	REAL*8 DAME_PR(20)
	INTEGER*4 IVPR(20), LVPR(20)
	LOGICAL*1 LTITL(80), LCMNT(80,10), LABST(80,100)
	LOGICAL*1 LLAB(80,20), LRMRK(40,1000), LWD(32,19)
	EQUIVALENCE (TITL,LTITL), (COMMENT,LCMNT), (ABSTRACT,LABST)
	EQUIVALENCE (LABEL,LLAB), (REMARK,LRMRK)
C
	REAL*4 DB(3,20)
C
	CHARACTER LINE*132, DUM*80
	LOGICAL*1 SLINE(132), SDUM(80)
	EQUIVALENCE (LINE,SLINE), (DUM,SDUM)
	LOGICAL ASK_IF, LOC_EVAL, TESTIF
	integer*4 LLL
	character*4 cll
	equivalence (LLL,cll)
	character*8 CMD(6), cblnk8
	real*8 blnk8
	equivalence (blnk8, cblnk8)
	character*4 cblnk4
	real*4 blnk4
	equivalence (blnk4, cblnk4)
C
	DATA NCMD /6/
	DATA CMD /'TITLE   ', 'ABSTRACT', 'COMMENTS', 'LABELS  ',
     >		  'DATA    ', '        '/
	DATA LS /'32S '/, MAXDAT/1000/
	data cblnk8 /'        '/, cblnk4 /'    '/
C====================================================================<
	WRITE (*,1111)
 1111	FORMAT (' XYREAD_DB: Retrieve Y(X) data from',
     >		' standard format "*.DB" file.')
	DAME(10) = blnk8
	NDIM = 0
	NDAT0 = NDAT
	NDATDB = 0
	NTYP = IFTYP
C-----------------------------------------------------------------------------
 1234	CALL CMLI_HELP ('XYFIT.HLB ', 'XYREADB ')
	CALL CMLI19 ('XYREAD_DB>', NCMD, 8, CMD, JCMD, 
     > LS,WD(1,1), LS,WD(1,2), LS,WD(1,3), LS,WD(1,4), LS,WD(1,5), 
     > LS,WD(1,6), LS,WD(1,7), LS,WD(1,8), LS,WD(1,9), LS,WD(1,10), 
     > LS,WD(1,11), LS,WD(1,12), LS,WD(1,13), LS,WD(1,14), LS,WD(1,15), 
     > LS,WD(1,16), LS,WD(1,17), LS,WD(1,18), LS,WD(1,19))
C
	GO TO  (1100,1200,1300,1400,1600,1234), JCMD
C
C		TITL ABST COMM LABE DATA blank
C-----------------------------------------------------------------------------
C...TITLE: 
C
 1100	CALL ASK1s ('>', '80S',TITL, NTTL)
	CALL TRIM_BLNK (NTTL,TITL)
	CALL MOVEC (NTTL,TITL,TITLE)
	CALL TITLPL (NTTL, TITL)
	GO TO 1234
C-----------------------------------------------------------------------------
C...ABSTRACT: 
C
 1200	CALL ASK1s ('>', '80S',SLINE, NCH)
	CALL TRIM_BLNK (NCH, SLINE)
	IF (NCH .EQ. 0) GO TO 1234
	GO TO 1200
C-----------------------------------------------------------------------------
C...COMMENT: 
C
 1300	CALL ASK1s ('>', '80S',SLINE, NCH)
	CALL TRIM_BLNK (NCH, SLINE)
	IF (NCH .EQ. 0) GO TO 1234
	IF (CMNT4 .EQ. blnk4) THEN
		CALL SETC (80,COMENT,' ')
		CALL MOVEC (NCH,SLINE,COMENT)
	END IF
	GO TO 1300
C-----------------------------------------------------------------------------
C...LABELS:
C
 1400	NLAB = 0
 1406	NLAB = NLAB + 1
 1407	CALL ASK1s ('>', '80S',LABEL(1,NLAB), NCHLAB(NLAB))
	CALL TRIM_BLNK (NCHLAB(NLAB),LABEL(1,NLAB))
	IF (NCHLAB(NLAB) .EQ. 0) GO TO 1234
	IF (NLAB .GE. 19) GO TO 1234
	GO TO 1406
C-----------------------------------------------------------------------------
C...DATA: 
C
 1600	NDIM = 0
	DO 1601 I=1,19
	IF (WD(1,I) .NE. blnk8) NDIM = NDIM + 1
 1601	DAME(I) = WD(1,I)
	ND3 = 3*NDIM
cccc	ENCODE (4,1611,LLL) ND3
	write (cll,fmt=1611) ND3
 1611	FORMAT (I3,'R')
C------------------------------------------------------------------------------
C...Decide which variables are "X" and "Y": 
C
	WRITE (*,1622) (J,DAME(J),(LABEL(I,J),I=1,10),J=1,NDIM)
 1622	FORMAT (1X,I2,2X,A8,' = ',10A4)
 1620	WRITE (*,1623) 
 1623	FORMAT (' Variable # for X > ',$)
	READ (*,1624) JVARX
	IF (JVARX .LE. 0  .OR.  JVARX .GT. NDIM) GO TO 1620
 1624	FORMAT (I2)
 1630	WRITE (*,1625) 
 1625	FORMAT (' Variable # for Y > ',$)
	READ (*,1624) JVARY
	IF (JVARY .LE. 0  .OR.  JVARY .GT. NDIM) GO TO 1630
	WRITE (*,1626) 
 1626	FORMAT (' Point Type > ',$)
	READ (*,1624) KODE
C------------------------------------------------------------------------------
	NLX = NCHLAB(JVARX)
	NLY = NCHLAB(JVARY)
	CALL SETC (40,XLABEL,' ')
	CALL SETC (40,YLABEL,' ')
	CALL MOVEC (NXL,LABEL(1,JVARX),XLABEL)
	CALL MOVEC (NYL,LABEL(1,JVARY),YLABEL)
	CALL HLAB (NLX, LABEL(1,JVARX))
	CALL VLAB (NLY, LABEL(1,JVARY))
C--------------------------------------------------------------------------
C...Now read into data arrays, one line at a time:
C
 1800	CALL ASK1S ('>', cll,DB, NCH)
	IF (NCH .EQ. 0) GO TO 2345
	IF (NDAT .GE. MAXDAT) GO TO 2345
	NDATDB = NDATDB + 1
	NDAT = NDAT + 1
	VI(NDAT) = DB(1,JVARX)
	IF (DB(3,JVARX) .EQ. 0.0) DB(3,JVARX) = DB(2,JVARX)
	DVI(NDAT) = 0.5*(DB(2,JVARX) + DB(3,JVARX))
	VD(NDAT) = DB(1,JVARY)
	IF (DB(3,JVARY) .EQ. 0.0) DB(3,JVARY) = DB(2,JVARY)
	DVD(NDAT) = 0.5*(DB(2,JVARY) + DB(3,JVARY))
	ITYP(NDAT) = KODE
	GO TO 1800
C-----------------------------------------------------------------------------
 2345	WRITE (*,5341) NDATDB, KODE, NDAT
 5341	FORMAT (1X,I4,' data points of type',I4,4X,' Total now',I6)
C
	NTYP = NTYP + 1
	IFTYP = NTYP
CCC	IFTYP = 0
CCC	IF (NTYP .GT. 1) IFTYP = NTYP
C
	DO 55 I=1,NDATDB
	IF (DVI(NDAT0+I) .NE. 0.0) GO TO 551
   55	CONTINUE
	RETURN
  551	IF (ASK_IF('Retain Error bars on Independent variable? '))
     >			RETURN
	DO 59 I=1,NDATDB
   59	DVI(NDAT0+I) = 0.0
C-----------------------------------------------------------------------------
	RETURN
	END
