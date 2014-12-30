C...WORDS...
C
C...A Word-counting program for Text files.
C 
C	  Under development, December 1986 -- Jess H. Brewer 
C		Revised Jan 94 -- JHB
C----------------------------------------------------------------------
	LOGICAL ASK_IF, EXCL_CTL
	character*512 cLINE
	character*1 LINE(512)
	equivalence(line,cline)
	character*128 NAME
	character*1 LAST, CR, CTL(10)
	integer*1 lcr
	equivalence(cr,lcr)
C 
        DATA LFREAD /1/
cccc        DATA lcr /Z0A/
        data lcr /10/
C=======================================================================
	WRITE (*,1111)
 1111	FORMAT (//,10X,'<<< W O R D S >>>',//,
     >		' Count the number of words in a text file:')
C-----------------------------------------------------------------------
C...Ask Operator for File Name:
C
 1234	CALL ASK1s (' Enter filename > ', '128A',NAME, NCH)
C
	CLOSE (LFREAD)
C
cccc	OPEN (LFREAD,FILE=NAME,STATUS='OLD',SHARED,ERR=1234)
	OPEN (LFREAD,FILE=NAME,STATUS='OLD',ERR=1234)
C-----------------------------------------------------------------------
	EXCL_CTL = ASK_IF ('Exclude Control words etc.? ') 
	IF (EXCL_CTL) THEN 
		CALL ASK1s 
     > ('Enter up to 10 control characters, e.g., ''{\%&#}'' : ', 
     >		'10S',CTL, NCTL)
		EXCL_CTL = (NCTL .GT. 0)
	END IF
C
	NWDS = 0
 1000	READ (LFREAD,2222,END=9000) cLINE
 2222	FORMAT (A)
	NCH = LEN(cLINE)
	IF (NCH .EQ. 0) GO TO 1000
C
	JCH = 0
	LAST = ' '
C
C...Start/Loop processing a line:
C
 2000	JCH = JCH + 1
	IF (JCH .GT. NCH) GO TO 1000
CCCC	IF (LINE(JCH) .EQ. CR) GO TO 2000
	IF (LAST .NE. ' ') GO TO 3000		! Was last character a blank? 
	IF (LINE(JCH) .EQ. ' ') GO TO 3000	! Is this character NOT a blank?
	IF (EXCL_CTL) THEN			! Yes.  Control character?
		DO I=1,NCTL
			IF (LINE(JCH) .EQ. CTL(I)) GO TO 3000
		END DO
	END IF					! No.  Looks like a legit Word!
	NWDS = NWDS + 1
C-------------------------------------------------------------------------------
 3000	LAST = LINE(JCH)
	GO TO 2000
C-------------------------------------------------------------------------------
 9000	WRITE (*,9999) NWDS
 9999	FORMAT (/,' Your file contains',I6,'  non-control words.',/)
	CALL EXIT
	END
