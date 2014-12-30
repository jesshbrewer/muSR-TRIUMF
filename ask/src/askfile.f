	SUBROUTINE ASKFILE (PROMPT, LUN, CHA, IERR)	! (IERR is now unused.)

C...Routine to ASsigN "LUN" to a disk FILe specified by caller via TI:

	IMPLICIT NONE
C
	CHARACTER PROMPT*(*)
	INTEGER LUN, IERR
	CHARACTER*1 CHA

	CHARACTER NAME*132
	INTEGER NCH
C-----------------------------------------------------
C...Ask Operator for File Name:
C
	CALL ASK1S(PROMPT, '132S', NAME, NCH)
	IERR = 1
	IF (NCH .EQ. 0) RETURN

	CLOSE (LUN)
	IF (CHA .EQ. 'R') 
     >		OPEN (LUN,FILE=NAME,STATUS='OLD',ERR=200)
	IF (CHA .EQ. 'U') 
     >		OPEN (LUN,FILE=NAME,STATUS='UNKNOWN',ERR=200)

	IERR = 0
	RETURN

 200	CALL TELL1('Could not open file ', '132S', NAME)
	IERR = -1
	RETURN
	END
