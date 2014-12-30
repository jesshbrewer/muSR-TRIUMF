C...DO_PROMPT...
C
C...Output a Prompt with cognizance of intended carriage control and 
C   wait for further input on the same line:
C
	SUBROUTINE DO_PROMPT (PROMPT)
C
	CHARACTER PROMPT*(*)
C-----------------------------------------------
	IF (PROMPT(1:1) .EQ. '+') GO TO 2000
	WRITE (*,1111) PROMPT
 1111	FORMAT ('$',A)
	RETURN
 2000	WRITE (*,2222) PROMPT
 2222	FORMAT (A,$)
	RETURN
	END
	END
