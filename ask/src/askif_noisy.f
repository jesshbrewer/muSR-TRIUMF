C...ASK_IF...    
C     
C...Short Logical Function to get .TRUE. or .FALSE. result as 
C   a Y or N answer to the Character QUESTION.  Read via ASK.
C     
	FUNCTION ASK_IF (QUESTION) 
C     
	LOGICAL ASK_IF   
	CHARACTER QUESTION*(*)
	CHARACTER*1 ANSWER(4)	! Give ASK plenty of room.
C-------------------------------------------------------------<     
	ASK_IF = .FALSE. 
 1000	CALL ASK1 (QUESTION, '4S',ANSWER)
C     
	IF (ANSWER(1) .EQ. 'Y') GO TO 2000   
	IF (ANSWER(1) .EQ. 'y') GO TO 2000   
C
	IF (ANSWER(1) .EQ. ' ') RETURN 	! Blank Line interpreted as "No."   
	IF (ANSWER(1) .EQ. 'N') RETURN  
	IF (ANSWER(1) .EQ. 'n') RETURN  
C     
	WRITE (*,*) 'Please answer Y or N.'
C	WRITE (*,2222)     
C 2222	FORMAT ('$Please answer Y or N: ')	
	GO TO 1000	
C     
 2000	ASK_IF = .TRUE.  
	RETURN    
C     
	END   
c================================================================
C...ASK_IFD...    
C     
C...Short Logical Function to get .TRUE. or .FALSE. result as 
C   a Y or N answer to the Character QUESTION.  Read via ASK.
c   Defaults to value IF_DEF.
C     
	FUNCTION ASK_IFD (QUESTION, IF_DEF) 
C     
	IMPLICIT NONE
	LOGICAL ASK_IFD, IF_DEF
	CHARACTER QUESTION*(*)
	CHARACTER ANSWER*(4)	! Give ASK plenty of room.
cc	CHARACTER*1 ANSWER(4)	! Give ASK plenty of room.
C-------------------------------------------------------------<     

 1000   CONTINUE
	IF (IF_DEF) THEN
	  ANSWER = 'Y   '
	ELSE
	  ANSWER = 'N   '
	ENDIF

 	CALL ASK1D (QUESTION, '4A',ANSWER)
     
	IF ((ANSWER(1:1) .EQ. 'Y') .OR. (ANSWER(1:1) .EQ. 'y')) THEN
	  ASK_IFD = .TRUE.
	  IF_DEF = .TRUE.
	ELSE IF ((ANSWER(1:1) .EQ. 'N') .OR. (ANSWER(1:1) .EQ. 'n')) THEN 
	  ASK_IFD = .FALSE.
	  IF_DEF = .FALSE.
	ELSE IF (ANSWER(1:1) .EQ. ' ') THEN 
	  ASK_IFD = IF_DEF
	ELSE
	  WRITE (*,*) 'Please answer Y or N.'
c	  WRITE (*,2222)     
c 2222	  FORMAT ('$Please answer Y or N: ')	
	  GO TO 1000	
	ENDIF

	RETURN    
	END   
