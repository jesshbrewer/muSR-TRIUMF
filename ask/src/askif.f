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
	WRITE (*,2222)     
 2222	FORMAT ('$Please answer Y or N: ')	
	GO TO 1000	
C     
 2000	ASK_IF = .TRUE.  
	RETURN    
C     
	END   
