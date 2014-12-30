C============================================================================
	SUBROUTINE SETVALS (VALUE, VAR, ELEMENT)
C============================================================================
C	Loads in a single character into a character array.
	CHARACTER*1 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALS ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE TRIMZERO_VALS (VALUE, VAR, ELEMENT)
C============================================================================
C	Replaces zero with VALUE in an element of a character array.
	CHARACTER*1 VALUE, VAR(*)
	INTEGER ELEMENT

c	write (*,*) 'TRIMZERO_VALS: ', element, ICHAR(VAR(ELEMENT))
	IF (ICHAR(VAR(ELEMENT)) .EQ. 0) THEN
	  VAR(ELEMENT) = VALUE
	ENDIF

	RETURN
	END


C============================================================================
	SUBROUTINE SETVALA (VALUE, VAR, ELEMENT)
C============================================================================
C	Loads in a characters into a character string.
	CHARACTER*1 VALUE
	CHARACTER VAR*(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALA ', VALUE
	VAR(ELEMENT:ELEMENT) = VALUE
	RETURN
	END


C============================================================================
	SUBROUTINE TRIMZERO_VALA (VALUE, VAR, ELEMENT)
C============================================================================
C	Replaces zero with VALUE in an element of a character string.
	CHARACTER*1 VALUE
	CHARACTER VAR*(*)
	INTEGER ELEMENT

c	write (*,*) 'TRIMZERO_VALA: ', element, 
c     &		ICHAR(VAR(ELEMENT:ELEMENT))
	IF (ICHAR(VAR(ELEMENT:ELEMENT)) .EQ. 0) THEN
	  VAR(ELEMENT:ELEMENT) = VALUE
	ENDIF
	RETURN
	END


C============================================================================
	SUBROUTINE SETVALK (VALUE, VAR, ELEMENT)
C============================================================================
	LOGICAL*2 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALK ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALM (VALUE, VAR, ELEMENT)
C============================================================================
	INTEGER*2 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALM ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALL (VALUE, VAR, ELEMENT)
C============================================================================
	LOGICAL*4 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALL ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALI (VALUE, VAR, ELEMENT)
C============================================================================
	INTEGER*4 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALI ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALR (VALUE, VAR, ELEMENT)
C============================================================================
	REAL*4 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALR ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALD (VALUE, VAR, ELEMENT)
C============================================================================
	REAL*8 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALD ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE SETVALC (VALUE, VAR, ELEMENT)
C============================================================================
	COMPLEX*8 VALUE, VAR(*)
	INTEGER ELEMENT

C	WRITE(*,*) 'SETVALC ', VALUE
	VAR(ELEMENT) = VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALS (VALUE, VAR, ELEMENT)
C============================================================================
	CHARACTER*1 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALS ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALA (VALUE, VAR, ELEMENT)
C============================================================================
	CHARACTER*1 VALUE
	CHARACTER VAR*(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT:ELEMENT)
C	WRITE(*,*) 'GETVALS ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALK (VALUE, VAR, ELEMENT)
C============================================================================
	LOGICAL*2 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALK ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALM (VALUE, VAR, ELEMENT)
C============================================================================
	INTEGER*2 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALM ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALL (VALUE, VAR, ELEMENT)
C============================================================================
	LOGICAL*4 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALL ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALI (VALUE, VAR, ELEMENT)
C============================================================================
	INTEGER*4 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALI ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALR (VALUE, VAR, ELEMENT)
C============================================================================
	REAL*4 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALR ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALD (VALUE, VAR, ELEMENT)
C============================================================================
	REAL*8 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALD ', VALUE
	RETURN
	END

C============================================================================
	SUBROUTINE GETVALC (VALUE, VAR, ELEMENT)
C============================================================================
	COMPLEX*8 VALUE, VAR(*)
	INTEGER ELEMENT

	VALUE = VAR(ELEMENT)
C	WRITE(*,*) 'GETVALC ', VALUE
	RETURN
	END

