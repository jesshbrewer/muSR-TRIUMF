C============================================================================
	SUBROUTINE SETVALS (VALUE, VAR, ELEMENT)
C============================================================================
	CHARACTER*1 VALUE, VAR(*)
	INTEGER ELEMENT

c	WRITE(*,*) 'SETVALS ', VALUE
	VAR(ELEMENT) = VALUE
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

