      subroutine TRIM_BLNK (N, STR)
c============================================================================
c...Modified 17 Feb 2001 by Donald Arseneau to use N "intent OUT",
c   and so STR must really be a character variable (use LEN intrinsic).
c...Modified 06 May 1998 by Jess Brewer to also strip off trailing zeroes...
c============================================================================
      implicit none
      intrinsic LEN
      integer*4 N
      character*(*) STR
      character*1 cz
      integer*1 bz
      equivalence (cz,bz)
      data bz /0/
c============================================================================

      N = LEN(STR)
      do while (N .ge. 1 .and. 
     >     ((STR(N:N) .eq. ' ' .or. STR(N:N) .eq. cz)) )
         N = N - 1
      end do
      end

c======================================================================
      subroutine CHTRIM_BLNK (N, STR)
c======================================================================
C       Trims off trailing blanks and zeros off character string.
c       Based on TRIM_BLNK. Replaces zeros with blanks.
c======================================================================
c...Modified 17 Feb 2001 by Donald Arseneau to use N "intent OUT"
c...Modified 06 May 1998 by Jess Brewer to also strip off trailing zero
c======================================================================
      implicit none
      intrinsic LEN
      integer*4 N
      character STR*(*)
      character*1 cz
      integer*1 bz
      equivalence (cz,bz)
      data bz /0/
c======================================================================

      N = LEN(STR)
      do while (N .ge. 1 .and. 
     &		(STR(N:N) .eq. ' ' .or. STR(N:N) .eq. cz)) 
         if (STR(N:N) .eq. cz) STR(N:N) = ' '
         N = N - 1
      end do
      end



c======================================================================
        integer function trimstart(thestring)
c======================================================================
c Return the position of the first non-blank character of the string,
c where blanks are trailing spaces ' ' and zeros (from uninitialized strings).
c======================================================================

        implicit none
        intrinsic       len
        character*(*)   thestring

        integer         lens, n

        lens = len(thestring)
        n = 1
        do while ((n .lt. lens) .and.  ((thestring(n:n) .eq. ' ') .or. 
     &		(ichar(thestring(n:n)) .eq. 0) )) 
	  n = n + 1
        enddo

	trimstart = n

        return
        end

c======================================================================
        integer function trimstop(thestring)
c======================================================================
c       Return the length of the string, ignoring trailing spaces ' '
c       and trailing zeros (from uninitialized strings).
c======================================================================

        implicit none
        character*(*) thestring

        integer         n

        n = len(thestring)
        do while ((n .ge. 1) .and.  ((thestring(n:n) .eq. ' ') .or. 
     &		(ichar(thestring(n:n)) .eq. 0) )) 
	  n = n - 1
        enddo

	trimstop = n

        return
        end

c======================================================================
        integer function chtrim(thestring)
c======================================================================
c       Return the length of the string, ignoring trailing spaces ' '
c       and trailing zeros (from uninitialized strings), just like 
c       trimstop, but do not return 0 for blank strings; return 1 
c       instead.  Why?  Because fortran doesn't have zero-length 
c       strings.  This facilitates s(1:chtrim(s)).
c======================================================================

        implicit none
        character*(*) thestring

        integer         n

        n = len(thestring)
        do while ((n .gt. 1) .and.  ((thestring(n:n) .eq. ' ') .or. 
     &		(ichar(thestring(n:n)) .eq. 0) )) 
	  n = n - 1
        enddo

	chtrim = n

        return
        end

c======================================================================
      integer function nospaces(cio)
c======================================================================
C     Routine to remove spaces from string cio (cio is modified).
C     Return the compressed length (return 1 if all spaces)
c======================================================================

      character*(*) cio  !  INTENT:  IN OUT

      character*1 c
      integer L,j,k

      k=1
      L=len(cio)
      do j = 1,L
         c=cio(j:j)
	 if (c.eq.char(0)) c = ' '
         if (c.ne.' ') then
            cio(k:k)=c
            k=k+1
         endif
      enddo
      if (k.le.L) cio(k:)=' '
      nospaces=max(k-1,1)
      return
      end
