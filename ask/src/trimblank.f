      subroutine TRIM_BLNK (N, STR)
c============================================================================
      implicit none
      integer*4 N
      character*1 STR(*)
      character*1 cz
      byte bz
      equivalence (cz,bz)
      data bz /0/
c============================================================================
c...Modified 06 May 1998 by Jess Brewer to also strip off trailing zeroes...

      do while (N .ge. 1 .and. (STR(N) .eq. ' ' .or. STR(N) .eq. cz))
         N = N - 1
      end do
      end
