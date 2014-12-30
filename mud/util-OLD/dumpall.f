      implicit none

      include 'fileio.cmn'
      include 'runhdr.cmn'
      include 'scalers.cmn'
      include 'histhdrs.cmn'
      include 'hists.cmn'

      integer*4 IO, getRun
      integer*4 i, j, k, l, n

      integer*4 JJ1, JJ2

      character bigst*256, lilst*16
      character*1 stlil(16)
      equivalence (stlil(1), lilst)

      intrinsic len_trim, iargc, getarg
      logical loc_eval
      real*8 value
      integer*4 len_trim0

        integer*4 year
        character*24 BeginTime, EndTime

C-------------------------------------------------------------

      k = IArgC() ! number of command arguments
      if (k .lt. 1) stop

cc      call GETINPUT( 'MUD file: ', bigst )
      call getarg(1,bigst)
      call TRUNC_BLNK( bigst )
      n = len_trim0( bigst )

c      write( *, 1101 ) bigst(1:n)
c 1101 format( /,' Opening MUD file ',A,':' )

      IO = getRun( bigst(1:n) )

c      write (*,1111) IO
c 1111 format (/,' getRun returns',I10,/)

c      write (*,2221) nch_title
c 2221 format( /,' Title is',I4,' characters long.',/)

      call ctime( EndTime, timeEnd )
      call ctime( BeginTime, timeBegin )
      read (EndTime(21:24), '(I4)') year

      JJ1 = 0
      JJ2 = 0
      if (k .gt. 1) call getarg(2,bigst)
      call TRUNC_BLNK( bigst )
      n = len_trim0( bigst )
      if (loc_eval(bigst(1:n), value)) JJ1 = value
      if (k .gt. 2) then
        call getarg(3,bigst)
        call TRUNC_BLNK( bigst )
        n = len_trim0( bigst )
        if (loc_eval(bigst(1:n), value)) JJ2 = value
      else if (JJ1 .gt. 0) then
        JJ2 = JJ1 + 100
      end if

      write (*,2222) runNumber, 
     >     area(1:nch_area), 
     >     lab(1:nch_lab), 
     >     year,
     >     exptNumber, 
     >     experimenter(1:nch_experimenter), 
     >     title(1:nch_title), 
     >     nHists, 
     >     sample(1:nch_sample), 
     >     orient(1:nch_orient), 
     >     temperature(1:nch_temperature), 
     >     field(1:nch_field), 
     >     elapsedSec, BeginTime, EndTime, 
     >     method(1:nch_method), 
     >     das(1:nch_das), 
     >     apparatus(1:nch_apparatus), 
     >     insert(1:nch_insert), 
     >     nSclrs
 2222 format( /,
     >  ' Run ',I5,'  from ',A,' at ',A,' in ',I4,
     >  ' under Expt',I4,' by ',A,':',//,
     >  ' "',A,'"  [',I2,' H]',//,
     >  ' sample "',A,'",',
     >  ' orientation "',A,'",',
     >  ' temperature "',A,'",',
     >  ' field "',A,'"',/,
     >  I8,' sec elapsed = ',A,' to ',A,/,
     >  ' method "',A,'"',
     >  '	 das "',A,'"',/,
     >  ' apparatus "',A,'"',
     >  '	 mode "',A,'"',
     >  '	 nSclrs:',I3,/)


      do i=1,nSclrs
         lilst = sclrLabel(i)
         n = nch_sclrLabel(i)
         write (*,3333) sclrNum(i), 
     >        lastCount(i), totalCount(i), 
     >        lilst(1:n)
 3333    format ('    Scaler',I3,': ',2I10,2X,'"',A,'"')
      end do

      do i=1,nHists
         j = 10*(i-1)+1
         k = j + nch_Httl(i) - 1
         write (*,4444) i, 
     > hTitle(j:k), 
     > nBins(i),         ! number of bins in this Histogram
     > fsPerBin(i),      ! femtoseconds per bin
     > bkgd1(i),         ! first bin of t<0 Background data
     > bkgd2(i),         ! last bin of t<0 Background data
     > t0_ps(i),         ! left edge of bin 1 to t=0 [picosec]
     > t0_bin(i),        ! bin number where t=0
     > goodBin1(i),      ! first bin of good t>0 data 
     > goodBin2(i),      ! last bin of good t>0 data 
     > nEvents(i)       ! total number of events in Histogram
 4444    format (/,' Histogram ',I2,': "',A,'", ',I6,' bins of ',
     >        I10,' fs/bin',/,
     >        ' Background bins: ',I4,' to ',I6,/,
     >        ' t=0 at',I10,' ps in bin',I6,/,
     >        ' Good bins: ',I4,' to ',I6,/,
     >        2X,I10,' total counts')

         if ( JJ1 .gt. 0 .and. JJ2 .gt. 0 ) then
            write (*,5550) JJ1, JJ2
 5550       format (' Bins',I5,' to',I5,':')
            write (*,5555) (iH(j,i),j=JJ1,JJ2)
 5555       format (1X,10I7)
         end if
      end do
c
      stop
      end

      subroutine TRUNC_BLNK( CHSTR )
      integer nch
      character*(*) CHSTR
      character ZERO
      logical*1 LZERO
      equivalence (ZERO,LZERO)
      data LZERO /0/

      nch = len( CHSTR )
      call TRIM_BLNK( nch, CHSTR )
      CHSTR = CHSTR(1:nch)
      CHSTR(nch+1:nch+1) = ZERO

      return
      end
