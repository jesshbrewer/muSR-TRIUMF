*     Functions to convert floating-point to chatacter strings with
*     the best use of space for significant digits.
*
*     realtoch_* : convert real (*4) single-precision to character string
*     dbltoch_* : convert real (*8) double-precision to character string
*
*     *toch_f : fixed-width output, right justified
*     *toch_v : variable-width output -- left justified; trailing 0 removed
*
*     These versions avoid internal write statements, so they can be
*     used in write statements themselves even with old compilers like
*     g77.  [Better (like Vax Fortran) and newer (F95) compilers allow
*     recursive IO anyway.]  Despite the complexity, this new dbltoch_f
*     runs twice as fast as the old, which used internal writes (g77
*     -O3).
*
*     Donald Arseneau
*
*=========================================================================
*
*     double precision real to character string, fixed width format

      character*32 function dbltoch_f (d,jwid,ndec)

*     Return a character string representation of floating point number "d"
*     within the width "jwid". Use the method that gives the most significant
*     digits in that width, giving preference to F format in case of a tie.
*     The sign gets a space even if "d" is positive.  Don't give more than
*     "ndec" digits after the decimal point (and no more than 16, even if
*     ndec is higher).
*
*     E format is forced when abs(d) .ge. 2E9.  When round-off causes a
*     "carry" across the whole number, you will get a mantissa of 10.
*     For example, 999999999.,9,9 gives 10.00E+8, not 1.000E+9.
*
*     There are options for using "E" or "e" for exponents, and for
*     always printing the exponent's sign.  Note that if the + sign
*     is omitted, you should use "e" because in numbers like 5.394E7
*     the "E" hides too well!  Use 5.394e7 or 5.394E+7 instead.  These
*     two styles are selected (at compilation) by setting the parameters
*     (named constants) signed_exp and exp_e.  If you change exp_e here,
*     make sure you change it in dbltoch_v too!
*
*     The extraction of digits is performed using integer variables.
*
*=========================================================================

      implicit none

      double precision  d, ad, afd
      integer  jwid,ndec

      intrinsic len,abs,min,max,log10,int,mod,char,ichar

      logical signed_exp
      parameter (signed_exp = .true.)
      character*1 exp_e
      parameter (exp_e = 'E')  ! see below in dbltoch_v

      integer iwid,imoved,ndp,lose,nd,i,j,ne,iip,ifp1,ifp2
      integer nbefore,nafter
      character*5 cexp
      character*32 buff
      integer ic0

      integer ip10(0:10) /1,10,100,1000,10000,100000,1000000,
     >        10000000,100000000,1000000000,2147483647/
*     The "astute reader" will notice that 2147483647 .ne. 10**10
*     but it the maximum integer, and will work for detecting 
*     ten-digit integer-parts.

      ic0=ichar('0')
      iwid = jwid
      if (iwid.gt.len(dbltoch_f) .or. iwid.le.2) iwid=len(dbltoch_f)

*     get dispacement of decimal point from x.xxxx ( = power of 10 in sci not)
      ad = abs(d)
      if (d .eq. 0.0d0) then
         imoved = 0
      else  ! Only take int() of positive numbers to round down.
         imoved = int(log10(ad)+1000.000001d0)-1000
*        fixup possible roundoff error if log10() is poor
         if (ad .lt. 10.0d0**imoved ) imoved = imoved - 1
         if (ad .ge. 10.0d0**(imoved+1) ) imoved = imoved + 1
      endif

*     With F format, waste max(0,-imoved) digits.
*     With E format, waste at least 2;
*                    plus 1 if abs(imoved) >= 10
*                    plus 1 if imoved < 0

*     lose: Number of digits wasted for E format:
      lose = 2
      if (abs(imoved).ge.500) goto 666
      if (abs(imoved).ge.100) lose = lose+1
      if (abs(imoved).ge.10) lose = lose+1
      if (signed_exp .or. imoved.lt.0) then
         lose=lose+1
      endif
*     ndp: Number of places after decimal when using F format:
      ndp = min(ndec,iwid-3-max(imoved,0))

      ne=0
      if (ndp.lt.0 .or. lose.lt.max(0,-imoved) .or. ad.ge.2.d9) then
*        Do E format;
*        cexp = string like "E-12"; ne = number of characters in cexp
*        Make ad be the mantissa
         ad = ad * 10.0d0**(-imoved)
         cexp = exp_e
         ne=1
         if (imoved.lt.0) then
            cexp(2:2)='-'
            ne=2
         elseif (signed_exp) then
            cexp(2:2)='+'
            ne=2
         endif
         i=abs(imoved)
         if (i.ge.100) then
            ne=ne+1
            cexp(ne:ne)=char(i/100+ic0)
            i=mod(i,100)
         endif
         if (i.ge.10) then
            ne=ne+1
            cexp(ne:ne)=char(i/10+ic0)
            i=mod(i,10)
         endif
         ne=ne+1
         cexp(ne:ne)=char(i+ic0)

         if (ne.ne.lose) goto 661

      endif

*     Do F format (or mantissa of E format)


*     Extract and hold integer part (as integer)

      iip=ad

 10   CONTINUE  ! For re-do when carry creates an extra digit


*     See how many digits in integer part (may use altered iip from loop-back)
      nbefore=1
      do j=1,9
         if (iip .ge. ip10(j)) nbefore=j+1
      enddo

*     Put 16 digits of fractional part into integers
      iip=ad
      afd = ad - iip
      ifp1 = afd*1.0d8
      afd = afd*1.0d8 - ifp1
      ifp2 = afd*1.0d8 + 0.5d0

*     Round off those integers to the required number of decimal places,
*     carrying digits up when necessary.

      nafter = min(iwid-nbefore-2-ne, ndec, 16)
      if (nafter.ge.8) ifp2 = ifp2 + ip10(16-nafter)/2
      if (ifp2.ge.100000000) then
         ifp1 = ifp1 + ifp2/100000000
         ifp2 = mod(ifp2,100000000)
      endif
      if (nafter.lt.8) ifp1 = ifp1 + ip10(8-nafter)/2
      if (ifp1.ge.100000000) then
         iip = iip + ifp1/100000000
         ifp1 = mod(ifp1,100000000)
      endif

*     If carry has created new leading digit (1) then repeat.
      if (iip .ge. ip10(nbefore)) goto 10

      nd = max( iwid-nbefore-2-nafter-ne, 0)
      buff = ' '
      nd=nd+1
      if (d.lt.0.0d0) buff(nd:nd) = '-'

*     Do the integer-part digits plus decimal point
      if (iip.eq.0) then
         nd=nd+2
         buff(nd-1:nd)='0.'
      else
         do j=nbefore-1,0,-1
            i = iip/ip10(j)
            iip = iip - i * ip10(j)
            nd = nd+1
            buff(nd:nd) = char(i+ic0)
         enddo
         nd=nd+1
         buff(nd:nd)='.'
      endif

*     Do fractional digits
      do while (nd.lt.iwid-ne)
         nd = nd+1
         buff(nd:nd) = char (ifp1/10000000 + ic0)
         ifp1 = mod(ifp1*10,100000000) + ifp2/10000000
         ifp2 = mod(ifp2*10,100000000)
      enddo

      if (ne.gt.0) buff(nd+1:) = cexp
      dbltoch_f=buff
      goto 999

 661  dbltoch_f='ne.ne.lose'
      goto 999
 666  dbltoch_f='NAN'

 999  return
      end



*=========================================================================
*
*     double precision real to character string, variable width format

      character*32 function dbltoch_v (d,jwid,ndec)

*     As for dbltoch_f, but leading spaces and insignificant trailing
*     zeros are removed.
*
*=========================================================================

      implicit none

      double precision  d
      integer  jwid,ndec

      character*32 dbltoch_f
      external     dbltoch_f
      integer      chtrim,trimstart
      external     chtrim,trimstart

      character*32 bufstr
      intrinsic  index

      integer i,j

      character*1 exp_e
      parameter (exp_e = 'E') ! Must agree with dbltoch_f

      dbltoch_v = dbltoch_f (d, jwid, ndec)

      if (index(dbltoch_v, '.') .eq. 0) return

      bufstr = dbltoch_v(trimstart(dbltoch_v):)

      i = index(bufstr, exp_e)

      if (i.gt.0) then
         j = i-1
      else
	 j = chtrim(bufstr)
	 i = j + 1
      endif

      dbltoch_v = bufstr
      if (j.lt.3) return

      do while ( bufstr(j:j).eq.'0' .and. j.gt.2 )
         j = j-1
      enddo

      dbltoch_v(j+1:) = bufstr(i:)

      return
      end

*=========================================================================
*
*     Ordinary real to character string, fixed width format

      character*32 function realtoch_f (a,jwid,ndec)

*
*=========================================================================

      implicit none

      real a
      integer jwid,ndec

      character*32 dbltoch_f
      external     dbltoch_f

      double precision d

      d = dble(a)
      realtoch_f = dbltoch_f (d, jwid, ndec)

      return
      end


*=========================================================================
*
*     Ordinary real to character string, variable width format

      character*32 function realtoch_v (a,jwid,ndec)

*
*=========================================================================

      implicit none

      real a
      integer jwid,ndec

      character*32 dbltoch_v
      external     dbltoch_v

      double precision d

      d = dble(a)
      realtoch_v = dbltoch_v (d, jwid, ndec)

      return
      end


