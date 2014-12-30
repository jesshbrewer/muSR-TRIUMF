cnote: too many missing pieces to compile right now --- much easier to
cjust include the triumf gplot libraries. Note that there are several
cother routines which have gplot equivalents. Tanya rightly suggested that
cthey should all be moved to a separate subdirectory (or actually, several
cseparate subdirectories; one for each system: linux, vms+triumflib, vms).
C	T.R. JULY, 1998 All GPLOT bits are stolen stands alone:
c GPLOT_OBJ =  find_unit_noisy.obj, ibatch_not.obj, read_key.obj, \
c             sysmsg_noisy.obj, tt_get_input_noisy.obj, tt_input_noisy.obj

      SUBROUTINE TT_GET_INPUT(PROMPT,LINE,LENGTH,LUNOUT,CTRLZI)
C======================================================================C
C   
C     Terminal input interface routine which allows for line
C     input recall as is done for DCL commands. The PF1 key
C     lists the current recall buffer lines (up to 40). The
C     PF2 key invokes a HELP facility. PF3 list a set (up to 41)
C     of recallable, user loadable (via CTRL-L), set of input lines.
C     PF4 key invokes a simple desk calculator. The status of the
C     current line is restored if no selection is made from the
C     static, dynamic, or keypad buffers list, as well as after 
C     terminating the desk calculator.
C  
C     PROMPT: Character string used for line input prompting.
C             The first character is used for carriage control
C             and thus is usually blank.
C     LINE  : Character string containing the current line
C             returned to the calling program if on input
C             LENGTH was set to .LE. 0. If LENGTH>0 then
C             LINE is used as the initial default input
C             which the terminal user can edit as desired
C             before returning the contents back to the
C             calling program.
C             It is recommended that LINE be dimensioned
C             CHARACTER*255 in the calling program!
C     LENGTH: Both input and output. If >0 then input string LINE
C             is used as the starting line buffer. The returned
C             LINE length is returned in LENGTH.
C     LUNOUT: Logical unit number for terminal prompts and
C             other terminal output (usually 6).
C             Note: The logical input unit is usually set to 5
C                   for those using the TRIUMF graphics package
C                   (via CLEAR_PLOT) but if this is not the case
C                   the user MUST set IINS (usually to 5) in the
C                   common block COMMON /PLOT_INPUT_UNIT/ IINS
C                   to avoid a fatal read on unit 0.
C  
C     CTRLZI: LOGICAL*4 variable returned .TRUE. if LINE
C             was entered using a CTRL-Z instead of a C/R.
C             (or CTRL-D for ULTRIX)
C     CTRLSP: LOGICAL*4 variable in common block TT_CTRLSP
C             set to .TRUE. is control-space (=null) typed
C             can be used as another decision flag
C     
C            For the Tektronix 4010 devices (TK4010) this interface
C            will not have all the VT100 features
C
C   Modified by J. Chuma on October 25, 1988 so that simple 
C     Carriage Returns are not stored in the buffer
C   Modified by C.Kost on Jan/31/89 to use non-error trapping version
C   of EVALUATE called EVALUATE_NO_TRAP
C   Now (Mar. 2/90) call EVALUATE2_NO_TRAP (input from SYS$COMMAND).
C      C.J.K
C   Modified by C. Kost on May 13/90 so successive duplicates not
C     stored.
C   Modified by J. Chuma on September 21, 1990 to include CTRLSP 
C   Modified by J. Chuma on October 9, 1990 to allow user defined buffer
C     length (up to a max. of 35)
C   Modified by J. Chuma on October 11, 1990 to allow CTRL-P to save the
C     buffer list to a file and CTRL-N to read a buffer list from a file
C   Modified by J. Chuma on November 21, 1990 to include F14 key 
C     same as CTRL-A (insert/overstrike)
C   Modified 04-Aug-93 by FWJ: fixed bad error/eof returns when doing
C     conventional Fortran read.
C   Modified 19-JUL-94 by FWJ: removed failure mode where IINS is
C     not initialized (e.g. by CLEAR_PLOT not being loaded).
C     Input unit for conventional reads now defaults to 5.
C   Modified Feb 3, 1995 by J. Chuma:  added an end-of-file counter
C     for unix to solve an infinitely repeating ctrl-d problem.
C   Modified Feb 17, 1995 by J. Chuma:  use end-of-file counter
C     for vms as well as unix
C   Modified 29-JUN-95 by FWJ: added OPEN(UNIT=5) work-around for
C     Irix 5.3.  Otherwise calling this routine generates a core dump
C     when used in conjuction with X Window graphics.
c   Modified 24-Jul-2000 By T.Riseman Added IMPLICIT NONE.
C
C======================================================================C

C  MXBFP1 should be multiple of 4 to keep proper common block 
C  alignment for ULTRIX

      implicit none
      CHARACTER*(*) PROMPT
      CHARACTER*(*) LINE
      INTEGER LENGTH,LUNOUT
      LOGICAL CTRLZI

c...	functions
      INTEGER LIB$GETDVI
      EXTERNAL DVI$_TT_INSERT

C...	Local variables
      INTEGER MXBUF, MXBFP1
      PARAMETER (MXBUF=35,MXBFP1=36)

C  To allow an external routine to 'preload' the dynamic recall buffers
C  BUFFER, the statics buffers BUFFERS, and the KEYPAD buffers
C  we place the following in a  COMMON block TT_BUFFER.

      COMMON /TT_BUFFER/ BUFFER,  LENBUF,  NBUFF,  LAST,
     &                   BUFFERS, LENBUFS, NBUFFS, LASTS,
     &                   LENBUFK, BUFFERK
      CHARACTER*255 FNAME, DBUF, BUFFER(MXBFP1), BUFFERS(MXBFP1),
     &                           BUFFERK(13), TBUFF
      DATA TBUFF/' '/
      DATA BUFFERK/13*' '/
      INTEGER*4 LENBUF(MXBFP1)/MXBFP1*-1/,LENBUFS(MXBFP1)/MXBFP1*-1/,
     &                                    LENBUFK(13)/13*-1/

C   LBUF allows the calling program to define the length of the recall
C     buffer (default is 20)
C   The maximum value is MXBUF=35 and should be checked in the
C     calling program

      INTEGER LBUF
      COMMON /TT_LEN_BUFF/ LBUF
      DATA LBUF /20/

      CHARACTER*(MXBFP1) RECAL /'123456789abcdefghijklmnopqrstuvwxyz$'/
      CHARACTER*(MXBFP1) RECALL/'123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ$'/
      CHARACTER*13       RECALK/',-.0123456789'/
      CHARACTER*1 TAG
      LOGICAL BLNK, INSERT_DCL, INSERT
      LOGICAL UPLIM/.FALSE./,LOWLIM/.FALSE./
      LOGICAL CTRLSP
      COMMON /TT_CTRLSP/ CTRLSP

C  Common block to allow a program to deactivate the recall shell
C  Installed by J. Chuma, July 23/90

      LOGICAL ACTIVE
      COMMON /TT_ACTIVE/ ACTIVE
      DATA ACTIVE /.TRUE./

      INTEGER*4 NBUFF/0/     ! # of recallable dynamic buffers filled
                             ! so far
      INTEGER*4 NBUFFS/0/    ! # of recallable static buffers filled.
      INTEGER*4 LAST/0/      ! last DYNAMIC buffer # written into
      INTEGER*4 LASTP1/1/    ! LAST + 1
      INTEGER*4 LASTS/0/     ! last STATIC buffer # written into

      INTEGER*4 NGET         ! pointer for ^ v arrows
      COMMON /TT_NGET/ NGET  ! common block installed by J.Chuma
                             ! March 15,1992
      DATA NGET/1/

      INTEGER*4 LOCCUR/1/    ! location of cursor (from end of prompt)
      CHARACTER*1 KEY,ALTKEY,READ_KEY
      EXTERNAL READ_KEY

      CHARACTER*1 CTRLSPC,CTRLA,CTRLB,CTRLD,CTRLE,CTRLF,CTRLH,CTRLI,
     &    CTRLJ,CTRLK,CTRLL,CTRLM,CTRLN,CTRLP,CTRLR,CTRLU,CTRLW,
     &    CTRLX,CTRLZ,CTRLHAT,
     &    ESC,DELETE,PF1,PF2,PF3,PF4

cNotUsed CHARACTER*1  NEWLINE,CTRLT

      COMMON /COM_TERMNAME/ TERMNAME    ! Application program sets this
      CHARACTER*6 TERMNAME/'VT640 '/    ! default 
      LOGICAL     TT_INPUT

      INTEGER IINS
      COMMON /PLOT_INPUT_UNIT/ IINS

      INTEGER IFORCE 
      COMMON /FORCE_TO_TERMINAL/IFORCE
      DATA IFORCE/0/

C   end-of-file counter is reset to zero before a return if input is
C   not ctrl-z (ctrl-d under unix), otherwise counter is incremented
C   by 1 and when count reaches 10, STOP to eliminate infinitely
C   repeating calls

      INTEGER*4 EOF_COUNTER
      DATA EOF_COUNTER /0/

      INTEGER LUNIN, NCHAR, ISTAT, IOUTVL, LP, MAXLEN, ICH, I, NB
      INTEGER NDEL, LOCSAVE, LFNAME, IFNAME, IERROR
      INTEGER NLNE, LNBF, LNFB, NM

      REAL DUM
CCC
      CTRLSPC=CHAR(0) ! null 
      CTRLA=CHAR(1)   ! toggles insert/overstrike modes
      CTRLB=CHAR(2)   ! position up 1 line (like ^ arrow)
      CTRLD=CHAR(4)   ! position left 1 space (like <--)
      CTRLE=CHAR(5)   ! position to end of line
      CTRLF=CHAR(6)   ! position right 1 space (like -->)
      CTRLH=CHAR(8)   ! position to column 1 (same as BACKSPACE)
      CTRLI=CHAR(9)   ! TAB
      CTRLJ=CHAR(10)  ! line feed-- deletes previous field 
                             !  (not yet!)
      CTRLK=CHAR(11)  ! vertical tab--- disables recall buffer
      CTRLL=CHAR(12)  ! form feed-- puts current line in static
                             !  buffer
      CTRLM=CHAR(13)  ! CARRIAGE RETURN
      CTRLN=CHAR(14)  ! read buffer list from a file
      CTRLP=CHAR(16)  ! write buffer list to a file
      CTRLR=CHAR(18)  ! refresh line 
      CTRLU=CHAR(21)  ! erase all before cursor 
      CTRLW=CHAR(23)  ! erase previous word (or part current word)
      CTRLZ=CHAR(26)  ! handle like c/r
      CTRLHAT=CHAR(30)! CTRL^ recalls dynamic buffer matching string
      ESC=CHAR(27)    ! ESC character
      DELETE=CHAR(127) ! delete previous character (except #1)
      PF1=CHAR(80)     ! ALTKEY: list, select dynamic recall buffers 
      PF2=CHAR(81)     ! ALTKEY: help facility invoked
      PF3=CHAR(82)     ! ALTKEY: list, load, select static buffers
      PF4=CHAR(83)     ! ALTKEY: calls up desk calculator
      CTRLX=CHAR(21)  ! erase all before cursor
C Initialize input lun for conventional reads

      LUNIN = 5
      IF( IINS.GT.0 )LUNIN=IINS
      NCHAR = 0
      CTRLZI = .FALSE.
      CTRLSP = .FALSE.
      
      IF( (IFORCE.NE.0) .AND. ACTIVE )GO TO 3

      IF( .NOT.(TT_INPUT(DUM)) .OR. (.NOT.ACTIVE) )THEN
C  Since we are either in batch or SYS$INPUT is not SYS$COMMAND
C  OR the recall shell has been disabled with a CTRL-K
C  and this routine was still called then we will do our best
C  and do a normal read which does not go into the recall shell.

        WRITE(LUNOUT,150)PROMPT
        READ(LUNIN,104,ERR=198,END=199)LINE
        LENGTH = LEN(LINE)
        CALL TRIM_BLNK(LENGTH, LINE)
        EOF_COUNTER = 0    ! reset counter, since not end-of-file

C  if first character is a '!' (if in terminal input mode) then
C  re-enable recall shell.

        IF( .NOT.(TT_INPUT(DUM)) )RETURN
        IF( LINE(1:1).EQ.'!' )THEN
          WRITE(LUNOUT,*)'Recall shell enabled'
          ACTIVE=.TRUE.
        END IF
        RETURN       
 198    WRITE(LUNOUT,*)'TT_GET_INPUT: error during read'
        RETURN
 199    CTRLZI = .TRUE.
        IF( EOF_COUNTER.EQ.10 )STOP 'end-of-file counter exceeded'
        EOF_COUNTER = EOF_COUNTER+1
        RETURN
      END IF

   3  IF( TERMNAME .EQ. 'TK4010' )THEN
        WRITE(LUNOUT,150)PROMPT
        READ(LUNIN,104,ERR=105,END=99)LINE
 104    FORMAT(A)
        LENGTH = LEN(LINE)
        CALL TRIM_BLNK(LENGTH, LINE)
        EOF_COUNTER = 0    ! reset counter, since not end-of-file
        RETURN
 105    WRITE(LUNOUT,*)'TT_GET_INPUT: error during read'
        RETURN
  99    CTRLZI = .TRUE.
        IF( EOF_COUNTER.EQ.10 )STOP 'end-of-file counter exceeded'
        EOF_COUNTER = EOF_COUNTER+1
        RETURN
      END IF

C  Set INSERT_DCL to TRUE if DCL set for INSERT mode
C  ie. user had $SET TERM/INSERT

      INSERT_DCL=.FALSE.      ! VMS default
      ISTAT=LIB$GETDVI(%LOC(DVI$_TT_INSERT),,'TT',IOUTVL)
      IF(ISTAT.AND.(IOUTVL.NE.0))INSERT_DCL=.TRUE.
      INSERT=INSERT_DCL

      LP=LEN(PROMPT)
      IF( LENGTH.GT.0 )THEN  ! Initialize buffer to input LINE
        WRITE(LUNOUT,150)PROMPT
        MAXLEN=MIN(131,LENGTH)
        WRITE(LUNOUT,151)LINE(1:MAXLEN)
 150    FORMAT(A,$)
 151    FORMAT('+',A,$)
        LOCCUR=MAXLEN+1
        NCHAR=MAXLEN
        TBUFF=LINE(1:MAXLEN)
      ELSE
        WRITE(LUNOUT,100)PROMPT
 100    FORMAT(A,$)
      END IF

C   Recycle point----- get another input character

  10  KEY = READ_KEY('KEYPAD  ',' ',ALTKEY,0)
      IF( KEY .EQ. CTRLZ )THEN
        CTRLZI = .TRUE.          ! set if line entered by CTRL-Z
        IF( EOF_COUNTER .EQ. 10 )STOP 'End-of-file counter exceeded 10'
        EOF_COUNTER = EOF_COUNTER+1
        GO TO 200
      END IF
      EOF_COUNTER = 0
      IF( KEY .EQ. CTRLM )GO TO 200
      IF( ICHAR(ALTKEY).EQ.0 )THEN  ! Normal( non-altkey)  key hit
         ICH=ICHAR(KEY)            ! Decimal code of key
         IF( (ICH.GE.32) .AND. (ICH.LE.126) )THEN ! normal character
           IF( INSERT )THEN       ! Insert a character
             WRITE(LUNOUT,440)KEY              ! write the character
             IF( NCHAR.GE.LOCCUR )THEN
               WRITE(LUNOUT,437)ESC            ! save cursor
               WRITE(LUNOUT,442)TBUFF(LOCCUR:NCHAR) ! write rest
             END IF
 440         FORMAT('+',A1,$)
 442         FORMAT('+',A,$)
             IF( LOCCUR.EQ.1 )THEN
               IF( NCHAR.EQ.0 )TBUFF=KEY       ! update
               IF( NCHAR.GT.0 )TBUFF=KEY//TBUFF(LOCCUR:NCHAR) ! update
             ELSE
               IF( LOCCUR.GT.NCHAR )TBUFF=TBUFF(1:LOCCUR-1)//KEY !update
               IF( LOCCUR.LE.NCHAR )
     &          TBUFF=TBUFF(1:LOCCUR-1)//KEY//TBUFF(LOCCUR:NCHAR)!update
             END IF
             IF( NCHAR.GE.LOCCUR )WRITE(LUNOUT,435)ESC ! restore cursor
             NCHAR = MIN(255,NCHAR+1)
             LOCCUR=LOCCUR+1                                  !update
             GO TO 10
           ELSE                      ! Overstrike
             WRITE(LUNOUT,450)KEY
 450         FORMAT('+',A1,$)
             TBUFF(LOCCUR:LOCCUR)=KEY
             IF( LOCCUR.GT.NCHAR )NCHAR=NCHAR+1
             LOCCUR=LOCCUR+1
             GO TO 10
           END IF
           GO TO 10
         END IF

C     Check for CTRL keys

         IF( KEY.EQ.CTRLHAT )THEN

C  search dynamic buffer for preceding string and if found recall
C  that buffer. Find a match with the dynamic buffer and place it
C  in current buffer, else ignore CTRL^

           DO I = 1, NBUFF
             NB = LAST-I+1
             IF( NB.LT.1 )NB = MXBFP1+NB
             IF(INDEX(BUFFER(NB)(1:LENBUF(NB)),TBUFF(1:NCHAR)).GT.0)THEN
C         Match found
               WRITE(LUNOUT,555)ESC                ! erase current line
               WRITE(LUNOUT,290)CTRLM              ! carriage return
               IF( LP.GE.2 )WRITE(LUNOUT,412)PROMPT(2:) 
               WRITE(LUNOUT,510)BUFFER(NB)(1:LENBUF(NB))
               TBUFF = BUFFER(NB)(1:LENBUF(NB))
               LOCCUR = LENBUF(NB)+1
               NCHAR = LENBUF(NB)
               GO TO 10
             END IF
           END DO
           GO TO 10
         ELSE IF( KEY.EQ.CTRLA )THEN ! toggle insert/overstrike mode
           INSERT=.NOT.INSERT
           GO TO 10
         ELSE IF( KEY.EQ.CTRLB )THEN ! like up arrow ^
           INSERT=INSERT_DCL           !reset INSERT mode to DCL mode
           IF( .NOT.UPLIM )NGET=NGET-1
           WRITE(LUNOUT,555)ESC   ! erase current line
           WRITE(LUNOUT,290)CTRLM ! carr.ret. sets curs. to left margin
           IF( LP.GE.2 )WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
           IF( LENBUF(NGET).GT.0 )THEN
             TBUFF=BUFFER(NGET)(1:LENBUF(NGET))
             WRITE(LUNOUT,510)TBUFF(1:MIN(131,LENBUF(NGET)))
             LOCCUR=LENBUF(NGET)+1
             NCHAR=LENBUF(NGET)
             LOWLIM=.FALSE.
             UPLIM=.FALSE.
             GO TO 10
           ELSE
             LOCCUR=1
             NCHAR=0
             UPLIM=.TRUE.
             GO TO 10
           END IF
         ELSE IF( KEY.EQ.CTRLE )THEN ! position cursor to end-of-line
           IF( NCHAR-LOCCUR+1.EQ.0 )GO TO 10  ! already at end of line
           WRITE(LUNOUT,310)ESC,NCHAR-LOCCUR+1
 310       FORMAT('+',A1,'[',I3.3,'C',$)     ! position cursor to end
           LOCCUR=NCHAR+1
           GO TO 10
         ELSE IF( KEY.EQ.CTRLD )THEN ! same action as left arrow
           IF( LOCCUR .GT. 1 )THEN   ! must be VMS, 
             LOCCUR=LOCCUR-1         ! unix already taken care of
             WRITE(LUNOUT,530)ESC
           END IF
           GO TO 10
         ELSE IF( KEY.EQ.CTRLF )THEN ! same action as right arrow
           IF( LOCCUR.EQ.NCHAR+1 )GO TO 10  ! like DCL
           IF( LOCCUR.LT.255 )THEN
             LOCCUR=LOCCUR+1
             WRITE(LUNOUT,540)ESC
           END IF
           GO TO 10
         ELSE IF( KEY.EQ.CTRLH )THEN ! Position cursor to begin of line
           IF( LOCCUR-1.GT.0 )WRITE(LUNOUT,411)ESC,LOCCUR-1
  411      FORMAT('+',A1,'[',I3.3,'D',$)    ! cursor LOCCUR-1 left
  410      FORMAT('+',A1,'[1D',$)         ! cursor 1 space left
           LOCCUR=1
           GO TO 10
         ELSE IF( KEY.EQ.CTRLR )THEN ! Refresh current line
           WRITE(LUNOUT,555)ESC   ! erase current line
           WRITE(LUNOUT,290)CTRLM ! CR sets cursor to left margin
           IF( LP.GE.2 )WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
           IF( LOCCUR.GT.1 )
     &      WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part line
           WRITE(LUNOUT,437)ESC              !save cursor position
           IF( NCHAR.GE.LOCCUR )
     &      WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR))! 2nd part line
           WRITE(LUNOUT,435)ESC              !restore cursor position
  412      FORMAT('+',A,$)
           GO TO 10
         ELSE IF( KEY.EQ.CTRLW )THEN ! Erase previous word
           BLNK=.FALSE.              ! to blank, tabs ignored for now
           NDEL=0
           IF( TBUFF(LOCCUR:LOCCUR).EQ.' ' )BLNK=.TRUE.
           LOCSAVE=LOCCUR
  415      IF( BLNK )THEN
             IF( LOCCUR.LE.1 )GO TO 419
             WRITE(LUNOUT,431)ESC           ! cursor left one space
             LOCCUR=LOCCUR-1
             NDEL=NDEL+1
             IF( TBUFF(LOCCUR:LOCCUR).NE.' ' )BLNK=.FALSE.
             GO TO 415
           END IF

C  not on a blank

  416      IF( LOCCUR.LE.1 )GO TO 419
           WRITE(LUNOUT,431)ESC            ! cursor left one space
           LOCCUR=LOCCUR-1
           NDEL=NDEL+1
           IF( TBUFF(LOCCUR:LOCCUR).EQ.' ' )THEN
             WRITE(LUNOUT,540)ESC     ! cursor right one space
             LOCCUR=LOCCUR+1
             NDEL=NDEL-1
             GO TO 419
           END IF
           GO TO 416
  419      WRITE(LUNOUT,437)ESC           !save cursor
           WRITE(LUNOUT,420)ESC              !erase to end of line
           IF( NCHAR.GT.LOCSAVE )WRITE(LUNOUT,433)TBUFF(LOCSAVE+1:NCHAR)
           WRITE(LUNOUT,435) ESC             !restore cursor
           IF( LOCCUR.GT.1 )TBUFF=TBUFF(1:LOCCUR-1)//TBUFF(LOCSAVE+1:)
           IF( LOCCUR.EQ.1 )TBUFF=TBUFF(LOCSAVE+1:)
           NCHAR=NCHAR-NDEL
           GO TO 10
         ELSE IF( (KEY.EQ.CTRLU) .OR. (KEY.EQ.CTRLX) )THEN
            TBUFF=TBUFF(LOCCUR:)  ! Erase left of cursor & left justify
            NCHAR=NCHAR-LOCCUR+1
            IF(LOCCUR.LE.1) GO TO 10
            WRITE(LUNOUT,555)ESC   ! erase current line
c		! carriage return sets cursor to left margin
            WRITE(LUNOUT,290)CTRLM 
c		!prompt without a line feed/cr
            IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
 420        FORMAT('+',A1,'[K',$)             ! erase to end of line
 430        FORMAT('+',A,$)
            WRITE(LUNOUT,437)ESC              !save cursor position
            IF(NCHAR.GT.0)WRITE(LUNOUT,430)TBUFF(1:NCHAR)
            WRITE(LUNOUT,435)ESC              !restore cursor position
            LOCCUR=1
            GO TO 10
         ELSE IF( KEY.EQ.DELETE )THEN ! erase character left of cursor
            IF( LOCCUR.EQ.1 )GO TO 10

C  delete from cursor to EOL, cursor left 1, save , write rest , restore

            WRITE(LUNOUT,431)ESC
            WRITE(LUNOUT,437)ESC
 431        FORMAT('+',A1,'[1D',$)       !cursor left one space
 437        FORMAT('+',A1,'7',$)         !save cursor
            IF(NCHAR.GE.LOCCUR) THEN
               WRITE(LUNOUT,433)TBUFF(LOCCUR:NCHAR)//' '
            ELSE
               WRITE(LUNOUT,433)                     ' '
            ENDIF               
            WRITE(LUNOUT,435)ESC
 433        FORMAT('+',A,$)
 435        FORMAT('+',A1,'8',$)         !restore cursor
            IF(LOCCUR.GT.2)THEN
               TBUFF=TBUFF(1:LOCCUR-2)//TBUFF(LOCCUR:)
            ELSE
               TBUFF=TBUFF(LOCCUR:)      !LOCCUR must be 2              
            ENDIF
            LOCCUR=LOCCUR-1
            NCHAR=NCHAR-1
            GO TO 10
         ELSE IF( KEY.EQ.CTRLK )THEN  ! Disable the recall buffer shell
            ACTIVE=.FALSE.
            IF(.NOT.ACTIVE)WRITE(LUNOUT,436)
 436        FORMAT(
     &     ' Recall shell disabled. Use "!" in column one to re-enable')
            LENGTH=0
            RETURN
         ELSE IF( KEY.EQ.CTRLSPC )THEN ! CTRL-spacebar
            CTRLSP = .TRUE.
            GO TO 200
         ELSE IF( KEY.EQ.CTRLN )THEN  ! Read buffer list from a file
4360        WRITE(LUNOUT,4361)
4361        FORMAT(1X,/,' Enter filename containing buffer list >> ',$)
            READ(LUNIN,4362,ERR=4360,END=43645)FNAME
4362        FORMAT(A)
            LFNAME = LEN(FNAME)
            CALL TRIM_BLNK(LFNAME, FNAME)
            IF( LFNAME .EQ. 0 )GO TO 43645
            CALL FIND_UNIT(IFNAME)
            OPEN(UNIT=IFNAME,FILE=FNAME(1:LFNAME),STATUS='OLD',
     &           READONLY,SHARED,ERR=4365,IOSTAT=IERROR)
            NLNE = 1
4363        READ(IFNAME,4362,END=4364,ERR=4367,IOSTAT=IERROR)DBUF
            LNBF = LEN(DBUF)
            CALL TRIM_BLNK(LNFB, DBUF)
            NLNE = NLNE+1
            IF( LNBF.GT.0 )THEN
              LAST = LAST+1
              IF( LAST.GT.MXBFP1 )LAST = 1
              BUFFER(LAST)=DBUF
              LENBUF(LAST)=LNBF
              NBUFF=NBUFF+1          ! NBUFF counts things in the buffer
              IF( NBUFF.GT.MXBUF )NBUFF=MXBUF
            END IF
            GO TO 4363
4364        LASTP1 = LAST+1
            IF( LASTP1.GT.MXBFP1 )LASTP1 = 1
            LENBUF(LASTP1)=-1
            NGET=LASTP1               ! set to empty buffer
            LOWLIM=.TRUE.
            UPLIM=.FALSE.
            TBUFF=' '
            NCHAR=0
            LOCCUR=1
            INSERT=INSERT_DCL        !reset INSERT mode to DCL mode
43643       CLOSE(UNIT=IFNAME)
c		!prompt without a line feed/cr
43645       IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
c		! first part of line
            IF(LOCCUR.GT.1)
     &       WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) 
            WRITE(LUNOUT,437)ESC              !save cursor position
            IF(NCHAR.GE.LOCCUR)
     &       WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! second part
            WRITE(LUNOUT,435)ESC              !restore cursor position
            GO TO 10
4365        WRITE(LUNOUT,4366)FNAME(1:LFNAME)
4366        FORMAT(' Error opening ',A)
            CALL SYSMSG(IERROR,LUNOUT)
            CLOSE(UNIT=IFNAME)
            GO TO 4360
4367        WRITE(LUNOUT,4368)FNAME(1:LFNAME),NLNE
4368        FORMAT(' Error reading ',A,': record number: ',I3)
            CALL SYSMSG(IERROR,LUNOUT)
            CLOSE(UNIT=IFNAME)
            GO TO 4360
         ELSE IF( KEY.EQ.CTRLP )THEN  !Write buffer list to a file
4370        WRITE(LUNOUT,4371)
4371        FORMAT(1X,/,' Enter filename to save buffer list >> ',$)
            READ(LUNIN,4372,ERR=4370,END=4374)FNAME
4372        FORMAT(A)
            LFNAME = LEN(FNAME)
            CALL TRIM_BLNK(LFNAME, FNAME)
            IF( LFNAME .EQ. 0 )GO TO 4374
            CALL FIND_UNIT(IFNAME)
            OPEN(UNIT=IFNAME,FILE=FNAME(1:LFNAME),STATUS='UNKNOWN',
     &       RECL=256,CARRIAGECONTROL='LIST',ERR=4375,IOSTAT=IERROR)
cNov2000            OPEN(UNIT=IFNAME,FILE=FNAME(1:LFNAME),STATUS='NEW',
cNov2000     &       RECL=256,CARRIAGECONTROL='LIST',ERR=4375,IOSTAT=IERROR)
            DO I=NBUFF,1,-1
              NB=LAST-I+1
              IF( NB.LT.1 )NB=MXBFP1+NB        ! wrap over bottom
              WRITE(IFNAME,4373)BUFFER(NB)(1:MIN(129,LENBUF(NB)))
4373          FORMAT(A)
            END DO
            CLOSE(UNIT=IFNAME)

C  Return and restore line and cursor as before PF1

c		!prompt without a line feed/cr
4374        IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
c		! first part of line
            IF(LOCCUR.GT.1)
     &       WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) 
            WRITE(LUNOUT,437)ESC              !save cursor position
            IF(NCHAR.GE.LOCCUR)
     &       WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! second part
            WRITE(LUNOUT,435)ESC              !restore cursor position
            GO TO 10
4375        WRITE(LUNOUT,4373)'Error opening '//FNAME(1:LFNAME)
            CALL SYSMSG(IERROR,LUNOUT)
            CLOSE(UNIT=IFNAME)
            GO TO 4370
         ELSE IF( KEY.EQ.CTRLL )THEN  ! Ignore CTRL-L
            GO TO 10
         ELSE IF( KEY.EQ.CTRLI )THEN  ! Ignore CTRL-I or TAB
            GO TO 10
         ELSE IF( KEY.EQ.CTRLJ )THEN  ! Ignore CTRL-J
            GO TO 10
         END IF
      ELSE                                         !ICHAR(ALTKEY).NE.0 
C
C     Keypad ( or arrow) key hit         
C
         IF(ALTKEY.EQ.'^')THEN
C
C     Up arrow
C
            INSERT=INSERT_DCL       !reset INSERT mode to DCL mode
            IF( .NOT.UPLIM )NGET=NGET-1
            IF( NGET.LT.1 )NGET=MXBFP1
            WRITE(LUNOUT,555)ESC   ! erase current line
c		! carriage return sets cursor to left margin
            WRITE(LUNOUT,290)CTRLM 
c		!prompt without a line feed/cr
            IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
            IF(LENBUF(NGET).GT.0)THEN
               TBUFF=BUFFER(NGET)(1:LENBUF(NGET))
               WRITE(LUNOUT,510)TBUFF(1:MIN(131,LENBUF(NGET)))
 510           FORMAT('+',A,$)
               LOCCUR=LENBUF(NGET)+1
               NCHAR=LENBUF(NGET)
               LOWLIM=.FALSE.
               UPLIM=.FALSE.
               GO TO 10
            ELSE
C               WRITE(LUNOUT,521)NGET
C 521           FORMAT('+ NGET=',I5,$)
               TBUFF=' '
               LOCCUR=1
               NCHAR=0
               UPLIM=.TRUE.
               GO TO 10
            ENDIF
C
         ELSE IF(ALTKEY.EQ.'v') THEN
C
C    Down arrow
C
            INSERT=INSERT_DCL        !reset INSERT mode to DCL mode
            IF( .NOT.LOWLIM )NGET=NGET+1
            IF( NGET.GT.MXBFP1 )NGET=1
            WRITE(LUNOUT,555)ESC   ! erase current line
            WRITE(LUNOUT,290)CTRLM ! CR sets cursor to left margin
            IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
            IF(LENBUF(NGET).GT.0)THEN
               TBUFF=BUFFER(NGET)(1:LENBUF(NGET))
               WRITE(LUNOUT,510)TBUFF(1:MIN(131,LENBUF(NGET)))
               LOCCUR=LENBUF(NGET)+1
               NCHAR=LENBUF(NGET)
               LOWLIM=.FALSE.
               UPLIM=.FALSE.
               GO TO 10
            ELSE
C               WRITE(LUNOUT,521)NGET
               LOCCUR=1
               NCHAR=0
               TBUFF=' '
               LOWLIM=.TRUE.
               GO TO 10
            ENDIF
C
         ELSE IF(ALTKEY.EQ.'<') THEN
C
C    Left arrow
C
               IF(LOCCUR.GT.1)THEN
                  LOCCUR=LOCCUR-1
                  WRITE(LUNOUT,530)ESC
 530              FORMAT('+',A1,'[1D',$)
               ENDIF
               GO TO 10
C
         ELSE IF(ALTKEY.EQ.'>') THEN
C
C    Right arrow
C
           IF(LOCCUR.EQ.NCHAR+1) GO TO 10  ! like DCL
           IF(LOCCUR.LT.255)THEN
              LOCCUR=LOCCUR+1
              WRITE(LUNOUT,540)ESC
 540          FORMAT('+',A1,'[1C',$)
           ENDIF
           GO TO 10
         ELSE IF(ALTKEY.EQ.PF1) THEN    
C
C PF1 (Gold) keypad key. Show buffer list and optionally select an entry
C
           WRITE(LUNOUT,550)ESC
 550       FORMAT(1X,A1,'[2J')           ! Clear screen
           WRITE(LUNOUT,560)ESC
 560       FORMAT(1X,A1,'[1;1H',$)       ! Home cursor  
           WRITE(LUNOUT,561)
 561       FORMAT(' # ----5----0----5----0----5----0----5----0',
     &                  '----5----0----5----0----5----0----5')
 563       FORMAT(' #  ----5----0----5----0----5----0----5----0',
     &                  '----5----0----5----0----5----0----5')
           DO I=1,MIN(LBUF,NBUFF)
             NB=LAST-I+1
             IF( NB.LT.1 )NB=MXBFP1+NB        ! wrap over bottom
             WRITE(LUNOUT,570)
     &       RECALL(I:I),BUFFER(NB)(1:MIN(129,LENBUF(NB)))
 570         FORMAT(1X,A1,1X,A)
 571         FORMAT(1X,2A1,1X,A)
           ENDDO
           WRITE(LUNOUT,580)
 580       FORMAT(' Type a recall code 1,2,3...Z or',
     &                ' a carriage return to resume ',$)
  60       KEY=READ_KEY(' ',' ',ALTKEY,0)
C
C    Return and restore line and cursor as before PF1
C
           IF(KEY.EQ.CTRLM)THEN
             WRITE(LUNOUT,550)ESC       ! clears screen
             WRITE(LUNOUT,560)ESC       ! home cursor
c		!prompt wthout a line feed/cr
             IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
c		! first part of line
             IF(LOCCUR.GT.1)
     &        WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) 
             WRITE(LUNOUT,437)ESC              !save cursor position
c		! second part
             IF(NCHAR.GE.LOCCUR)
     &        WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) 
             WRITE(LUNOUT,435)ESC              !restore cursor position
             GO TO 10
           ELSE 
             NM=INDEX(RECALL(1:NBUFF),KEY)           ! lower case check
             IF(NM.EQ.0)NM=INDEX(RECAL(1:NBUFF),KEY) ! UPPER CASE CHECK
             IF(NM.NE.0)THEN
               LOWLIM=.FALSE.
               UPLIM=.FALSE.
               NGET=LAST-NM+1
               IF( NGET.LT.1 )NGET=MXBFP1+NGET
               NCHAR=LENBUF(NGET)
               WRITE(LUNOUT,550)ESC       ! Clear screen
               WRITE(LUNOUT,560)ESC       ! home cursor
               TBUFF=BUFFER(NGET)(1:NCHAR)
c		!prompt wthout line feed/cr
               IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
CC              WRITE(LUNOUT,100)PROMPT
               WRITE(LUNOUT,510)BUFFER(NGET)(1:MIN(131,NCHAR))
               LOCCUR=NCHAR+1
               GO TO 10
             ELSE
               GO TO 60
             ENDIF
           ENDIF
         ELSE IF(ALTKEY.EQ.PF2) THEN
C
C    PF2 keypad key. Calls HELP facility.
C
            WRITE(LUNOUT,550)ESC       ! clears screen
            WRITE(LUNOUT,560)ESC       ! home cursor
            WRITE(LUNOUT,*)' This terminal interface closely mimics',
     &        ' the DCL command recall facility. '
            WRITE(LUNOUT,*)' The arrow, delete, backspace, and most',
     &        ' control keys, work as in DCL.'
            WRITE(LUNOUT,*)' '
            WRITE(LUNOUT,*)'CONTROL KEYS:'
            WRITE(LUNOUT,*)' CTRL-^ appended to a string',
     &        ' recalls the last command containing it'
            WRITE(LUNOUT,*)' CTRL-A toggles insert/overstrike mode'
            WRITE(LUNOUT,*)' CTRL-E moves cursor to end of line'
            WRITE(LUNOUT,*)' CTRL-H (BACKSPACE) moves cursor to',
     &        ' the beginning of the input line'
            WRITE(LUNOUT,*)' CTRL-K disables recall shell, a "!" in',
     &        ' column 1 re-enables it'
            WRITE(LUNOUT,*)' CTRL-N reads the dynamic recall buffer',
     &        ' from a file'
            WRITE(LUNOUT,*)' CTRL-P writes the dynamic recall buffer',
     &        ' to a file'
            WRITE(LUNOUT,*)' CTRL-R refreshs the current input line'
            WRITE(LUNOUT,*)' CTRL-W deletes previous word or ',
     &        'left part of current word'
            WRITE(LUNOUT,*)' CTRL-X (CTRL-U) erases input line to',
     &        ' the left of the cursor'
CC            WRITE(LUNOUT,*)' Currently LINEFEED (CTRL-J), ESC',
CC     &        ' and TAB (CTRL-I) are not enabled'
            WRITE(LUNOUT,*)' '
            WRITE(LUNOUT,*)'KEYPAD FUNCTION and ENTER KEYS:'
            WRITE(LUNOUT,*)' PF1 - list and allows selection of recall',
     &        ' buffer (dynamic)'
            WRITE(LUNOUT,*)' PF2 - lists this HELP facility '
            WRITE(LUNOUT,*)' PF3 - lists, loads (via CTRL-L), and',
     &        ' selects user input records (static)'
            WRITE(LUNOUT,*)' PF4 - invokes a simple desk CALCULATOR'
            WRITE(LUNOUT,*)' ENTER -loads current string into 13',
     &        ' possible keypad keys'
            WRITE(LUNOUT,*)' F14 - toggles insert/overstrike mode'
            WRITE(LUNOUT,*)' '
            WRITE(LUNOUT,*)' Contact Corrie Kost (TRIUMF local 310)',
     &        ' for problems, suggestions, etc.'
            WRITE(LUNOUT,581)
581         FORMAT(' TYPE ANY KEY TO RESUME',$)
            KEY=READ_KEY(' ',' ',ALTKEY,0)
C
C   Return and restore cursor position as before PF2
C
            WRITE(LUNOUT,550)ESC       ! clears screen
            WRITE(LUNOUT,560)ESC       ! home cursor
c		!prompt without a line feed/cr
            IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)
c		! first part of line
            IF(LOCCUR.GT.1)
     &       WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) 
            WRITE(LUNOUT,437)ESC              !save cursor position
c		! second part of line
            IF(NCHAR.GE.LOCCUR)
     &       WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) 
            WRITE(LUNOUT,435)ESC              !restore cursor position
            GO TO 10
         ELSE IF(ALTKEY.EQ.PF3) THEN    

C    Show (optionally load) static buffers and optionally select entry

           WRITE(LUNOUT,550)ESC          ! Clear screen
           WRITE(LUNOUT,560)ESC          ! Home cursor
           WRITE(LUNOUT,562)
 562       FORMAT('          STATIC BUFFERS LIST    ')
           WRITE(LUNOUT,561)             ! display a ruler
           DO I=1,MXBFP1
             IF(LENBUFS(I).GT.0)WRITE(LUNOUT,570)
     &        RECALL(I:I),BUFFERS(I)(1:MIN(129,LENBUFS(I)))
           END DO
           WRITE(LUNOUT,582)
 582       FORMAT(' Type a recall code 1,2,3...Z or',
     &      ' CTRL-L to load. C/R to resume ',$)
  70       KEY=READ_KEY(' ',' ',ALTKEY,0)
           IF( KEY.EQ.CTRLM )THEN
             WRITE(LUNOUT,550)ESC                     ! Clear screen
             WRITE(LUNOUT,560)ESC                     ! home cursor
             IF( LP.GE.2 )WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
             IF( LOCCUR.GT.1 )
     &        WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1))   ! 1st part 
             WRITE(LUNOUT,437)ESC              ! save cursor position
             IF( NCHAR.GE.LOCCUR )
     &        WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! 2nd part
             WRITE(LUNOUT,435)ESC              ! restore cursor position
             GO TO 10
           ELSE 
             NM = INDEX(RECALL(1:MXBFP1),KEY)           ! lower case
             IF( NM.EQ.0 )NM=INDEX(RECAL(1:MXBFP1),KEY) ! UPPER CASE
             IF( NM.NE.0 .AND. LENBUFS(NM).GT.0 )THEN ! Valid recall key
               LOWLIM=.FALSE.                         ! non-empty buffer
               UPLIM=.FALSE.
               NCHAR=LENBUFS(NM)
               WRITE(LUNOUT,550)ESC       ! Clear screen
               WRITE(LUNOUT,560)ESC       ! home cursor
               TBUFF=BUFFERS(NM)(1:NCHAR)
               IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
               WRITE(LUNOUT,510)BUFFERS(NM)(1:MIN(131,NCHAR))
               LOCCUR=NCHAR+1
               GO TO 10
             ELSE IF( KEY .EQ. CTRLL )THEN   ! load current TBUFF
  50           WRITE(LUNOUT,555)ESC
               DO I=1,70
                 WRITE(LUNOUT,530)ESC        ! cursor left 1 space
               END DO
               WRITE(LUNOUT,556)
 555           FORMAT('+',A1,'[2K',$)        ! erase current line
 556           FORMAT('+Enter store buffer # (1,...,9,A,...,Z),',
     &          ' C/R to abort: ',$)
               KEY=READ_KEY(' ',' ',ALTKEY,0)
               NM=INDEX(RECALL(1:MXBFP1),KEY)  ! lower case check
               IF( NM.EQ.0 )NM=INDEX(RECAL(1:MXBFP1),KEY) 
               IF( KEY.EQ.CTRLM )THEN ! Return/restore cursor position
                 WRITE(LUNOUT,550)ESC       ! Clear screen
                 WRITE(LUNOUT,560)ESC       ! home cursor
                 IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)!prompt w/o LF/CR
                 IF(LOCCUR.GT.1)
     &            WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part
                 WRITE(LUNOUT,437)ESC              !save cursor position
                 IF(NCHAR.GE.LOCCUR)
     &            WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR))!2nd part
                 WRITE(LUNOUT,435)ESC           !restore cursor position
                 GO TO 10
               END IF
               IF( NM.EQ.0 )GO TO 50       ! invalid key -- try again

C          Valid store key --- store

               BUFFERS(NM)=TBUFF
               LENBUFS(NM)=NCHAR
C           Return/restore cursor position
               WRITE(LUNOUT,550)ESC       ! Clear screen
               WRITE(LUNOUT,560)ESC       ! home cursor
               IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:)!prompt w/o LF/CR
               IF(LOCCUR.GT.1)
     &          WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part 
               WRITE(LUNOUT,437)ESC              !save cursor position
               IF(NCHAR.GE.LOCCUR)
     &          WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! 2nd part
               WRITE(LUNOUT,435)ESC            !restore cursor position
               GO TO 10
             ELSE                              !invalid key--again
               GO TO 70
             END IF
           END IF
         ELSE IF( ALTKEY.EQ.PF4 )THEN  ! Calls desk calculator
CCPAK           WRITE(LUNOUT,*)
CCPAK     &      '       WELCOME TO THE DESK-TOP CALCULATOR!!! '
CCPAK           WRITE(LUNOUT,*)
CCPAK     &      '    -->>> Type a carriage return to exit <<<-- '
CCPAK           CALL EVALUATE2_NO_TRAP
CCPAK           WRITE(LUNOUT,*)' Thank you... Come again'
           WRITE(LUNOUT,*)' Use \= instead'
           WRITE(LUNOUT,100)PROMPT ! restore cursor position
           IF( LOCCUR.GT.1 )
     &      WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part
           WRITE(LUNOUT,437)ESC              ! save cursor position
           IF( NCHAR.GE.LOCCUR )
     &      WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! 2nd part
           WRITE(LUNOUT,435)ESC              ! restore cursor position
           GO TO 10
         ELSE IF( ALTKEY.EQ.CTRLM )THEN    

C    ENTER of keypad key  typed. Show ( optionally load) KEYPAD buffers

           WRITE(LUNOUT,550)ESC          ! Clear screen
           WRITE(LUNOUT,560)ESC          ! Home cursor
           WRITE(LUNOUT,662)
 662       FORMAT('          KEYPAD BUFFERS LIST (* means "hot")    ')
           WRITE(LUNOUT,563)             ! display a ruler
           DO I=1,13
             TAG=' '
             IF(LENBUFK(I).GT.0)THEN 
               IF(BUFFERK(I)(LENBUFK(I):LENBUFK(I)) .EQ. CTRLM)TAG='*'
               WRITE(LUNOUT,571) 
     &          RECALK(I:I),TAG,BUFFERK(I)(1:MIN(129,LENBUFK(I)))
             ENDIF
           ENDDO
 681       WRITE(LUNOUT,682)
 682       FORMAT(' Type KEYPAD KEY to be loaded or C/R to resume ',$)
 670       KEY=READ_KEY('KEY ',' ',ALTKEY,0)
           IF( KEY.EQ.CTRLM )THEN

C    c/r  typed while in keypad ENTER mode.
C    Return and restore cursor position as before typing ENTER key.

             WRITE(LUNOUT,550)ESC       ! Clear screen
             WRITE(LUNOUT,560)ESC       ! home cursor
             IF( LP.GE.2 )WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR
             IF( LOCCUR.GT.1 )
     &        WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part 
             WRITE(LUNOUT,437)ESC              !save cursor position
             IF( NCHAR.GE.LOCCUR )
     &        WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! 2nd part
             WRITE(LUNOUT,435)ESC              !restore cursor position
             GO TO 10
           ELSE                !  load into specified keypad key
             NM=INDEX(RECALK(1:13),ALTKEY)
             IF( NM.EQ.0 )THEN                 ! invalid key--again
               WRITE(LUNOUT,655)ESC            ! erase current line
               DO I=1,70
                 WRITE(LUNOUT,530)ESC          ! cursor left 1 space
               ENDDO
               WRITE(LUNOUT,657)
 655           FORMAT('+',A1,'[2K',$)
 657       FORMAT('+Type KEYPAD KEY to be loaded or C/R to resume ',$)
               GO TO 670     
             END IF

C    A valid (non-PF) keypad key was typed while in keypad ENTER mode:
C    load current TBUFF into it.
C    Determine if key is to be "hot" (ie. includes c/r)

             WRITE(LUNOUT,655)ESC            ! erase current line
             DO I = 1, 70
               WRITE(LUNOUT,530)ESC          ! cursor left 1 space
             END DO
             WRITE(LUNOUT,658)
 658      FORMAT('+Type ENTER if "hot" key, any other key to resume ',$)
             KEY=READ_KEY('KEY ',' ',ALTKEY,0)
             BUFFERK(NM)=TBUFF              ! default to non-hot key
             LENBUFK(NM)=NCHAR

C          Check if response is ENTER key and string not null

             IF( ALTKEY.EQ.CTRLM .AND. NCHAR.GE.1 )THEN ! a "hot" key
               BUFFERK(NM)=TBUFF(1:NCHAR)//CTRLM
               LENBUFK(NM)=NCHAR+1              
             END IF
             WRITE(LUNOUT,550)ESC       ! Clear screen
             WRITE(LUNOUT,560)ESC       ! home cursor
             IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:) ! prompt w/o LF/CR 
             IF( LOCCUR.GT.1 )
     &        WRITE(LUNOUT,510)TBUFF(1:MIN(131,LOCCUR-1)) ! 1st part 
             WRITE(LUNOUT,437)ESC              !save cursor position
             IF( NCHAR.GE.LOCCUR )
     &        WRITE(LUNOUT,510)TBUFF(LOCCUR:MIN(131,NCHAR)) ! 2nd part
             WRITE(LUNOUT,435)ESC              !restore cursor position
             GO TO 10
           END IF
         ELSE IF( ICHAR(ALTKEY).GE.44 .AND. ICHAR(ALTKEY).LE.57 )THEN
C
C    keypad key (except PF1-4 and ENTER)
C
           NM=INDEX(RECALK(1:13),ALTKEY)
           IF(LENBUFK(NM) .LE. 0)GO TO 10        ! key not defined yet
           WRITE(LUNOUT,555)ESC                  ! erase current line
           WRITE(LUNOUT,290)CTRLM            ! set cursor at left margin
           IF(LP.GE.2)WRITE(LUNOUT,412)PROMPT(2:) !prompt without lf/cr
           NCHAR=LENBUFK(NM)
           IF( BUFFERK(NM)(NCHAR:NCHAR).EQ.CTRLM )THEN ! "HOT" key
             NCHAR=NCHAR-1
             TBUFF=BUFFERK(NM)(1:NCHAR)
             WRITE(LUNOUT,510)BUFFERK(NM)(1:MIN(131,NCHAR))
             LOCCUR=NCHAR+1
             GO TO 200                    ! not to 10 this time
           ELSE
             TBUFF=BUFFERK(NM)(1:NCHAR)
             WRITE(LUNOUT,510)BUFFERK(NM)(1:MIN(131,NCHAR))
             LOCCUR=NCHAR+1
             GO TO 10
           END IF
         ELSE IF( ICHAR(ALTKEY) .EQ. 110 )THEN ! F14 key = CTRL-A
           INSERT=.NOT.INSERT   ! toggle insert/overstrike mode
         END IF
      END IF
      GO TO 10

C    Update dynamic buffer and return line to calling program
C    after setting cursor to left margin (so user has some feed-back
C    that he entered a line into his application program.

 200  LINE=TBUFF
      LENGTH=NCHAR
      IF( LENGTH .EQ. 0 )GO TO 202
      IF( LAST .LT. 1 )GO TO 201
      IF( BUFFER(LAST).EQ.TBUFF )GO TO 202  !successive dups not made.
 201  LAST=LAST+1
      IF(LAST.GT.MXBFP1)LAST=1
      LASTP1=LAST+1
      IF(LASTP1.GT.MXBFP1)LASTP1=1

C   Update dynamic BUFFER (if not same as previous entry)

      BUFFER(LAST)=TBUFF
      LENBUF(LAST)=NCHAR
      LENBUF(LASTP1)=-1        ! MXBUF is the maximum buffer size
      NBUFF=NBUFF+1            ! NBUFF counts things in the buffer
      IF( NBUFF.GT.MXBUF )NBUFF=MXBUF
 202  NGET=LASTP1               ! set to empty buffer
      LOWLIM=.TRUE.
      UPLIM=.FALSE.
      TBUFF=' '
      WRITE(LUNOUT,290)CTRLM
 290  FORMAT('+',A1,$)
      NCHAR=0
      LOCCUR=1
      INSERT=INSERT_DCL        !reset INSERT mode to DCL mode
      RETURN
      END
