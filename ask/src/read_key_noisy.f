        CHARACTER*1 FUNCTION READ_KEY (OPTIONS, PROMPT, ALTKEY, TIMEOUT)
C
C                                       Henry Baragar
C                                       TRIUMF, UBC
C                                       May, 1984
C
C  reqd. KOSTL routines - none
C
C========================================================================
C=                                                                      =
C= READ_KEY                                                             =
C=                                                                      =
C= This routine gets a single key stroke from the terminal immediately. =
C= If an arrow key was hit, then READ_KEY returns as <null> with        =
C= with ALTKEY set, where:                                              =
C=              '^'     - means the up-arrow was hit;                   =
C=              'v'     - means the down-arrow was hit (NB. v not V);   =
C=              '<'     - means the left-arrow was hit;                 =
C=              '>'     - means the right-arrow was hit.                =
C=                                                                      =
C= If the 'KEYPAD' option is set, then READ_KEY returns as <null>       =
C= and ALTKEY is set to the character that you see on the KEYPAD key;   =
C= and the PF keys are returned as:                                     =
C=              'P'     - for PF1;                                      =
C=              'Q'     - for PF2;                                      =
C=              'R'     - for PF3;                                      =
C=              'S'     - for PF4.                                      =
C=              'n'     - for F14                                       =
C=                                                                      =
C= Also, if an <ESC> sequence could not be interpreted, then READ_KEY   =
C= is returned set to <ESC> and ALTKEY is returned with the character   =
C= at which interpretation was lost (I know this could cause problems   =
C= -- but if you don't like it, use the 'NOESC' option and interpret    =
C= the <ESC> sequence yourself!!)                                       =
C=                                                                      =
C= If the 'TIMEOUT' option is set, then if there is no character        =
C= typed within the TIMEOUT period then both READ_KEY and ALTKEY        =
C= return as <null>.                                                    =
C=                                                                      =
C= N.B. To get the opposite effect of an option, don't mention it.      =
C=      ie. the defaults are the opposite of the options available.     =
C=      eg. the default is that READ_KEY tries to interperate <ESC>     =
C=          sequences, and if it recognizes it, READ_KEY is returned    =
C=          as null, and ALTKEY contains a single character symbolic    =
C=          representation of the key (eg. for down-arrow ALTKEY='v').  =
C=                                                                      =
C= CALL:                                                                =
C=      KEY = READ_KEY (OPTIONS,PROMPT,ALTKEY[,[TIMEOUT]])              =
C=                                                                      =
C= INPUT:                                                               =
C=      OPTIONS  (Ch*(*)) - a list of options where:                    =
C=              option  / min abbrev  - description                     =
C=              -----------------------------------------------         =
C=              'ECHO' /'EC'    - echo the character read in;           =
C=              'KEYPAD' /'K'   - means turn the keypad on;             =
C=              'NOKEYPAD' /'NOK' - means turn the keypad off;          =
C=              'LC2UC' /'LC'   - translate all lower to upper case;    =
C=              'NOCTRL' /'NOC' - echo all alphanumerics;               =
C=              'NOESC' /'NOE'  - means don't try to interpret ESC      =
C=                                sequences;                            =
C=              'PROMPT' /'PR'  - write PROMPT before reading;          =
C=              'PURGE' /'PU'   - purge the type ahead buffer before    =
C=                                reading;                              =
C=              'TIMEOUT' /'TI' - if TIMEOUT seconds have elapsed       =
C=                                with out the user hitting a key:      =
C=                                READ_KEY=<null>, ALTKEY=<null>;       =
C=      ALTKEY  (Ch*1) - if READ_KEY is returns as <null>, then ALTKEY  =
C=              returns with a symbolic representation of the key hit:  =
C=              see above for symbolic values returned;                 =
C=      PROMPT (Ch*(*)) - the prompt to be written before reading       =
C=              the key (required even if no 'PROMPT' is mentioned);    =
C=      TIMEOUT (I*4)  - (optional) the number of seconds to wait       =
C=              before returning to the caller if the user has not      =
C=              hit a key.                                              =
C=                                                                      =
C= OUTPUT:                                                              =
C=      READ_KEY  (Ch*1) - the key that was hit, unless an <ESC>        =
C=              sequence was invoked and interpretted in which case     =
C=              it is <null>;                                           =
C=      ALTKEY  (Ch*1) - if READ_KEY=<null>, then ALTKEY returns with   =
C=              the symboic key (see above); else if READ_KEY=<ESC>     =
C=              and 'NOESC' was not invoked then ALTKEY returns with    =
C=              the character on which this routine choked; else ALTKEY =
C=              returns as <null>.                                      =
C=                                                                      =
C= EXAMPLES:                                                            =
C=              CHARACTER*1 KEY,ALTKEY,READ_KEY                         =
C=              KEY = READ_KEY (' ',' ',ALTKEY)                         =
C=                      !doesn't echo, translates the arrow keys, but   =
C=                      !does not translate the keypad keys (except     =
C=                      !for the PF keys).                              =
C=              KEY = READ_KEY ('PROMPT KEYPAD','Enter: ',ALTKEY)       =
C=                      !doesn't echo, translate both arrows and keypad =
C=                      !keys, and displays the prompt "Enter: " before =
C=                      !reading.                                       =
C=              KEY = READ_KEY ('PR K','Enter: ',ALTKEY)                =
C=                      !same as previous example.                      =
C=              KEY = READ_KEY ('TIMEOUT',' ',ALTKEY,45)               =
C=                      !doesn't echo, translates only arrows and PFs,  =
C=                      !and returns after 45 seconds if there has been =
C=                      !no imput.                                      =
C=                                                                      =
C= NOTES:                                                               =
C=          1)  For more detailed, though less lucid, information,      =
C=              refer to the "VAX/VMS I/O user's Guide (Volume 1)",     =
C=              section 9.4.                                            =
C=                                                                      =
C  Modified Apr 18/88 by F.W. Jones: channel assigned to terminal
C  placed in common block for use by VST_CROSSHAIR.
C  Modified Nov 21/90 by J.Chuma: allow F14 key (same as CTRL-A)
c  Modified 24-Jul-2000 by TMR: Added Implicit none.
c
C========================================================================
C  
	IMPLICIT NONE
CJUL2000        IMPLICIT INTEGER*4 (A-Z)

C...	passed variables
        CHARACTER*(*) OPTIONS 
        CHARACTER*1 ALTKEY
        CHARACTER*(*) PROMPT 
        INTEGER*4 TIMEOUT

c...	functions
        INTEGER*4 SS$_NORMAL, SS$_TIMEOUT
        EXTERNAL SS$_NORMAL, SS$_TIMEOUT
	INTEGER*4 SYS$ASSIGN, SYS$QIOW, SYS$OUPTUT

c...	local variables

        INTEGER*4 CHAN
	COMMON/READ_KEY_BLK/CHAN

        CHARACTER*1 NULL, CR, ESC
        PARAMETER (NULL=CHAR(0), CR=CHAR(13), ESC=CHAR(27))
        INTEGER*4 LENRK 
        PARAMETER (LENRK = 1)
        INTEGER*4 IO_READPROMPT
        PARAMETER (IO_READPROMPT=X'37')
        INTEGER*4 IO_M_NOECHO, IO_M_NOFILTR, IO_M_CVTLOW, IO_M_PURGE,
     +            IO_M_TIMED
        PARAMETER (IO_M_NOECHO=X'40',  IO_M_NOFILTR=X'200',
     +            IO_M_CVTLOW=X'100', IO_M_PURGE=X'800',
     +            IO_M_TIMED=X'80')

	INTEGER*4 ISTAT, OPTS, WAIT, LENPR, IFORCE

C  
C  Make sure that we have a channel (maybe one from a previous call)
C  
        IF (CHAN .EQ. 0) THEN
            ISTAT = SYS$ASSIGN ('TT:',CHAN,,)
            IF (ISTAT.NE.%LOC(SS$_NORMAL)) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
C  
C  Set the options up:
C  
        OPTS = IOR(IO_READPROMPT,IO_M_NOFILTR)
        IF (INDEX(OPTIONS,'EC') .EQ. 0) OPTS = IOR(OPTS,IO_M_NOECHO)
        IF (INDEX(OPTIONS,'K') .NE. 0) WRITE(6,62) ESC   !Turn keypad on
        IF (INDEX(OPTIONS,'NOK') .NE. 0) WRITE(6,61) ESC!Turn keypad off
        IF (INDEX(OPTIONS,'LC') .NE. 0) OPTS = IOR(OPTS,IO_M_CVTLOW)
        IF (INDEX(OPTIONS,'PU') .NE. 0) OPTS = IOR(OPTS,IO_M_PURGE)
        IF (INDEX(OPTIONS,'TI') .EQ. 0) THEN
            WAIT = 0
        ELSE
            OPTS = IOR(OPTS,IO_M_TIMED)
            WAIT = TIMEOUT
        ENDIF
C  
C  Read the key; with or without a prompt.
C  
        READ_KEY = NULL
        IF (INDEX(OPTIONS,'PR') .EQ. 0) THEN
            LENPR = 0
            ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +          %REF(READ_KEY), %VAL(LENRK), %VAL(WAIT), , 
     +          %REF(' '), %VAL(LENPR))
        ELSE
            LENPR = LEN(PROMPT)
            ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +          %REF(READ_KEY), %VAL(LENRK), %VAL(WAIT), , 
     +          %REF(PROMPT), %VAL(LENPR))
        ENDIF
        IF (ISTAT .EQ.%LOC(SS$_TIMEOUT)) THEN
            READ_KEY = NULL
            ALTKEY = NULL
            RETURN
        ENDIF
C        IF (ISTAT .NE. %LOC(SS$_NORMAL)) CALL LIB$SIGNAL(%VAL(ISTAT))
        IF (ISTAT .NE. %LOC(SS$_NORMAL)) THEN
            CALL SYSMSG(ISTAT,SYS$OUPTUT)
            IFORCE=1
            RETURN
        ENDIF
C  
C  Now echo if 'NOCTRL' was set, and try to interpret the <ESC> sequence
C  in 'NOESC' was not set.
C  
        IF (INDEX(OPTIONS,'NOC') .NE. 0    .AND. 
     +           ICHAR(READ_KEY) .GE. ICHAR(' '))
     +      WRITE (6,60) READ_KEY
        IF (INDEX(OPTIONS,'NOE') .EQ. 0) THEN
            OPTS = IAND(OPTS,NOT(IO_M_PURGE))
            OPTS = IAND(OPTS,NOT(IO_M_CVTLOW))
            IF (READ_KEY .EQ. ESC) THEN
                LENPR = 0
                ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +                  %REF(ALTKEY), %VAL(LENRK), %VAL(WAIT), , 
     +                  %REF(' '), %VAL(LENPR))
                IF (ALTKEY .EQ. '[') THEN
                    ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +                  %REF(ALTKEY), %VAL(LENRK), %VAL(WAIT), , 
     +                  %REF(' '), %VAL(LENPR))
C  
C  We think an arrow key has been hit.
C  
                    IF (ALTKEY .EQ. 'A') THEN
                        ALTKEY = '^'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'B') THEN
                        ALTKEY = 'v'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'C') THEN
                        ALTKEY = '>'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'D') THEN
                        ALTKEY = '<'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. '2') THEN
                      ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +                  %REF(ALTKEY), %VAL(LENRK), %VAL(WAIT), , 
     +                  %REF(' '), %VAL(LENPR))
                      IF (ALTKEY .EQ. '6') THEN
                        ISTAT = SYS$QIOW (,%VAL(CHAN),%VAL(OPTS), , , , 
     +                   %REF(ALTKEY), %VAL(LENRK), %VAL(WAIT), , 
     +                   %REF(' '), %VAL(LENPR))
                        IF (ALTKEY .EQ. '~') THEN
                          ALTKEY = 'n'              ! found F14 key
                          READ_KEY = NULL
                        END IF
                      END IF
                    ENDIF
                ELSE IF (ALTKEY .EQ. 'O') THEN
                    ISTAT = SYS$QIOW (, %VAL(CHAN), %VAL(OPTS), , , , 
     +                  %REF(ALTKEY), %VAL(LENRK), %VAL(WAIT), , 
     +                  %REF(' '), %VAL(LENPR))
C  
C  We think a keypad key has been hit.
C  
                    IF (ICHAR(ALTKEY) .GE. ICHAR('p') .AND. 
     +                  ICHAR(ALTKEY) .LE. ICHAR('y')) THEN
                        ALTKEY = CHAR(
     +                          ICHAR(ALTKEY) - ICHAR('p') + ICHAR('0'))
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'l') THEN
                        ALTKEY = ','
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'm') THEN
                        ALTKEY = '-'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'n') THEN
                        ALTKEY = '.'
                        READ_KEY = NULL
                    ELSE IF (ALTKEY .EQ. 'M') THEN
                        ALTKEY = CR
                        READ_KEY = NULL
                    ELSE IF (ICHAR(ALTKEY) .GE. ICHAR('P') .AND. 
     +                          ICHAR(ALTKEY) .LE. ICHAR('S')) THEN
                        READ_KEY = NULL
                    ENDIF
                ENDIF
            ELSE
                ALTKEY = NULL
            ENDIF
C  
C  If we get to here, we're not suppose to know about <ESC> sequences.
C  
        ELSE
            ALTKEY = NULL
        ENDIF   
        RETURN
C  
C  FORMAT statments.
C  
   60   FORMAT('+',A,$)
   61   FORMAT('+',A,'>',$)
   62   FORMAT('+',A,'=',$)
        END
