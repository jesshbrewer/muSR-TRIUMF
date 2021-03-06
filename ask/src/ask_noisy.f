C============================================================================
	SUBROUTINE BASEASK (PROMPT)
C============================================================================
C...Evolved descendant of ASKFOR (FREAD-with-Prompt):  
C   New version includes Expression Evaluation via    LOC_EVAL
C   and Indirect File processing (nesting up to 10 deep); 
C   also list processing, "Comments and \Command lines (e.g., \CALC)
C	
C...Deliver a one-line question stored in PROMPT (must not be empty!)
C   and get back (on the same line) a list of    up to 20 
C   Fields of Answer(s),                         ========
C   each Field specified as either a single variable or an array; 
C   any Field may be: 
C
C	'D' (Real*8)		'R' (Real*4)
C	'I' (Integer*4)		'M' (Integer*2)
C	'S' (character Array)   'L' (Logical*4)
C       'C' (Complex*8)         'K' (Logical*2)
C	'A' (character String)  'W' ("Word" space-delimited String)
C
C   in any sequence.
C   Thus (e.g.)
C   	'3R' calls for an array of 3 Real*4 variables
C   	'D' calls for a single Real*8 variable.  
C   	'80S' calls for a character array of 80 characters [character*1 str(80)]
C   	'80A' calls for a character string of 80 characters [character str*(80)]
c
C------------------------------------------------------------------------------
C...Currently Recognized COMMANDS:
C
C   \H = HELP.
C   ?? = HELP.
C   \C = CALC mode: interactive (breaks out of ind. file) until blank line.  
C   \= to perform some CALC operation (arithmetic, assignment, ...) directly.  
C   \T = TYPE a File.
C   \D = Add Delimiter character.
C   \-D = Delete Delim. char.
C   \E = ECHO indirect dialogue.
C   \-E = Turn ECHO OFF.
C   @ = Read from Ind. File 
C   \@ = Resume reading from Ind. File (if any).
C   \-@ = Suspend reading from Ind. File (if any).
C   @+ = Create a "capture" Indirect File (record keystrokes)
C   @- = Close to "capture" Indirect Ind. File (if any).
c   \N  =make capture file noisy, by including prompt as a comment.
c   \-N = turn off noisy mode for capture file.
C   \-I = Indirect mode OFF. 
C   \L <n> = Loop back to the beginning of this Ind. File (if any) <n> times.
c	New Nov 2000: @#
C   @# = Save ASK constants to a file, suitable to use as an indirect file.
CRemovedJul00   \W <n> = Wait <n> seconds, using Kost's SLEEP3 subroutine. 
C------------------------------------------------------------------------------
C...Associated Entry Points for Programmed Control:
c
C	Where * is a number of fields between 1 and 20, and there are the same 
c	number of 'FORMAT', VAR pairs:
C   CALL ASK*(PROMPT, 'FORMAT', VAR, ...)
c	Plain ask. If value is not entered by the user, zero or ' ' is returned.
C   CALL ASK*S(PROMPT, 'FORMAT', VAR, ..., NCH)
c	Plain ask with number of characters entered returned in NCH. 
c	If value is not entered by the user, zero or ' ' is returned.
C   CALL ASK*D(PROMPT, 'FORMAT', VAR, ...)
c	Default ask. If value is not entered by the user, the original
c	value of VAR is retained.
C
C...CALL SET_ASK_FILE (Filename) to redirect Input to Filename. 
C				 (same as "@Filename" in input stream; 
C				 can be nested up to 10 deep.)
C
C...CALL SET_ASK_INDIR (IND, LUN) to turn Indirect processing via LUN 
C			on (IND = .TRUE.) or off (IND = .FALSE.)
C			(in Local Indirect mode, IND = .FALSE. 
C			closes current file and decrements pointer.)
C
C...CALL SET_ASK_DELIMS (', !...') to define Delimiter 
C			characters recognized by ASK (',' recommended!)
C
C...CALL SET_ASK_ECHO (ECH) to turn Indirect-mode Echoing 
C			on (ECH = .TRUE.) or off (ECH = .FALSE.)
C
C...CALL INIT_HELP('path', 'hlp_fil', 'level') to select the help file
c	which otherwise defaults to USERS or ASK.
c	Leave off the path and extention on the file name 'hlp_fil'.TR July 2000
c
C		-- Jess Brewer (August 1985)
C		-- revised August 1987 to take up to 20 "fields".  
C               -- revised DA * dimensioning
C-----------------------------------------------------------------------

c	Force typing so that we can find which variables
c	are stomping on LUN_CR
      implicit none
      include 'help.cmn'

      CHARACTER PROMPT*(*), FILE_NAME*(*), DELIMS*(*)
	CHARACTER*1 L1(*), L2(*), L3(*), L4(*), L5(*), 
     >     L6(*), L7(*), L8(*), L9(*), L10(*), 
     >     L11(*), L12(*), L13(*), L14(*), L15(*), 
     >     L16(*), L17(*), L18(*), L19(*), L20(*)
	REAL*4 A1(*), A2(*), A3(*), A4(*), A5(*), 
     >       A6(*), A7(*), A8(*), A9(*), A10(*), 
     >       A11(*), A12(*), A13(*), A14(*), A15(*), 
     >       A16(*), A17(*), A18(*), A19(*), A20(*)
	CHARACTER LINE*8192, CHSTR*132
cNoChArray	CHARACTER*1 SLINE(8192), STRING(132)
cNoChArray	EQUIVALENCE (LINE,SLINE), (CHSTR,STRING)
cNoChArray	CHARACTER*1 FILE_LINE(80)
	CHARACTER FILE_LINE*80

	REAL*8 VALUE
	LOGICAL PARSEERR, TELL_NCH
	LOGICAL USE_DEF 
	INTEGER NFIELDS, NCH, NCH_Q
	CHARACTER*131 NEWPROMPT
C       
	LOGICAL INDIRECT
	INTEGER*4 MY_LUN(10), JLUN, LUN_EXT
	COMMON /ASK_LUNS/ JLUN, MY_LUN, INDIRECT, LUN_EXT
C       
	LOGICAL LOC_EVAL, LOC_BATCH
	LOGICAL ONLINE, BATCH, SUSPEND_IND
	LOGICAL CREATE, ECHO, CALC
	LOGICAL NOISY_CR
	LOGICAL INDIR, ECH, IN_USE

c..	functions	TR
cTR	integer trimlen
	integer trimstop
cTR	integer trimstart

c...	local variables
        INTEGER LOC, NEWLOC, ILOOP, NLOOP
     	INTEGER LUN_CR, MY_LUN_0, MYLUNSAV, JLUNSAV, LUN_TYP, LUNEXT 

cHELP        save HLP_FIL*80, KEY_WD*40
C??	SAVE LOC
	SAVE LUN_CR, MYLUNSAV, MY_LUN_0, JLUNSAV, LUN_TYP
	SAVE ILOOP, NLOOP
	SAVE CREATE, ECHO, SUSPEND_IND, CALC
	SAVE ONLINE, BATCH, NOISY_CR
        DATA HLP_FIL /' '/, KEY_WD /' '/
	DATA INDIRECT /.FALSE./, CREATE /.FALSE./, ECHO /.TRUE./
	DATA SUSPEND_IND /.FALSE./
	DATA CALC /.FALSE./
	DATA MY_LUN_0 /83/
	DATA NOISY_CR /.TRUE./
c       
	data iloop /0/
	
C============================================================================
	ENTRY ASK0 (PROMPT)
	NFIELDS = 0
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK1 (PROMPT, L1,A1)
	NFIELDS = 1
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK2 (PROMPT, L1,A1, L2,A2)
	NFIELDS = 2
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK3 (PROMPT, L1,A1, L2,A2, L3,A3)
	NFIELDS = 3
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK4 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4)
	NFIELDS = 4
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK5 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5)
	NFIELDS = 5
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK6 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6)
	NFIELDS = 6
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK7 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7)
	NFIELDS = 7
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK8 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8)
	NFIELDS = 8
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK9 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9)
	NFIELDS = 9
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK10 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10)
	NFIELDS = 10
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK11 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11)
	NFIELDS = 11
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK12 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12)
	NFIELDS = 12
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK13 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13)
	NFIELDS = 13
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK14 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14)
	NFIELDS = 14
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK15 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15)
	NFIELDS = 15
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK16 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16)
	NFIELDS = 16
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK17 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17)
	NFIELDS = 17
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK18 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18)
	NFIELDS = 18
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK19 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19)
	NFIELDS = 19
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK20 (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19, L20,A20)
	NFIELDS = 20
	TELL_NCH = .FALSE.
	USE_DEF = .FALSE.
	GOTO 1
	

C============================================================================
c	Note: Number of ASK*D limited by TELL*D. Unreasonable to 
c	have too many default values in the prompt!
	ENTRY ASK1D (PROMPT, L1,A1)
	NFIELDS = 1
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK2D (PROMPT, L1,A1, L2,A2)
	NFIELDS = 2
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK3D (PROMPT, L1,A1, L2,A2, L3,A3)
	NFIELDS = 3
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK4D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4)
	NFIELDS = 4
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK5D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5)
	NFIELDS = 5
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK6D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6)
	NFIELDS = 6
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK7D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7)
	NFIELDS = 7
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK8D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8)
	NFIELDS = 8
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK9D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9)
	NFIELDS = 9
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
	ENTRY ASK10D (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10)
	NFIELDS = 10
	TELL_NCH = .FALSE.
	USE_DEF = .TRUE.
	GOTO 1
	
C============================================================================
	ENTRY ASK0S (PROMPT, NCH_Q)
	NFIELDS = 0
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK1S (PROMPT, L1,A1, NCH_Q)
	NFIELDS = 1
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK2S (PROMPT, L1,A1, L2,A2, NCH_Q)
	NFIELDS = 2
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK3S (PROMPT, L1,A1, L2,A2, L3,A3, NCH_Q)
	NFIELDS = 3
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK4S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4, NCH_Q)
	NFIELDS = 4
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK5S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, NCH_Q)
	NFIELDS = 5
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK6S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, NCH_Q)
	NFIELDS = 6
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK7S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, NCH_Q)
	NFIELDS = 7
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK8S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, NCH_Q)
	NFIELDS = 8
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK9S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, NCH_Q)
	NFIELDS = 9
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK10S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, NCH_Q)
	NFIELDS = 10
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK11S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, NCH_Q)
	NFIELDS = 11
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK12S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, NCH_Q)
	NFIELDS = 12
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK13S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, NCH_Q)
	NFIELDS = 13
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK14S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, NCH_Q)
	NFIELDS = 14
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK15S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15, NCH_Q)
	NFIELDS = 15
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK16S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, NCH_Q)
	NFIELDS = 16
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK17S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, NCH_Q)
	NFIELDS = 17
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK18S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, NCH_Q)
	NFIELDS = 18
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK19S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19, NCH_Q)
	NFIELDS = 19
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
	ENTRY ASK20S (PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19, L20,A20, 
     >       NCH_Q)
	NFIELDS = 20
	TELL_NCH = .TRUE.
	USE_DEF = .FALSE.
	GOTO 1
	
C============================================================================
 1	CONTINUE
C----------------------------------------------------------------------------

	BATCH = LOC_BATCH()	! Check on every call.
	ONLINE = .NOT. BATCH
	NEWPROMPT = PROMPT

C...    Check if this is a comment line:
       	IF (NFIELDS .LT. 1) THEN
	   IF (INDIRECT .OR. JLUN .GT. 0) RETURN
	   WRITE (*,1001) PROMPT
 1001	   FORMAT (' ',A)
c	   WRITE (*,1001) ' ', PROMPT
c 1001	   FORMAT (1A)
	   RETURN		! No answer desired, no input needed.
	ENDIF
C----------------------------------------------------------------------------
 1234	CONTINUE
 3000	LOC = 1
C...    Get input line:
C
 3001	IF (INDIRECT) THEN 
	   READ (LUN_EXT,3111,END=3100) CHSTR
	ELSE IF (JLUN .GT. 0) THEN
	   READ (MY_LUN(JLUN),3111,END=3100) CHSTR
	ELSE
	   IF (USE_DEF) THEN
c	     WRITE (*,*) 'ASK*D: making new prompt from ', 
c     &		prompt(1:trimstop(prompt))
	     IF (NFIELDS .EQ. 1) THEN
cDEBUG	       call tell1('ask: pre-tell1d nch=', 'i', nch)
cDEBUG	       call tell1('ASK*D: Itty bit ', L1,A1)
	       CALL TELL1D(PROMPT, L1,A1, NEWPROMPT)
cDEBUG	       call tell1('ASK*D: Itty bit ', L1,A1)
cDEBUG	       call tell1('ask: post-tell1d nch=', 'i', nch)
	     ELSE IF (NFIELDS .EQ. 2) THEN
	       CALL TELL2D(PROMPT, L1,A1, L2,A2, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 3) THEN
	       CALL TELL3D(PROMPT, L1,A1, L2,A2, L3,A3, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 4) THEN
	       CALL TELL4D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 5) THEN
	       CALL TELL5D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 6) THEN
	       CALL TELL6D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, L6,A6, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 7) THEN
	       CALL TELL7D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, L6,A6, L7,A7, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 8) THEN
	       CALL TELL8D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, L6,A6, L7,A7, L8,A8, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 9) THEN
	       CALL TELL9D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, NEWPROMPT)
	     ELSE IF (NFIELDS .EQ. 10) THEN
	       CALL TELL10D(PROMPT, L1,A1, L2,A2, L3,A3, L4,A4,
     >           L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, NEWPROMPT)
	     ELSE 
		CALL TELL1('ASK: Hey Programmer! Only 1-10 for Tell*D!',
     &			'I', NFIELDS)
	     ENDIF		     
c	     WRITE (*,*) 'ASK*D: making newprompt ', 
c     &		NEWPROMPT(1:TRIMSTOP(NEWPROMPT)+1), '...'
CCCC	     PROMPT = NEWPROMPT(1:TRIMSTOP(NEWPROMPT)+1)
c	     WRITE (*,*) 'ASK*D: making newprompt ', prompt, '...'
	   ENDIF

cDEBUG	   IF (USE_DEF .AND. (NFIELDS .EQ. 1)) THEN
cDEBUG	       call tell1('ASK*D: PRE-GETINPUT Itty bit ', L1,A1)
cDEBUG	   ENDIF

	   CALL GETINPUT (NEWPROMPT(1:TRIMSTOP(NEWPROMPT)+1), CHSTR)

cDEBUG	   IF (USE_DEF .AND. (NFIELDS .EQ. 1)) THEN
cDEBUG	       call tell1('ASK*D: POST-GETINPUT Itty bit ', L1,A1)
cDEBUG	   ENDIF

	END IF

 3111	FORMAT (A132)
	NCH = LEN(CHSTR)
	CALL CHTRIM_BLNK (NCH,CHSTR)
cDEBUG	call tell1('ask: after CHTRIM_BLNK nch=', 'i', nch)
CNoChArray	CALL TRIM_BLNK (NCH,STRING)

C...    Save to capture file if it is something the user typed:
	IF (CREATE .AND. .NOT. (INDIRECT .OR. JLUN .GT. 0)) THEN
C	  write(*,*) 'ask: create ', LUN_CR, 
C     &		' ', newprompt(1:min(30,len(prompt))), '...'
cNoChArray	  IF (.NOT. (STRING(1) .EQ. '@' .AND. STRING(2) .EQ. '-')) THEN
	  IF (CHSTR(1:2) .NE. '@-') THEN
	    IF (NOISY_CR) THEN
	      WRITE (LUN_CR,3115) newPROMPT(1:TRIMSTOP(NEWPROMPT)+1)
c	      WRITE (LUN_CR,3115) 
c     &		'!! '//newPROMPT(1:MIN(TRIMLEN(newPROMPT),77))
c	      write (*,*) 'ASK: being noisy'
	    else
c	      write (*,*) 'ASK: not being noisy'
	    ENDIF
cNoChArray	    if ((string(1) .eq. '\') .or. (string(1) .eq. '@') .or. 
cNoChArray     &		(string(1) .eq. '?') .or. (string(1) .eq. '$') ) then
	    if ((CHSTR(1:1) .eq. '\') .or. (CHSTR(1:1) .eq. '@') .or. 
     &		(CHSTR(1:2) .eq. '??') .or.  (CHSTR(1:1) .eq. '?') .or.
     &		(CHSTR(1:1) .eq. '$') ) then
	      WRITE (LUN_CR,3113) CHSTR(1:NCH)
cNoChArray	      WRITE (LUN_CR,3113) (STRING(I),I=1,NCH)
	    else
	      WRITE (LUN_CR,3112) CHSTR(1:NCH)
cNoChArray	      WRITE (LUN_CR,3112) (STRING(I),I=1,NCH)
c	      write(*,*) 'append "', CHSTR(1:NCH), '" to script.'
	    endif
	  ENDIF
	END IF
 3112	FORMAT (' ',A)
 3113	FORMAT (A)
coffset 3112	FORMAT (' ',A131)
cOffset 3113	FORMAT (A132)
cNoChArray 3112	FORMAT (' ',132A1)
cNoChArray 3113	FORMAT (132A1)
 3115   FORMAT('!! ', A)
cOffset 3115   FORMAT('!! ', A77)
c 3115   FORMAT(A80)

C...    Echo back if requested
	IF ((INDIRECT  .OR.  JLUN .GT. 0)  .AND.  ECHO) THEN
	   IF (LOC .EQ. 1) THEN
	      WRITE (*,3113) CHSTR(1:NCH)
cNoChArray     >   	WRITE (*,3112) (STRING(I),I=1,NCH)
	   ELSE  IF (LOC .GT. 1) THEN
	      WRITE (*,3114) CHSTR(1:NCH)
cNoChArray     >   	WRITE (*,3114) (STRING(I),I=1,NCH)
	   ENDIF
	END IF
 3114	FORMAT ('+',A)
cOffset 3114	FORMAT ('+',A131)
cNoChArray 3114	FORMAT ('+',132A1)

C...    Deal with continuation lines   
	NEWLOC = LOC + NCH - 1
	IF (NEWLOC .LE. 8192) THEN
c	  WRITE (*,*) 'ASK: LINE(LOC:) = CHSTR',CHSTR(1:TRIMSTOP(CHSTR))
	  LINE(LOC:) = CHSTR ! Copy in this input...
c	  WRITE (*,*) 'ASK: LINE(LOC:) = CHSTR', LINE(1:TRIMSTOP(LINE))
	ENDIF
	LOC = NEWLOC
	IF (LINE(LOC:LOC) .EQ. '\') GO TO 3001 ! Continuation char = \

C...    Line captured, so jump to processing
	IF (LINE(1:1) .NE. '@') GO TO 3900
	GO TO 3200
C-----------------------------------------------------------------------
C...    EOF on Input: 
C       
 3100	IF (ILOOP .GT. 0) THEN
	   IF (ILOOP .GE. NLOOP) GO TO 3101
	   ILOOP = ILOOP + 1
	   IF (JLUN .GE. 1) THEN
	      REWIND MY_LUN(JLUN)
cccc	      write (*,*) 'ask: loop jlun', iloop, MY_LUN(JLUN)
	      GO TO 1234
	   END IF
	   IF (INDIRECT) THEN
cccc	      write (*,*) 'ask: loop LUN_EXT', iloop, LUN_EXT
	      REWIND LUN_EXT
	      GO TO 1234
	   END IF
	END IF
 3101	ILOOP = 0
	IF (INDIRECT) GO TO 3110
	IF (JLUN .GT. 0) GO TO 3120
	IF (BATCH) STOP		! Otherwise infinite loop!
	IF (CREATE) GO TO 3220	! Close Created Indirect file.  
	GO TO 1234		! Disregard gratuitous ^Z's.  
C       
 3110	INDIRECT = .FALSE.	! LUN_EXT should NOT be Closed by ASK!
	IF (ECHO) GO TO 3000
	GO TO 1234		! Troubles may arise in Batch.
C       
 3120	CLOSE (MY_LUN(JLUN))	! Close current Ind. file.
	JLUN = MAX0(0,JLUN-1)	! Decrement pointer.
	IF (ECHO) GO TO 3000	! (Revert to previous Ind. file.)
	GO TO 1234
C------------------------------------------------------------------------------
C...    Indirect file processing:
C       
 3200	IF (LINE(2:2) .EQ. '-') GO TO 3220 ! Close Created Indirect file.
	INDIRECT = .FALSE.
	IF (LINE(2:2) .EQ. '+') GO TO 3210 ! Create a new Indirect file.
	IF (LINE(2:2) .EQ. '#') GO TO 3215 ! Create a new Indirect file.
C       
C...    Plain '@'   Open a new Indirect File: 
C       
	JLUN = JLUN + 1		! Increment pointer.
	IF (JLUN .GT. 10) THEN 
	   WRITE (*,3209) 
 3209	   FORMAT (/,' *** ASK:  Attempt to nest Indirect',
     >   	' files more than 10 deep ***',/)
	   JLUN = 10
	   IF (BATCH) STOP
	END IF
	MY_LUN(JLUN) = MY_LUN_0
c	 ! Search Downward for an unused LUN:
 3201	MY_LUN(JLUN) = MY_LUN(JLUN) - 1
	INQUIRE (UNIT=MY_LUN(JLUN), OPENED=IN_USE)
	IF (IN_USE) GO TO 3201
	OPEN (MY_LUN(JLUN), FILE=LINE(2:80),
cTR     >		SHARED,  
cALpha doesn't like readonly      >		SHARED, READONLY, 
ccomment out for linux       >		SHARED, READONLY, 
     >       STATUS='OLD', ERR=3202)
ccc	WRITE (*,*) 'ASK: OPENED READONLY SHARED ', MY_LUN(JLUN), LINE(2:40)
	GO TO 3000
C       
 3202	IF (ONLINE) WRITE (*,3219) JLUN, MY_LUN(JLUN), LINE(2:33)
 3219	FORMAT (' *** ASK:  Cannot Open #',I2,
     >       ' Indirect file on LUN',I3,': ',A32)
	CLOSE (MY_LUN(JLUN), ERR=3203) ! Just in case. 
 3203	JLUN = MAX0(0,JLUN-1)	! Decrement pointer.
	GO TO 1234
C       
C...    Create a new Indirect File: 
C       
 3210	IF (CREATE) CLOSE (LUN_CR) ! In case one is already Created.
	CREATE = .TRUE. 
	LUN_CR = MY_LUN_0 - 1
 3211	LUN_CR = LUN_CR + 1	! Search Higher LUNs for an unused one.
	INQUIRE (UNIT=LUN_CR, OPENED=IN_USE)
	IF (IN_USE) GO TO 3211
	OPEN (LUN_CR, FILE=LINE(3:80), 
cTR     >          SHARED, 
c comment out for linux       >          SHARED, 
     >       STATUS='UNKNOWN', ERR=3220)
ccc	WRITE (*,*) 'ASK: CREATE SHARED ', LUN_CR, LINE(2:40)
	GO TO 1234
C       
C...    Create a new Indirect File containing ASK constants: 
C       
 3215	call evalsave(LINE(3:80), .true.)
	GO TO 1234
C       
C...    Close Created Indirect File: 
C       
 3220	CLOSE (LUN_CR)
	CREATE = .FALSE. 
	GO TO 1234
C----------------------------------------------------------------------------
C...    Got input line.  Now process it:
C       
 3900	IF (LINE(1:1) .EQ. '!') GO TO 1234 ! Ignore this line!
	IF (LINE(1:1) .EQ. '"') GO TO 5000 ! Issue a COMMENT.
	IF (LINE(1:1) .EQ. '\') GO TO 6000 ! Process a COMMAND. 
	IF (LINE(1:1) .EQ. '$') GO TO 7000 ! DCL Command.
	IF (LINE(1:2) .EQ. '??') GO TO 8000 ! Access help file 
C----------------------------------------------------------------------------
C...    Line contains actual input data (not control instructions)!  EVALuate it:
C       
c	write(*,*)'In ask, Line contains actual input' !debug
	CALL PARSE (LINE)
	IF (NFIELDS .GE. 1) then
c	  call tell1('ask: pre-PARSEVALUE', L1,A1) ! debug
	  CALL PARSEVALUE(L1,A1,USE_DEF,PARSEERR)
c	  call tell1('ask: post-PARSEVALUE', L1,A1) ! debug
	endif
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 2) CALL PARSEVALUE(L2,A2,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 3) CALL PARSEVALUE(L3,A3,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 4) CALL PARSEVALUE(L4,A4,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 5) CALL PARSEVALUE(L5,A5,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 6) CALL PARSEVALUE(L6,A6,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 7) CALL PARSEVALUE(L7,A7,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 8) CALL PARSEVALUE(L8,A8,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 9) CALL PARSEVALUE(L9,A9,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 10) CALL PARSEVALUE(L10,A10,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 11) CALL PARSEVALUE(L11,A11,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 12) CALL PARSEVALUE(L12,A12,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 13) CALL PARSEVALUE(L13,A13,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 14) CALL PARSEVALUE(L14,A14,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 15) CALL PARSEVALUE(L15,A15,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 16) CALL PARSEVALUE(L16,A16,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 17) CALL PARSEVALUE(L17,A17,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 18) CALL PARSEVALUE(L18,A18,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 19) CALL PARSEVALUE(L19,A19,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (NFIELDS .GE. 20) CALL PARSEVALUE(L20,A20,USE_DEF,PARSEERR)
	IF (PARSEERR) GO TO 1234
	IF (TELL_NCH) NCH_Q = NCH
	RETURN
C---------------------------------------------------------------
C...    Write out a Comment (if in INDIRECT and ONLINE modes):
C       
 5000	IF (BATCH) GO TO 1234
	IF (INDIRECT  .OR.  JLUN .GT. 0) THEN
            WRITE (*,'(1X, A128)') LINE(1:NCH)
	ENDIF
cNoChArray	IF (INDIRECT  .OR.  JLUN .GT. 0)
cNoChArray     >       WRITE (*,5555) (SLINE(I),I=2,NCH)
cNoChArray 5555	FORMAT (1X,128A1)
	GO TO 1234
C---------------------------------------------------------------
C...    Process an ASK  Command:
C       
cNoChArray 6000	CALL CASE_UPPER (2,SLINE(2))
 6000	CALL STR_UPPER (LINE(2:3))
	IN_USE = CALC
	CALC = .FALSE.
	IF (LINE(2:2) .EQ. 'C') GO TO 6600 ! \C = CALC mode.
	IF (LINE(2:2) .EQ. '=') GO TO 6601 ! \= -- Immed. CALC operation.
	IF (LINE(2:2) .EQ. 'T') GO TO 6800 ! \T = TYPE a file.
	IF (LINE(2:2) .EQ. 'D') GO TO 6400 ! \D = Add Delimiter character.
	IF (LINE(2:2) .EQ. 'G') GO TO 6413 ! \G = Add ignored character.
	IF (LINE(2:2) .EQ. 'E') GO TO 6100 ! \E = ECHO indirect dialogue.
CCCC    IF (LINE(2:2) .EQ. 'J') GO TO 6200	! \J n = JUMP in Ind. File.
	IF (LINE(2:2) .EQ. '@') GO TO 6750 ! \@ = Resume Ind. File 
	IF (LINE(2:2) .EQ. 'N') GO TO 6150 ! \N = Noisy Create ind file
	IF (LINE(2:2) .EQ. 'H') GO TO 6900 ! \H = HELP.
	IF (LINE(2:2) .EQ. 'L') GO TO 6010 ! \L n = Loop n times (REW I.F.)
cJul00	IF (LINE(2:2) .EQ. 'W') GO TO 6020 ! \W n = Wait n seconds....
	IF (LINE(2:2) .NE. '-') GO TO 6300
	IF (LINE(3:3) .EQ. 'D') GO TO 6500 ! \-D = Delete Delim. char.
	IF (LINE(3:3) .EQ. 'G') GO TO 6513 ! \-G = Delete Ignored char.
	IF (LINE(3:3) .EQ. 'E') GO TO 6110 ! \-E = Turn ECHO OFF.
	IF (LINE(3:3) .EQ. '@') GO TO 6700 ! \-@ = Suspend Ind. File 
	IF (LINE(3:3) .EQ. 'N') GO TO 6160 ! \-N = quiet Create ind file
	IF (LINE(3:3) .EQ. 'I') GO TO 6770 ! \-I = Indirect mode OFF. 
	GO TO 6300
C--------------------------------------------------------------------
C...    Rewind & Continue reading from current Indirect File (if any):  
C       
c	! Treat subsequent arrival at \L as EOF.
 6010	IF (ILOOP .GT. 0) GO TO 3100 
	NLOOP = 0
	IF (LOC_EVAL(LINE(3:LOC), VALUE)) NLOOP = VALUE
	IF (JLUN .GE. 1) THEN
	   REWIND MY_LUN(JLUN)
	   ILOOP = ILOOP + 1
cccc	   write (*,*) 'ask: init iloop nloop jlun', 
cccc     &		iloop, NLOOP, MY_LUN(JLUN)
	   GO TO 1234
	END IF
C       
C...    Rewind & Continue reading from "Called" Indirect File (if any):
C       
	IF (INDIRECT) THEN
	   REWIND LUN_EXT
	   ILOOP = ILOOP + 1
cccc	   write (*,*) 'ask: init iloop nloop LUN_EXT', 
cccc     &		iloop, nloop, LUN_EXT
	   GO TO 1234
	END IF
	GO TO 1234
C--------------------------------------------------------------------
C...    Wait for the requested number of seconds before continuing.... REMOVED
C       
cJul00 6020	IF (LOC_EVAL(LINE(3:LOC), VALUE)) CALL SLEEP (VALUE)
cjul00	GO TO 1234
C--------------------------------------------------------------------
 6100	ECHO = .TRUE.
	GO TO 1234
 6110	ECHO = .FALSE.
	GO TO 1234
 6150	NOISY_CR = .TRUE.
	GO TO 1234
 6160	NOISY_CR = .FALSE.
	GO TO 1234
 6300	IF (BATCH) GO TO 3000
	IF (IN_USE) GO TO 1234
	WRITE (*,6666) LINE(1:2)
 6666	FORMAT (' *** ASK:  Unknown Command "',A2,'"')
cNoChArray	WRITE (*,6666) (SLINE(I),I=1,2)
cNoChArray 6666	FORMAT (' *** ASK:  Unknown Command "',2A1,'"')
	GO TO 1234
C---------------------------------------------------------------
C...Process a system Command:
C
 7000	CALL SYSTEM(LINE(2:))
	GO TO 1234
C--------------------------------------------------------------------
C...    Enter HELP mode:
C	Note: If using CMLI, need to use ?? here instead of ?, so that CMLI
c	can use ? to list all the commands available. 
c	Since CMLI uses ask to get a line, the ? will get caught before CMLI
c	has a chance to list all currently available commands.       
 8000	IF (.NOT.(BATCH.OR.INDIRECT)) then
	  if (trimstop(line) .le. 3) then 
	    CALL HELP(HLP_PATH, HLP_FIL, ' ')
	  else
	    CALL HELP(HLP_PATH, LINE(3:trimstop(line)), ' ')
	  endif
c	  write (*,*) '?... ', trimstart(line(2:)), trimstop(line(2:))
c	  write (*,*) '?... ', 
c     &		LINE(1+trimstart(line(2:)):1+trimstop(line(2:)))
c	  CALL HELP(LINE(1+trimstart(line(2:)):1+trimstop(line(2:))), 
c     &		' ') 
	endif
c 8000	IF (.NOT.(BATCH.OR.INDIRECT)) CALL HELP('ASK', ' ') 
	GO TO 1234
C--------------------------------------------------------------------
C...    Set Delimiter Characters:
C       
 6400	CALL PARSE_ADD_DELIM(LINE(4:4))
	IF (ONLINE) CALL PARSE_WRITE_DELIMS
	GOTO 1234
 6500	CALL PARSE_DELETE_DELIM(LINE(5:5))
	IF (ONLINE) CALL PARSE_WRITE_DELIMS
	GOTO 1234
C--------------------------------------------------------------------
C...    Set Ignored Characters:
C       
 6413	CALL PARSE_ADD_IGNORE(LINE(4:4))
	IF (ONLINE) CALL PARSE_WRITE_IGNORE
	GOTO 1234
 6513	CALL PARSE_DELETE_IGNORE(LINE(5:5))
	IF (ONLINE) CALL PARSE_WRITE_IGNORE
	GOTO 1234
C--------------------------------------------------------------------
C...    Enter CALC mode:
C       
 6600	CALC = .TRUE.
	IF (ONLINE) CALL GETINPUT('CALC> ', CHSTR)
	IF (BATCH) READ (MY_LUN(JLUN),3111,END=3000) CHSTR
	NCH=LEN(CHSTR)
	CALL CHTRIM_BLNK (NCH,CHSTR)
cNoChArray	CALL TRIM_BLNK (NCH,STRING)
	IF (NCH .EQ. 0) GO TO 1234 ! Exit on blank line.
	IF (CHSTR(1:1) .EQ. '\') GO TO 6000
	IF (LOC_EVAL (CHSTR(1:NCH), VALUE)) GO TO 6630
	GO TO 6600
C       
 6630	IF (ONLINE  .AND.  .NOT. INDIRECT) WRITE (*,6633) VALUE
 6633	FORMAT (G20.8)
	GO TO 6600
C       
 6601	IF (.NOT. LOC_EVAL (CHSTR(3:LOC), VALUE)) GO TO 1234
	IF (ONLINE  .AND.  .NOT. INDIRECT) WRITE (*,6633) VALUE
	GO TO 1234
C--------------------------------------------------------------------
C...    Suspend reading from current Indirect File (if any):  
C       
 6700	IF (JLUN .LT. 1) GO TO 1234
	IF (SUSPEND_IND) GO TO 1234
	SUSPEND_IND = .TRUE.
	MYLUNSAV = MY_LUN(JLUN)
	JLUNSAV = JLUN
	JLUN = 0
	WRITE (*,6711)
 6711	FORMAT (' *** ASK:  Indirect input Suspended', 
     >       ' by "\-@" command.  Resume with "\@"  ***')
	GO TO 1234
C--------------------------------------------------------------------
C...    Resume reading from current Indirect File (if any):
C       [DIRECT MODE ONLY -- otherwise can corrupt nesting!] 
C       
 6750	IF (.NOT. SUSPEND_IND) GO TO 1234
	IF (JLUN .GT. 0) GO TO 1234 ! Not allowed from an Ind. File!
	SUSPEND_IND = .FALSE.
	JLUN = JLUNSAV 
	MY_LUN(JLUN) = MYLUNSAV 
	JLUNSAV = 0
	GO TO 1234
C--------------------------------------------------------------------
C...    Stop reading from "Called" Indirect File: 
C       
 6770	IF (.NOT. INDIRECT) GO TO 1234
	INDIRECT = .FALSE.
	GO TO 1234
C--------------------------------------------------------------------
C...    TYPE a File:
C       
 6800	IF (LINE(3:6) .EQ. '    ') THEN
	   IF (BATCH  .OR.  INDIRECT) GO TO 1234
	   CALL GETINPUT('Filename to TYPE? ', LINE(4:80))
	END IF
	LUN_TYP = MY_LUN_0
 6801	LUN_TYP = LUN_TYP + 1	! Search Upward for an unused LUN:
	IF (LUN_TYP .GT. 99) GO TO 1234 ! Give up.
	INQUIRE (UNIT=LUN_TYP, OPENED=IN_USE)
	IF (IN_USE) GO TO 6801
	OPEN (LUN_TYP, FILE=LINE(3:80), 
cTR     >          SHARED, 
cAlpha doesn't like read only     >          READONLY, SHARED, 
c commet out for linux      >          READONLY, SHARED, 
     >       STATUS='OLD', ERR=6808)
ccc	WRITE (*,*) 'ASK: OPENED READONLY SHARED ', LUN_TYP, LINE(3:40)
 6802	READ (LUN_TYP,'(A80)',ERR=6808,END=6809) FILE_LINE
	WRITE (*,'(1x,a79)') FILE_LINE
cNoChArray 6802	READ (LUN_TYP,6833,ERR=6808,END=6809) FILE_LINE
cNoChArray 6833	FORMAT (80A1)
cNoChArray	WRITE (*,6844) (FILE_LINE(I),I=1,72)
cNoChArray 6844	FORMAT (1X,72A1)
	GO TO 6802
 6808	IF (ONLINE) WRITE (*,6899) LINE(3:35), LUN_TYP
 6899	FORMAT (' *** ASK:  Cannot Open ',A32,' on LUN',I3)
 6809	CLOSE (LUN_TYP, ERR=1234) ! Just in case. 
	GO TO 1234
C--------------------------------------------------------------------
C...    Enter HELP mode:
C       
 6900	IF (.NOT.(BATCH.OR.INDIRECT)) then
	  if (trimstop(line) .le. 3) then 
	    CALL HELP(HLP_PATH, HLP_FIL, 'ASK')
	  else
	    CALL HELP(HLP_PATH, LINE(3:trimstop(line)),'ASK')
	  ENDIF
c	  write (*,*) '\h... ', trimstart(line(3:)), trimstop(line(3:))
c	  write (*,*) '\h... ', 
c     &		LINE(2+trimstart(line(3:)):2+trimstop(line(3:)))
c	  CALL HELP(LINE(2+trimstart(line(3:)):2+trimstop(line(3:))), 
c     &		' ') 
	endif
	GO TO 1234

C-----------------------------------------------------------------------------
	ENTRY SET_ASK_FILE (FILE_NAME) ! Open a new Indirect File: 

	JLUN = JLUN + 1		! Increment pointer.
	IF (JLUN .GT. 10) THEN 
	   WRITE (*,3209) 
	   JLUN = 10
	   STOP
	END IF
	MY_LUN(JLUN) = MY_LUN_0
c	! Search Downward for an unused LUN:
 9001	MY_LUN(JLUN) = MY_LUN(JLUN) - 1 
	IF (MY_LUN(JLUN) .LT. 1) GO TO 9002
	INQUIRE (UNIT=MY_LUN(JLUN), OPENED=IN_USE)
	IF (IN_USE) GO TO 9001
	OPEN (MY_LUN(JLUN), FILE=FILE_NAME, 
cTR     >          SHARED, 
cALpha doesn't like readonly     >          READONLY, SHARED, 
c comment out for linux       >          READONLY, SHARED, 
     >       STATUS='OLD', ERR=9002)
ccc	WRITE (*,*) 'ASK: OPENED READONLY SHARED ', MY_LUN(JLUN), LINE(1:40)
	RETURN
C       
 9002	IF (ONLINE) WRITE (*,3219) JLUN, MY_LUN(JLUN), FILE_NAME
	CLOSE (MY_LUN(JLUN), ERR=9003) ! Just in case. 
 9003	JLUN = MAX0(0,JLUN-1)	! Decrement pointer.
	RETURN

C-----------------------------------------------------------------------------
C-----------------------------------------------------------------------------
	ENTRY SET_ASK_INDIR (INDIR, LUNEXT) ! LUNEXT assumed Open already. 
ccc	write (*,*) 'SET_ASK_INDIR: indir, lunext ', indir, lunext
ccc	write (*,*) 'SET_ASK_INDIR: closing JLUN, MY_LUN(JLUN)', 
ccc     &		JLUN, MY_LUN(JLUN)
C       
	IF (INDIRECT) GO TO 6920
	IF (INDIR) GO TO 6920
	IF (JLUN .EQ. 0) GO TO 6920
	CLOSE (MY_LUN(JLUN))	! Close current Ind. file.
	JLUN = MAX0(0,JLUN-1)	! Decrement pointer.
 6920	INDIRECT = INDIR
	IF (LUNEXT .NE. 0) LUN_EXT = LUNEXT ! 0 means to leave unchanged. 
ccc	write (*,*) 'SET_ASK_INDIR: now JLUN ', JLUN
ccc	write (*,*) 'SET_ASK_INDIR: LUN_EXT ', LUN_EXT
	RETURN

C-----------------------------------------------------------------------------
	ENTRY SET_ASK_DELIMS (DELIMS)
	CALL PARSE_SET_DELIMS(DELIMS)
	IF (ONLINE) CALL PARSE_WRITE_DELIMS
 	RETURN

C-----------------------------------------------------------------------------
	ENTRY SET_ASK_ECHO (ECH)
	ECHO = ECH
	RETURN
	END 
C================================================================


        SUBROUTINE INIT_HELP (HLPPATH, HLPFIL, KEY)
	IMPLICIT NONE
        include 'help.cmn'

	CHARACTER HLPPATH*(*), HLPFIL*(*)
	CHARACTER KEY*(*)


	HLP_PATH = HLPPATH
        HLP_FIL = HLPFIL
        KEY_WD = KEY

c	write (*,*) 'init_help: HLP_PATH ', HLP_PATH
c	write (*,*) 'init_help: HLP_FIL ', HLP_FIL
c	write (*,*) 'init_help: KEY_WD ', KEY_WD

        RETURN
	END

C==============================================================================
C
C       Are we currently interacting with user?
C
C==============================================================================

	LOGICAL FUNCTION INTERACT()

C       Common block for ASK status:
	LOGICAL INDIRECT
	INTEGER*4 MY_LUN(10), JLUN, LUN_EXT
	COMMON /ASK_LUNS/ JLUN, MY_LUN, INDIRECT, LUN_EXT

C       Function for batch mode
	LOGICAL  LOC_BATCH
	EXTERNAL LOC_BATCH

C       DA: I don't think I understand what the INDIRECT variable means.
C       However, JLUN is an indicator of the level of nested input:
C       0 means direct from terminal.

	INTERACT = .NOT. ( LOC_BATCH() .OR. INDIRECT .OR. JLUN .GT. 0 )

	RETURN
	END
