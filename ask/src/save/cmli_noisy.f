C...CMLI...
C
C...ComMand Line Interpreter for general use: 
C
C...Using the ASK utility, request (using PROMPT), receive and interpret 
C   a User Command Line beginning with an NCH-character Command 
C   which must match one of the NCMD Commands CMDS(i) defined by the caller, 
C   followed by up to 19 "fields" of arguments A1, etc., of types L1, etc., 
C   and return the command number JCMD and arguments to the calling program.  
C   In case of erroneous input (e.g., an unrecognizable Command) the user 
C   will be prompted for new input.  For examples see the HELP library below. 
C
C...User input is normally converted to upper case, meaning of course 
C   that any command appearing in the CMDS(i) list which contains lower case 
C   characters can never be matched.  This is a good way to indicated 
C   as-yet unsupported commands.  To turn off upper case conversion 
C   and allow more flexibility/confusion, CALL CMLI_UPPER (.FALSE.)
C	
C...Abbreviations are normally accepted for all commands, meaning of course 
C   that ambiguities are resolved in favour of the command appearing earliest 
C   in the CMDS(i) list.  To reject abbreviations, CALL CMLI_ABBREV (.FALSE.)
C	
C...There are two extra "special" Commands:
C
C   ?		produces a list of commands recognized (at this calling);
C
C   HELP	transfers control (temporarily) to a HELP library, either the 
C   		default (DSK1:[JESS.UTIL]USERS.HLB, 'CMLI' keyword) or a 
C		user library previously specified by 
C		CALL CMLI_HELP ('DEV:[ACCT]', 'LIBNAME', 'KEYWD'). 
cccoldC		CALL CMLI_HELP ('DEV:[ACCT]LIBNAME.HLB', 'KEYWD'). 
C   (Note that the full-path filename must always be specified.) 
C
C		-- Jess Brewer (Jan. 1984)
C		-- Updated August 1987 to allow up to 19 "fields".  
C   09-Apr-2001  DA: Use new "W" data type (space-delimited string) for 
C               the command string.
C----------------------------------------------------------------------
	SUBROUTINE BASECMLI
C-----------------------------------------------------------------------
	IMPLICIT NONE
        include 'help.cmn'
	CHARACTER PROMPT*(*)
	CHARACTER*1 CMDS(*)
	INTEGER NCMD, NCH, JCMD
	CHARACTER*1 L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,
     >			L11,L12,L13,L14,L15,L16,L17,L18,L19
	REAL*4  A1(*), A2(*), A3(*), A4(*), A5(*), A6(*), A7(*),
     >		A8(*), A9(*), A10(*), A11(*), A12(*), A13(*), A14(*), 
     >		A15(*), A16(*), A17(*), A18(*), A19(*)
	CHARACTER*1 CMD(80)
	CHARACTER*4 CTYP
	CHARACTER HLPPATH*(*), HLPFIL*(*), KEY*(*)
c	CHARACTER HLPFIL*(*), KEY*(*), HLP_FIL*80, KEY_WD*40
	CHARACTER*4 KOMD
	EQUIVALENCE (KOMD, CMD(1)) 

	INTEGER MCH, NFIELDS, NCHR, I, LCH, LOC, ICH, NCH8M, NPC, NCPL
	INTEGER NLIN, JC, LIN, JCLOC, I0, NFP1
C
	LOGICAL INDIRECT
	INTEGER*4 MY_LUN, JLUN, LUN_EXT
	COMMON /ASK_LUNS/ JLUN, MY_LUN(10), INDIRECT, LUN_EXT

C
	LOGICAL LOC_BATCH
	LOGICAL UCYESNO, UPPER, ABYESNO, ABBREV, INDOK, INDIR_OK
c        SAVE HLP_FIL, KEY_WD 
	DATA ABBREV /.TRUE./	! Default is to accept abbreviations. 
	DATA UPPER /.TRUE./	! Default is automatic upper case conversion.
	DATA INDIR_OK /.FALSE./	! Default is to release any indirect 
C				! files (see ASK) in case of error. 

c	Already initialized in ASK_NOISY.for
cc	DATA HLP_FIL /' '/, KEY_WD /' '/
c	DATA HLP_FIL /' '/, KEY_WD /'CMLI'/
C============================================================================
	ENTRY CMLI0 (PROMPT, NCMD, NCH, CMDS, JCMD)
	NFIELDS = 0
	GOTO 1
	
	ENTRY CMLI1 (PROMPT, NCMD, NCH, CMDS, JCMD, L1,A1)
	NFIELDS = 1
	GOTO 1
	
	ENTRY CMLI2 (PROMPT, NCMD, NCH, CMDS, JCMD, L1,A1, L2,A2)
	NFIELDS = 2
	GOTO 1
	
	ENTRY CMLI3 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3)
	NFIELDS = 3
	GOTO 1
	
	ENTRY CMLI4 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4)
	NFIELDS = 4
	GOTO 1
	
	ENTRY CMLI5 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5)
	NFIELDS = 5
	GOTO 1
	
	ENTRY CMLI6 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6)
	NFIELDS = 6
	GOTO 1
	
	ENTRY CMLI7 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7)
	NFIELDS = 7
	GOTO 1
	
	ENTRY CMLI8 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8)
	NFIELDS = 8
	GOTO 1
	
	ENTRY CMLI9 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9)
	NFIELDS = 9
	GOTO 1
	
	ENTRY CMLI10 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10)
	NFIELDS = 10
	GOTO 1
	
	ENTRY CMLI11 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11)
	NFIELDS = 11
	GOTO 1
	
	ENTRY CMLI12 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12)
	NFIELDS = 12
	GOTO 1
	
	ENTRY CMLI13 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13)
	NFIELDS = 13
	GOTO 1
	
	ENTRY CMLI14 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14)
	NFIELDS = 14
	GOTO 1
	
	ENTRY CMLI15 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15)
	NFIELDS = 15
	GOTO 1
	
	ENTRY CMLI16 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16)
	NFIELDS = 16
	GOTO 1
	
	ENTRY CMLI17 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17)
	NFIELDS = 17
	GOTO 1
	
	ENTRY CMLI18 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18)
	NFIELDS = 18
	GOTO 1
	
	ENTRY CMLI19 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4,
     >       L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19)
	NFIELDS = 19
	GOTO 1
	
C============================================================================
 1	IF (NFIELDS .LT. 0) RETURN
	NFIELDS = MIN(NFIELDS,19)
	NFP1 = NFIELDS + 1
	JCMD = 0
	IF (NCMD .LT. 1) RETURN
	MCH = ABS(NCH)
	WRITE (CTYP,1111) MCH
 1111	FORMAT (I3,'W')
C------------------------------------------------------------------------
C...Read in Command CMD and NARG Arguments ARGS(i) via ASK:
C	
 1234	CALL PARSE_PUSH_DELIMS(',')
	GO TO (1000,1001,1002,1003,1004,1005,
     >		1006,1007,1008,1009,1010,
     >		1011,1012,1013,1014,1015,
     >		1016,1017,1018,1019), NFP1
C
 1000	CALL ASK1 (PROMPT, CTYP, CMD)
	GO TO 2000
 1001	CALL ASK2 (PROMPT, CTYP, CMD, L1,A1)
	GO TO 2000
 1002	CALL ASK3 (PROMPT, CTYP, CMD, L1,A1, L2,A2)
	GO TO 2000
 1003	CALL ASK4 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3)
	GO TO 2000
 1004	CALL ASK5 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4)
	GO TO 2000
 1005	CALL ASK6 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4, L5,A5)
	GO TO 2000
 1006	CALL ASK7 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4, L5,A5, L6,A6)
	GO TO 2000
 1007	CALL ASK8 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4, L5,A5, L6,A6, L7,A7)
	GO TO 2000
 1008	CALL ASK9 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4, L5,A5, L6,A6, L7,A7, L8,A8)
	GO TO 2000
 1009	CALL ASK10(PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		  L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9)
	GO TO 2000
 1010	CALL ASK11 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10)
	GO TO 2000
 1011	CALL ASK12 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11)
	GO TO 2000
 1012	CALL ASK13 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12)
	GO TO 2000
 1013	CALL ASK14 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13)
	GO TO 2000
 1014	CALL ASK15 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14)
	GO TO 2000
 1015	CALL ASK16 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14, L15,A15)
	GO TO 2000
 1016	CALL ASK17 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14, L15,A15, 
     >		L16,A16)
	GO TO 2000
 1017	CALL ASK18 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14, L15,A15, 
     >		L16,A16, L17,A17)
	GO TO 2000
 1018	CALL ASK19 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14, L15,A15, 
     >		L16,A16, L17,A17, L18,A18)
	GO TO 2000
 1019	CALL ASK20 (PROMPT, CTYP, CMD, L1,A1, L2,A2, L3,A3, 
     >		L4,A4, L5,A5, L6,A6, L7,A7, L8,A8, L9,A9, L10,A10, 
     >		L11,A11, L12,A12, L13,A13, L14,A14, L15,A15, 
     >		L16,A16, L17,A17, L18,A18, L19,A19)
C
 2000	CALL PARSE_POP_DELIMS
	CONTINUE
c	WRITE (*,*) 'Found [', (CMD(I), I=1, NCH), ']'
	NCHR = MCH
	DO 10 I=1,MCH
   10	IF (CMD(MCH+1-I) .EQ. ' ') NCHR = NCHR - 1
	NCHR = MAX0(1,NCHR)
	IF (UPPER) CALL CASE_UPPER (NCHR, CMD)
C	
 1100	IF (KOMD .EQ. '?   ') GO TO 8000
	IF (KOMD .EQ. 'HELP') GO TO 9000
C	
	LCH = MCH
	IF (ABBREV) LCH = NCHR  
C
	DO 200 JCMD=1,NCMD   
	LOC = MCH*(JCMD-1) + LCH + 1     
C	
	DO 20 ICH=1,LCH  
	LOC = LOC - 1
	IF (CMD(LCH-ICH+1) .NE. CMDS(LOC)) GO TO 200	
   20	CONTINUE   
C	
C...Got it!	
C	
	RETURN     
C	
  200	CONTINUE   
C	
C...No such Command.   
C    
 2200	NCH8M = MIN0(MCH,8)  
	WRITE (*,222) (CMD(I),I=1,NCH8M)     
  222	FORMAT (' *** CMLI -- Unrecognized Command "',8A1,'"'/)
	IF (LOC_BATCH()) STOP
	IF (INDIR_OK) GO TO 1234	! Don't disturb indir. files, if any.
C
 2220	IF (.NOT. INDIRECT  .AND.  JLUN .EQ. 0) GO TO 1234
	CALL SET_ASK_INDIR (.FALSE., 0)	! Turn off all indirect processing. 
	GO TO 2220
C
C...Request for "?" (List recognised Commands and Argument syntax, if any):
C	
 8000	IF (LOC_BATCH()) GO TO 2200
	WRITE (*,8881) NCMD	
 8881	FORMAT (1X,I2,' CMLI Commands:') 
	NPC = MCH + 2    
	NCPL = 72/NPC    
	NLIN = NCMD/NCPL 
	IF (NLIN*NCPL .LT. NCMD) NLIN = NLIN + 1  
	JC = 0     
	DO 8100 LIN=1,NLIN    
	DO 8110 I=1,72	
 8110	CMD(I) = ' '
	DO 8120 JCLOC=1,NCPL   
	JC = JC + 1	
	IF (JC .GT. NCMD) GO TO 8100	
	I0 = MCH*(JC-1)  
	LOC = NPC*(JCLOC-1)  
	DO 8121 I=1,MCH    
 8121	CMD(LOC+I) = CMDS(I0+I)    
 8120	CONTINUE   
 8100	WRITE (*,8882) (CMD(I),I=1,72) 
 8882	FORMAT (1X,72A1)
	WRITE (*,8883)
 8883	FORMAT ('  ')
C
	GO TO 1234
C PAK EFFECTIVELY COMMENT OUT THE NEXT GARBAGE --- JESS DOESN'T
C PAK USE THE ARGUMENT FIELDS IN A MEANINGFUL WAY
	IF (NFIELDS .LT. 1) GO TO 1234
	WRITE (*,8884) NFIELDS
 8884	FORMAT (1X,I2,' Argument fields:  ',$)
 8885	FORMAT (1X,A4,$)
C
	WRITE (*,8885) L1
	IF (NFIELDS .LT. 2) GO TO 8300
	WRITE (*,8885) L2
	IF (NFIELDS .LT. 3) GO TO 8300
	WRITE (*,8885) L3
	IF (NFIELDS .LT. 4) GO TO 8300
	WRITE (*,8885) L4
	IF (NFIELDS .LT. 5) GO TO 8300
	WRITE (*,8885) L5
	IF (NFIELDS .LT. 6) GO TO 8300
	WRITE (*,8885) L6
	IF (NFIELDS .LT. 7) GO TO 8300
	WRITE (*,8885) L7
	IF (NFIELDS .LT. 8) GO TO 8300
	WRITE (*,8885) L8
	IF (NFIELDS .LT. 9) GO TO 8300
	WRITE (*,8885) L9
	IF (NFIELDS .LT. 10) THEN
		GO TO 8300
	ELSE
		WRITE (*,8883)
		WRITE (*,8886) L10
 8886		FORMAT (1X,A4,$)
		END IF
	IF (NFIELDS .LT. 11) GO TO 8300
	WRITE (*,8885) L11
	IF (NFIELDS .LT. 12) GO TO 8300
	WRITE (*,8885) L12
	IF (NFIELDS .LT. 13) GO TO 8300
	WRITE (*,8885) L13
	IF (NFIELDS .LT. 14) GO TO 8300
	WRITE (*,8885) L14
	IF (NFIELDS .LT. 15) GO TO 8300
	WRITE (*,8885) L15
	IF (NFIELDS .LT. 16) GO TO 8300
	WRITE (*,8885) L16
	IF (NFIELDS .LT. 17) GO TO 8300
	WRITE (*,8885) L17
	IF (NFIELDS .LT. 18) GO TO 8300
	WRITE (*,8885) L18
	IF (NFIELDS .LT. 19) GO TO 8300
	WRITE (*,8885) L19
 8300	WRITE (*,8883)
	GO TO 1234 
C
 9000	IF (LOC_BATCH()) GO TO 2200
	CALL HELP(HLP_PATH, HLP_FIL, KEY_WD)
	GO TO 1234
C================================================================
	ENTRY CMLI_HELP (HLPPATH, HLPFIL, KEY)
	HLP_PATH = HLPPATH
	HLP_FIL = HLPFIL
	KEY_WD = KEY
c...	Added Nov 21, 2000, forget it.
c	CALL HELP(HLP_PATH, HLP_FIL, KEY_WD)
	RETURN
C================================================================
	ENTRY CMLI_UPPER (UCYESNO)
	UPPER = UCYESNO
	RETURN
C================================================================
	ENTRY CMLI_ABBREV (ABYESNO)
	ABBREV = ABYESNO
	RETURN
C================================================================
	ENTRY CMLI_INDIR_OK (INDOK)
	INDIR_OK = INDOK
	RETURN
C
	END  
