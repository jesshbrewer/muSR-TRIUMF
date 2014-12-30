C...CMLI...
C
C...ComMand Line Interpreter for general use: 
C
C...Using the ASK utility, request (using PROMPT), receive and interpret 
C   a User Command Line beginning with an NCH-character Command 
C   which must match one of the NCMD Commands CMDS(i) defined by the caller, 
C   followed by up to 50 "fields" of arguments A1, etc., of types L1, etc., 
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
C		CALL CMLI_HELP ('DEV:[ACCT]LIBNAME.HLB', 'KEYWD'). 
C   (Note that the full-path filename must always be specified.) 
C
C		-- Jess Brewer (Jan. 1984)
C		-- Updated August 1987 to allow up to 19 "fields".  
C               -- Special case June 2010 for exactly 50 "fields".
C----------------------------------------------------------------------
	SUBROUTINE BASECMLI50
C-----------------------------------------------------------------------
	IMPLICIT NONE
	CHARACTER PROMPT*(*)
	CHARACTER*1 CMDS(*)
	INTEGER NCMD, NCH, JCMD
	CHARACTER*1 L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,
     >		    L11,L12,L13,L14,L15,L16,L17,L18,L19,L20,
     >		    L21,L22,L23,L24,L25,L26,L27,L28,L29,L30,
     >		    L31,L32,L33,L34,L35,L36,L37,L38,L39,L40,
     >		    L41,L42,L43,L44,L45,L46,L47,L48,L49,L50
	REAL*4  A1(*),A2(*),A3(*),A4(*),A5(*),
     >   A6(*),A7(*),A8(*),A9(*),A10(*),
     >	 A11(*),A12(*),A13(*),A14(*),A15(*),
     >   A16(*),A17(*),A18(*),A19(*),A20(*),
     >	 A21(*),A22(*),A23(*),A24(*),A25(*),
     >   A26(*),A27(*),A28(*),A29(*),A30(*),
     >	 A31(*),A32(*),A33(*),A34(*),A35(*),
     >   A36(*),A37(*),A38(*),A39(*),A40(*),
     >	 A41(*),A42(*),A43(*),A44(*),A45(*),
     >   A46(*),A47(*),A48(*),A49(*),A50(*)
	CHARACTER*1 CMD(80)
	CHARACTER*4 CTYP
	CHARACTER HLPFIL*(*), KEY*(*), HLP_FIL*80, KEY_WD*40
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
	DATA ABBREV /.TRUE./	! Default is to accept abbreviations. 
	DATA UPPER /.TRUE./	! Default is automatic upper case conversion.
	DATA INDIR_OK /.FALSE./	! Default is to release any indirect 
C				! files (see ASK) in case of error. 
	DATA HLP_FIL /' '/, KEY_WD /'CMLI'/
C============================================================================
	ENTRY CMLI50 (PROMPT, NCMD, NCH, CMDS, JCMD,
     >       L1,A1, L2,A2, L3,A3, L4,A4, L5,A5, 
     >       L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19, L20,A20,
     >       L21,A21, L22,A22, L23,A23, L24,A24, L25,A25, 
     >       L26,A26, L27,A27, L28,A28, L29,A29, L30,A30,
     >       L31,A31, L32,A32, L33,A33, L34,A34, L35,A35,
     >       L36,A36, L37,A37, L38,A38, L39,A39, L40,A40,
     >       L41,A41, L42,A42, L43,A43, L44,A44, L45,A45,
     >       L46,A46, L47,A47, L48,A48, L49,A49, L50,A50)
	NFIELDS = 50
	GOTO 1
	
C============================================================================
 1	IF (NFIELDS .LT. 0) RETURN
	NFIELDS = MIN0(NFIELDS,50)
	NFP1 = NFIELDS + 1
	JCMD = 0
	IF (NCMD .LT. 1) RETURN
	MCH = IABS(NCH)
	WRITE (CTYP,1111) MCH
 1111	FORMAT (I3,'S')
C------------------------------------------------------------------------
C...Read in Command CMD and NARG Arguments ARGS(i) via ASK:
C	
 1234	CALL PARSE_PUSH_DELIMS(' ,')
        CALL ASK51 (PROMPT, CTYP, CMD, 
     >       L1,A1, L2,A2, L3,A3, L4,A4, L5,A5, 
     >       L6,A6, L7,A7, L8,A8, L9,A9, L10,A10,
     >       L11,A11, L12,A12, L13,A13, L14,A14, L15,A15,
     >       L16,A16, L17,A17, L18,A18, L19,A19, L20,A20,
     >       L21,A21, L22,A22, L23,A23, L24,A24, L25,A25, 
     >       L26,A26, L27,A27, L28,A28, L29,A29, L30,A30,
     >       L31,A31, L32,A32, L33,A33, L34,A34, L35,A35,
     >       L36,A36, L37,A37, L38,A38, L39,A39, L40,A40,
     >       L41,A41, L42,A42, L43,A43, L44,A44, L45,A45,
     >       L46,A46, L47,A47, L48,A48, L49,A49, L50,A50)
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
C
 9000	IF (LOC_BATCH()) GO TO 2200
	CALL HELP(HLP_FIL, KEY_WD)
	GO TO 1234
C
	END  
