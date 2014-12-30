#include <stdio.h>

void help_(char*node, char*def, int nodelen, int deflen)
{
  printf("Help presently unavailable on unix.\n");
}
/*
 6903	CALL CASE_UPPER(10,SLINE)
	IF (LINE(3:7) .EQ. 'EXPR') GO TO 6904
	WRITE(*,*)' ASK prompt facility                                  '
	WRITE(*,*)' Enter the values requested, separating each by one of'
	WRITE(*,*)' the delimiters (presently ',DELIM,') and with spaces.'
	WRITE(*,*)' The list of values may be extended to the next line  '
	WRITE(*,*)' by placing a join character \ at the end. Any value '
	WRITE(*,*)' left blank or unassigned will evaluate to zero. An   '
	WRITE(*,*)' expression may be used instead of a value. If no     '
	WRITE(*,*)' value or expresion is given, zero will be assumed.   '
	WRITE(*,*)'                                                      '
	WRITE(*,*)' The following commands may be given at any time:     '
	WRITE(*,*)' @+file   : capture commands to a script file        '
	WRITE(*,*)' @-       : end capture                              '
	WRITE(*,*)' @file    : play script file                         '
	WRITE(*,*)' \E, \-E  : echo/stop echoing commands from scripts  '
	WRITE(*,*)' \-@      : (in script) suspend script playback      '
	WRITE(*,*)' \@       : resume script from where we left off     '
	WRITE(*,*)' \Ln      : (in script) play leading commands n times'
CCCC    WRITE(*,*)' \Jn      : (in script) skip the next n commands     '
	WRITE(*,*)' \-I      : (in app. script) stop script playback    '
	WRITE(*,*)' \Tfile   : display contents of a file               '
	WRITE(*,*)' \Dc, \-Dc: add/remove delimiter character c         '
	WRITE(*,*)' \H       : display this text                        '
	WRITE(*,*)' \HEXPR   : display text on expression syntax        '
CCCC    WRITE(*,*)' \Wn      : wait n seconds                           '
	WRITE(*,*)' \C       : calc until blank line or next command    '
	WRITE(*,*)' \=expr   : print value of expression                '
	GO TO 1234

 6904	WRITE(*,*)' Expression Syntax                                    '
	WRITE(*,*)' This version only accepts constants. Even simple     '
	WRITE(*,*)' arithmetic is unacceptable, never mind constants,    '
	WRITE(*,*)' variables or function calls. Okay, there are two     '
	WRITE(*,*)' built-in constants:                                  '
	WRITE(*,*)'      T TRUE .T .TRUE .T. .TRUE. evaluate to 1        '
	WRITE(*,*)'      F FALSE .F .FALSE .F. .FALSE. evaluate to 0     '
	WRITE(*,*)' This should tell you that any prompt which expects a '
	WRITE(*,*)' logical will accept an expression which evaluates to '
	WRITE(*,*)' one or zero.  Actually, [-.5,.5) evaluate to false   '
	WRITE(*,*)' and everything else is true.                         '
	*/
