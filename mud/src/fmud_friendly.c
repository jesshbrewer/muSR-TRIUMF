/*
 *  fmud_friendly.c --
 *
 *  A friendly Fortran programming interface to the MUD library,
 *  (using cfortran.h)
 *
 *  Copyright (c) 1996-2003 TRIUMF 
 *
 *  Author:
 *    Ted Whidden, TRIUMF Data Acquisition Group 
 *    Donald Arseneau, TRIUMF CMMS
 *    4004 Wesbrook Mall, Vancouver, BC, Canada, V6T 2A3
 *    
 *  Released under the GNU LGPL - see http://www.gnu.org/licenses
 *
 *  This program is free software; you can distribute it and/or modify it under 
 *  the terms of the Lesser GNU General Public License as published by the Free 
 *  Software Foundation; either version 2 of the License, or any later version. 
 *  Accordingly, this program is distributed in the hope that it will be useful, 
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License 
 *  for more details.
 *
 *  Modification history:
 *    26-Feb-1996  TW  Created
 *    07-Sep-2001  DA  Fix for Linux G77 Fortran
 *    29-Oct-2001  DA  Alter for get-string lengths
 *    22-Apr-2003  DA  Add fMUD_openReadWrite
 *
 *  Description:
 *
 *    This module provides a "friendly" Fortran programming interface
 *    to the MUD (MuSR Data) data format.
 * 
 */

/*
 * g77 follows f2c internal function naming by default, but can adopt almost
 * any scheme based on the options (-fno-underscoring, -fno-second-underscore,
 * -f*-case-*).
 */

#ifdef  g77
#define f2cFortran 1
#endif

#include "cfortran.h"

#undef  fcallsc
#define fcallsc(UN,LN)   preface_fcallsc(F,f,UN,LN)

#define FMUDFUN1 FCALLSCFUN1
#define FMUDFUN2 FCALLSCFUN2
#define FMUDFUN3 FCALLSCFUN3
#define FMUDFUN4 FCALLSCFUN4
#define FMUDFUN5 FCALLSCFUN5

/* 
 * f2cFortran includes f2c and g77 fortran, which do the following
 * conversion of fortran names:  
 *   1.  Names are converted to lower case
 *   2.  An underscore is appended to the name
 *   3.  If the name already had an underscore, append another underscore.
 * But cfortran.h fails to implement rule 3!
 *
 * Our purpose here is to make a library of fortran-callable C functions,
 * without any of the reverse.  Moreover, all our functions have an
 * underscore in the name.  Therefore, for real f2c, append a double
 * underscore.  For g77, let's provide all three names:
 * fMUD_function, fMUD_function_, and fMUD_function__
 * so we can use the library whichever underscoring option is chosen.
 */

#ifdef f2cFortran
#ifdef g77 /* g77 -- make all 3 underscore variations */
#undef FMUDFUN1
#undef FMUDFUN2
#undef FMUDFUN3
#undef FMUDFUN4
#undef FMUDFUN5
#undef  CFC_
#define CFC_(UN,LN) LN
#define FMUDFUN1(TYP,MCASE,UCASE,LCASE,T1) \
  FCALLSCFUN1(TYP,MCASE,UCASE,LCASE,T1) \
  FCALLSCFUN1(TYP,MCASE,_(UCASE,_),_(LCASE,_),T1) \
  FCALLSCFUN1(TYP,MCASE,_(UCASE,__),_(LCASE,__),T1)
#define FMUDFUN2(TYP,MCASE,UCASE,LCASE,T1,T2) \
  FCALLSCFUN2(TYP,MCASE,UCASE,LCASE,T1,T2) \
  FCALLSCFUN2(TYP,MCASE,_(UCASE,_),_(LCASE,_),T1,T2) \
  FCALLSCFUN2(TYP,MCASE,_(UCASE,__),_(LCASE,__),T1,T2) 
#define FMUDFUN3(TYP,MCASE,UCASE,LCASE,T1,T2,T3) \
  FCALLSCFUN3(TYP,MCASE,UCASE,LCASE,T1,T2,T3) \
  FCALLSCFUN3(TYP,MCASE,_(UCASE,_),_(LCASE,_),T1,T2,T3) \
  FCALLSCFUN3(TYP,MCASE,_(UCASE,__),_(LCASE,__),T1,T2,T3)
#define FMUDFUN4(TYP,MCASE,UCASE,LCASE,T1,T2,T3,T4) \
  FCALLSCFUN4(TYP,MCASE,UCASE,LCASE,T1,T2,T3,T4) \
  FCALLSCFUN4(TYP,MCASE,_(UCASE,_),_(LCASE,_),T1,T2,T3,T4) \
  FCALLSCFUN4(TYP,MCASE,_(UCASE,__),_(LCASE,__),T1,T2,T3,T4)
#define FMUDFUN5(TYP,MCASE,UCASE,LCASE,T1,T2,T3,T4,T5) \
  FCALLSCFUN5(TYP,MCASE,UCASE,LCASE,T1,T2,T3,T4,T5) \
  FCALLSCFUN5(TYP,MCASE,_(UCASE,_),_(LCASE,_),T1,T2,T3,T4,T5) \
  FCALLSCFUN5(TYP,MCASE,_(UCASE,__),_(LCASE,__),T1,T2,T3,T4,T5)
#else  /* real f2c -- second underscore for all */
#undef  CFC_
#define CFC_(UN,LN) _(LN,__)
#endif
#endif

/*
 * Modifications for fMUD_get-string functions
 *
 * As of October 2001, the C MUD_get-string functions take the maximum
 * string length as their final parameter (thus avoiding buffer overflow
 * errors) but Fortran already passes the length of character variables.
 * We could force the Fortran programmer to write
 * 
 *    fmud_getSomeString( fd, somestring, len(somestring))
 * 
 * but that is redundant, inefficient, and prone to errors.  Moreover,
 * cfortran.h generates inefficient code by using (allocating) a
 * temporary string buffer which is unnecessary in this case.  Here are
 * the minimal definitions of a new cfortran type "gPSTRING" to handle
 * the MUD_get-string functions, passing the actual (hidden) character
 * variable length to the C function.
 *
 * The down-side is that cfortran.h is very complex, and modifications
 * may not be robust.  Nevertheless, let's use this, and see if it still
 * works on new platforms.
 */

#if !defined(__gnu_linux__)
/* Linux libc already defines strnlen.  Please add others above */

/*
 * "strnlen": strlen up to a limit
 */
static int 
strnlen( char* s, int lm )
{
  int l;
  for( l=0; l<lm; l++ )
    if( s[l] == '\0' ) return( l );
  return( lm );
}
#endif

/*****  Begin gPSTRING macro definitions for cfortran.h  *****/

#define gRRRRPSTR(A,C) memset(A+strnlen(A,C),' ',C-strnlen(A,C));

#ifdef vmsFortran
#define gPSTRING_cfN(T,A) fstring * A
#define gPSTRING_cfT(F,N,A,B,C) A->dsc$a_pointer, A->dsc$w_length
#define gPSTRING_cfR(A,B,C) gRRRRPSTR(A->dsc$a_pointer, A->dsc$w_length)
#else
#ifdef CRAYFortran
#define gPSTRING_cfN(T,A) _fcd A
#define gPSTRING_cfT(F,N,A,B,C) _fcdtocp(A),B,_fcdlen(A)
#define gPSTRING_cfR(A,B,C) gRRRRPSTR(_fcdtocp(A),B,_fcdlen(A))
#else
#define gPSTRING_cfN(T,A) char * A
#define gPSTRING_cfT(F,N,A,B,C) A, C
#define gPSTRING_cfR(A,B,C) gRRRRPSTR(A, C)
#endif
#endif

#define gPSTRING_cfQ(V)
#define gPSTRING_cfH PSTRING_cfH

#define gPSTRING_cfSTR(N,T,A,B,C,D,E) _(CFARGS,N)(T,gPSTRING,A,B,C,D,E)

#define gPSTRING_cfINT PSTRING_cfINT
#define gPSTRING_cfSEP PSTRING_cfSEP

/*****  End gPSTRING definitions  *****/


FMUDFUN2(INT,MUD_openRead,MUD_OPENREAD,mud_openread,STRING,PINT)
FMUDFUN2(INT,MUD_openWrite,MUD_OPENWRITE,mud_openwrite,STRING,INT)
FMUDFUN2(INT,MUD_openReadWrite,MUD_OPENREADWRITE,mud_openreadwrite,STRING,PINT)
FMUDFUN1(INT,MUD_closeRead,MUD_CLOSEREAD,mud_closeread,INT)
FMUDFUN1(INT,MUD_closeWrite,MUD_CLOSEWRITE,mud_closewrite,INT)
FMUDFUN2(INT,MUD_closeWriteFile,MUD_CLOSEWRITEFILE,mud_closewritefile,INT,STRING)

FMUDFUN2(INT,MUD_getRunDesc,MUD_GETRUNDESC,mud_getrundesc,INT,PINT)
FMUDFUN2(INT,MUD_getExptNumber,MUD_GETEXPTNUMBER,mud_getexptnumber,INT,PINT)
FMUDFUN2(INT,MUD_getRunNumber,MUD_GETRUNNUMBER,mud_getrunnumber,INT,PINT)
FMUDFUN2(INT,MUD_getElapsedSec,MUD_GETELAPSEDSEC,mud_getelapsedsec,INT,PINT)
FMUDFUN2(INT,MUD_getTimeBegin,MUD_GETTIMEBEGIN,mud_gettimebegin,INT,PINT)
FMUDFUN2(INT,MUD_getTimeEnd,MUD_GETTIMEEND,mud_gettimeend,INT,PINT)
FMUDFUN2(INT,MUD_getTitle,MUD_GETTITLE,mud_gettitle,INT,gPSTRING)
FMUDFUN2(INT,MUD_getLab,MUD_GETLAB,mud_getlab,INT,gPSTRING)
FMUDFUN2(INT,MUD_getArea,MUD_GETAREA,mud_getarea,INT,gPSTRING)
FMUDFUN2(INT,MUD_getMethod,MUD_GETMETHOD,mud_getmethod,INT,gPSTRING)
FMUDFUN2(INT,MUD_getApparatus,MUD_GETAPPARATUS,mud_getapparatus,INT,gPSTRING)
FMUDFUN2(INT,MUD_getInsert,MUD_GETINSERT,mud_getinsert,INT,gPSTRING)
FMUDFUN2(INT,MUD_getSample,MUD_GETSAMPLE,mud_getsample,INT,gPSTRING)
FMUDFUN2(INT,MUD_getOrient,MUD_GETORIENT,mud_getorient,INT,gPSTRING)
FMUDFUN2(INT,MUD_getDas,MUD_GETDAS,mud_getdas,INT,gPSTRING)
FMUDFUN2(INT,MUD_getExperimenter,MUD_GETEXPERIMENTER,mud_getexperimenter,INT,gPSTRING)
/* not in TRI_TI */
FMUDFUN2(INT,MUD_getTemperature,MUD_GETTEMPERATURE,mud_gettemperature,INT,gPSTRING)
FMUDFUN2(INT,MUD_getField,MUD_GETFIELD,mud_getfield,INT,gPSTRING)
/* TRI_TI only */
FMUDFUN2(INT,MUD_getSubtitle,MUD_GETSUBTITLE,mud_getsubtitle,INT,gPSTRING)
FMUDFUN2(INT,MUD_getComment1,MUD_GETCOMMENT1,mud_getcomment1,INT,gPSTRING)
FMUDFUN2(INT,MUD_getComment2,MUD_GETCOMMENT2,mud_getcomment2,INT,gPSTRING)
FMUDFUN2(INT,MUD_getComment3,MUD_GETCOMMENT3,mud_getcomment3,INT,gPSTRING)

FMUDFUN2(INT,MUD_setRunDesc,MUD_SETRUNDESC,mud_setrundesc,INT,INT)
FMUDFUN2(INT,MUD_setExptNumber,MUD_SETEXPTNUMBER,mud_setexptnumber,INT,INT)
FMUDFUN2(INT,MUD_setRunNumber,MUD_SETRUNNUMBER,mud_setrunnumber,INT,INT)
FMUDFUN2(INT,MUD_setElapsedSec,MUD_SETELAPSEDSEC,mud_setelapsedsec,INT,INT)
FMUDFUN2(INT,MUD_setTimeBegin,MUD_SETTIMEBEGIN,mud_settimebegin,INT,INT)
FMUDFUN2(INT,MUD_setTimeEnd,MUD_SETTIMEEND,mud_settimeend,INT,INT)
FMUDFUN2(INT,MUD_setTitle,MUD_SETTITLE,mud_settitle,INT,STRING)
FMUDFUN2(INT,MUD_setLab,MUD_SETLAB,mud_setlab,INT,STRING)
FMUDFUN2(INT,MUD_setArea,MUD_SETAREA,mud_setarea,INT,STRING)
FMUDFUN2(INT,MUD_setMethod,MUD_SETMETHOD,mud_setmethod,INT,STRING)
FMUDFUN2(INT,MUD_setApparatus,MUD_SETAPPARATUS,mud_setapparatus,INT,STRING)
FMUDFUN2(INT,MUD_setInsert,MUD_SETINSERT,mud_setinsert,INT,STRING)
FMUDFUN2(INT,MUD_setSample,MUD_SETSAMPLE,mud_setsample,INT,STRING)
FMUDFUN2(INT,MUD_setOrient,MUD_SETORIENT,mud_setorient,INT,STRING)
FMUDFUN2(INT,MUD_setDas,MUD_SETDAS,mud_setdas,INT,STRING)
FMUDFUN2(INT,MUD_setExperimenter,MUD_SETEXPERIMENTER,mud_setexperimenter,INT,STRING)
/* not in TRI_TI */
FMUDFUN2(INT,MUD_setTemperature,MUD_SETTEMPERATURE,mud_settemperature,INT,STRING)
FMUDFUN2(INT,MUD_setField,MUD_SETFIELD,mud_setfield,INT,STRING)
/* TRI_TI only */
FMUDFUN2(INT,MUD_setSubtitle,MUD_SETSUBTITLE,mud_setsubtitle,INT,STRING)
FMUDFUN2(INT,MUD_setComment1,MUD_SETCOMMENT1,mud_setcomment1,INT,STRING)
FMUDFUN2(INT,MUD_setComment2,MUD_SETCOMMENT2,mud_setcomment2,INT,STRING)
FMUDFUN2(INT,MUD_setComment3,MUD_SETCOMMENT3,mud_setcomment3,INT,STRING)

FMUDFUN3(INT,MUD_getComments,MUD_GETCOMMENTS,mud_getcomments,INT,PINT,PINT)
FMUDFUN3(INT,MUD_getCommentPrev,MUD_GETCOMMENTPREV,mud_getcommentprev,INT,INT,PINT)
FMUDFUN3(INT,MUD_getCommentNext,MUD_GETCOMMENTNEXT,mud_getcommentnext,INT,INT,PINT)
FMUDFUN3(INT,MUD_getCommentTime,MUD_GETCOMMENTTIME,mud_getcommenttime,INT,INT,PINT)
FMUDFUN3(INT,MUD_getCommentAuthor,MUD_GETCOMMENTAUTHOR,mud_getcommentauthor,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getCommentTitle,MUD_GETCOMMENTTITLE,mud_getcommenttitle,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getCommentBody,MUD_GETCOMMENTBODY,mud_getcommentbody,INT,INT,gPSTRING)

FMUDFUN3(INT,MUD_setComments,MUD_SETCOMMENTS,mud_setcomments,INT,INT,INT)
FMUDFUN3(INT,MUD_setCommentPrev,MUD_SETCOMMENTPREV,mud_setcommentprev,INT,INT,INT)
FMUDFUN3(INT,MUD_setCommentNext,MUD_SETCOMMENTNEXT,mud_setcommentnext,INT,INT,INT)
FMUDFUN3(INT,MUD_setCommentTime,MUD_SETCOMMENTTIME,mud_setcommenttime,INT,INT,INT)
FMUDFUN3(INT,MUD_setCommentAuthor,MUD_SETCOMMENTAUTHOR,mud_setcommentauthor,INT,INT,STRING)
FMUDFUN3(INT,MUD_setCommentTitle,MUD_SETCOMMENTTITLE,mud_setcommenttitle,INT,INT,STRING)
FMUDFUN3(INT,MUD_setCommentBody,MUD_SETCOMMENTBODY,mud_setcommentbody,INT,INT,STRING)

FMUDFUN3(INT,MUD_getHists,MUD_GETHISTS,mud_gethists,INT,PINT,PINT)
FMUDFUN3(INT,MUD_getHistType,MUD_GETHISTTYPE,mud_gethisttype,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistNumBytes,MUD_GETHISTNUMBYTES,mud_gethistnumbytes,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistNumBins,MUD_GETHISTNUMBINS,mud_gethistnumbins,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistBytesPerBin,MUD_GETHISTBYTESPERBIN,mud_gethistbytesperbin,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistFsPerBin,MUD_GETHISTFSPERBIN,mud_gethistfsperbin,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistSecondsPerBin,MUD_GETHISTSECONDSPERBIN,mud_gethistsecondsperbin,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getHistT0_Ps,MUD_GETHISTT0_PS,mud_gethistt0_ps,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistT0_Bin,MUD_GETHISTT0_BIN,mud_gethistt0_bin,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistGoodBin1,MUD_GETHISTGOODBIN1,mud_gethistgoodbin1,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistGoodBin2,MUD_GETHISTGOODBIN2,mud_gethistgoodbin2,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistBkgd1,MUD_GETHISTBKGD1,mud_gethistbkgd1,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistBkgd2,MUD_GETHISTBKGD2,mud_gethistbkgd2,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistNumEvents,MUD_GETHISTNUMEVENTS,mud_gethistnumevents,INT,INT,PINT)
FMUDFUN3(INT,MUD_getHistTitle,MUD_GETHISTTITLE,mud_gethisttitle,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getHistData,MUD_GETHISTDATA,mud_gethistdata,INT,INT,PVOID)
FMUDFUN3(INT,MUD_getHistTimeData,MUD_GETHISTTIMEDATA,mud_gethisttimedata,INT,INT,PVOID)

FMUDFUN3(INT,MUD_setHists,MUD_SETHISTS,mud_sethists,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistType,MUD_SETHISTTYPE,mud_sethisttype,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistNumBytes,MUD_SETHISTNUMBYTES,mud_sethistnumbytes,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistNumBins,MUD_SETHISTNUMBINS,mud_sethistnumbins,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistBytesPerBin,MUD_SETHISTBYTESPERBIN,mud_sethistbytesperbin,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistFsPerBin,MUD_SETHISTFSPERBIN,mud_sethistfsperbin,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistSecondsPerBin,MUD_SETHISTSECONDSPERBIN,mud_sethistsecondsperbin,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setHistT0_Ps,MUD_SETHISTT0_PS,mud_sethistt0_ps,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistT0_Bin,MUD_SETHISTT0_BIN,mud_sethistt0_bin,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistGoodBin1,MUD_SETHISTGOODBIN1,mud_sethistgoodbin1,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistGoodBin2,MUD_SETHISTGOODBIN2,mud_sethistgoodbin2,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistBkgd1,MUD_SETHISTBKGD1,mud_sethistbkgd1,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistBkgd2,MUD_SETHISTBKGD2,mud_sethistbkgd2,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistNumEvents,MUD_SETHISTNUMEVENTS,mud_sethistnumevents,INT,INT,INT)
FMUDFUN3(INT,MUD_setHistTitle,MUD_SETHISTTITLE,mud_sethisttitle,INT,INT,STRING)
FMUDFUN3(INT,MUD_setHistData,MUD_SETHISTDATA,mud_sethistdata,INT,INT,PVOID)
FMUDFUN3(INT,MUD_setHistTimeData,MUD_SETHISTTIMEDATA,mud_sethisttimedata,INT,INT,PVOID)

FMUDFUN5(INT,MUD_pack,MUD_PACK,mud_pack,INT,INT,PVOID,INT,PVOID)
FMUDFUN5(INT,MUD_unpack,MUD_UNPACK,mud_unpack,INT,INT,PVOID,INT,PVOID)

FMUDFUN3(INT,MUD_getScalers,MUD_GETSCALERS,mud_getscalers,INT,PINT,PINT)
FMUDFUN3(INT,MUD_getScalerLabel,MUD_GETSCALERLABEL,mud_getscalerlabel,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getScalerCounts,MUD_GETSCALERCOUNTS,mud_getscalercounts,INT,INT,PVOID)

FMUDFUN3(INT,MUD_setScalers,MUD_SETSCALERS,mud_setscalers,INT,INT,INT)
FMUDFUN3(INT,MUD_setScalerLabel,MUD_SETSCALERLABEL,mud_setscalerlabel,INT,INT,STRING)
FMUDFUN3(INT,MUD_setScalerCounts,MUD_SETSCALERCOUNTS,mud_setscalercounts,INT,INT,PVOID)

FMUDFUN3(INT,MUD_getIndVars,MUD_GETINDVARS,mud_getindvars,INT,PINT,PINT)
FMUDFUN3(INT,MUD_getIndVarLow,MUD_GETINDVARLOW,mud_getindvarlow,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getIndVarHigh,MUD_GETINDVARHIGH,mud_getindvarhigh,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getIndVarMean,MUD_GETINDVARMEAN,mud_getindvarmean,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getIndVarStddev,MUD_GETINDVARSTDDEV,mud_getindvarstddev,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getIndVarSkewness,MUD_GETINDVARSKEWNESS,mud_getindvarskewness,INT,INT,PDOUBLE)
FMUDFUN3(INT,MUD_getIndVarName,MUD_GETINDVARNAME,mud_getindvarname,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getIndVarDescription,MUD_GETINDVARDESCRIPTION,mud_getindvardescription,INT,INT,gPSTRING)
FMUDFUN3(INT,MUD_getIndVarUnits,MUD_GETINDVARUNITS,mud_getindvarunits,INT,INT,gPSTRING)

FMUDFUN3(INT,MUD_getIndVarNumData,MUD_GETINDVARNUMDATA,mud_getindvarnumdata,INT,INT,PINT)
FMUDFUN3(INT,MUD_getIndVarElemSize,MUD_GETINDVARELEMSIZE,mud_getindvarelemsize,INT,INT,PINT)
FMUDFUN3(INT,MUD_getIndVarDataType,MUD_GETINDVARDATATYPE,mud_getindvardatatype,INT,INT,PINT)
FMUDFUN3(INT,MUD_getIndVarHasTime,MUD_GETINDVARHASTIME,mud_getindvarhastime,INT,INT,PINT)
FMUDFUN3(INT,MUD_getIndVarData,MUD_GETINDVARDATA,mud_getindvardata,INT,INT,PVOID)
FMUDFUN3(INT,MUD_getIndVarTimeData,MUD_GETINDVARTIMEDATA,mud_getindvartimedata,INT,INT,PVOID)

FMUDFUN3(INT,MUD_setIndVars,MUD_SETINDVARS,mud_setindvars,INT,INT,INT)
FMUDFUN3(INT,MUD_setIndVarLow,MUD_SETINDVARLOW,mud_setindvarlow,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setIndVarHigh,MUD_SETINDVARHIGH,mud_setindvarhigh,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setIndVarMean,MUD_SETINDVARMEAN,mud_setindvarmean,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setIndVarStddev,MUD_SETINDVARSTDDEV,mud_setindvarstddev,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setIndVarSkewness,MUD_SETINDVARSKEWNESS,mud_setindvarskewness,INT,INT,DOUBLE)
FMUDFUN3(INT,MUD_setIndVarName,MUD_SETINDVARNAME,mud_setindvarname,INT,INT,STRING)
FMUDFUN3(INT,MUD_setIndVarDescription,MUD_SETINDVARDESCRIPTION,mud_setindvardescription,INT,INT,STRING)
FMUDFUN3(INT,MUD_setIndVarUnits,MUD_SETINDVARUNITS,mud_setindvarunits,INT,INT,STRING)

FMUDFUN3(INT,MUD_setIndVarNumData,MUD_SETINDVARNUMDATA,mud_setindvarnumdata,INT,INT,INT)
FMUDFUN3(INT,MUD_setIndVarElemSize,MUD_SETINDVARELEMSIZE,mud_setindvarelemsize,INT,INT,INT)
FMUDFUN3(INT,MUD_setIndVarDataType,MUD_SETINDVARDATATYPE,mud_setindvardatatype,INT,INT,INT)
FMUDFUN3(INT,MUD_setIndVarData,MUD_SETINDVARDATA,mud_setindvardata,INT,INT,PVOID)
FMUDFUN3(INT,MUD_setIndVarTimeData,MUD_SETINDVARTIMEDATA,mud_setindvartimedata,INT,INT,PVOID)

