#ifndef _IMUSR_HDR_H_
#define _IMUSR_HDR_H_

/*
 *   trii_fmt.h
 *   Format of old TRIUMF i-musr run files
 *
 *   Copyright (C) 1994,1996 TRIUMF (Vancouver, Canada)
 *
 *   Authors: T. Whidden
 *
 *   Released under the GNU LGPL - see http://www.gnu.org/licenses
 *
 *   This program is free software; you can distribute it and/or modify it under
 *   the terms of the Lesser GNU General Public License as published by the Free
 *   Software Foundation; either version 2 of the License, or any later version.
 *   Accordingly, this program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *   or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License
 *   for more details.
 *
 *  Modification history:
 *    16-Feb-1996  TW  Updated to version 9
 *                 TW  Updated to version 10 (changed exptNumber to int)
 */

typedef struct { 
	 INT16	headtype;	/* -1 */ 
	 INT16	runno;		/* 0 */ 
	 INT16	begindata;	/* 257 */ 
	 INT16	npoints_1;	/* 1 */    /* this is the next data point to be written */
	 INT16	ipdat1;		/* 31 */   /* pointer to start datetime */
	 INT16	ipdat2;		/* 41 */   /* pointer to end datetime */
	 INT16	iptitl;		/* 51 */   /* pointer to title */
	 INT16	ipdesc;		/* 92 */   /* pointer to data description */
	 INT16	nvers;		/* 10 */   /* end of standard IMUSR pointers */
	 INT16	ipsubtit;	/* 95 */   /* pointer to length of subtitle */
	 INT16	ipcomment1;	/* 136 */  /* pointer to length of comment1 */
	 INT16	ipcomment2;	/* 176 */  /* pointer to length of comment2 */
	 INT16	ipcomment3;	/* 216 */  /* pointer to length of comment3 */
	 INT16	nescalers;	/* 6 */    /* number of escalers read (def 6) */
	 INT16	nascalers;	/* 0 */    /* number of ascalers read  */
	 INT16	currentblk;	/* 2 */    /* next block to be written */
	 INT32	last_dac;	/* 0 */    /* last value of dac */
    	 INT16  ncamp;
         INT16  exptNumber;
         INT16  len_experimenter;
         char   experimenter[18];
	 INT16	lendatestrt;	/* 9 */   /* length of date-time in 2byte words */
	 char	dattimstart[18]; /* ' ' */ 
	 INT16	lendatend;	/* 0 */    /* this is zero unless run is ended normally */
	 char	dattimeend[18];	/* ' ' */ 
	 INT16	lentitle;	/* 40 */   
	 char	title[80];	/* ' ' */  /* should be at the end of the 91'th 2byte word  */
	 INT16	lendatadesc;	/* 2 */ 
	 char	datadescfor[2];	/* ' F' */ 
	 INT16	datadesclen;	/* 32 */   /* data descrip=' F'32 -> fixed 32-1byte words */
	 /* this ends the standard IMUSR header format */
	 INT16	lensubtitle;	/* 40 */   /* this should be the 95'th 2byte word */
	 char	subtitle[80];	/* ' ' */      
	 INT16	lencomment1;	/* 39 */   /* this should be the 136'th 2byte word */
	 char	comment1[78];	/* ' ' */ 
	 INT16  lencomment2;	/* 39 */   /* this should be the 176'th 2byte word */
	 char	comment2[78];	/* ' ' */ 
	 INT16  lencomment3;	/* 39 */   /* this should be the 216'th 2byte word */
	 char	comment3[78];	/* ' ' */  /* this ends at the 255'th 2byte word */
	 INT16  lasthdrword;	/* 0 */    /* end at the 256'th 2byte word */
} IMUSR_HDR;


typedef struct {
	 INT32 data[16];
} IMUSR_PACKT;


typedef struct {
	 IMUSR_PACKT packet[8];
} IMUSR_BLK;


typedef struct {
    INT16 type;
    INT16 len_path;
    char path[50];
    INT16 len_title;
    char title[19];
    INT16 len_units;
    char units[8];
} IMUSR_CAMP_VAR;


typedef struct {
    IMUSR_CAMP_VAR var[6];
    char dummy[2];
} IMUSR_CAMP_BLOCK;


typedef struct {
    INT32 type;
    INT16 len_path;
    char path[114];
    INT16 len_title;
    char title[31];
    INT16 len_units;
    char units[15];
} IMUSR_CAMP_VAR_V8;


typedef struct {
    IMUSR_CAMP_VAR_V8 var[3];
    char dummy[2];
} IMUSR_CAMP_BLOCK_V8;


#endif
