/*
 * Run Parameters block for TD_muSR data.
 *
 *   Copyright (C) 1994 TRIUMF (Vancouver, Canada)
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
 */

typedef struct {
	  INT16	     mrun;
	  INT16	     mhists;
	  INT16	     msclr;
	  INT16	     msupd;
	  INT32	     jtsc[18];
	  INT32	     jdsc[18];
	  INT16	     mmin;
	  INT16	     msec;
	  INT16	     mtnew[6];
	  INT16	     mtend[6];
	  INT16	     mlston[4];
	  INT16	     mcmcsc;
	  INT16	     mlocsc[2][6];
	  INT16	     mrsta;
	  INT32	     acqtsk;
	  char	     logfil[10];
	  INT16	     muic;
	  INT32	     nevtot;
	  INT16	     mhsts;
	  INT16	     mbins;
	  INT16	     mshft;
	  INT16	     mspare[7];
	  char	     title[40];
	  char	     sclbl[72];
	  char	     coment[144];
} TMF_F_HDR;

typedef struct {
	union {
	    struct {
		INT16	     ihist;
		INT16	     length;
		INT32	     nevtot;
		INT16	     ntpbin;
		INT32	     mask;
		INT16	     nt0;
		INT16	     nt1;
		INT16	     nt2;
		char	     htitl[10];
		char	     id[2];
		char	     fill[32];
		INT16	     head_bin;
	    } h;
	    INT16	     data[256];
	} u;
} TMF_H_RECD;

