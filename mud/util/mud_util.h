#ifndef _MUD_UTIL_H_
#define _MUD_UTIL_H_

#include "mud.h"

/*
 *
 *   Copyright (C) 1994-2007 TRIUMF (Vancouver, Canada)
 *
 *   Authors: T. Whidden, D. Arseneau
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

int streq  _ANSI_ARGS_(( char *a , char *b ));
int strmatch _ANSI_ARGS_(( register char *string, register char *pattern ));

/* Gnu string utils provides strndup, but always use our private version */
char *strndup_priv  _ANSI_ARGS_(( const char *str, const size_t max_len ));
char *stoupper  _ANSI_ARGS_(( char *inString ));
char *stolower  _ANSI_ARGS_(( char *inString ));
char *stoprint  _ANSI_ARGS_(( char *str_in , char *str_out ));
char *skipBlanks  _ANSI_ARGS_(( char *ptr ));
char *trimBlanks  _ANSI_ARGS_(( char *inStr , char *outStr ));
void padString  _ANSI_ARGS_(( char *s , char c , int len ));
void baseset _ANSI_ARGS_(( char *base, const char *full ));
char *file_nodir  _ANSI_ARGS_(( const char *file_in , char *file_out ));
char *file_dir  _ANSI_ARGS_(( const char *file_in , char *file_out ));
char *file_nover  _ANSI_ARGS_(( const char *file_in , char *file_out ));
char *file_noext  _ANSI_ARGS_(( const char *file_in , char *file_out ));
void mu_interp_time _ANSI_ARGS_(( TIME *ptimval_out, char in_date[], char in_time[] ));
BOOL validateHistList _ANSI_ARGS_(( int nhsel, int hsel[], int *pmhists ));
#endif
