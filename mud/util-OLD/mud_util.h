#ifndef _MUD_UTIL_H_
#define _MUD_UTIL_H_

#include "mud.h"

int streq  _ANSI_ARGS_(( char *a , char *b ));
int strmatch _ANSI_ARGS_(( register char *string, register char *pattern ));
/*  #ifndef linux  /* This was conditional.  Why? */
char *strndup  _ANSI_ARGS_(( char *str, int max_len ));
/*  #endif */
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
void mu_interp_time _ANSI_ARGS_(( time_t *ptimval_out, char in_date[], char in_time[] ));

#endif
