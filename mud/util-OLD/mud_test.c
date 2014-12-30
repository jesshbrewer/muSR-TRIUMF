
/*
 *  mud_test.c -- main and CLI dispatch routines for
 *		      µSR Data Test Utility
 *
 *  Adapted from Ted Whidden's mud_util by Jess H. Brewer
 *
 *  Revision history:
 *   v1.0   MayJan-1997  JHB  Initial version 
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef VAXC
#define bcopy(src,dest,len) memcpy(dest,src,len)
#define bcmp(src,dest,len)  memcmp(src,dest,len)
#define bzero(src,len)      memset(src,0,len)
#endif /* VAXC */

#include "mud_test.h"
#include "getopt.h"

int MUD2TXT_convert _ANSI_ARGS_(( char* fin_name, char* fout_name ));
int MUD2DMP_convert _ANSI_ARGS_(( char* fin_name, int bin_1st, int bin_last ));

void
usage()
{
    printf( "\n" );
    printf( "Usage: mud_test <command> [<infile>]\n" );
    printf( "\n" );
    printf( "\t<command> is one of the following:\n" );
    printf( "\t   show      -  diagnostic display of header contents\n" );
    printf( "\t   dump      -  dump histograms to stdout\n" );
    printf( "\t   mud2txt   -  dump histograms to text file *.txt\n" );
    printf( "\t   help      -  print this screen\n" );
    printf( "\t   exit/quit -  exit program\n" );
    printf( "\t<infile>     -  input filename\n" );
    printf( "\n" );
    printf( "Examples:  mud_test show /prv18/m13/1997/010024.msr\n" );
    printf( "           mud_test dump 010024 101 1100\n" );
    printf( "           mud_test mud2txt 000160 160.txt\n" );
    printf( "\n" );
}


int
main( argc, argv )
    int argc;
    char* argv[];
{

  /*
   * printf( "\n argc=%d  ", argc );
   * printf( " argv[0]=%s ", argv[0] );
   * printf( " argv[1]=%s ", argv[1] );
   * printf( " argv[2]=%s \n\n", argv[2] );
   */

    if( argc > 1 )
    {
        do_command( argc, argv );
        exit( 0 );
    }
    else
    {
	int i;
	char line[128];
        char* p;
        int aargc;
        char* aargv[32];

        aargv[0] = NULL;

	do {
          printf( "MUDtest> " );
          gets( line );

          p = line;
          aargc = 1;
          while( 1 )
          {
            while( *p && !isgraph( *p ) ) *p++ = 0;
            if( *p == 0 ) break;
            aargv[aargc++] = p;
            while( isgraph( *p ) ) p++;
          }
	  /*
	   *  printf( "\n aargc=%d  ", aargc );
	   *  printf( " aargv[0]=%s ", aargv[0] );
	   *  printf( " aargv[1]=%s ", aargv[1] );
	   *  printf( " aargv[2]=%s \n\n", aargv[2] );
	   */
        } while( do_command( aargc, aargv ) != -1 );
    }
}

int
do_command( argc, argv )
    int argc;
    char* argv[];
{
  int c;
  int status;
  int i;
  char* p;
  char infile[128];
  char outfile[128];
  char command[32];
  int jj1;
  int jj2;

/* printf( "\n do_command:  argc=%d  ", argc );
 * printf( " argv[0]=%s ", argv[0] );
 * printf( " argv[1]=%s ", argv[1] );
 * printf( " argv[2]=%s \n\n", argv[2] );
 */

  infile[0] = '\0';
  outfile[0] = '\0';

  switch( argc )
  {
      case 1:
	return( -1 );

      case 2:
        strcpy( command, argv[argc-1] );
        strcpy( infile, "" );
        break;

      case 3:
        strcpy( command, argv[argc-2] );
        strcpy( infile, argv[argc-1] );
        break;

      case 4:
        strcpy( command, argv[argc-3] );
        strcpy( infile, argv[argc-2] );
        strcpy( outfile, argv[argc-1] );
        break;

      case 5:
        strcpy( command, argv[argc-4] );
        strcpy( infile, argv[argc-3] );
        sscanf( argv[argc-2], "%d", &jj1 );
        sscanf( argv[argc-1], "%d", &jj2 );
	/*	printf( "dump %s %d %d\n", infile, jj1, jj2 ); */
        break;

      default:
        usage();
        return( 0 );
  }

  if( !strncmp( command, "h", 1 ) || !strncmp( command, "?", 1 ) )
  {
      usage();
      return( 0 );
  }
  if( !strncmp( command, "e", 1 ) || !strncmp( command, "q", 1 ) )
  {
      return( -1 );
  }
  else if( !strncmp( command, "show", 4 ) )
  {
      FILE* fin;
      MUD_SEC_GRP* pMUD_fileGrp;

      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      /*
       *  Read the file
       */
      fin = MUD_openInput( infile );
      if( fin == NULL ) 
      {
        fprintf( stderr, "failed to open file \"%s\"\n", infile );
        return( 0 );
      }

      pMUD_fileGrp = MUD_readFile( fin );
      if( pMUD_fileGrp == NULL )
      {
        printf( "error while reading file %s\n", infile );
        fclose( fin );
        return( 0 );
      }

      fclose( fin );

      /*
       *  Print out the contents
       */
      MUD_show( pMUD_fileGrp, MUD_ALL );

      MUD_free( pMUD_fileGrp );
  }
  else if( !strncmp( command, "dump", 4 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      MUD2DMP_convert( infile, jj1, jj2 );
  }

  else if( !strncmp( command, "mud2txt", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".txt" );
      /*	printf( "final outfile=%s\n", outfile ); */

      MUD2TXT_convert( infile, outfile );
  }
  else
  {
      usage();
      return( 0 );
  }

  return( 0 );
}


      /*----------------------------------------*/
     /*  String handling routines              */
    /*----------------------------------------*/

/*
 *  string equality 
 *
 *    Returns 1 (true) or 0 (false)
 */
int 
streq( a, b )
    char* a;
    char* b;
{
    return( strcmp( a, b ) == 0 );
}


/*
 *----------------------------------------------------------------------
 *
 * strmatch --  Adapted from Tcl_StringMatch
 *              (added % character for VMS)
 *
 *	See if a particular string matches a particular pattern.
 *
 * Results:
 *	The return value is 1 if string matches pattern, and
 *	0 otherwise.  The matching operation permits the following
 *	special characters in the pattern: *?\[] (see the manual
 *	entry for details on what these mean).
 *
 * Side effects:
 *	None.
 *
 *----------------------------------------------------------------------
 */

int
strmatch( string, pattern )
    register char* string;
    register char* pattern;
{
    char c2;

    while (1) {
	/* See if we're at the end of both the pattern and the string.
	 * If so, we succeeded.  If we're at the end of the pattern
	 * but not at the end of the string, we failed.
	 */
	
	if (*pattern == 0) {
	    if (*string == 0) {
		return 1;
	    } else {
		return 0;
	    }
	}
	if ((*string == 0) && (*pattern != '*')) {
	    return 0;
	}

	/* Check for a "*" as the next pattern character.  It matches
	 * any substring.  We handle this by calling ourselves
	 * recursively for each postfix of string, until either we
	 * match or we reach the end of the string.
	 */
	
	if (*pattern == '*') {
	    pattern += 1;
	    if (*pattern == 0) {
		return 1;
	    }
	    while (1) {
		if (strmatch(string, pattern)) {
		    return 1;
		}
		if (*string == 0) {
		    return 0;
		}
		string += 1;
	    }
	}
    
	/* Check for a "?" as the next pattern character.  It matches
	 * any single character.
	 */

	if (*pattern == '?') {
	    goto thisCharOK;
	}

#ifdef VMS
	/* 
    	 * Check for a "%" as the next pattern character.  It matches
	 * any single character.
	 */

	if (*pattern == '%') {
	    goto thisCharOK;
	}
#endif /* VMS */

	/* Check for a "[" as the next pattern character.  It is followed
	 * by a list of characters that are acceptable, or by a range
	 * (two characters separated by "-").
	 */
	
	if (*pattern == '[') {
	    pattern += 1;
	    while (1) {
		if ((*pattern == ']') || (*pattern == 0)) {
		    return 0;
		}
		if (*pattern == *string) {
		    break;
		}
		if (pattern[1] == '-') {
		    c2 = pattern[2];
		    if (c2 == 0) {
			return 0;
		    }
		    if ((*pattern <= *string) && (c2 >= *string)) {
			break;
		    }
		    if ((*pattern >= *string) && (c2 <= *string)) {
			break;
		    }
		    pattern += 2;
		}
		pattern += 1;
	    }
	    while ((*pattern != ']') && (*pattern != 0)) {
		pattern += 1;
	    }
	    goto thisCharOK;
	}
    
	/* If the next pattern character is '/', just strip off the '/'
	 * so we do exact matching on the character that follows.
	 */
	
	if (*pattern == '\\') {
	    pattern += 1;
	    if (*pattern == 0) {
		return 0;
	    }
	}

	/* There's no special character.  Just make sure that the next
	 * characters of each string match.
	 */
	
	if (*pattern != *string) {
	    return 0;
	}

	thisCharOK: pattern += 1;
	string += 1;
    }
}


/*
 *  malloc and duplicate part of a string
 */
char*
strndup( str, max_len )
    char* str;
    int max_len;
{
    char* buf;
    char* pi;
    char* po;
    int i;

    buf = (char*)malloc( max_len + 1 );
    for( i = 0, pi = str, po = buf; 
         ( i < max_len ) && ( *pi != '\0' ); 
         pi++, po++, i++ )
    {
        *po = *pi;
    }

    *po = '\0';
    return( buf );
}


/*
 *  stoupper
 *  uppercase a character string
 */
char*
stoupper( inString )
    char* inString;
{
    char *pos = inString;
    while( *pos = toupper( *pos ) ) pos++;
    return( inString );
}


/*
 *  stolower
 *  lowercase a character string
 */
char*
stolower( inString )
    char* inString;
{
    char *pos = inString;
    while( *pos = tolower( *pos ) ) pos++;
    return( inString );
}


char*
stoprint( str_in, str_out )
    char* str_in;
    char* str_out;
{
    char* p_in;     
    char* p_out;

    for( p_in = str_in, p_out = str_out; *p_in != '\0'; p_in++, p_out++ ) 
    {
	*p_out = ( isprint( *p_in ) ) ? *p_in : ' ';
    }
    *p_out = '\0';

    return( str_out );
}


char*
stoprint_expand( str_in, str_out )
    char* str_in;
    char* str_out;
{
    char* p_in;     
    char* p_out;

    for( p_in = str_in, p_out = str_out; *p_in != '\0'; p_in++ ) 
    {
	if( !isascii( *p_in ) )
	{
	    sprintf( p_out, "\\%03o", (unsigned char)*p_in );
	    p_out += 4;
	}
	else if( !isprint( *p_in ) )
	{
	    sprintf( p_out, "\\%03o", (unsigned char)*p_in );
	    p_out += 4;
	}
	else if( *p_in == '\\' )
	{
	    sprintf( p_out, "\\\\", *p_in );
	    p_out += 2;
	}
	else
	{
	    *p_out = *p_in;
	    p_out++;
	}
    }
    *p_out = '\0';

    return( str_out );
}


/* 
 *  skipBlanks() - return a pointer to the first non-blank in the string
 */
char*
skipBlanks( ptr )
    char* ptr;
{
    while( ( *ptr != '\0' ) && !isgraph( *ptr ) ) 
    {
    	ptr++;
    }
    return( ptr );
}


/* 
 *  trimBlanks() - return a string with no leading or trailing blanks
 */
char*
trimBlanks( inStr, outStr )
    char* inStr;
    char* outStr;
{
    char* p1;
    char* p2;
    int len;

    p1 = skipBlanks( inStr );
    if( *p1 == '\0' )
    {
    	outStr[0] = '\0';
    	return( outStr );
    }

    for( p2 = &inStr[strlen(inStr)-1]; ( p2 > p1 ) && !isgraph( *p2 ); p2-- );
    len = p2 - p1 + 1;
    strncpy( outStr, p1, len );
    outStr[len] = '\0';

    return( outStr );
}


#ifdef __STDC__
void
padString( char* s, char c, int len )
#else
void
padString( s, c, len )
    char* s;
    char c;
    int len;
#endif /* __STDC__ */
{
    int slen = strlen( s );
    memset( &s[slen], c, len-slen );
}


