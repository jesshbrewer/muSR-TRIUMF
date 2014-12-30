/*
 *  mud_util.c -- main and CLI dispatch routines for
 *		      µSR Data Format Utility
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
 *  Revision history:
 *   v1.0   26-Jan-1994  TW  Initial version
 *   v1.1   16-Feb-1994  TW  Groups with member index
 *   v1.1a  21-Mar-1994  TW  Added tri2spreadsheet
 *   v1.1b  16-Sep-1994  TW  Output 2DAT and DAT2 have fixed 6 digit filename
 *   v1.1c  20-Sep-1994  TW  Output 2TRI and TRI2 have fixed 6 digit filename
 *   v1.1d  17-Jan-1996  TW  Output files have 3 digit extensions
 *   v2.0   06-Feb-1996  TW  Got rid of VMS-specific input routines;
 *                           generalize using getopt.
 *   v2.1      Nov-1999  DA  VME clock codes support
 *   v2.2   01-Aug-1999  DA  header (show) 
 *   v2.3   03-Jul-2001  DA  add combine command
 *   v2.4   13-Nov-2001  DA  add mud2col command like mud2txt
 *   v2.5   16-Jul-2002  DA  Accept a path/directory for "-o file" 
 *   v2.6   25-Nov-2003  DA  Histogram selection
 *   (switch to CVS log in descending order)
 *
 * $Log: mud_util.c,v $
 * Revision 1.9  2010/08/26 00:07:50  asnd
 * Release under GNU LGPL (mostly)
 * Changes for 64-bit Linux, especially for larger time_t (change time handling)
 * Always use private version of strndup
 *
 * Revision 1.8  2007/02/03 05:42:38  asnd
 * Incorporate ral2mud and psi2mud for (old) RAL and PSI data files
 *
 * Revision 1.7  2003/11/25 15:41:06  asnd
 * Add -h option for selecting histograms
 *
 * Revision 1.6  2002/07/17 01:03:35  asnd
 * Accept output file path, using input name
 *
 * Revision 1.5  2001/11/14 04:15:53  asnd
 * Make output files default to current working directory.
 * Add mud2col: multi-column mud2txt.
 *
 * Revision 1.4  2001/09/13 05:06:05  asnd
 * DA  - Use specified experimental area for header
 *
 * Revision 1.3  2001/07/04 04:52:00  asnd
 * Add combine command to add/subtract/concatenate runs
 *
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

#include "mud_util.h"
#include "getopt.h"

#ifdef usereadline
#include <readline/readline.h>
#include <readline/history.h>
#endif

int combine_mud     _ANSI_ARGS_(( char* inFile, char* addFile, char* outFile, int iAddSub ));
int MUD2TRI_convert _ANSI_ARGS_(( char* inFile, char* outFile, int nhsel, int hsel[] ));
int MUD2DAT_convert _ANSI_ARGS_(( char* inFile, char* outFile ));
int RAL2MUD_convert _ANSI_ARGS_(( char* inFile, char* outFile, char* exptnumber, char* exptmember, char* exptarea ));
int PSI2MUD_convert _ANSI_ARGS_(( char* infile, char* outfile, char* exptnumber, char* exptmember, char* exptarea ));
int TRI2MUD_convert _ANSI_ARGS_(( char* inFile, char* outFile, char* exptnumber, char* exptmember, char* exptarea ));
int DAT2MUD_convert _ANSI_ARGS_(( char* inFile, char* outFile, char* exptnumber, char* exptmember, char* exptarea ));
int TRI2spreadsheet_convert _ANSI_ARGS_(( char* inFile, char* outFile_pref ));
int MUD2TXT_convert _ANSI_ARGS_(( char* fin_name, char* fout_name, int nhsel, int hsel[] ));
int MUD2COL_convert _ANSI_ARGS_(( char* fin_name, char* fout_name, int nhsel, int hsel[] ));

void
usage()
{
    printf( "\n" );
    printf( "Usage: mud_util <command> [options] [<infile>]\n" );
    printf( "\n" );
    printf( "Commands:\n" );
    printf( "\ttri2mud/mud2tri  -  convert between TRIUMF TD-MuSR and MUD\n" );
    printf( "\tdat2mud/mud2dat  -  convert between TRIUMF I-MuSR and MUD\n" );
    printf( "\tpsi2mud/ral2mud  -  convert old PSI or RAL data to MUD\n" );
    printf( "\tmud2txt/mud2col  -  write histograms to text file\n" );
    printf( "\tcombine   -  combine two runs (with -a<file> or -s<file>)\n" );
    printf( "\theaders   -  display mud headers and scalers\n" );
    printf( "\tdump      -  diagnostic dump of mud file\n" );
    printf( "\thelp/?    -  print this screen\n" );
    printf( "\texit/quit -  exit program\n" );
    printf( "Options:\n" );
    printf( "\t-o<outfile>  -  specify output filename or path\n" );
    printf( "\t-e<expt#>  -a<area>  -m<members>  -  header info for __2mud\n" );
    printf( "\t-h<n1,n2...> -  select subset of histograms for mud2__\n" );
    printf( "\t-a/-s<file>  -  for combine: add/subtract <file> to <infile>\n" );
    printf( "Examples:  mud_util tri2mud 000160.tri\n" );
    printf( "           mud_util dump ed:[dlog]010024.msr\n" );
    printf( "           mud_util -e101 -mSRK&JB -o000158.msr dat2mud 158.dat\n" );
    printf( "           mud_util combine 005432.msr -a 005431.msr -o 00543x.msr\n" );
    printf( "\n" );
}


int
main( argc, argv )
    int argc;
    char* argv[];
{
    if( argc > 1 )
    {
        do_command( argc, argv );
        exit( 0 );
    }
    else
    {
	int i;
	char line[202];
        char* p;
        int aargc;
        char* aargv[32];

        aargv[0] = NULL;

	do {
#ifdef usereadline
          p = readline( "MUDutil> " );
          if( p == NULL ) exit( 0 );
          add_history( p );
          strncpy( line, p, 200 );
          _free( p );
#else
          printf( "MUDutil> " );
          fgets(line,200,stdin);
#endif
          p = line;
          aargc = 1;
          while( 1 )
          {
            while( *p && !isgraph( *p ) ) *p++ = 0;
            if( *p == 0 ) break;
            aargv[aargc++] = p;
            while( isgraph( *p ) ) p++;
          }
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
  char exptnumber[64];
  char exptmember[64];
  char exptarea[64];
  char infile[128];
  char outfile[128];
  char addfile[128];
  char command[32];
  int  iaddsub;
  int  nhistsel;
  int  hi[16];

  exptnumber[0] = '\0';
  exptmember[0] = '\0';
  exptarea[0] = '\0';
  infile[0] = '\0';
  outfile[0] = '\0';
  addfile[0] = '\0';
  iaddsub = 0;
  nhistsel = 0;

  optind = 0;

  if( argc == 1 ) return( 0 );

  while( 1 )
  {
    c = getopt( argc, argv, "e:m:o:s:a:h:" );
    if( c == EOF ) break;

    switch( c )
    {
      case 'e': 
        strcpy( exptnumber, optarg ); 
        break;

      case 'm':
        strcpy( exptmember, optarg );
        break;

      case 'o':
        strcpy( outfile, optarg );
        break;

      case 's':
        iaddsub = -1;
        strcpy( addfile, optarg );
        break;

      case 'a':
        iaddsub = 1;
        /* 
           -a gets dual-use:  
           the add-file name for combine
           the experimental area for xxx2mud
        */
        strcpy( addfile, optarg );
        strcpy( exptarea, optarg );
        break;

      case 'h':
        nhistsel = sscanf( 
              optarg,
              "%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d",
              &hi[0],&hi[1],&hi[2],&hi[3],&hi[4],&hi[5],&hi[6],&hi[7],
              &hi[8],&hi[9],&hi[10],&hi[11],&hi[12],&hi[13],&hi[14],&hi[15]
            );
        break;

      default:
        printf( "invalid option %c\n", c );
        usage();
        return( 0 );
    }
  }

  switch( argc-optind )
  {
      case 1:
        strcpy( command, argv[argc-1] );
        strcpy( infile, "" );
        break;

      case 2:
        strcpy( command, argv[argc-2] );
        strcpy( infile, argv[argc-1] );
        break;

      default:
        usage();
        return( 0 );
  }

  if( !strncmp( command, "e", 1 ) || !strncmp( command, "q", 1 ) )
  {
      return( -1 );
  } 
  else if( !strncmp( command, "mud2tri", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strcmp( outfile, "" ) ) 
      {
          baseset( outfile, infile );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              baseset( outfile+strlen(outfile), infile);
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".tri" );

      MUD2TRI_convert( infile, outfile, nhistsel, hi );
  }
  else if( !strncmp( command, "mud2dat", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strcmp( outfile, "" ) ) 
      {
        baseset( outfile, infile );
        while( outfile[0] == '0' ) strcpy( outfile, outfile+1 );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              p = outfile+strlen(outfile);
              baseset( p, infile);
              while( *p == '0' ) strcpy( p, p+1 );
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".dat" );

      MUD2DAT_convert( infile, outfile );
  }
  else if( !strncmp( command, "tri2mud", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".tri" );

      if( !strcmp( outfile, "" ) ) 
      {
          baseset( outfile, infile );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              baseset( outfile+strlen(outfile), infile);
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

      TRI2MUD_convert( infile, outfile, exptnumber, exptmember, exptarea );
  }
  else if( !strncmp( command, "dat2mud", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".dat" );

      if( !strcmp( outfile, "" ) ) 
      {
        baseset( outfile, infile );
        /*        p = outfile + strlen( outfile ); */
	i = strtol( outfile, &p, 10 );
	if( *p == 0 ) sprintf( outfile, "%06d", i );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              baseset( addfile, infile);
              i = strtol( addfile, &p, 10 );
              if( *p == 0 ) sprintf( outfile+strlen(outfile), "%06d", i );
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

      DAT2MUD_convert( infile, outfile, exptnumber, exptmember, exptarea );
  }
  else if( !strncmp( command, "psi2mud", 7 ) )    /* PSI2MUD function. -- GDM, 2002.1.2     */
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".BIN" );

      if( !strcmp( outfile, "" ) )
      {
          baseset( outfile, infile );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              baseset( outfile+strlen(outfile), infile);
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

      PSI2MUD_convert( infile, outfile, exptnumber, exptmember, exptarea );
  }
  else if( !strncmp( command, "ral2mud", 7 ) )    /* RAL2MUD function.   */
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".ral" ); /* ??? */

      if( !strcmp( outfile, "" ) )
      {
          baseset( outfile, infile );
      }
      else
      {
          baseset( addfile, outfile );
          if( !strcmp( addfile, "" ) ) /* outfile is just a path */
          {
              baseset( outfile+strlen(outfile), infile);
          }
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

      RAL2MUD_convert( infile, outfile, exptnumber, exptmember, exptarea );
  }
  else if( !strncmp( command, "combine", 7 ) )
  {
      if( !strcmp( infile, "" ) || !strcmp( addfile, "" ) || iaddsub == 0  )
      {
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strchr( addfile, '.' ) ) strcat( addfile, ".msr" );

      if( !strcmp( outfile, "" ) )
      {
        baseset( outfile, infile );
        /*  p = outfile + strlen( outfile );  */
	i = strtol( outfile, &p, 10 );
	if( *p == 0 ) sprintf( outfile, "%06d", i );
      }

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

      combine_mud( infile, addfile, outfile, iaddsub );
  }
  else if( !strncmp( command, "dump", 4 ) || !strncmp( command, "show", 4 ) )
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
       *  Don't put restrictions on file name (DA Jul 2002)
      if( !strstr( infile, ".msr" ) )
      {
        printf( "operation permitted on mud files (*.msr) only \n");
        return(0);
      }
      */

      /*
       *  Read the file
       */
      fin = MUD_openInput( infile );
      if( fin == NULL ) 
      {
        printf( "failed to open file \"%s\"\n", infile );
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
  else if( !strncmp( command, "head", 4 ) )
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
        printf( "failed to open file \"%s\"\n", infile );
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
      MUD_heads( pMUD_fileGrp, MUD_ALL );

      MUD_free( pMUD_fileGrp );
  }
  else if( !strncmp( command, "tri2txt", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".tri" );

      if( !strcmp( outfile, "" ) ) baseset( outfile, infile );

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".txt" );

      TRI2spreadsheet_convert( infile, outfile );
  }
  else if( !strncmp( command, "mud2txt", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strcmp( outfile, "" ) ) baseset( outfile, infile );

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".txt" );

      MUD2TXT_convert( infile, outfile, nhistsel, hi );
  }
  else if( !strncmp( command, "mud2col", 7 ) )
  {
      if( !strcmp( infile, "" ) ) 
      { 
        usage();
        return( 0 );
      }

      if( !strchr( infile, '.' ) ) strcat( infile, ".msr" );

      if( !strcmp( outfile, "" ) ) baseset( outfile, infile );

      if( !strchr( outfile, '.' ) ) strcat( outfile, ".txt" );

      MUD2COL_convert( infile, outfile, nhistsel, hi );
  }
  else if( !strncmp( command, "h", 1 ) )
  {
      usage();
      return( 0 );
  }
  else
  {
      usage();
      return( 0 );
  }

  return( 0 );
}

/* 
 * Check for valid list of chosen histograms.  Return FALSE for invalid,
 * TRUE for valid.  Also adjusts mhists to reflect number selected.
 */
BOOL
validateHistList ( nhsel, hsel, pmhists )
     int nhsel;
     int hsel[];
     int* pmhists;
{
  int i,j;

  if ( nhsel > 0 ) 
  {   /* If specific histograms are selected */
      /* check the number selected is reasonable */
      if ( nhsel > 16 || nhsel > *pmhists )  return( FALSE );
      /* check that each selection is in range, and not duplicated */
      for ( i=0; i<nhsel; i++ )
      {
          if ( hsel[i] < 1 || hsel[i] > *pmhists ) return( FALSE );
          if ( i > 0 ) 
          {
              for ( j=0; j<i; j++ )
              {
                  if ( hsel[i] == hsel[j] ) return( FALSE );
              }
          }
      }
      *pmhists = nhsel;
  }
  return( TRUE );
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
 *  Get base name from full file name
 */
void
baseset( base, full )
    char * base;
    const char * full;
{
    int j,s,d;
    char c;

    for( j=0, s=0, d=0;  
         (c = full[j]) != '\0';
         j++ )
    {
        if( c=='\\' || c=='/' || c==':' || c==']' ) s = j+1;
        if( c=='.' ) d = j;
    }

    if( d < s ) d = j;

    strncpy( base, full+s, d-s);
    base[d-s]='\0';

    return;
}


/*
 *  malloc and duplicate part of a string
 *  Always use this private version instead of strndup just to avoid the
 *  hassles of determining availability (no ./configure).
 */
char*
strndup_priv( str, max_len )
    const char* str;
    const size_t max_len;
{
    char* buf;
    char* pi;
    char* po;
    int i;

    buf = (char*)malloc( max_len + 1 );
    for( i = 0, pi = (char *)str, po = buf; 
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


