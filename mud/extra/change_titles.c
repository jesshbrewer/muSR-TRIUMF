
/*
 *  change_titles.c -- Change MUD file header fields.
 *
 *   Copyright (C) 2005 TRIUMF (Vancouver, Canada)
 *
 *   Authors: Donald Arseneau
 *
 *   Released under the GNU GPL - see http://www.gnu.org/licenses
 *
 *   This program is free software; you can distribute it and/or modify it under
 *   the terms of the GNU General Public License as published by the Free
 *   Software Foundation; either version 2 of the License, or any later version.
 *   Accordingly, this program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *   or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
 *   for more details.
 *
 * $Log: change_titles.c,v $
 * Revision 1.2  2010/08/26 00:12:34  asnd
 * Release under the GNU GPL.
 *
 * Revision 1.1  2005/06/14 01:38:31  asnd
 * Create extra utilities dir, containing initial version of change_titles
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "mud.h"

#define MAX_ERR 100


int change_titles( char* fname);
static int matchcmd(char* command, char* prototype, int len);
static int trimSpace( char* str );

static int open_mudfile( char* fname );
static void field_error( int i_fh, char* Name);
static int displaynum( int i_fh, int (*MUD_get)(int,UINT32*), char* Name );
static int displaystr( int i_fh, int (*MUD_get)(int,char*,int), char* Name );
static int displayhistnum( int i_fh, int (*MUD_get)(int,int,UINT32*), char* Name, int NumH );
static int displayhiststr( int i_fh, int (*MUD_get)(int,int,char*,int), char* Name, int NumH );
static int replacestr(int i_fh, int (*MUD_set)(int,char*), char* Name, char* value);
static int replacenum(int i_fh, int (*MUD_set)(int,UINT32), char* Name, char* value);
static int replacehiststr(int i_fh, int (*MUD_set)(int,int,char*), char* Name, int NumH, char* value);
static int replacehistnum(int i_fh, int (*MUD_set)(int,int,UINT32), char* Name, int NumH, char* value);

int errcnt;

static void usage()
{
    printf( "\n" );
    printf( "Usage: change_titles [file-name-or-run-number]\n" );
    printf( "When prompted, enter replacement fields as <field name> value(s)\n" );
    printf( "or one of the commands: exit, quit, show, help.\n" );
    printf( "\n" );
}

static void helpText()
{
  usage();

  printf("Type 'exit' to save changes and finish; 'quit' to abandon changes;\n");
  printf("'show' to display current values; 'help' for this help.\n");
  printf("\n");
  printf("When replacing header fields, the field name is case-insensitive,\n");
  printf("and may be abbreviated to 4 characters.\n");
  printf("\n");
  printf("Histogram parameters should be typed as a comma-separated list.\n");
  printf("Spaces around the commas are insignificant.  Blank items are\n");
  printf("left unchanged in the run file.  (You can't enter a blank histogram\n");
  printf("title, or one with a comma! Nor should you.)\n");
  printf("\n");
}


int main( argc, argv )
    int argc;
    char* argv[];
{
  char* p;

  if( argc > 2 )
    {
      usage();
      exit( 1 );
    }
  else if( argc == 2 )
    {  /*   interactive with prompting  */
      exit ( change_titles( argv[1] ) );
    }
  else
    {  /*   prompt for file name; then interactive with prompting  */ 
      int stat;
      char fname[128];
      do {
        printf( "Enter Mud file name or run number: " );
        if( !fgets(fname,126,stdin) ) exit( 0 );
        fname[127]='\0';
        p = strchr( fname, '\n' );
        if( p ) *p = '\0';
        stat = change_titles( fname );
      } while ( stat == 2 || stat == 1 );
      exit ( stat );
    }
}

#define DisplayNUM(Object,Name) \
  if( !displaynum( i_fh, MUD_get##Object, Name ) ) \
  { field_error(i_fh,Name); return ( 2 ); }

#define DisplaySTR(Object,Name) \
  if( !displaystr( i_fh, MUD_get##Object, Name ) ) \
  { field_error(i_fh,Name); return ( 2 ); }

#define DisplayHistNUM(Object,Name) \
  if( !displayhistnum( i_fh, MUD_get##Object, Name, NumH ) ) \
  { field_error(i_fh,Name); return ( 2 ); }

#define DisplayHistSTR(Object,Name) \
  if( !displayhiststr( i_fh, MUD_get##Object, Name, NumH) ) \
  { field_error(i_fh,Name); return ( 2 ); }

#define ReplaceNUM(Object,Name) \
  if( matchcmd(command,Name,4) ) {\
    replacenum(i_fh,MUD_set##Object,Name,arg1);\
    continue;}

#define ReplaceSTR(Object,Name) \
  if( matchcmd(command,Name,4) ) {\
    replacestr(i_fh,MUD_set##Object,Name,arg1);\
    continue;}

#define ReplaceHistNUM(Object,Name) \
  if( matchcmd(command,Name,4) ) {\
    replacehistnum(i_fh,MUD_set##Object,Name,NumH,arg1);\
    continue;}

#define ReplaceHistSTR(Object,Name) \
  if( matchcmd(command,Name,4) ) {\
    replacehiststr(i_fh,MUD_set##Object,Name,NumH,arg1);\
    continue;}

/* 
 *  change_titles:
 *  This is the main routine.  It does:
 *   - Open the mud data file
 *   - Read and display the existing headers
 *   - Loop, prompting for replacement commands (or quit/exit)
 *   - Close file (abandon changes on quit)
 *
 *  Parameter: string: file name or portion.
 *  Return value is the exit code:
 *  0 OK  
 *  1 no file
 *  2 failure to read file
 *  3 changes abandoned (EOF)
 *  4 abandon due to error(s)
 */

int change_titles( char *arg )
{
  int rnum;
  char c;
  char fname[127+1];

  int i_fh;
  UINT32 i_Type;
  UINT32 HType, NumH;

  char cmdline[256], command[256], arg1[512], rest[256];
  int na;

      
  errcnt = 0;

  /*
   * Check for run number vs file name
   */
  if ( sscanf( arg, "%d%c", &rnum, &c ) == 1 )
    { /* pure number, construct file name */
      sprintf( fname, "%06d.msr", rnum );
    }
  else
    { /* non-numeric */
      strncpy( fname, arg, 127 );
      fname[127]='\0';
    }

  /*
   *  Attempt open
   */
  i_fh = MUD_openReadWrite( fname, &i_Type);
  if (i_fh < 0)
    {
      /* failure.  See if we need .msr appended */
      if ( (strlen(fname) < 127-4) && (!strstr( fname, ".msr" )) )
        {
          strcat( fname, ".msr" );
          i_fh = MUD_openReadWrite( fname, &i_Type);
        }
    }
  if (i_fh < 0)
    {
      fprintf( stderr, "Could not open file %s for modification.\n%s\n",
               fname, "Check that it exists and you have write access.");
      return( 1 );
    }

  /*
   * Opened file; get run type identifier
   */

  if (MUD_getRunDesc( i_fh, &i_Type ) == 0) 
    {
      MUD_closeRead( i_fh );
      fprintf( stderr, "Could not read the run header from %s.\n%s\n",
               fname, "Is it really a MUD file?");
      return( 2 );
    }

  /*
   *  Do whole operation repeatedly if user asks "show"
   */

  while ( 1 )
    {

      /*
       * Display Header fields
       */
      DisplayNUM(RunNumber,"RunNumber");
      DisplayNUM(ExptNumber,"Experiment");
      DisplaySTR(Experimenter,"Operator");
      DisplaySTR(Title,"Title");
      DisplaySTR(Sample,"Sample");
      DisplaySTR(Orient,"Orient");
      
      if (i_Type == MUD_SEC_TRI_TI_RUN_DESC_ID) {
	DisplaySTR(Subtitle,"Subtitle");
      }
      else {
	DisplaySTR(Temperature,"Temperature");
	DisplaySTR(Field,"Field");
      }
      DisplaySTR(Area,"Beamline");
      DisplaySTR(Apparatus,"Rig");
      DisplaySTR(Insert,"Mode");
      
      if ( i_Type == MUD_SEC_TRI_TI_RUN_DESC_ID ) {
	DisplaySTR(Comment1,"Cmt1");
	DisplaySTR(Comment2,"Cmt2");
	DisplaySTR(Comment3,"Cmt3");
      }
      
      DisplayNUM(TimeBegin,"Startsec");
      DisplayNUM(TimeEnd,"Endsec");
      DisplayNUM(ElapsedSec,"Elapsedsec");
      
      /* Then the lists of parameters from the histogram headers */
 
      if ( MUD_getHists( i_fh, &HType, &NumH ) == 0 ) return( 2 );

      DisplayHistSTR(HistTitle,"HTitles");
	
      if ( i_Type != MUD_SEC_TRI_TI_RUN_DESC_ID ) {
        DisplayHistNUM(HistT0_Bin,"t0Bins");
        DisplayHistNUM(HistT0_Ps,"t0Ps");
        DisplayHistNUM(HistGoodBin1,"t1Bins");
        DisplayHistNUM(HistGoodBin2,"t2Bins");
        DisplayHistNUM(HistBkgd1,"Bg1Bins");
        DisplayHistNUM(HistBkgd2,"Bg2Bins");
      }

      printf( "\nNow enter replacement lines or one of: quit, exit, show, help\n\n" );

      /*
       * Finished initial display.  Now prompt for changes
       */
      
      while( 1 )
	{
	  if( errcnt > MAX_ERR ) {
	    printf( "Too many errors; quitting.\n" );
	    MUD_closeRead( i_fh );
	    return( 4 );
	  }
	  
	  printf( "ct> " );
	  fflush( stdout );
	  
	  if( fgets( cmdline, 256, stdin ) == NULL ) {
	    MUD_closeRead( i_fh );
	    return( 3 );
	  }
	  cmdline[255]='\0';
	  arg1[0] = '\0';

	  na = sscanf( cmdline, "%s %s%[^\n]", command, arg1, rest );

	  if( na < 1 ) {
	    printf( "Type exit to finish, quit to abandon\n");
	    errcnt++;
	    continue;
	  }

	  if( !strncasecmp( command, "exit", 4 ) ) {
	    if( MUD_closeWrite(i_fh) == 0) {
	      fprintf( stderr, "Could not write to file %s.\n%s\n",
		       fname, "Has it just disappeared?" );
	      MUD_closeRead( i_fh );
	      
	      return( 4 );
	    }
	    return( 0 );
	  }
	  
	  if( !strcasecmp( command, "quit" ) || !strcasecmp( command, "q" ) ) {
	    MUD_closeRead( i_fh );
	    return( 3 );
	  }
	  
	  if( !strcasecmp( command, "show" ) ) {
	    /* finish inner loop and go back to repeat initial display */
	    break;
	  }
	  
	  if( !strcasecmp( command, "help" ) ) {
            helpText();
	    continue;
	  }
	  
          /*
	  if( na < 2 ) {
	    printf( "Invalid replacement command; use format as shown above.\nType exit to finish, quit to abandon\n");
	    errcnt++;
	    continue;
	  }
          */
	  
	  if( na > 2 ) {
	    strncat( arg1, rest, 255 );
	  }
	  
	  ReplaceNUM(RunNumber,"RunNumber");
	  ReplaceNUM(ExptNumber,"Experiment");
	  ReplaceSTR(Experimenter,"Operator");
	  ReplaceSTR(Title,"Title");
	  ReplaceSTR(Sample,"Sample");
	  ReplaceSTR(Orient,"Orient");
	  
	  if (i_Type == MUD_SEC_TRI_TI_RUN_DESC_ID) {
	    ReplaceSTR(Subtitle,"Subtitle");
	  }
	  else {
	    ReplaceSTR(Temperature,"Temperature");
	    ReplaceSTR(Field,"Field");
	  }
	  ReplaceSTR(Area,"Beamline");
	  ReplaceSTR(Apparatus,"Rig");
	  ReplaceSTR(Insert,"Mode");
	  
	  if (i_Type == MUD_SEC_TRI_TI_RUN_DESC_ID) {
	    ReplaceSTR(Comment1,"Cmt1");
	    ReplaceSTR(Comment2,"Cmt2");
	    ReplaceSTR(Comment3,"Cmt3");
	  }
	  
	  ReplaceNUM(TimeBegin,"Startsec");
	  ReplaceNUM(TimeEnd,"Endsec");
	  ReplaceNUM(ElapsedSec,"Elapsedsec");

	  if( NumH > 0 ) 
	    {
	      ReplaceHistSTR(HistTitle,"HTitles");
	      
	      if ( i_Type != MUD_SEC_TRI_TI_RUN_DESC_ID ) {
		ReplaceHistNUM(HistT0_Bin,"t0Bins");
		ReplaceHistNUM(HistT0_Ps,"t0Ps");
		ReplaceHistNUM(HistGoodBin1,"t1Bins");
		ReplaceHistNUM(HistGoodBin2,"t2Bins");
		ReplaceHistNUM(HistBkgd1,"Bg1Bins");
		ReplaceHistNUM(HistBkgd2,"Bg2Bins");
	      }
	    }
	  
	  printf( "Error: Unknown title field.\nType exit to finish, quit to abandon\n");
	  errcnt++;
	} /* get next user entry */
    } /* go back and display entries */
}



static void field_error( int i_fh, char* Name)
{
  MUD_closeRead( i_fh );
  fprintf( stderr, "Error processing %s.\n", Name);
}

/*
 * displaynum and displaystr:
 * Show a field.  Return boolean for success (1=success)
 */
static int displaynum( int i_fh, int (*getProc)(int,UINT32*), char* name )
{
  UINT32 num;

  if( (*getProc)(i_fh,&num) == 0 ) return 0;

  printf( "%-12s %d\n", name, num );

  return 1;
}

static int displaystr( int i_fh, int (*getProc)(int,char*,int), char* name )
{
  char field[256];

  if( (*getProc)(i_fh,field,255) == 0 ) return 0;

  printf( "%-12s %s\n", name, field );

  return 1;
}


static int displayhistnum( int i_fh, int (*getProc)(int,int,UINT32*), char* name, int nh )
{
  UINT32 num;
  int j;

  for( j=1; j<=nh; j++) {
    if ( (*getProc)(i_fh,j,&num) == 0 ) return 0;
    if( j==1 ) 
      printf("%-12s %d", name, num);
    else 
      printf( ",%d",num);
  }
  printf("\n");
  return 1;
}

static int displayhiststr( int i_fh, int (*getProc)(int,int,char*,int), char* name, int nh )
{
  char str[64];
  int j;

  for( j=1; j<=nh; j++) {
    if ( (*getProc)(i_fh,j,str,64) == 0 ) return 0;
    if( j==1 )
      printf("%-12s %s",name,str);
    else
      printf( ",%s",str);
  }
  printf("\n");
  return 1;
}

static int replacestr(int i_fh, int (*setProc)(int,char*), char* name, char* value)
{
  if( *value == '\0' ) return 1;
  if( (*setProc)(i_fh, value) ) return 1;
  printf( "Error: Invalid %s string\n", name );
  errcnt++;
  return 0;
}

static int replacenum(int i_fh, int (*setProc)(int,UINT32), char* name, char* value)
{
  UINT32 num;
  char c;

  if( *value == '\0' ) return 1;
  if ( sscanf( value, "%d%c", &num, &c ) == 1 ) {
    if( (*setProc)(i_fh, num) ) return 1;
  }
  printf( "Error: Invalid %s value\n", name );
  errcnt++;
  return 0;
}

static int replacehiststr(int i_fh, int (*setProc)(int,int,char*), char* name, int nh, char* value)
{
  char str[64];
  int j;
  char *p;
  int errflag = 0;

  if( *value == '\0' ) return 1;
  p = value;
  for( j=1; j<=nh; j++) {
    str[0] = '\0';
    if( sscanf( p, "%[^,]", str ) ) {
      p += strlen(str);
    }
    if( *p == ',' ) p++;
    if( trimSpace( str ) > 0 ) {
      if( (*setProc)(i_fh, j, str) == 0) {
	printf( "Error: Invalid %s value for hist %d\n", name, j );
	errflag = 1;
      }
    }
  }
  if( errflag ) {
    errcnt++;
    return 0;
  }
  return 1;
}
      
static int replacehistnum(int i_fh, int (*setProc)(int,int,UINT32), char* name, int nh, char* value)
{
  char str[64];
  UINT32 num;
  int j;
  char *p;
  char c;
  int errflag = 0;

  if( *value == '\0' ) return 1;
  p = value;
  for( j=1; j<=nh; j++) {
    str[0] = '\0';
    if( sscanf( p, "%[^,]", str ) ) {
      p += strlen(str);
    }
    if( *p == ',' ) p++;
    if( trimSpace( str ) > 0 ) {
      strcat( str, ",." );
      if( sscanf( str, "%d,%c", &num, &c) == 2 ) {
        if( (*setProc)(i_fh, j, num) == 0) {
          printf( "Error: Invalid %s value for hist %d\n", name, j );
          errflag = 1;
        }
      }
      else {
        printf( "Error: Invalid %s value for hist %d\n", name, j );
        errflag = 1;
      } 
    }
  }
  if( errflag ) {
    errcnt++;
    return 0;
  }
  return 1;
}



/*
 * trimSpace:
 * Erase leading and trailing spaces.
 * Return final string length.
 */
static int trimSpace( char str[] ) 
{
  int j = 0;
  int k = 0;

  while( str[j] == ' ') j++ ;

  while( str[j] ) str[k++] = str[j++];

  str[k] = '\0';

  while( k ) {
    if( str[k-1] != ' ') break;
    str[--k] = '\0';
  }

  return k;
}

/*
 * matchcmd
 * Match two strings to see if command matches prototype.
 * Match is case-insensitive, and command may be abbreviated to
 * lesser of len parameter and length of prototype
 */

static int matchcmd(char* command, char* prototype, int len)
{
  int lp, lc, lm;

  lp = strlen(prototype);
  lp = (lp < len ? lp : len );
  lc = strlen(command);
  lm = (lc > lp ? lc : lp );

  return ( !strncasecmp(command, prototype, lm) );
}

