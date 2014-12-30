
/*
 *  badbin.c -- Fix "bad bins"
 *
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
 * $Log: badbin.c,v $
 * Revision 1.3  2010/08/26 00:12:34  asnd
 * Release under the GNU GPL.
 *
 * Revision 1.2  2005/06/18 07:49:10  asnd
 * First polishing.
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "mud.h"

#define FNAME_LEN 64

/* 
 *  Perform comparisons of bin values against standard deviations expanded
 *  by the formula:   STD_FAC * std_dev + STD_ADD
 */
#define STD_FAC 10.0
#define STD_ADD 50.0

#define OOPS goto oops

static void usage()
{
  printf( "\n" );
  printf( "Usage: badbin [-b] [-i] [-l] [-a] [file-name-or-run-number]\n" );
  printf( "The run file will be searched for wildly erroneous bins.\n" );
  printf( "Available options are:\n" );
  printf( " -b  batch        fix all bad bins without prompting\n" );
  printf( " -i  interactive  prompt for fixing each bad bin\n" );
  printf( " -l  list         list bad bins without fixing\n" );
  printf( " -a  all bins     look at all bins, not just the \"good\" ranges\n" );
  printf( "The default for b/i mode depends on if standard-input is a terminal.\n" );
  printf( "\n" );
}

int main( argc, argv )
     int argc;
     char* argv[];
{
  char* p;
  int c;
  char fname[FNAME_LEN] = { 0 } ;
  int rnum;
  int ih;

  int i_fh;
  UINT32 i_Type;
  UINT32 HType, NumH;
  UINT32 NumBins, T0_Bin, GoodBin1, GoodBin2, Bkgd1, Bkgd2, numEvents;
  UINT32 * hData = NULL;
  void* pData = NULL;
  int dimData = 0;

  double ave, std;
  int j;
  int allchange, histchange;

  int interact = 1;
  int listonly = 0;
  int allbins  = 0;

  BOOL fixok;
  BOOL begin;
  char ans[16] = { 0 };

  interact = isatty( fileno( stdin ) );

  /*
   * Process optional parameters
   */
  optind = 0;
  while( 1 )
    {
      c = getopt( argc, argv, "abilh" );
      if( c == EOF ) break;
      
      switch( c )
        {
        case 'a': /* all bins */
          allbins = 1;
          break;

        case 'b': /* batch */
          interact = 0;
          break;

        case 'i': /* interactive */
          interact = 1;
          break;

        case 'l': /* listing */
          listonly = 1;
          break;

        case 'h': /* help */
          usage();
          return( 0 );

        default:
          usage();
          return( 0 );
        }
    }

  /*
   * Process non-option (but optional) run identity parameter
   */
  switch( argc-optind )  /* number of non-option parameters */
    {
    case 0:  /* prompt for file name or number (even with -b specified!) */
      printf( "Enter Mud file name or run number: " );
      if( !fgets(fname,FNAME_LEN,stdin) ) exit( 0 );
      fname[FNAME_LEN]='\0';
      p = strchr( fname, '\n' );
      if ( p ) *p = '\0';
      break;

    case 1:  /* file name or number was provided */
      strncpy( fname, argv[optind], FNAME_LEN );
      fname[FNAME_LEN-1] = '\0';
      break;

    default:  /* too many parameters */
      usage();
      return( 0 );
    }

  /*
   * Check for run number vs file name
   */
  if ( sscanf( fname, "%d%c", &rnum, &c ) == 1 )
    { /* pure number, construct file name */
      sprintf( fname, "%06d.msr", rnum );
    }

  /*
   *  Attempt open
   */
  if ( listonly ) 
    i_fh = MUD_openRead( fname, &i_Type);
  else
    i_fh = MUD_openReadWrite( fname, &i_Type);

  if (i_fh < 0)
    {
      /* failure.  See if we need .msr appended */
      if ( (strlen(fname) < FNAME_LEN-4) && (!strstr( fname, ".msr" )) )
        {
          strcat( fname, ".msr" );
          if ( listonly ) 
            i_fh = MUD_openRead( fname, &i_Type);
          else
            i_fh = MUD_openReadWrite( fname, &i_Type);
        }
    }
  if (i_fh < 0)
    {
      fprintf( stderr, "Could not open file %s for modification.\n", fname );
      fprintf( stderr, "Check that it exists" );
      if (!listonly )  fprintf( stderr, " and you have write access");
      fprintf( stderr, ".\n" );
      return( 1 );
    }

  /*
   * Opened file; get run type identifier and histogram type ident
   */

  if (MUD_getRunDesc( i_fh, &i_Type ) == 0) 
    {
      MUD_closeRead( i_fh );
      fprintf( stderr, "Could not read the run header from %s.\n%s\n",
               fname, "Is it really a MUD file?");
      return( 2 );
    }

  if ( MUD_getHists( i_fh, &HType, &NumH ) == 0 ) OOPS;
  if ( HType == MUD_GRP_GEN_HIST_ID ) 
    {
      MUD_closeRead( i_fh );
      printf( "Histograms aren't counts.\n");
      return( 0 );
    }

  /* 
   *  Loop over histograms
   */
  allchange = 0;
  for ( ih = 1; ih <= NumH; ih++ )
    {
      if( MUD_getHistNumBins(i_fh, ih, &NumBins) == 0 ) OOPS;
      if( MUD_getHistT0_Bin(i_fh, ih, &T0_Bin) == 0 ) OOPS;
      if( MUD_getHistGoodBin1(i_fh, ih, &GoodBin1) == 0 ) OOPS;
      if( MUD_getHistGoodBin2(i_fh, ih, &GoodBin2) == 0 ) OOPS;
      if( MUD_getHistBkgd1(i_fh, ih, &Bkgd1) == 0 ) OOPS;
      if( MUD_getHistBkgd2(i_fh, ih, &Bkgd2) == 0 ) OOPS;
   
      if( NumBins < 10 ) continue;
      if( NumBins > dimData ) 
        {
          pData = realloc( (void*)hData, 4*NumBins );
          if( pData )
            {
              hData = (UINT32*)pData;
              dimData = NumBins;
            }
          else
            {
              OOPS;
            }
        }
      if( MUD_getHistData(i_fh, ih, pData) == 0 ) OOPS;

      if( allbins )
        {
          GoodBin1 = 2;
          GoodBin2 = NumBins-2;
        }

      /*
       * Loop over bins in the histogram, detecting bad bins.
       */
      histchange = 0;
      begin = TRUE;
      for( j=GoodBin1-1; j<=GoodBin2-1; j++ )
        {
          /*
           *  Specially ignore the "bad bin" that is the first with any data
           */
          if( begin && hData[j-2] == 0 && hData[j-1] == 0 )
            {
              continue;
            }
          begin = FALSE;

          /*
           * Calculate properties
           */
          ave = (hData[j-2]+hData[j-1]+hData[j+1]+hData[j+2])/4.0;
          std = sqrt(  pow(((double)hData[j-2]-ave),2)
                     + pow(((double)hData[j-1]-ave),2)
                     + pow(((double)hData[j+1]-ave),2)
                     + pow(((double)hData[j+2]-ave),2) + 4.0*ave) / 2.0;
          /*
           * The test is whether a point differs from the average
           * of its four nearest neighbours by more than 
           *   STD_FAC * std + STD_ADD
           * where "std" is the scatter [sqrt(variance)] plus the expected 
           * poisson error, added in quadrature.
           */
          if( fabs( ave - (double)hData[j] ) > STD_FAC * std + STD_ADD )
            {
              /*
               * Failed test; we have a bad bin. 
               */
              printf("Bad bin %d in hist %d.  Value %d, expect %d.\n",
                     j+1, ih, hData[j], (UINT32)ave);
              /*
               printf("Region:  %d  %d  %d  %d  %d\n",
                    hData[j-2], hData[j-1], hData[j], hData[j+1], hData[j+2] );
               printf("Ave: %f,  Std: %f,  Diff: %f\n", ave, std,
                     fabs( ave - (double)hData[j] ) );
               */
              /*
               *  When only listing, then we are done with this point
               */
              if( listonly ) continue;
              
              /*
               * Perhaps ask to fix this point
               */
              fixok = TRUE;
              if( interact )
                {
                  printf("Fix it? [Yn] ");
                  fflush( stdout );
                  if ( fgets( ans, 16, stdin ) )
                    {
                      if( ans[0]=='n' || ans[0]=='N' || ans[0]=='f' || ans[0]=='F' || ans[0]=='0' )
                        fixok = FALSE;
                    }
                  else
                    {
                      MUD_closeRead( i_fh );
                      if ( pData ) free( pData );
                      if ( allchange )
                        printf( "Aborted\n" );
                      return( 0 );
                    }
                }
              
              /*
               * Apply and note the fix, if approved.
               */
              if ( fixok )
                {
                  hData[j] = (UINT32)ave;
                  allchange++;
                  histchange++;
                }
            } /* end handling bad point */
        } /* end points in this histogram */
      
      /*
       * Finished this histogram.  If any changes, then write it back
       */
      
      if( histchange )
        {
          numEvents = 0;
          for( j=0; j<NumBins; j++ )
            numEvents += hData[j];
          
          if( MUD_setHistData(i_fh, ih, pData) == 0 ||
              MUD_setHistNumEvents( i_fh, ih, numEvents ) == 0 )
            {
              MUD_closeRead( i_fh );
              if ( pData ) free( pData );
              fprintf( stderr, "Could not apply changed histogram %d.\n", ih );
              return( 4 );
            }
        }

    } /* End loop over histograms */

  if ( listonly || allchange == 0 ) 
    {
      MUD_closeRead( i_fh );
    }
  else
    {
      MUD_closeWrite( i_fh );
      printf( "%d bad bins repaired.\n", allchange );
    }

  return( 0 );

 oops:
  MUD_closeRead( i_fh );
  if ( pData ) free( pData );
  fprintf( stderr, "Could not read some histogram information from %s.\n", fname );
  return( 2 );

}
