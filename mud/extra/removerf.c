/*
 *  removerf.c -- remove the 23 MHz cyclotron frequency from data.
 *
 *   Copyright (C) 2007,2008 TRIUMF (Vancouver, Canada)
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
 * $Log: removerf.c,v $
 * Revision 1.4  2010/08/26 00:12:34  asnd
 * Release under the GNU GPL.
 *
 * Revision 1.3  2008/08/18 13:52:27  asnd
 * Apply conversion also before first background bin.
 *
 * Revision 1.2  2007/11/14 14:10:58  asnd
 * Fixed removerf; Changed (recent) outputToFile to closeWriteFile
 *
 * Revision 1.1  2007/06/22 02:10:47  asnd
 * New utility to subtract RF frequency background
 *
 *
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "mud.h"

#define FNAME_LEN 128

#define RFFREQ 23.06     /* default frequency, MHz */

#define OOPS goto oops

void baseset ( char *base, const char *full );

static void usage()
{
  printf( "\n" );
  printf( "Usage: removerf [-b bgBin1,bgBin2] [-o output-file] [-f frequency] file-name-or-number\n" );
  printf( "The cyclotron's RF frequency will be flattened.\n" );
  printf( "Available options are:\n" );
  printf( " -b  num,num   the flat background bin range (all hists); else use the run header\n" );
  printf( " -o  filename  output file name; else use the input file name in current directory\n" );
  printf( " -f  num       specify cyclotron frequency, in MHz; else use %f\n", RFFREQ );
  printf( "\n" );
}

double rffreq = RFFREQ;
int bg1 = 0;
int bg2 = 0;

#define MUD_MAX_FILES 16
extern MUD_SEC_GRP* pMUD_fileGrp[];
extern FILE* mud_f[];


int main( argc, argv )
     int argc;
     char* argv[];
{
  FILE* fout = NULL;

  char* p;
  int c;
  char infile[FNAME_LEN] = { 0 } ;
  char outfile[FNAME_LEN] = { 0 } ;
  char buf[FNAME_LEN] = { 0 } ;
  int rnum;
  int ih;

  int i_fh;
  UINT32 i_Type;
  UINT32 HType, NumH;
  UINT32 NumBins, T0_Bin, GoodBin1, GoodBin2, Bkgd1, Bkgd2, numEvents;
  REAL64 BinSec;
  UINT32 * hData = NULL;
  void * pData = NULL;
  int dimData = 0;

  double rfperiod;
  UINT32 bgTot, bgCount, bgT, bgC ;
  int rfbin;
  UINT32 * rfCount = NULL ;
  double * rfData  = NULL ;
  int numrf = 0;
  double binsize, time, flat, new;
  int j;

  /*
   * Process optional parameters
   */
  optind = 0;
  while( 1 )
    {
      c = getopt( argc, argv, "b:o:f:h" );
      if( c == EOF ) break;
      
      switch( c )
        {
        case 'b': /* background bins range */
          
          if ( sscanf( optarg, "%i%*[ ,]%i", &bg1, &bg2 ) != 2 ) {
            printf( "invalid background-bin range: '%s'\n", optarg );
            usage();
            return( 0 );
          }

          break;

        case 'o': /* output */
          if ( strlen( optarg ) >= FNAME_LEN ) {
            printf( "filename too long: '%s'\n", optarg );
            usage();
            return( 0 );
          }
          strcpy( outfile, optarg );
          break;

        case 'f': /* frequency */
          if ( sscanf( optarg, "%lf", &rffreq ) != 1 ) {
            printf( "invalid RF frequency: '%s'\n", optarg );
            usage();
            return( 0 );
          }
          break;

        case 'h': /* help */
          usage();
          return( 0 );

        default:
          usage();
          return( 0 );

        }
    }

  switch( argc-optind )
  {
      case 1:
        strncpy( infile, argv[optind], FNAME_LEN );
        infile[FNAME_LEN-1] = '\0';
        break;

      default:
        usage();
        return( 0 );
  }

  if( !strcmp( infile, "" ) ) { 
    usage();
    return( 0 );
  }

  /*
   * Check for run number vs file name
   */
  if ( sscanf( infile, "%d%c", &rnum, &c ) == 1 ) {
    /* pure number, construct file name */
    sprintf( infile, "%06d.msr", rnum );
  }

  /*
   *  Attempt open
   */
  i_fh = MUD_openRead( infile, &i_Type);

  if (i_fh < 0) {
    /* failure.  See if we need .msr appended */
    if ( (strlen(infile) < FNAME_LEN-4) && (!strstr( infile, ".msr" )) ) {
      strcat( infile, ".msr" );
      i_fh = MUD_openRead( infile, &i_Type);
    }
  }

  if (i_fh < 0) {
    fprintf( stderr, "Could not open file %s.\n", infile );
    return( 1 );
  }


  /*
   *  Determine output file
   */
  if( !strcmp( outfile, "" ) ) baseset( outfile, infile );

  if( !strchr( outfile, '.' ) ) strcat( outfile, ".msr" );

  /*
   * Opened file; get run type identifier and histogram type ident
   */

  if ( i_Type != MUD_FMT_TRI_TD_ID ) {
    MUD_closeRead( i_fh );
    fprintf( stderr, "Data file is not TD MuSR.\n" );
    return( 2 );
  }

  if ( MUD_getRunDesc( i_fh, &i_Type ) == 0 ) {
    MUD_closeRead( i_fh );
    fprintf( stderr, "Could not read the run header from %s.\n%s\n",
             infile, "Is it really a MUD file?");
    return( 2 );
  }

  if ( i_Type != MUD_SEC_GEN_RUN_DESC_ID ) {
    MUD_closeRead( i_fh );
    fprintf( stderr, "Data file is not TD MuSR.\n" );
    return( 2 );
  }

  if ( MUD_getHists( i_fh, &HType, &NumH ) == 0 ) OOPS;
  if ( HType == MUD_GRP_GEN_HIST_ID ) {
    MUD_closeRead( i_fh );
    printf( "Histograms aren't counts.\n");
    return( 0 );
  }

  /* 
   *  Loop over histograms
   */
  for ( ih = 1; ih <= NumH; ih++ )
    {
      if( MUD_getHistNumBins(i_fh, ih, &NumBins) == 0 ) OOPS;
      if( MUD_getHistSecondsPerBin(i_fh, ih, &BinSec) == 0 ) OOPS;
      if( MUD_getHistT0_Bin(i_fh, ih, &T0_Bin) == 0 ) OOPS;
      if( MUD_getHistGoodBin1(i_fh, ih, &GoodBin1) == 0 ) OOPS;
      if( MUD_getHistGoodBin2(i_fh, ih, &GoodBin2) == 0 ) OOPS;
      if( MUD_getHistBkgd1(i_fh, ih, &Bkgd1) == 0 ) OOPS;
      if( MUD_getHistBkgd2(i_fh, ih, &Bkgd2) == 0 ) OOPS;
      if ( bg1 > 0 ) Bkgd1 = bg1;
      if ( bg2 > 0 ) Bkgd2 = bg2;
   
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

      /* 
       * RF cycle and bin size in ns; allocate arrays for rf cycle
       */
      rfperiod = 1000.0 / rffreq;
      binsize = 1.0e9 * BinSec;

      if ( numrf != (int)(rfperiod/binsize + 4.0) ) {
        numrf = (int)(rfperiod/binsize + 4.0);
        rfData = (double*) realloc ( (void*)rfData, 8 * (numrf+1) );
        rfCount = (UINT32*) realloc ( (void*)rfCount, 4 * (numrf+1) );
      }

      /*
       * Zero data for RF waveform
       */
      bgTot = 0;
      bgCount = 0;
      for ( j=0; j<numrf; j++ ) {
        rfData[j] = 0.0;
        rfCount[j] = 0;
      }

      /*
       * Loop over background bins in the histogram, building waveform
       */
      time = 0.0;
      bgTot = 0;
      bgCount = 0;
      bgT = 0;
      bgC = 0;
      for ( j=Bkgd1; j<=Bkgd2; j++ )
        {
          while ( time > rfperiod ) {
            time -= rfperiod ;
            /* 
             * Find flat portion of background by averaging in chunks of the rf period
             */
            bgTot += bgT;
            bgCount += bgC;
            bgT = 0;
            bgC = 0;
          }
          rfbin = (int)( time/binsize );
          rfData[rfbin] += (double)hData[j];
          rfCount[rfbin]++;
          bgT += hData[j];
          bgC++;
          time += binsize;
        } 

      /*
       * If no background counts, go on to next histogram 
       */
      if ( bgTot < 5 || bgCount < 2 ) continue;

      /*
       * Take averages in waveform
       */

      flat = ((double)bgTot) / ((double)bgCount);
      for ( j=0; j<numrf; j++ )
        {
          rfData[j] = rfData[j] / ((double)rfCount[j]);
          /* printf( "%5i   %5i   %8.2f\n",j, rfCount[j],rfData[j]); */
        }
      /*
       * If last count is much less than the average count, then put the first count there
       */
      if ( rfCount[numrf-1] < 0.5*(double)(Bkgd2-Bkgd1+1)/(double)(numrf) + 1.0 )
        {
          rfData[numrf-1] = rfData[0];
        }
      rfData[numrf] = rfData[0];

      /* 
       * Go through the data, subtracting the rf waveform and adding the flat average.
       * First, go up from the beginning of the nominal background range
       */
      time = 0.0;
      for ( j=Bkgd1; j<NumBins; j++ )
        {
          while ( time > rfperiod )  {
            time -= rfperiod ;
          }
          rfbin = (int)( time/binsize );
          new = (double)( 0.5 + hData[j] - rfData[rfbin] + flat ) ;
          /* printf( "%5i  %3i  %8i - %9.1f + %9.1f = %9.1f\n", j,rfbin,hData[j],rfData[rfbin],flat, new); */
          if ( new > 0.0 ) {
            hData[j] = (UINT32)( new );
          }
          time += binsize;
        } 
      /*
       * Second, go earlier, below the stated background bin range, until zero bins are detected
       */
      time = 0.0;
      for ( j=Bkgd1-1; j>0; j-- )
        {
          time -= binsize;
          while ( time < 0.0 )  {
            time += rfperiod ;
          }
          rfbin = (int)( time/binsize );
          new = (double)( 0.5 + hData[j] - rfData[rfbin] + flat ) ;
          /* printf( "%5i  %3i  %8i - %9.1f + %9.1f = %9.1f\n", j,rfbin,hData[j],rfData[rfbin],flat, new); */
          if ( hData[j] == 0 || new <= 0.0 ) break ;
          hData[j] = (UINT32)( new );
        } 
      /*
       * Finished this histogram.  Write the data back
       */
      numEvents = 0;
      for ( j=0; j<NumBins; j++ )
        numEvents += hData[j];
          
      if( MUD_setHistData(i_fh, ih, pData) == 0 ||
          MUD_setHistNumEvents( i_fh, ih, numEvents ) == 0 )
        {
          MUD_closeRead( i_fh );
          if ( pData ) free( pData );
          fprintf( stderr, "Could not apply changed histogram %d.\n", ih );
          return( 4 );
        }

    } /* End loop over histograms */

  /*
   *  Now write out the modified data to the outfile. 
   */

  if ( MUD_closeWriteFile( i_fh, outfile ) == 0 ) 
    {
      fprintf( stderr, "failed to output file \"%s\"\n", outfile );
      MUD_closeRead( i_fh );
      return(4);
    }

  return( 0 );

 oops:
  MUD_closeRead( i_fh );
  if ( pData ) free( pData );
  fprintf( stderr, "Could not read some histogram information from %s.\n", infile );
  return( 2 );

}

/*
 *  Get base name from full file name (same as in mud_util.c)
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

