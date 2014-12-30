/*
 *  ral2mud.c -- Conversion of RAL to MUD format
 *
 *
 *   Copyright (C) 1995-2000 TRIUMF (Vancouver, Canada)
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
 *   15-Aug-1995  TW  partial
 *   19-Apr-2000  DA  first (semi-)functional
 */

#include <stdio.h>
#include <math.h>

#include "mud_util.h"
#include "ral_fmt.h"

int RecordInfo;  /* number of (2-byte) ints of VMS file record info */

int getMonth _ANSI_ARGS_(( char *s ));

#define  _ral_str_cpy( to, from, nc ) \
              strncpy( tempString, from, nc ); \
              tempString[nc] = '\0'; \
              trimBlanks( tempString, tempString ); \
              to = strdup( tempString )

static int
RAL2MUD_hist( pRalHeader, pHistName, ralHist, 
	      pMUD_histHdr, pMUD_histDat )
    RAL_HEADER* pRalHeader;
    char* pHistName;
    UINT32 ralHist[];
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
{
    int status;
    INT32 nt0, nt00, nt01, nmid, ncl;
    UINT32 nevtot;
    int i,j,iz;
    char tempString[32];
    long int b20us;

    _ral_str_cpy( pMUD_histHdr->title, pHistName, 10 );
    pMUD_histHdr->histType = MUD_SEC_TRI_TD_HIST_ID;

    nevtot = 0;
    for( j = 0; j < pRalHeader->histlen; j++ )
    {
        nevtot += ralHist[j];
    }
    pMUD_histHdr->nEvents = _swap32( nevtot );

    pMUD_histHdr->nBins = pRalHeader->histlen;

    pMUD_histHdr->fsPerBin = 1000*pRalHeader->res_pscnds;

    pMUD_histHdr->bytesPerBin = 0;

    nt0 = pRalHeader->t0bin[0]; /* ??? */
    pMUD_histHdr->t0_bin = nt0;
    pMUD_histHdr->goodBin1 = pRalHeader->tgood_begin[0];  /* ??? */
    pMUD_histHdr->goodBin2 = pRalHeader->tgood_end[0];    /* ??? */
    pMUD_histHdr->t0_ps = 0.001*(pMUD_histHdr->fsPerBin)*( (double)nt0 - 0.5 );

    /* 
     * find some decent background bin range (there is no valid t<0 range)
     *
     * Use the time after 20 microseconds if histograms are long enough,
     * Otherwise search for bins with zero counts.
     */

    b20us = 20000000 / pRalHeader->res_pscnds;  /* bins for 20 µs */

    if( b20us*21/20 < pRalHeader->histlen )
    {
        pMUD_histHdr->bkgd1 = pRalHeader->histlen/2 + b20us*19/40 ;
        pMUD_histHdr->bkgd2 = pRalHeader->histlen - 1;
    }
    else
    {
        pMUD_histHdr->bkgd1 = 0;
	pMUD_histHdr->bkgd2 = 0;
	for( j=0; j<pRalHeader->histlen; j++ )
	{
	    if( ralHist[j] == 0 )
	    {
		pMUD_histHdr->bkgd1 = j;
		pMUD_histHdr->bkgd2 = j;
		break;
	    }
	}
    }

    pMUD_histHdr->nBytes = MUD_SEC_GEN_HIST_pack( pMUD_histHdr->nBins, 
		    4, ralHist, 
		    pMUD_histHdr->bytesPerBin, pMUD_histDat->pData );
    pMUD_histDat->nBytes = pMUD_histHdr->nBytes;

    return( SUCCESS );
}

int
RAL2MUD_runDesc( pRalHeader, pMUD_desc, exptnumber, exptmember, exptarea )
    RAL_HEADER* pRalHeader;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    int status;
    int i;
    char tempString[256];

    int expt;
    char* p;

    if( pRalHeader->nrun32 < 1 ) pMUD_desc->runNumber = pRalHeader->nrun;
    else pMUD_desc->runNumber = pRalHeader->nrun32;

    expt = strtol( exptnumber, &p, 0 );
    if( *p == 0 ) pMUD_desc->exptNumber = expt;
    pMUD_desc->experimenter = strdup( exptmember );

    /*
     *  Assemble time info (from dat2mud)
     */
    mu_interp_time( &pMUD_desc->timeBegin, pRalHeader->start_date, pRalHeader->start_time );
    mu_interp_time( &pMUD_desc->timeEnd, pRalHeader->stop_date, pRalHeader->stop_time );
    pMUD_desc->elapsedSec = pMUD_desc->timeEnd - pMUD_desc->timeBegin;

    pMUD_desc->method = strdup( "TD-µSR" );
    pMUD_desc->lab = strdup( "RAL" );
    pMUD_desc->das = strdup( "RAL" );

    _ral_str_cpy( pMUD_desc->apparatus, pRalHeader->apparat, 8 );

    if (exptarea && *exptarea) pMUD_desc->area = strdup( exptarea );
    else _ral_str_cpy( pMUD_desc->area, pRalHeader->apparat, 8 );

    _ral_str_cpy( pMUD_desc->title, pRalHeader->rtitle, 39 );

    pMUD_desc->sample = NULL;

    strncpy( tempString, &pRalHeader->rtitle[11], 8 );
    tempString[8] = '\0';
    trimBlanks( tempString, tempString );
    strcat( tempString, " K" );
    pMUD_desc->temperature = strdup( tempString );

    strncpy( tempString, &pRalHeader->rtitle[20], 8 );
    tempString[8] = '\0';
    trimBlanks( tempString, tempString );
    strcat( tempString, " G" );
    pMUD_desc->field = strdup( tempString );

    if( pRalHeader->rtitle[28] == 't' )
    {
        strncpy( tempString, "transverse", 13 );
    }
    else if( pRalHeader->rtitle[28] == 'l' )
    {
        strncpy( tempString, "longitudinal", 13 );
    }
    else if( strncmp( &pRalHeader->rtitle[30], "NONE", 4 ) == 0 )
    {
        strncpy( tempString, "none", 5 );
    }
    else
    {    
        strncpy( tempString, &pRalHeader->rtitle[30], 10 );
    }
    pMUD_desc->orient = strdup( tempString );

    pMUD_desc->insert = NULL;

    return( SUCCESS );
}


int
RAL2MUD_hists( fin, pRalHeader, pMUD_histGrp )
    FILE* fin;
    RAL_HEADER* pRalHeader;
    MUD_SEC_GRP* pMUD_histGrp;
{
    int status;
    int im;
    int i;
    int j;
    int k;
    int numMudHis;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    UINT32 ralData[2048];
    UINT32 ralHists[8][2048] = { 0 };
    unsigned char *pd;
    int reclen;
    UINT16 recCon[2] = { 0 };

    int nhtot,mhtot,nrhist;
    int num;
    int size;
    int ralSize;
    int num_blocks;

    BOOL mapping = { FALSE } ;
    int longi_map[64] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                          1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
			  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
			  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3 };
    int trans_map[64] = { 2,1,1,1,1,0,0,0,0,3,3,3,3,2,2,2,
                          2,3,3,3,3,0,0,0,0,1,1,1,1,2,2,2,
			  6,5,5,5,5,4,4,4,4,7,7,7,7,6,6,6,
                          6,7,7,7,7,4,4,4,4,5,5,5,5,6,6,6 };
    int* hist_map, rhdat;

    char ral_h_names[2][8][16] = {
      "forw", "back", "forw", "back", "", "", "", "", /* LF */
      "up", "back", "down", "forw", "up", "back", "down", "forw", /* TF */
    };
    char hist_name[10];
    char rg_names[2][4] = { "_r", "_g" };
    int lftf = 0;

    /* 
     * We want to combine or "group" the RAL histograms.  We might
     * read in a "grouping" file to assign the mapping, but for now
     * we will use two defaults:
     *
     * transverse:
     *   1 top       9  6  7  8 25 22 23 24 (next 32 in same order)
     *   2 backward  5  2  3  4 29 26 27 28
     *   3 bottom    1 17 14 15 16 32 30 31
     *   4 forward  13 10 11 12 21 18 19 20
     *
     * longitudinal:
     *   1 forward    1-16 (and 33-48)
     *   2 backward  17-32 (and 49-64)
     *
     * unknown:
     *   each histogram maps one to one.
     */

    nhtot = pRalHeader->numhis;

    mhtot = nhtot;
    if( nhtot == 32 )
      {
	if( pRalHeader->rtitle[28] == 'l' ) 
	  {
	    lftf = 0;
	    mhtot = 2;
	    hist_map = longi_map;
	    mapping = TRUE;
	  }
	else if( pRalHeader->rtitle[28] == 't' ) 
	  {
	    lftf = 1;
	    mhtot = 4;
	    hist_map = trans_map;
	    mapping = TRUE;
	  }
      }

    nrhist = mhtot; /*number of "red" histograms */

    if( pRalHeader->rgmode != 0 )
    {
        nhtot *= 2;
	mhtot *= 2;
    }

    size = 4*pRalHeader->histlen;

    if( mapping ) 
    {
	/* 
	 * Pre-Read RAL histograms, accumulating in the appropriate destination arrays
	 */
	for( i = 0; i < nhtot; i++ )
	{

	  pd = (unsigned char*) ralData;
	  for( j=0; j<size; )
	    {
	      switch (RecordInfo)
		{
		case 0:
		  reclen = size;
		  break;
		case 1:
		  num = fread( &recCon[0], 2, 1, fin );
		  if( num!=1 ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
		  reclen = _min( 2042, (size-j) );
		  break;
		case 2:
		  num = fread( &recCon[0], 2, 2, fin );
		  if( num!=2 ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
		  reclen = recCon[0] - 2;
		  break;
		}
	      num = fread( pd+j, 1, reclen, fin );
#ifdef ral_debug
     printf( "Reading histogram %i; have %i bytes, read %i more, got %i.\n", 
         i, j, reclen, num );
#endif
	      if( num!=reclen ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
	      j += reclen;
	    }
  
	  im = hist_map[i];

	  /*
	  printf("\n\nRaw hist %i (contributes to %i):",i,im);
	  printf( "\n       -----------------------------------------------------------\n");
	  printf( "          0     1     2     3     4     5     6     7     8     9  " );
	  printf( "\n       -----------------------------------------------------------\n");

	  for( j=0; j<pRalHeader->histlen; j++ )
	    {
	      if( j%10 == 0 ) printf("\n%5i",j);
	      printf("%6i",ralData[j]);
	    }
	  */

	  for( j = 0; j < pRalHeader->histlen; j++ )
            {
                ralHists[im][j] += ralData[j];
            }
	}
	/* 
	 *Store all resultant histograms
	 */
	for( i = 0; i < mhtot; i++ )
	{
	    pMUD_histHdr = (MUD_SEC_GEN_HIST_HDR*)MUD_new( MUD_SEC_GEN_HIST_HDR_ID, i+1 );
	    if( pMUD_histHdr == NULL ) 
	    {
		fprintf( stderr, "failed to malloc pMUD_histHdr\n" );
		goto error;
	    }
	    
	    pMUD_histDat = (MUD_SEC_GEN_HIST_DAT*)MUD_new( MUD_SEC_GEN_HIST_DAT_ID, i+1 );
	    if( pMUD_histDat == NULL ) 
	    {
		fprintf( stderr, "failed to malloc pMUD_histDat\n" );
		goto error;
	    }

	    pMUD_histDat->pData = (char*)zalloc( size );
	    if( pMUD_histDat->pData == NULL )
	    {
		goto error;
	    }
	    
	    bcopy( ralHists[i], pMUD_histDat->pData, size );

	    strncpy( hist_name, ral_h_names[lftf][i], 8);
	    if( pRalHeader->rgmode != 0 ) 
	    {
		strncat( hist_name, rg_names[ i/nrhist ], 3 );
	    }
	    
	    status = RAL2MUD_hist( pRalHeader, hist_name, ralHists[i], pMUD_histHdr, pMUD_histDat );
	    
	    MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
	    MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
	}
    }
    else
    {
	/* 
	 * Don't know any mapping, so make a mud histogram for every RAL hist.
	 */
      for( i = 0; i < nhtot; i++ )
	{
	  /*
	   *  Read the histogram
	   */
	  pd = (unsigned char*) ralData;
	  for( j=0; j<size; )
	    {
	      switch (RecordInfo)
		{
		case 0:
		  reclen = size;
		  break;
		case 1:
		  num = fread( &recCon[0], 2, 1, fin );
		  if( num!=1 ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
		  reclen = _min( 2042, (size-j) );
		  break;
		case 2:
		  num = fread( &recCon[0], 2, 2, fin );
		  if( num!=2 ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
		  reclen = recCon[0] - 2;
		  break;
		}
	      num = fread( pd+j, 1, reclen, fin );
#ifdef ral_debug
     printf( "Reading histogram %i; have %i bytes, read %i more, got %i.\n", 
         i, j, reclen, num );
#endif
	      if( num!=reclen ){ fprintf( stderr, "unexpected end-of-file in input file\n" ); goto error; }
	      j += reclen;
	    }
  
	  pMUD_histHdr = (MUD_SEC_GEN_HIST_HDR*)MUD_new( MUD_SEC_GEN_HIST_HDR_ID, i+1 );
	  if( pMUD_histHdr == NULL ) 
	    {
	      fprintf( stderr, "failed to malloc pMUD_histHdr\n" );
	      goto error;
	    }
	  
	  pMUD_histDat = (MUD_SEC_GEN_HIST_DAT*)MUD_new( MUD_SEC_GEN_HIST_DAT_ID, i+1 );
	  if( pMUD_histDat == NULL ) 
	    {
	      fprintf( stderr, "failed to malloc pMUD_histDat\n" );
	      goto error;
	    }
	  
	  pMUD_histDat->pData = (char*)zalloc( size );
	  if( pMUD_histDat->pData == NULL )
	    {
	      goto error;
	    }
	  
	  bcopy( pMUD_histDat->pData, ralData, size );
	  
	  sprintf( hist_name, "h%d\0", (i%nrhist)+1 );
	  if( pRalHeader->rgmode != 0 ) 
	    {
	      strncat( hist_name, rg_names[ i/nrhist ], 3 );
	    }
	    
	  status = RAL2MUD_hist( pRalHeader, hist_name, ralData, pMUD_histHdr, pMUD_histDat );
	  
	  MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
	  MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
      }
    }
    
    return( SUCCESS );
error:
    MUD_free( pMUD_histHdr );
    MUD_free( pMUD_histDat );
    return( FAILURE );
}


int
RAL2MUD_convert( inFile, outFile, exptNumber, exptMember, exptArea )
    char* inFile;
    char* outFile;
    char* exptNumber;
    char* exptMember;
    char* exptArea;
{
    int status;
    FILE* fin = NULL;
    FILE* fout = NULL;
    RAL_HEADER* pRalHeader;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc = NULL;
    MUD_SEC_GRP* pMUD_scalGrp = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;
    char buf[2048];
    char* p;
    int i;

    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", inFile );
	goto error;
    }

    pMUD_fileGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_FMT_TRI_TD_ID );
    if( pMUD_fileGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_fileGrp\n" );
	goto error;
    }

    /*
     *  Convert the run description
     */
    pMUD_desc = (MUD_SEC_GEN_RUN_DESC*)MUD_new( MUD_SEC_GEN_RUN_DESC_ID, 1 );
    if( pMUD_desc == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_desc\n" );
	goto error;
    }
/*
    if( sizeof( ralHeader ) != 1024 )
    {
        fprintf( stderr, 
                 "RAL Header structure is of incorrect size: %i != 1024, check code\n",
		 sizeof( ralHeader ) );
        goto error;
    }
*/
    pRalHeader = (RAL_HEADER*) zalloc( sizeof( RAL_HEADER ) );

    fread( buf, 6, 1, fin );
    p = &buf[0];

    if( buf[0] == 'R' && ( buf[1] == 'D' || buf[1] == 'C' ) )
    {
        RecordInfo = 0;
	fread( buf+6, 1018, 1, fin );
#ifdef ral_debug
	printf( "File appears to be a clean stream (great, but how?)\n");
#endif
    }
    else if( buf[2] == 'R' && ( buf[3] == 'D' || buf[3] == 'C' ) )
    {
        RecordInfo = 1;
	fread( buf+6, 1020, 1, fin );
        p += 2;
#ifdef ral_debug
	printf( "File has 1 word record info (binary ftp)\n");
#endif
    }
    else if( buf[4] == 'R' && ( buf[5] == 'D' || buf[5] == 'C' ) )
    {
        RecordInfo = 2;
	fread( buf+6, 1022, 1, fin );
        p += 4;
#ifdef ral_debug
	printf( "File has 2 words record info (nfs)\n");
#endif
    }
    else
    {
        fprintf( stderr, "could not interpret record format\n" );
	goto error;
    }

    bdecode_2( p, &pRalHeader->istfla ); p += 2;
    bdecode_2( p, &pRalHeader->rescod ); p += 2;
    p += 2;
    bdecode_2( p, &pRalHeader->nrun ); p += 2;
    p += 4;
    bdecode_4( p, &pRalHeader->res_pscnds ); p += 4;
    p += 4;
    bdecode_4( p, &pRalHeader->nrun32 ); p += 4;
    p += 4;
    bdecode_2( p, &pRalHeader->histlen ); p += 2;
    bdecode_2( p, &pRalHeader->numhis ); p += 2;
    p += 96;
    bdecode_2( p, &pRalHeader->numrec ); p += 2;
    bdecode_2( p, &pRalHeader->lenrec ); p += 2;
    bdecode_2( p, &pRalHeader->rgmode ); p += 2;
    bdecode_2( p, &pRalHeader->nreads ); p += 2;
    bdecode_2( p, &pRalHeader->nframes ); p += 2;
    bdecode_obj( p, &pRalHeader->rtitle, 40 ); p += 40;
    p += 20;
    bdecode_obj( p, &pRalHeader->apparat, 10 ); p += 10;
    p += 10;
    bdecode_obj( p, &pRalHeader->start_date, 9 ); p += 9;
    p += 1;
    bdecode_obj( p, &pRalHeader->stop_date, 9 ); p += 9;   
    p += 1;
    bdecode_obj( p, &pRalHeader->start_time, 8 ); p += 8;
    bdecode_obj( p, &pRalHeader->stop_time, 8 ); p += 8;
    p += 64;
    for( i=0; i<32; ++i ) { bdecode_2( p, &pRalHeader->t0bin[i] ); p += 2; }
    for( i=0; i<32; ++i ) { bdecode_2( p, &pRalHeader->tgood_begin[i] ); p += 2; }
    for( i=0; i<32; ++i ) { bdecode_2( p, &pRalHeader->tgood_end[i] ); p += 2; }
    for( i=0; i<32; ++i ) { bdecode_4( p, &pRalHeader->ntotal[i] ); p += 4; }
    p += 222;
    bdecode_obj( p, &pRalHeader->comment, 62 ); p += 62;
    p += 60;
    p += 40;
    bdecode_2( p, &pRalHeader->ngroups ); p += 2;
    
    status = RAL2MUD_runDesc( pRalHeader, pMUD_desc, exptNumber, exptMember, exptArea );
    if( _failure( status ) ) 
    {
	goto error;
    }

    /*
     *  Make some null scalers
     */
    pMUD_scalGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_SCALER_ID );
    if( pMUD_scalGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_scalGrp\n" );
	goto error;
    }

    /*
     *  Convert the histograms
     */
    pMUD_histGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID );
    if( pMUD_histGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_histGrp\n" );
	goto error;
    }

    status = RAL2MUD_hists( fin, pRalHeader, pMUD_histGrp );
    if( _failure( status ) )
    {
	goto error;
    }

    fclose( fin );

    /*
     *  Assemble the first level sections
     */
    MUD_addToGroup( pMUD_fileGrp, pMUD_desc );
    MUD_addToGroup( pMUD_fileGrp, pMUD_scalGrp );
    MUD_addToGroup( pMUD_fileGrp, pMUD_histGrp );

    /*
     *  Do the write
     */
    fout = MUD_openOutput( outFile );
    if( fout == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", outFile );
	goto error;
    }

    MUD_writeFile( fout, pMUD_fileGrp );

    fclose( fout );

    /*
     *  Free malloc'ed mem
     */
    MUD_free( pMUD_fileGrp );

    return( SUCCESS );
error:
    if( fin != NULL ) fclose( fin );
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_desc );
    MUD_free( pMUD_scalGrp );
    MUD_free( pMUD_histGrp );
    MUD_free( pMUD_fileGrp );
    return( FAILURE );
}


