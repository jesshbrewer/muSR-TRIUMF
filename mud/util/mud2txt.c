/*
 *  mud2txt
 *
 *  Writes out histograms of a TD-MuSR experiment data file
 *  (in MUD format) to ASCII format.  Each histogram begins
 *  with a line:
 *     "Histogram <i> - <n> bins"
 *  The data for each histogram follows with each bin on a 
 *  separate line.
 *
 *   Copyright (C) 2001-2003 TRIUMF (Vancouver, Canada)
 *
 *   Authors: D. Arseneau
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
 *  Written for Clint Smallman of E685
 *   13-Nov-2001  DA  fix memory leak and make mud2col like mud2txt
 *   25-Nov-2003  DA  Histogram selection
 */


#include <stdio.h>
#include <string.h>

#include "mud_util.h"


int
MUD2TXT_convert( fin_name, fout_name, nhsel, hsel )
    char* fin_name;
    char* fout_name;
    int   nhsel;
    int   hsel[];
{
    FILE* fin = NULL;
    FILE* fout = NULL;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    int num_hists;
    int i, j, n;
    UINT32* pData = NULL;
    int nbhist = 0;
    char* p;
    
    /*
     *  Read the file
     */
    fin = MUD_openInput( fin_name );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", fin_name );
	goto error;
    }

    pMUD_fileGrp = MUD_readFile( fin );
    if( pMUD_fileGrp == NULL )
    {
	fprintf( stderr, "error while reading file %s\n", fin_name );
	goto error;
    }

    fclose( fin );

    if( MUD_instanceID( pMUD_fileGrp ) == MUD_FMT_TRI_TD_ID )
    {

	pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID,
			       (UINT32)0 );
    }
    else if( MUD_instanceID( pMUD_fileGrp ) == MUD_FMT_TRI_TI_ID )
    {

	pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID,
			       (UINT32)0 );
    }
    else
    {
	fprintf( stderr, "error: wrong format ID\n" );
	goto error;
    }

    if( pMUD_histGrp == NULL )
    {
	fprintf( stderr, "error: bad file format: histogram group not found\n" );
	goto error;
    }

    num_hists = pMUD_histGrp->num/2;
    if ( ! validateHistList( nhsel, hsel, &num_hists ) )
    {
	printf( "warning: bad hist list -- ignored\n" );
	nhsel = 0;
    }

    fout = fopen( fout_name, "w" );
    if( fout == NULL )
    {
        fprintf( stderr, "failed to fopen output file \"%s\"\n", fout_name );
	goto error;
    }

    for( i = 0; i < num_hists; i++ )
    {
        if( nhsel > 0 )
          n = hsel[i];
        else
          n = i+1;

	/*
	 *  Find the histogram header
	 */
	pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histHdr == NULL ) 
	{
	    printf( "error: bad file format: histogram header #%d not found", n );
	    goto error;
	}

#ifdef DEBUG
        printf( "found hist hdr %d (%p)\n", n, pMUD_histHdr );
#endif /* DEBUG */

	/*
	 *  Find the histogram data
	 */
	pMUD_histDat = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histDat == NULL ) 
	{
	    printf( "error: bad file format: histogram data #%d not found", n );
	    goto error;
	}

#ifdef DEBUG
        printf( "found hist data %d (%p)\n", n, pMUD_histDat );
#endif /* DEBUG */

	/*
	 *  Write a line before the histogram
	 */
	fprintf( fout, "Histogram %d - %ld bins\n", n, pMUD_histHdr->nBins );

	/*
	 *  Allocate memory for the unpacked histogram
	 */
        if( pMUD_histHdr->nBins > nbhist )
        {
            pData = (UINT32*)realloc( pData, 4*pMUD_histHdr->nBins );
        }
	/*
	 *  Unpack the histogram
	 */
	MUD_SEC_GEN_HIST_unpack( pMUD_histHdr->nBins, 
	    pMUD_histHdr->bytesPerBin, pMUD_histDat->pData, 
	    4, pData );

	for( j = 0; j < pMUD_histHdr->nBins; j++ )
	{
	    fprintf( fout, "%u\n", pData[j] );
	}
    }

    fclose( fout );
    MUD_free( pMUD_fileGrp );
    if( pData != NULL ) free( pData );
    return( SUCCESS );
error:
    if( fin != NULL ) fclose( fin );
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_fileGrp );
    if( pData != NULL ) free( pData );
    return( FAILURE );
}


int
MUD2COL_convert( fin_name, fout_name, nhsel, hsel )
    char* fin_name;
    char* fout_name;
    int nhsel;
    int hsel[];
{
    FILE* fin = NULL;
    FILE* fout = NULL;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    int num_hists;
    int i, j, n;
    UINT32* pData[32] = { NULL };
    UINT32 nbMin = 2000000;
    char* p;
    
    /*
     *  Read the file
     */
    fin = MUD_openInput( fin_name );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", fin_name );
	goto error;
    }

    pMUD_fileGrp = MUD_readFile( fin );
    if( pMUD_fileGrp == NULL )
    {
	fprintf( stderr, "error while reading file %s\n", fin_name );
	goto error;
    }

    fclose( fin );

    if( MUD_instanceID( pMUD_fileGrp ) == MUD_FMT_TRI_TD_ID )
    {

	pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID,
			       (UINT32)0 );
    }
    else if( MUD_instanceID( pMUD_fileGrp ) == MUD_FMT_TRI_TI_ID )
    {

	pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID,
			       (UINT32)0 );
    }
    else
    {
	fprintf( stderr, "error: wrong format ID\n" );
	goto error;
    }

    if( pMUD_histGrp == NULL )
    {
	fprintf( stderr, "error: bad file format: histogram group not found\n" );
	goto error;
    }

    num_hists = pMUD_histGrp->num/2;
    if ( ! validateHistList( nhsel, hsel, &num_hists ) )
    {
	printf( "warning: bad hist list -- ignored\n" );
        nhsel = 0;
    }

    fout = fopen( fout_name, "w" );
    if( fout == NULL )
    {
        fprintf( stderr, "failed to fopen output file \"%s\"\n", fout_name );
	goto error;
    }

    for( i = 0; i < num_hists; i++ )
    {
        if( nhsel > 0 )
          n = hsel[i];
        else
          n = i+1;

	/*
	 *  Find the histogram header
	 */
	pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histHdr == NULL ) 
	{
	    printf( "error: bad file format: histogram header #%d not found", n );
	    goto error;
	}

#ifdef DEBUG
        printf( "found hist hdr %d (%p)\n", n, pMUD_histHdr );
#endif /* DEBUG */

	/*
	 *  Find the histogram data
	 */
	pMUD_histDat = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histDat == NULL ) 
	{
	    printf( "error: bad file format: histogram data #%d not found", n );
	    goto error;
	}

#ifdef DEBUG
        printf( "found hist data %d (%p)\n", n, pMUD_histDat );
#endif /* DEBUG */

        /*
         *  Allocate memory for the unpacked histogram
         */
        pData[i] = (UINT32*)malloc( 4*pMUD_histHdr->nBins );
        if( nbMin > pMUD_histHdr->nBins ) nbMin = pMUD_histHdr->nBins;
        /*
         *  Unpack the histogram
         */
        MUD_SEC_GEN_HIST_unpack( pMUD_histHdr->nBins, 
                                 pMUD_histHdr->bytesPerBin, pMUD_histDat->pData, 
                                 4, pData[i] );

    }

    for( j = 0; j < nbMin; j++ )
    {
        for( i = 0; i < num_hists; i++ )
        {
            fprintf( fout, "%u, ", pData[i][j] );
        }
        fprintf( fout, "\n" );
    }

    fclose( fout );
    MUD_free( pMUD_fileGrp );
    for( i = 0; i < num_hists; i++ )
    {
        if( pData[i] != NULL ) free( pData[i] );
    }
    return( SUCCESS );
error:
    if( fin != NULL ) fclose( fin );
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_fileGrp );
    for( i = 0; i < num_hists; i++ )
    {
        if( pData[i] != NULL ) free( pData[i] );
    }
    return( FAILURE );
}

