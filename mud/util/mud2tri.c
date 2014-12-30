/*
 *  msr2tri.c -- Conversion of MSR-TD to TRIUMF-TD 
 *		 for µSR Data Format Utility
 *
 *   Copyright (C) 1994-2003 TRIUMF (Vancouver, Canada)
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
 *   26-Jan-1994  TW  Initial version
 *   15-Feb-1994  TW  Split ...GEN_HIST to ...GEN_HIST_HDR
 *                       and ...GEN_HIST_DAT
 *   17-Feb-1994  TW  Groups with member index
 *   17-Apr-1994  TW  Added temperature and field
 *   14-Jul-1994  TW  Tidying
 *   28-Oct-1998  DA  Fix conversion of time-per-bin to code, including BNC clock codes
 *   12-Feb-1999  DA  Truncate long histograms to 65280 (255*256)
 *   25-Nov-2003  DA  Histogram selection
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"
#include "trid_fmt.h"

static int MUD2TRI_header  _ANSI_ARGS_(( MUD_SEC_GEN_RUN_DESC *pMUD_desc , MUD_SEC_GRP *pMUD_scalGrp , MUD_SEC_GRP *pMUD_histGrp , TMF_F_HDR *pTmf_f_hdr ));
static int MUD2TRI_hists  _ANSI_ARGS_(( FILE *fout , TMF_F_HDR *pTmf_f_hdr , MUD_SEC_GRP *pMUD_histGrp, int nhsel, int hsel[] ));

static UINT32 TRI2MUD_BinSizes[32] =
    {    78125,   156250,   312500,    625000,   1250000,   2500000,    5000000,   10000000,
      20000000, 40000000, 80000000, 160000000, 320000000, 640000000, 1280000000, 2560000000u,
            16,       17,       18,    390625,    781250,   1562500,    3125000,    6250000,
      12500000, 25000000, 50000000, 100000000, 200000000, 400000000,  800000000, 1600000000};

int
TRI_writeHdr( fout, pTmf_f_hdr )
    FILE* fout;
    TMF_F_HDR *pTmf_f_hdr;
{
  char buf[512];
  char* p;
  int i, j;

  bzero( buf, 512 );
  p = buf;

  bencode_2( p, &pTmf_f_hdr->mrun ); p += 2;
  bencode_2( p, &pTmf_f_hdr->mhists ); p += 2;
  bencode_2( p, &pTmf_f_hdr->msclr ); p += 2;
  bencode_2( p, &pTmf_f_hdr->msupd ); p += 2;
  for( i = 0; i < 18; i++ ) { bencode_4( p, &pTmf_f_hdr->jtsc[i] ); p += 4; }
  for( i = 0; i < 18; i++ ) { bencode_4( p, &pTmf_f_hdr->jdsc[i] ); p += 4; }
  bencode_2( p, &pTmf_f_hdr->mmin ); p += 2;
  bencode_2( p, &pTmf_f_hdr->msec ); p += 2;
  for( i = 0; i < 6; i++ ) { bencode_2( p, &pTmf_f_hdr->mtnew[i] ); p += 2; }
  for( i = 0; i < 6; i++ ) { bencode_2( p, &pTmf_f_hdr->mtend[i] ); p += 2; }
  for( i = 0; i < 4; i++ ) { bencode_2( p, &pTmf_f_hdr->mlston[i] ); p += 2; }
  bencode_2( p, &pTmf_f_hdr->mcmcsc ); p += 2;
  for( i = 0; i < 2; i++ ) 
  { 
    for( j = 0; j < 6; j++ ) { bencode_2( p, &pTmf_f_hdr->mlocsc[i][j] ); p += 2; }
  }
  bencode_2( p, &pTmf_f_hdr->mrsta ); p += 2;
  bencode_4( p, &pTmf_f_hdr->acqtsk ); p += 4;
  bencode_obj( p, pTmf_f_hdr->logfil, 10 ); p += 10;
  bencode_2( p, &pTmf_f_hdr->muic ); p += 2;
  bencode_4( p, &pTmf_f_hdr->nevtot ); p += 4;
  bencode_2( p, &pTmf_f_hdr->mhsts ); p += 2;
  bencode_2( p, &pTmf_f_hdr->mbins ); p += 2;
  bencode_2( p, &pTmf_f_hdr->mshft ); p += 2;
  for( i = 0; i < 7; i++ ) { bencode_2( p, &pTmf_f_hdr->mspare[i] ); p += 2; }
  bencode_obj( p, pTmf_f_hdr->title, 40 ); p += 40;
  bencode_obj( p, pTmf_f_hdr->sclbl, 72 ); p += 72;
  bencode_obj( p, pTmf_f_hdr->coment, 144 ); p += 144;

  return( fwrite( buf, 512, 1, fout ) );
}

int
TRI_writeHistHdr( fout, pTmf_h_recd )
    FILE* fout;
    TMF_H_RECD* pTmf_h_recd;
{
  char buf[512];
  char* p;

  bzero( buf, 512 );
  p = buf;

  bencode_2( p, &pTmf_h_recd->u.h.ihist ); p += 2;
  bencode_2( p, &pTmf_h_recd->u.h.length ); p += 2;
  bencode_4( p, &pTmf_h_recd->u.h.nevtot ); p += 4;
  bencode_2( p, &pTmf_h_recd->u.h.ntpbin ); p += 2;
  bencode_4( p, &pTmf_h_recd->u.h.mask ); p += 4;
  bencode_2( p, &pTmf_h_recd->u.h.nt0 ); p += 2;
  bencode_2( p, &pTmf_h_recd->u.h.nt1 ); p += 2;
  bencode_2( p, &pTmf_h_recd->u.h.nt2 ); p += 2;
  bencode_obj( p, pTmf_h_recd->u.h.htitl, 10 ); p += 10;
  bencode_obj( p, pTmf_h_recd->u.h.id, 2 ); p += 2;

  return( fwrite( buf, 64, 1, fout ) );
}


int
MUD2TRI_convert( inFile, outFile, nhsel, hsel )
    char* inFile;
    char* outFile;
    int   nhsel;
    int   hsel[];
{
    int status;
    FILE* fin = NULL;
    FILE* fout = NULL;
    TMF_F_HDR tmf_f_hdr;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc = NULL;
    MUD_SEC_GRP* pMUD_scalGrp = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;
    int n;

    bzero( &tmf_f_hdr, sizeof( TMF_F_HDR ) );

    /*
     *  Read the file
     */
    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", inFile );
	return( FAILURE ); 
    }

    pMUD_fileGrp = MUD_readFile( fin );
    if( pMUD_fileGrp == NULL )
    {
	printf( "error while reading file %s\n", inFile );
	goto error;
    }

#ifdef DEBUG
    printf( "done reading file\n" );
#endif /* DEBUG */

    fclose( fin );

    if( MUD_instanceID( pMUD_fileGrp ) != MUD_FMT_TRI_TD_ID )
    {
	printf( "error: wrong format ID\n" );
	goto error;
    }

    pMUD_desc = MUD_search( pMUD_fileGrp->pMem, 
			    MUD_SEC_GEN_RUN_DESC_ID, (UINT32)1, 
			    (UINT32)0 );
    if( pMUD_desc == NULL )
    {
	printf( "error: bad file format: run description not found\n" );
	goto error;
    }

#ifdef DEBUG
    printf( "found run description\n" );
#endif /* DEBUG */

    pMUD_scalGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_SCALER_ID, 
			       (UINT32)0 );
    if( pMUD_scalGrp == NULL )
    {
	printf( "error: bad file format: scaler group not found\n" );
	goto error;
    }

#ifdef DEBUG
    printf( "found scaler group\n" );
#endif /* DEBUG */

    pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID, 
			       (UINT32)0 );
    if( pMUD_histGrp == NULL )
    {
	printf( "error: bad file format: histogram group not found\n" );
	goto error;
    }

#ifdef DEBUG
    printf( "found hist group\n" );
#endif /* DEBUG */

#ifdef VMS
    fout = fopen( outFile, "w", "rfm=fix", "mrs=512" );
#else
    fout = fopen( outFile, "wb" );
#endif /* VMS */
    if( fout == NULL )
    {
        printf( "failed to fopen output file \"%s\"\n", outFile );
	goto error;
    }

#ifdef DEBUG
    printf( "opened the file\n" );
#endif /* DEBUG */

    /*
     *  Convert the header info
     */
    status = MUD2TRI_header( pMUD_desc, pMUD_scalGrp, pMUD_histGrp, 
			     &tmf_f_hdr );
    if( _failure( status ) )
    {
	goto error;
    }

#ifdef DEBUG
    printf( "converted header\n" );
#endif /* DEBUG */

    n = tmf_f_hdr.mhists;
    if ( validateHistList( nhsel, hsel, &n ) )
    {
        tmf_f_hdr.mhists = (INT16) n ;
    }
    else
    {
	printf( "warning: bad hist list -- ignored\n" );
        nhsel = 0;
    }

    TRI_writeHdr( fout, &tmf_f_hdr );

#ifdef DEBUG
    printf( "wrote header, pos = %d\n", ftell( fout ) );
#endif /* DEBUG */

    /*
     *  Convert the histograms
     */
    status = MUD2TRI_hists( fout, &tmf_f_hdr, pMUD_histGrp, nhsel, hsel );
    if( _failure( status ) )
    {
	goto error;
    }

#ifdef DEBUG
    printf( "converted histograms\n" );
#endif /* DEBUG */

    fclose( fout );
    MUD_free( pMUD_fileGrp );
    return( SUCCESS );
error:
    if( fin != NULL ) fclose( fin );
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_fileGrp );
    return( FAILURE );
}


static int
MUD2TRI_header( pMUD_desc, pMUD_scalGrp,
		pMUD_histGrp, pTmf_f_hdr )
    MUD_SEC_GEN_RUN_DESC* pMUD_desc;
    MUD_SEC_GRP* pMUD_scalGrp;
    MUD_SEC_GRP* pMUD_histGrp;
    TMF_F_HDR* pTmf_f_hdr;
{
    int status;
    int i, j;
    char buf[512];
    INT32 tempTime[6];
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_SCALER* pMUD_scal;

    pTmf_f_hdr->mrun = pMUD_desc->runNumber;
    pTmf_f_hdr->mhists = ( pMUD_histGrp->num )/2;
    pTmf_f_hdr->msclr = pMUD_scalGrp->num;

    pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
			       MUD_SEC_GEN_HIST_HDR_ID, (UINT32)1, 
			       (UINT32)0 );
    if( pMUD_histHdr == NULL ) 
    {
	printf( "error: bad file format: histogram #%d not found\n", 1 );
	return( FAILURE );
    }

#ifdef DEBUG
    printf( "found hist header 1\n" );
#endif /* DEBUG */

/* Wrong - msupd is the save_period of DARC, nothing to do with the data
    pTmf_f_hdr->msupd = ( pMUD_histHdr->t0_ps + 500 )/1000;
*/
    pTmf_f_hdr->msupd = 0;

    /*
     *  Scaler totals & labels
     */
    bzero( buf, 512 );
    for( i = 0; i < pMUD_scalGrp->num; i++ )
    {
        pMUD_scal = MUD_search( pMUD_scalGrp->pMem, 
				MUD_SEC_GEN_SCALER_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_scal == NULL ) 
	{
	    printf( "error: bad file format: scaler #%d not found\n", i+1 );
	    return( FAILURE );
	}

#ifdef DEBUG
        printf( "found scaler %d, counts %ld\n", i, pMUD_scal->counts[0] );
#endif /* DEBUG */

	pTmf_f_hdr->jtsc[i] = _swap32( pMUD_scal->counts[0] );

	strncpy( &buf[i*4], pMUD_scal->label, 4 );
	padString( &buf[i*4], ' ', 4 );
    }
    padString( buf, ' ', 72 );
    bcopy( buf, pTmf_f_hdr->sclbl, 72 );

    /*
     *  elapsed time & dates
     */
    pTmf_f_hdr->mmin = pMUD_desc->elapsedSec/60;
    pTmf_f_hdr->msec = pMUD_desc->elapsedSec%60;

    GMF_LOCALTIME( &pMUD_desc->timeBegin, tempTime );
    for( i = 0; i < 6; i++ ) pTmf_f_hdr->mtnew[i] = tempTime[i];

    for( i = 0; i < 4; i++ ) pTmf_f_hdr->mlston[i] = pTmf_f_hdr->mtnew[i+2];

    GMF_LOCALTIME( &pMUD_desc->timeEnd, tempTime );
    for( i = 0; i < 6; i++ ) pTmf_f_hdr->mtend[i] = tempTime[i];

    /*
     *  CAMAC modules
     */
    pTmf_f_hdr->mcmcsc = 0;
    for( i = 0; i < 6; i++ ) pTmf_f_hdr->mlocsc[0][i] = 0;
    for( i = 0; i < 6; i++ ) pTmf_f_hdr->mlocsc[1][i] = 0;
    pTmf_f_hdr->mrsta = 0;
    pTmf_f_hdr->acqtsk = 0;

    /*
     *  logfil
     */
    buf[0] = '\0';
    sprintf( buf, "%06d.TRI", pMUD_desc->runNumber );
    padString( buf, ' ', 10 );
    bcopy( buf, pTmf_f_hdr->logfil, 10 );

    /*
     *  
     */
    pTmf_f_hdr->muic = 0;

    /*
     *  Sum of all histogram events totals
     */
    pTmf_f_hdr->nevtot = 0;
    for( i = 0; i < pTmf_f_hdr->mhists; i++ )
    {
        pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(i+1), 
				   (UINT32)0 );
	if( pMUD_histHdr == NULL ) 
	{
	    printf( "error: bad file format: histogram #%d not found\n", i+1 );
	    return( FAILURE );
	}

#ifdef DEBUG
    printf( "found hist %d, events %ld\n", i, pMUD_histHdr->nEvents );
#endif /* DEBUG */

	pTmf_f_hdr->nevtot += pMUD_histHdr->nEvents;
    }
    pTmf_f_hdr->nevtot = _swap32( pTmf_f_hdr->nevtot );

    /*
     *  
     */
    pTmf_f_hdr->mhsts = 0;
    pTmf_f_hdr->mbins = 0;
    pTmf_f_hdr->mshft = 0;
    for( i = 0; i < 7; i++ ) pTmf_f_hdr->mspare[i] = 0;

    /*
     *  title
     */
    bzero( buf, 512 );
    strncpy( buf, pMUD_desc->title, 40 );
    padString( buf, ' ', 40 );
    bcopy( buf, pTmf_f_hdr->title, 40 );

    /*
     *  comment
     */
    bzero( buf, 512 );
    strncpy( &buf[0],   pMUD_desc->title, 80 );
    padString( &buf[0], ' ', 80 );
    strncpy( &buf[80],  pMUD_desc->sample, 10 );
    padString( &buf[80], ' ', 10 );
    strncpy( &buf[90],  pMUD_desc->temperature, 10 );
    padString( &buf[90], ' ', 10 );
    strncpy( &buf[100],  pMUD_desc->field, 10 );
    padString( &buf[100], ' ', 10 );
    strncpy( &buf[110], pMUD_desc->orient, 10 );
    padString( &buf[110], ' ', 10 );
    strncpy( &buf[120], pMUD_desc->apparatus, 10 );
    padString( &buf[120], ' ', 10 );
    strncpy( &buf[130], pMUD_desc->insert, 10 );
    padString( &buf[130], ' ', 10 );
    padString( &buf[140], ' ', 4 );
    bcopy( buf, pTmf_f_hdr->coment, 144 );

    return( SUCCESS );
}


static int
MUD2TRI_hists( fout, pTmf_f_hdr, pMUD_histGrp, nhsel, hsel )
    FILE* fout;
    TMF_F_HDR* pTmf_f_hdr;
    MUD_SEC_GRP* pMUD_histGrp;
    int nhsel;
    int hsel[];
{
    int i, j, k, n, ic;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    TMF_H_RECD tmf_h_recd;
    int nRec;
    int nBinsTri;
    UINT16* ps;
    UINT16 s;
    UINT16* pTmf_h_data;

    if( nhsel > 0 )
        k = nhsel;
    else
        k = ( pMUD_histGrp->num )/2;

    for( i = 0; i < k ; i++ )
    {
        if( nhsel > 0 )
          n = hsel[i];
        else
          n = i+1;

	pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histHdr == NULL ) 
	{
	    printf( "error: bad file format: histogram header #%d not found\n", n );
	    return( FAILURE );
	}

	pMUD_histDat = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(n), 
				   (UINT32)0 );
	if( pMUD_histDat == NULL ) 
	{
	    printf( "error: bad file format: histogram data #%d not found\n", n );
	    return( FAILURE );
	}

	nBinsTri = _min( pMUD_histHdr->nBins, 255*256 );
	nRec = (int)( ( 64 + 2*nBinsTri + 511 )/512 );
	bzero( &tmf_h_recd, sizeof( tmf_h_recd ) );

	tmf_h_recd.u.h.ihist = n;
	tmf_h_recd.u.h.length = nBinsTri;
	tmf_h_recd.u.h.nevtot = _swap32( pMUD_histHdr->nEvents );

  /*    tmf_h_recd.u.h.ntpbin = (UINT32)( log( (double)pMUD_histHdr->fsPerBin/78125.0 ) / log( 2.0 ) + 0.5 );  */
  /*    Reverse-engineer time-per-bin code from mud's time per bin (fsPerBin).   */

	if( pMUD_histHdr->fsPerBin < 100 )
	{
	    tmf_h_recd.u.h.ntpbin = (UINT32)( pMUD_histHdr->fsPerBin );
	} 
	else 
	{
	    tmf_h_recd.u.h.ntpbin = (UINT32)( pMUD_histHdr->fsPerBin / 1000 );
	    for( ic = 0; ic < 31; ic++ )
	    {
		if( pMUD_histHdr->fsPerBin == TRI2MUD_BinSizes[ic] )
		{
		    tmf_h_recd.u.h.ntpbin = (UINT32)( ic );
		}
	    }
	}

	tmf_h_recd.u.h.mask = 0;
	tmf_h_recd.u.h.nt0 = pMUD_histHdr->t0_bin;
	tmf_h_recd.u.h.nt1 = pMUD_histHdr->goodBin1;
	tmf_h_recd.u.h.nt2 = _min( pMUD_histHdr->goodBin2, nBinsTri );

	strncpy( tmf_h_recd.u.h.htitl, pMUD_histHdr->title, 10 );
	padString( tmf_h_recd.u.h.htitl, ' ', 10 );

	tmf_h_recd.u.h.id[0] = 0;  /* '1' for spike data */
	tmf_h_recd.u.h.id[1] = 0;  /* 'B' for spike data */

	pTmf_h_data = (UINT16*)zalloc( 2*pMUD_histHdr->nBins );

	MUD_SEC_GEN_HIST_unpack( pMUD_histHdr->nBins, 
	    pMUD_histHdr->bytesPerBin, pMUD_histDat->pData, 
	    2, pTmf_h_data );

        /*
         *  Encode the histogram
         *  This is needed only to handle byte ordering
         */
        ps = pTmf_h_data;
        for( j = 0; j < pMUD_histHdr->nBins; j++ )
        {
          s = *ps;
          bencode_2( ps, &s );
          ps++;
        }

        TRI_writeHistHdr( fout, &tmf_h_recd );

#ifdef DEBUG
        printf( "wrote hist hdr %d (pos %d)\n", i, ftell( fout ) );
#endif /* DEBUG */

	fwrite( pTmf_h_data, 2*nBinsTri, 1, fout );

#ifdef DEBUG
        printf( "wrote hist data %d (pos %d)\n", n, ftell( fout ) );
#endif /* DEBUG */

	/*
	 *  Pad histogram with zeros to align on 512 byte block
	 */
 	bzero( &tmf_h_recd, sizeof( tmf_h_recd ) );
	j = nRec*512 - 2*nBinsTri - 64;
	fwrite( &tmf_h_recd, j, 1, fout );

#ifdef DEBUG
        printf( "padded hist %d (pos %d)\n", i, ftell( fout ) );
#endif /* DEBUG */

	free( pTmf_h_data );
    }

    return( SUCCESS );
}


