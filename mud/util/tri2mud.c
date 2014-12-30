/*
 *  tri2mud.c -- Conversion of TRIUMF-TD to MSR MUD-TD 
 *		 for µSR Data Format Utility
 *
 *   Copyright (C) 1994-2010 TRIUMF (Vancouver, Canada)
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
 *   14-Jul-1994  TW  Tidying
 *   18-Jul-1994  TW  Added temperature and field
 *   20-Sep-1994  TW  Run title fixed for older files
 *   26-Sep-1994  TW  fsPerBin fixed for older files
 *   28-Oct-1998  DA  fsPerBin (re-)fixed, and include BNC TDC codes
 *   31-Aug-2001  DA  Use specified experimental area
 *   24-Aug-2010  DA  banner and license
 */ 

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"
#include "trid_fmt.h"

static int TRI2MUD_runDesc  _ANSI_ARGS_(( TMF_F_HDR *pTmf_f_hdr, MUD_SEC_GEN_RUN_DESC *pMUD_desc, char* exptnumber, char* exptmember, char* exptarea ));
static int TRI2MUD_scalers  _ANSI_ARGS_(( TMF_F_HDR *pTmf_f_hdr , MUD_SEC_GRP *pMUD_scalGrp ));
static int TRI2MUD_hists  _ANSI_ARGS_(( FILE *fin , TMF_F_HDR *pTmf_f_hdr , MUD_SEC_GRP *pMUD_histGrp ));
static int TRI2MUD_hist  _ANSI_ARGS_(( TMF_F_HDR* pTmf_f_hdr, TMF_H_RECD* pTmf_h_recd, UINT16* pTmf_h_data, MUD_SEC_GEN_HIST_HDR* pMUD_histHdr, MUD_SEC_GEN_HIST_DAT* pMUD_histDat ));

static UINT32 TRI2MUD_BinSizes[32] =
    {    78125,   156250,   312500,    625000,   1250000,   2500000,    5000000,   10000000,
      20000000, 40000000, 80000000, 160000000, 320000000, 640000000, 1280000000, 2560000000u,
            16,       17,       18,    390625,    781250,   1562500,    3125000,    6250000,
      12500000, 25000000, 50000000, 100000000, 200000000, 400000000,  800000000, 1600000000};

int
TRI_readHdr( fin, pTmf_f_hdr )
    FILE* fin;
    TMF_F_HDR *pTmf_f_hdr;
{
  char buf[512];
  char* p;
  int i, j, count;

  count = fread( buf, 512, 1, fin );
  if( count == 0 ) return( 0 );
  p = buf;

  bdecode_2( p, &pTmf_f_hdr->mrun ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->mhists ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->msclr ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->msupd ); p += 2;
  for( i = 0; i < 18; i++ ) { bdecode_4( p, &pTmf_f_hdr->jtsc[i] ); p += 4; }
  for( i = 0; i < 18; i++ ) { bdecode_4( p, &pTmf_f_hdr->jdsc[i] ); p += 4; }
  bdecode_2( p, &pTmf_f_hdr->mmin ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->msec ); p += 2;
  for( i = 0; i < 6; i++ ) { bdecode_2( p, &pTmf_f_hdr->mtnew[i] ); p += 2; }
  for( i = 0; i < 6; i++ ) { bdecode_2( p, &pTmf_f_hdr->mtend[i] ); p += 2; }
  for( i = 0; i < 4; i++ ) { bdecode_2( p, &pTmf_f_hdr->mlston[i] ); p += 2; }
  bdecode_2( p, &pTmf_f_hdr->mcmcsc ); p += 2;
  for( i = 0; i < 2; i++ ) 
  { 
    for( j = 0; j < 6; j++ ) { bdecode_2( p, &pTmf_f_hdr->mlocsc[i][j] ); p += 2; }
  }
  bdecode_2( p, &pTmf_f_hdr->mrsta ); p += 2;
  bdecode_4( p, &pTmf_f_hdr->acqtsk ); p += 4;
  bdecode_obj( p, pTmf_f_hdr->logfil, 10 ); p += 10;
  bdecode_2( p, &pTmf_f_hdr->muic ); p += 2;
  bdecode_4( p, &pTmf_f_hdr->nevtot ); p += 4;
  bdecode_2( p, &pTmf_f_hdr->mhsts ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->mbins ); p += 2;
  bdecode_2( p, &pTmf_f_hdr->mshft ); p += 2;
  for( i = 0; i < 7; i++ ) { bdecode_2( p, &pTmf_f_hdr->mspare[i] ); p += 2; }
  bdecode_obj( p, pTmf_f_hdr->title, 40 ); p += 40;
  bdecode_obj( p, pTmf_f_hdr->sclbl, 72 ); p += 72;
  bdecode_obj( p, pTmf_f_hdr->coment, 144 ); p += 144;

  return( count );
}

int
TRI_readHistHdr( fin, pTmf_h_recd )
    FILE* fin;
    TMF_H_RECD* pTmf_h_recd;
{
  char buf[512];
  char* p;
  int count;

  count = fread( buf, 64, 1, fin );
  if( count == 0 ) return( 0 );
  p = buf;

  bdecode_2( p, &pTmf_h_recd->u.h.ihist ); p += 2;
  bdecode_2( p, &pTmf_h_recd->u.h.length ); p += 2;
  bdecode_4( p, &pTmf_h_recd->u.h.nevtot ); p += 4;
  bdecode_2( p, &pTmf_h_recd->u.h.ntpbin ); p += 2;
  bdecode_4( p, &pTmf_h_recd->u.h.mask ); p += 4;
  bdecode_2( p, &pTmf_h_recd->u.h.nt0 ); p += 2;
  bdecode_2( p, &pTmf_h_recd->u.h.nt1 ); p += 2;
  bdecode_2( p, &pTmf_h_recd->u.h.nt2 ); p += 2;
  bdecode_obj( p, pTmf_h_recd->u.h.htitl, 10 ); p += 10;
  bdecode_obj( p, pTmf_h_recd->u.h.id, 2 ); p += 2;

  return( count );
}

int
TRI2MUD_convert( inFile, outFile, 
                 exptnumber, exptmember, exptarea )
    char* inFile;
    char* outFile;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    int status;
    FILE* fin = NULL;
    FILE* fout = NULL;
    TMF_F_HDR tmf_f_hdr;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc = NULL;
    MUD_SEC_GRP* pMUD_scalGrp = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;

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
    pMUD_desc = (MUD_SEC_GEN_RUN_DESC*)MUD_new( MUD_SEC_GEN_RUN_DESC_ID, (UINT32)1 );
    if( pMUD_desc == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_desc\n" );
	goto error;
    }

    TRI_readHdr( fin, &tmf_f_hdr );

    status = TRI2MUD_runDesc( &tmf_f_hdr, pMUD_desc, exptnumber, exptmember, exptarea );
    if( _failure( status ) ) 
    {
	goto error;
    }

    /*
     *  Convert the scalers
     */
    pMUD_scalGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_SCALER_ID );
    if( pMUD_scalGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_scalGrp\n" );
	goto error;
    }

    status = TRI2MUD_scalers( &tmf_f_hdr, pMUD_scalGrp );
    if( _failure( status ) ) 
    {
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

    status = TRI2MUD_hists( fin, &tmf_f_hdr, pMUD_histGrp );
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


static int
TRI2MUD_runDesc( pTmf_f_hdr, pMUD_desc,
                 exptnumber, exptmember, exptarea )
    TMF_F_HDR* pTmf_f_hdr;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    int status;
    int i;
    INT32 tempTime[6];
    char tempString[256];
    int expt;
    char* p;

    pMUD_desc->runNumber = pTmf_f_hdr->mrun;

    expt = strtol( exptnumber, &p, 0 );
    if( *p == 0 ) pMUD_desc->exptNumber = expt;

    pMUD_desc->experimenter = strdup( exptmember );

    for( i = 0; i < 6; i++ ) tempTime[i] = pTmf_f_hdr->mtnew[i];
    GMF_MKTIME( &pMUD_desc->timeBegin, tempTime );

    for( i = 0; i < 6; i++ ) tempTime[i] = pTmf_f_hdr->mtend[i];
    GMF_MKTIME( &pMUD_desc->timeEnd, tempTime );

    pMUD_desc->elapsedSec = (UINT32)pTmf_f_hdr->msec + 
                            (60L)*((UINT32)pTmf_f_hdr->mmin);

    pMUD_desc->method = strdup( "TD-µSR" );
    pMUD_desc->lab = strdup( "TRIUMF" );
    pMUD_desc->das = strdup( "MODAS" );

    if( *exptarea ) pMUD_desc->area = strdup( exptarea );
    else if( pMUD_desc->runNumber < 5000 ) pMUD_desc->area = strdup( "M20" );
    else if( pMUD_desc->runNumber < 10000 ) pMUD_desc->area = strdup( "M15" );
    else if( pMUD_desc->runNumber < 15000 ) pMUD_desc->area = strdup( "M13" );
    else if( pMUD_desc->runNumber < 20000 ) pMUD_desc->area = strdup( "M9" );
    else if( pMUD_desc->runNumber < 25000 ) pMUD_desc->area = strdup( "DASDEV" );
    else if( pMUD_desc->runNumber >= 30001 && pMUD_desc->runNumber < 35000 )
        pMUD_desc->area = strdup( "TEST" );

    /*
     *  Correction for older TD DAT files:
     *  take the run title from the 40 character run title
     *  ignore the first 80 characters of comment (of
     *  which the 40 character run title is a substring).
     */
    strncpy( tempString, &pTmf_f_hdr->title[0], 40 );
/*    strncpy( &tempString[40], &pTmf_f_hdr->coment[40], 40 );
    tempString[80] = '\0';
*/
    tempString[40] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->title = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[80], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->sample = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[90], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->temperature = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[100], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->field = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[110], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->orient = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[120], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->apparatus = strdup( tempString );

    strncpy( tempString, &pTmf_f_hdr->coment[130], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->insert = strdup( tempString );

    return( SUCCESS );
}


static int
TRI2MUD_scalers( pTmf_f_hdr, pMUD_scalGrp )
    TMF_F_HDR* pTmf_f_hdr;
    MUD_SEC_GRP* pMUD_scalGrp;
{
    int status;
    char tempString[256];
    int i;
    MUD_SEC_GEN_SCALER* pMUD_scal;

    for( i = 0; i < pTmf_f_hdr->msclr; i++ )
    {
	pMUD_scal = (MUD_SEC_GEN_SCALER*)MUD_new( MUD_SEC_GEN_SCALER_ID, i+1 );
	if( pMUD_scal == NULL ) 
	{
	    fprintf( stderr, "failed to malloc pMUD_scal\n" );
	    return( FAILURE );
	}

	pMUD_scal->counts[0] = _swap32( pTmf_f_hdr->jtsc[i] );
	strncpy( tempString, &pTmf_f_hdr->sclbl[4*i], 4 );
	tempString[4] = '\0';
        trimBlanks( tempString, tempString );
	pMUD_scal->label = strdup( tempString );

	MUD_addToGroup( pMUD_scalGrp, pMUD_scal );
    }

    return( SUCCESS );
}


static int
TRI2MUD_hists( fin, pTmf_f_hdr, pMUD_histGrp )
    FILE* fin;
    TMF_F_HDR* pTmf_f_hdr;
    MUD_SEC_GRP* pMUD_histGrp;
{
    int status;
    int i, j;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    TMF_H_RECD tmf_h_recd;
    UINT16* pTmf_h_data;
    UINT16* ps;
    UINT16 s;
    int num;
    int size;
    char buf[512];

    for( i = 0; i < pTmf_f_hdr->mhists; i++ )
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

	/*
	 *  Read the first block of the histogram
	 *  (includes the 64 byte histogram header)
	 */
	num = TRI_readHistHdr( fin, &tmf_h_recd );
	if( num == 0 ) 
	{
	    fprintf( stderr, "unexpected end-of-file in input file\n" );
	    goto error;
	}

	size = 2*tmf_h_recd.u.h.length;

	/* 
	 *  Allocate this 512 bytes too much because
	 *  histograms are aligned on 512 block boundaries
	 *  (i.e. there is up to 511 bytes more than
	 *  the size of the histogram and header)
	 */
	pTmf_h_data = (UINT16*)zalloc( size );
	if( pTmf_h_data == NULL )
	{
	    goto error;
	}

	pMUD_histDat->pData = (char*)zalloc( size+16 );
	if( pMUD_histDat->pData == NULL )
	{
	    goto error;
	}

	/*
	 *  Read the histogram data
	 */
	num = fread( pTmf_h_data, size, 1, fin );
	if( num != 1 ) 
	{
	    fprintf( stderr, "unexpected end-of-file in input file\n" );
	    goto error;
	}

	/*
	 *  Position past to beginning of next hist
	 */
	fread( buf, 448-size%512, 1, fin );

        /*
         *  Decode the histograms
         *  This is needed only to handle byte ordering
         */
        ps = pTmf_h_data;
        for( j = 0; j < tmf_h_recd.u.h.length; j++ )
        {
          bdecode_2( ps, &s );
          *ps++ = s;
        }

	status = TRI2MUD_hist( pTmf_f_hdr, &tmf_h_recd, pTmf_h_data, 
			       pMUD_histHdr, pMUD_histDat );
	if( _failure( status ) ) 
	{
	    goto error;
	}
	free( pTmf_h_data );

	MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
	MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
    }

    return( SUCCESS );
error:
    _free( pTmf_h_data );
    MUD_free( pMUD_histHdr );
    MUD_free( pMUD_histDat );
    return( FAILURE );
}


static int
TRI2MUD_hist( pTmf_f_hdr, 
	      pTmf_h_recd, 
	      pTmf_h_data,   
	      pMUD_histHdr, 
	      pMUD_histDat )
    TMF_F_HDR* pTmf_f_hdr;
    TMF_H_RECD* pTmf_h_recd;
    UINT16* pTmf_h_data;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
{
    int status;
    INT32 nt0, nt00, nt01, nmid, ncl;
    char tempString[32];

    strncpy( tempString, pTmf_h_recd->u.h.htitl, 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_histHdr->title = strdup( tempString );    

    pMUD_histHdr->histType = MUD_SEC_TRI_TD_HIST_ID;
    pMUD_histHdr->nEvents = _swap32( pTmf_h_recd->u.h.nevtot );
    pMUD_histHdr->nBins = pTmf_h_recd->u.h.length;
    if( ( pTmf_h_recd->u.h.ntpbin >= 0 ) &&
	( pTmf_h_recd->u.h.ntpbin <= 31 ) )
    {
	/*
	 *  TDC code (0-31); convert to time or leave as code
	 */
	pMUD_histHdr->fsPerBin = TRI2MUD_BinSizes[pTmf_h_recd->u.h.ntpbin];
        /* 78125*pow( (double)2.0, (double)pTmf_h_recd->u.h.ntpbin ); */
    }
    else
    {
	/* 
	 *  Not TDC code: just straight picoseconds per bin
	 */
	pMUD_histHdr->fsPerBin = 1000*pTmf_h_recd->u.h.ntpbin;
    }
    pMUD_histHdr->bytesPerBin = 0;

    nt0 = pTmf_h_recd->u.h.nt0;
    if( nt0 == 0 )
    {
        UINT16* h;

        h = (UINT16*)pTmf_h_data;

	nt0 = pTmf_h_recd->u.h.length/2;
	ncl = 0;
	while( h[nt0-1] > ncl )
	{
	    nt0 /= 2;
	    ncl = h[nt0-1];
	}
	nt00 = nt0;
	nt01 = 2*nt0;
	nmid = ncl/3;
	for( nt0 = nt00; nt0 < nt01; nt0++ )
	    if( h[nt0-1] > nmid ) break;
    }

    pMUD_histHdr->t0_bin = nt0;
    pMUD_histHdr->goodBin1 = pTmf_h_recd->u.h.nt1;
    pMUD_histHdr->goodBin2 = pTmf_h_recd->u.h.nt2;
    pMUD_histHdr->t0_ps = 0.001*(pMUD_histHdr->fsPerBin)*( (double)nt0 - 0.5 );
    pMUD_histHdr->bkgd1 = 0;
    pMUD_histHdr->bkgd2 = 0;

    if( pMUD_histHdr->bkgd1 == 0 && pMUD_histHdr->bkgd2 == 0 && nt0 > 1 )
    {
        INT32 kmid, kb1, kb2;
        INT32 nb;
        double avg, diff, err, val;
        UINT16* h;

        h = pTmf_h_data;
        kmid = nt0/2;
        nb = 0;
        avg = ( h[kmid-2] + h[kmid-1] + h[kmid] )/3;
        for( kb1 = kmid-1; kb1 >= 0; kb1-- )
        {
            val = fabs( (double)h[kb1] );
            diff = fabs( val - avg );
            err = _max( 1.0, sqrt( avg ) );
            if( diff > 5*err ) break;
            avg = nb*avg + val;
            nb++;
            avg = avg/nb;
        }
        kb1 = _min( kb1 + 5, kmid-1 );

        for( kb2 = kmid-1; kb2 < nt0; kb2++ )
        {
            val = fabs( (double)h[kb2] );
            diff = fabs( val - avg );
            err = _max( 1.0, sqrt( avg ) );
            if( diff > 5*err ) break;
            avg = nb*avg + val;
            nb++;
            avg = avg/nb;
        }
        kb2 = _max( kmid-1, kb2 - 5 );

        pMUD_histHdr->bkgd1 = kb1 + 1;
        pMUD_histHdr->bkgd2 = kb2 + 1;
    }

#ifdef DEBUG
	printf( " Packing hist %ld, nBins=%ld\n", 
			    MUD_instanceID( pMUD_histHdr ), 
			    pMUD_histHdr->nBins );
#endif /* DEBUG */

    pMUD_histHdr->nBytes = MUD_SEC_GEN_HIST_pack( pMUD_histHdr->nBins, 
		    2, pTmf_h_data, 
		    pMUD_histHdr->bytesPerBin, pMUD_histDat->pData );
    pMUD_histDat->nBytes = pMUD_histHdr->nBytes;

    return( SUCCESS );
}


