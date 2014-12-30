/*
 *  msr2dat.c -- Conversion of MSR-TI to TRIUMF-TI
 *		 for µSR Data Format Utility
 *
 *   Copyright (C) 1994-1996 TRIUMF (Vancouver, Canada)
 *
 *   Authors: T. Whidden
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
 *                    and ...GEN_HIST_DAT
 *   17-Feb-1994  TW  Groups with member index
 *   22-Feb-1996  TW  Convert independent variables, experimenter and
 *                    exptNumber.  Convert to nvers=9.
 *   27-Feb-1996  TW  nvers=10, exptNumber is integer.
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"
#include "trii_fmt.h"

static char *getMonthString  _ANSI_ARGS_(( int i ));
static int DAT_writeHdr _ANSI_ARGS_(( FILE* fin, IMUSR_HDR* pImusr_hdr ));
static int DAT_writeCamp _ANSI_ARGS_(( FILE* fin, IMUSR_CAMP_BLOCK* pImusr_blk, int num ));
static int MUD2DAT_runDesc  _ANSI_ARGS_(( MUD_SEC_TRI_TI_RUN_DESC *pMUD_desc , MUD_SEC_GRP *pMUD_histGrp , IMUSR_HDR *pImusr_hdr ));
static int MUD2DAT_hists  _ANSI_ARGS_(( FILE *fout , IMUSR_HDR *pImusr_hdr , MUD_SEC_GRP *pMUD_histGrp, MUD_SEC_GRP *pMUD_indVarGrp ));


static int
DAT_writeHdr( fin, pImusr_hdr )
    FILE* fin;
    IMUSR_HDR* pImusr_hdr;
{
    char buf[512];
    char* p;

    bzero( buf, 512 );
    p = buf;
    bencode_2( p, &pImusr_hdr->headtype ); p += 2;
    bencode_2( p, &pImusr_hdr->runno ); p += 2;
    bencode_2( p, &pImusr_hdr->begindata ); p += 2;
    bencode_2( p, &pImusr_hdr->npoints_1 ); p += 2;
    bencode_2( p, &pImusr_hdr->ipdat1 ); p += 2;
    bencode_2( p, &pImusr_hdr->ipdat2 ); p += 2;
    bencode_2( p, &pImusr_hdr->iptitl ); p += 2;
    bencode_2( p, &pImusr_hdr->ipdesc ); p += 2;
    bencode_2( p, &pImusr_hdr->nvers ); p += 2;
    bencode_2( p, &pImusr_hdr->ipsubtit ); p += 2;
    bencode_2( p, &pImusr_hdr->ipcomment1 ); p += 2;
    bencode_2( p, &pImusr_hdr->ipcomment2 ); p += 2;
    bencode_2( p, &pImusr_hdr->ipcomment3 ); p += 2;
    bencode_2( p, &pImusr_hdr->nescalers ); p += 2;
    bencode_2( p, &pImusr_hdr->nascalers ); p += 2;
    bencode_2( p, &pImusr_hdr->currentblk ); p += 2;
    bencode_4( p, &pImusr_hdr->last_dac ); p += 4;
    bencode_2( p, &pImusr_hdr->ncamp ); p += 2;
    bencode_2( p, &pImusr_hdr->exptNumber ); p += 2;
    bencode_2( p, &pImusr_hdr->len_experimenter ); p += 2;
    bencode_obj( p, pImusr_hdr->experimenter, 18 ); p += 18;
    bencode_2( p, &pImusr_hdr->lendatestrt ); p += 2;
    bencode_obj( p, pImusr_hdr->dattimstart, 18 ); p += 18;
    bencode_2( p, &pImusr_hdr->lendatend ); p += 2;
    bencode_obj( p, pImusr_hdr->dattimeend, 18 ); p += 18;
    bencode_2( p, &pImusr_hdr->lentitle ); p += 2;
    bencode_obj( p, pImusr_hdr->title, 80 ); p += 80;
    bencode_2( p, &pImusr_hdr->lendatadesc ); p += 2;
    bencode_obj( p, pImusr_hdr->datadescfor, 2 ); p += 2;
    bencode_2( p, &pImusr_hdr->datadesclen ); p += 2;
    bencode_2( p, &pImusr_hdr->lensubtitle ); p += 2;
    bencode_obj( p, pImusr_hdr->subtitle, 80 ); p += 80;
    bencode_2( p, &pImusr_hdr->lencomment1 ); p += 2;
    bencode_obj( p, pImusr_hdr->comment1, 78 ); p += 78;
    bencode_2( p, &pImusr_hdr->lencomment2 ); p += 2;
    bencode_obj( p, pImusr_hdr->comment2, 78 ); p += 78;
    bencode_2( p, &pImusr_hdr->lencomment3 ); p += 2;
    bencode_obj( p, pImusr_hdr->comment3, 78 ); p += 78;
    bencode_2( p, &pImusr_hdr->lasthdrword ); p += 2;

    return( fwrite( buf, 512, 1, fin ) );
}

static int
DAT_writeCamp( fin, pImusr_blk, num )
    FILE* fin;
    IMUSR_CAMP_BLOCK* pImusr_blk;
    int num;
{
    char buf[512];
    char* p;
    int i;

    bzero( buf, 512 );
    p = buf;
    for( i = 0; i < num; i++ )
    {
      bencode_2( p, &pImusr_blk->var[i].type ); p += 2;
      bencode_2( p, &pImusr_blk->var[i].len_path ); p += 2;
      bencode_obj( p, pImusr_blk->var[i].path, 50 ); p += 50;
      bencode_2( p, &pImusr_blk->var[i].len_title ); p += 2;
      bencode_obj( p, pImusr_blk->var[i].title, 19 ); p += 19;
      bencode_2( p, &pImusr_blk->var[i].len_units ); p += 2;
      bencode_obj( p, pImusr_blk->var[i].units, 8 ); p += 8;
    }
    return( fwrite( buf, 512, 1, fin ) );
}


int
MUD2DAT_convert( inFile, outFile )
    char* inFile;
    char* outFile;
{
    int status;
    FILE* fin;
    FILE* fout;
    IMUSR_HDR* pImusr_hdr;
    MUD_SEC_GRP* pMUD_fileGrp;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_desc;
    MUD_SEC_GRP* pMUD_histGrp;
    MUD_SEC_GRP* pMUD_indVarGrp;

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
	fclose( fin );
	return( FAILURE );
    }

    fclose( fin );

    if( MUD_instanceID( pMUD_fileGrp ) != MUD_FMT_TRI_TI_ID )
    {
	printf( "error - wrong format ID\n" );
	MUD_free( pMUD_fileGrp );
	return( FAILURE );
    }

    pMUD_desc = MUD_search( pMUD_fileGrp->pMem, 
			    MUD_SEC_TRI_TI_RUN_DESC_ID, (UINT32)1, 
			    (UINT32)0 );
    if( pMUD_desc == NULL )
    {
	printf( "error - bad file format, Run Description not found\n" );
	MUD_free( pMUD_fileGrp );
	return( FAILURE );
    }

    pMUD_histGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID, 
			       (UINT32)0 );
    if( pMUD_histGrp == NULL )
    {
	printf( "error - bad file format, Histogram Grp not found\n" );
	MUD_free( pMUD_fileGrp );
	return( FAILURE );
    }

#ifdef VMS
    fout = fopen( outFile, "w", "rfm=fix", "mrs=512" );
#else
    fout = fopen( outFile, "wb" );
#endif /* VMS */
    if( fout == NULL )
    {
        printf( "failed to fopen output file <%s>\n", outFile );
        return( FAILURE );
    }

    /*
     *  Convert the header info
     */
    pImusr_hdr = (IMUSR_HDR*)zalloc( sizeof( IMUSR_HDR ) );

    pMUD_indVarGrp = MUD_search( pMUD_fileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ARR_ID, 
			       (UINT32)0 );
    /*
     *  Calculate the number of independent variables
     *  Two sections per independent variable!
     */
    if( pMUD_indVarGrp == NULL ) pImusr_hdr->ncamp = 0;
    else pImusr_hdr->ncamp = pMUD_indVarGrp->num/2;

    status = MUD2DAT_runDesc( pMUD_desc, pMUD_histGrp, pImusr_hdr );
    if( _failure( status ) )
    {
        _free( pImusr_hdr );
        fclose( fout );
        MUD_free( pMUD_fileGrp );
        return( FAILURE );
    }

    DAT_writeHdr( fout, pImusr_hdr );

    /*
     *  Convert the histograms
     */
    status = MUD2DAT_hists( fout, pImusr_hdr, pMUD_histGrp, pMUD_indVarGrp );
    if( _failure( status ) )
    {
        _free( pImusr_hdr );
        fclose( fout );
        MUD_free( pMUD_fileGrp );
        return( FAILURE );
    }

    fclose( fout );

    MUD_free( pMUD_fileGrp );
    _free( pImusr_hdr );

    return( SUCCESS );
}


static char*
getMonthString( i )
    int i;
{
    static char *months[] = { "JAN","FEB","MAR","APR","MAY","JUN",
			      "JUL","AUG","SEP","OCT","NOV","DEC" };
    return( ( i >= 0 || i < 12 ) ? months[i] : NULL );
}


static int
MUD2DAT_runDesc( pMUD_desc, pMUD_histGrp,
		 pImusr_hdr )
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_desc;
    MUD_SEC_GRP* pMUD_histGrp;
    IMUSR_HDR* pImusr_hdr;
{
    int i;
    INT32 tempTime[6];
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;

    pImusr_hdr->headtype = -1;
    pImusr_hdr->runno = pMUD_desc->runNumber;
    pImusr_hdr->begindata = 257;
    pImusr_hdr->ipdat1 = 31;
    pImusr_hdr->ipdat2 = 41;
    pImusr_hdr->iptitl = 51;
    pImusr_hdr->ipdesc = 92;
    pImusr_hdr->nvers = 11;
    pImusr_hdr->ipsubtit = 95;    /* should be 95, used to be 94 */
    pImusr_hdr->ipcomment1 = 136;
    pImusr_hdr->ipcomment2 = 176;
    pImusr_hdr->ipcomment3 = 216;

    pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
			       MUD_SEC_GEN_HIST_HDR_ID, (UINT32)1, 
			       (UINT32)0 );

    pMUD_histDat = MUD_search( pMUD_histGrp->pMem, 
			       MUD_SEC_GEN_HIST_DAT_ID, (UINT32)1, 
			       (UINT32)0 );

    pImusr_hdr->npoints_1 = pMUD_histHdr->nBins + 1;
    /* Block of next write (add 1 for header) */
    pImusr_hdr->currentblk = 1 + ( pImusr_hdr->npoints_1 + 7 )/8;
    pImusr_hdr->last_dac = ((INT32*)pMUD_histDat->pData)[pMUD_histHdr->nBins-1];

    for( i = 2; i < 8; i++ )
    {
      if( ( pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
			                 MUD_SEC_GEN_HIST_HDR_ID, (UINT32)i, 
					 (UINT32)0 ) ) != NULL )
      {
	pImusr_hdr->nescalers++;
      }
    }

    for( i = 8; i < 11; i++ )
    {
      if( ( pMUD_histHdr = MUD_search( pMUD_histGrp->pMem, 
				         MUD_SEC_GEN_HIST_HDR_ID, (UINT32)i, 
					 (UINT32)0 ) ) != NULL )
      {
	pImusr_hdr->nascalers++;
      }
    }

    /*
     *  Assemble time info
     */
    GMF_LOCALTIME( &pMUD_desc->timeBegin, tempTime );
    sprintf( pImusr_hdr->dattimstart, "%02d%3s%04d %02d:%02d:%02d",
	     tempTime[3], getMonthString( tempTime[4]-1 ), 
	     tempTime[5]+1900, 
	     tempTime[2], tempTime[1], tempTime[0] ); 
    pImusr_hdr->lendatestrt = strlen( pImusr_hdr->dattimstart );

    GMF_LOCALTIME( &pMUD_desc->timeEnd, tempTime );
    sprintf( pImusr_hdr->dattimeend,  "%02d%3s%04d %02d:%02d:%02d",
	     tempTime[3], getMonthString( tempTime[4]-1 ), 
	     tempTime[5]+1900, 
	     tempTime[2], tempTime[1], tempTime[0] ); 
    pImusr_hdr->lendatend = strlen( pImusr_hdr->dattimeend );

    strncpy( pImusr_hdr->experimenter, pMUD_desc->experimenter, 18 );
    pImusr_hdr->len_experimenter = strlen( pImusr_hdr->experimenter );
    padString( pImusr_hdr->experimenter, ' ', 18 );

    pImusr_hdr->exptNumber = pMUD_desc->exptNumber;

    strncpy( pImusr_hdr->title, pMUD_desc->title, 80 );
    pImusr_hdr->lentitle = strlen( pImusr_hdr->title );
    padString( pImusr_hdr->title, ' ', 80 );

    pImusr_hdr->lendatadesc = 2;
    strncpy( pImusr_hdr->datadescfor, " F", 2 );
    pImusr_hdr->datadesclen = 32;

    strncpy( pImusr_hdr->subtitle, pMUD_desc->subtitle, 80 );
    /* Do it like imusr_write_hdr.for */
    pImusr_hdr->lensubtitle = 40;
/*    pImusr_hdr->lensubtitle = strlen( pImusr_hdr->subtitle ); */
    padString( pImusr_hdr->subtitle, ' ', 80 );

    strncpy( pImusr_hdr->comment1, pMUD_desc->comment1, 78 );
    pImusr_hdr->lencomment1 = strlen( pImusr_hdr->comment1 );
    padString( pImusr_hdr->comment1, ' ', 78 );

    strncpy( pImusr_hdr->comment2, pMUD_desc->comment2, 78 );
    pImusr_hdr->lencomment2 = strlen( pImusr_hdr->comment2 );
    padString( pImusr_hdr->comment2, ' ', 78 );

    strncpy( pImusr_hdr->comment3, pMUD_desc->comment3, 78 );
    pImusr_hdr->lencomment3 = strlen( pImusr_hdr->comment3 );
    padString( pImusr_hdr->comment3, ' ', 78 );

    return( SUCCESS );
}


static int
MUD2DAT_hists( fout, pImusr_hdr, 
	       pMUD_histGrp, pMUD_indVarGrp )
    FILE* fout;
    IMUSR_HDR* pImusr_hdr;
    MUD_SEC_GRP* pMUD_histGrp;
    MUD_SEC_GRP* pMUD_indVarGrp;
{
    int h, b, d, p;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    MUD_SEC_GEN_IND_VAR* pMUD_indVarHdr;
    MUD_SEC_GEN_ARRAY* pMUD_indVarDat;
    IMUSR_BLK* pImusr_blk;
    IMUSR_CAMP_BLOCK imusr_camp_blk;
    int nBlocks;

    nBlocks = ( pImusr_hdr->npoints_1 - 1 + 7 )/8;
    pImusr_blk = (IMUSR_BLK*)zalloc( nBlocks*512 );
    bzero( &imusr_camp_blk, sizeof( IMUSR_CAMP_BLOCK ) );

    /*
     *  Do the DAC and scalers
     */
    for( h = 0; h < 7 + pImusr_hdr->nascalers; h++ )
    {
	pMUD_histDat = MUD_search( pMUD_histGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(h+1), 
				   (UINT32)0 );

	if( ( h >= 1 + pImusr_hdr->nescalers ) && ( h < 7 ) ) continue;

	for( d = 0; d < pImusr_hdr->npoints_1 - 1; d++ )
	{
	    b = d/8;
	    p = d%8;
 
            /*
             *  No need to decode/encode
             */
            bcopy( &((UINT32*)pMUD_histDat->pData)[d],
    	           &pImusr_blk[b].packet[p].data[h], 4 );
	}
    }

    /*
     *  Now do the independent variables
     *  Convert to I-MuSR format nvers=11
     */
    for( h = 10; h < 16 && h < 10 + pImusr_hdr->ncamp; h++ )
    {
	pMUD_indVarHdr = MUD_search( pMUD_indVarGrp->pMem, 
				  MUD_SEC_GEN_IND_VAR_ID, (UINT32)(h-9), 
				  (UINT32)0 );
	pMUD_indVarDat = MUD_search( pMUD_indVarGrp->pMem, 
				  MUD_SEC_GEN_ARRAY_ID, (UINT32)(h-9), 
				  (UINT32)0 );

	for( d = 0; d < pImusr_hdr->npoints_1 - 1; d++ )
	{
	    b = d/8;
	    p = d%8;

            bencode_float( (char*)&pImusr_blk[b].packet[p].data[h],
    			   &((float*)pMUD_indVarDat->pData)[d] );
        }

    	imusr_camp_blk.var[h-10].type = 289; /* CAMP_VAR_TYPE_FLOAT */

    	strncpy( imusr_camp_blk.var[h-10].path, pMUD_indVarHdr->name, 50 );
        imusr_camp_blk.var[h-10].len_path = strlen( pMUD_indVarHdr->name );
        padString( imusr_camp_blk.var[h-10].path, ' ', 50 );

    	strncpy( imusr_camp_blk.var[h-10].title, pMUD_indVarHdr->description, 19 );
        imusr_camp_blk.var[h-10].len_title = strlen( pMUD_indVarHdr->description );
        padString( imusr_camp_blk.var[h-10].title, ' ', 19 );

    	strncpy( imusr_camp_blk.var[h-10].units, pMUD_indVarHdr->units, 8 );
        imusr_camp_blk.var[h-10].len_units = strlen( pMUD_indVarHdr->units );
        padString( imusr_camp_blk.var[h-10].units, ' ', 8 );
    }

    fwrite( pImusr_blk, 512, nBlocks, fout );
    if( pImusr_hdr->ncamp > 0 ) 
    {
      DAT_writeCamp( fout, &imusr_camp_blk, pImusr_hdr->ncamp );
    }
    _free( pImusr_blk );

    return( SUCCESS );
}


