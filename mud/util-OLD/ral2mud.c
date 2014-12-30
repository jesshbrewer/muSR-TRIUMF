/*
 *  ral2mud.c -- Conversion of RAL to MUD format
 *
 *  Revision history:
 *   15-Aug-1995  TW  Initial
 */

#include <stdio.h>
#include <math.h>

#include "mud_util.h"
#include "ral_fmt.h"

int
RAL2MUD_convert( exptNumber, inFile, outFile )
    UINT32 exptNumber;
    char* inFile;
    char* outFile;
{
    int status;
    FILE* fin = NULL;
    FILE* fout = NULL;
    RAL_HEADER ralHeader;
    MUD_SEC_GRP* pMUD_fileGrp = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc = NULL;
    MUD_SEC_GRP* pMUD_histGrp = NULL;

    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", inFile );
	goto error;
    }

    pMUD_fileGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_FMT_RAL_ID );
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

    if( sizeof( ralHeader ) != 1024 )
    {
        fprintf( stderr, 
                 "RAL Header structure is of incorrect size, check code\n" );
        goto error;
    }

    fread( &ralHeader, 1024, 1, fin );

    status = RAL2MUD_runDesc( exptNumber, &ralHeader, pMUD_desc );
    if( _failure( status ) ) 
    {
	goto error;
    }

    /*
     *  Convert the histograms
     */
    pMUD_histGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_RAL_HIST_ID );
    if( pMUD_histGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_histGrp\n" );
	goto error;
    }

    status = RAL2MUD_hists( fin, &tmf_f_hdr, pMUD_histGrp );
    if( _failure( status ) )
    {
	goto error;
    }

    fclose( fin );

    /*
     *  Assemble the first level sections
     */
    MUD_addToGroup( pMUD_fileGrp, pMUD_desc );
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
    MUD_free( pMUD_histGrp );
    MUD_free( pMUD_fileGrp );
    return( FAILURE );
}


int
RAL2MUD_runDesc( exptNumber, 
	         pRalHeader, 
                 pMUD_desc )
    UINT32 exptNumber;
    RAL_HEADER* pRalHeader;
    MUD_SEC_GEN_RUN_DESC* pMUD_desc;
{
    int status;
    int i;
    INT32 tempTime[6];
    char tempString[256];

    pMUD_desc->runNumber = pRalHeader->nrun;
    pMUD_desc->exptNumber = exptNumber;

    /*
     *  TO DO: timeBegin, timeEnd, elapsedSec
     */
/*
    for( i = 0; i < 6; i++ ) tempTime[i] = pTmf_f_hdr->mtnew[i];
    GMF_MKTIME( (INT32*)&pMUD_desc->timeBegin, tempTime );

    for( i = 0; i < 6; i++ ) tempTime[i] = pTmf_f_hdr->mtend[i];
    GMF_MKTIME( (INT32*)&pMUD_desc->timeEnd, tempTime );

    pMUD_desc->elapsedSec = ;
*/
    pMUD_desc->method = strdup( "RAL" );
    pMUD_desc->lab = strdup( "RAL" );
    pMUD_desc->das = strdup( "RAL" );
    pMUD_desc->area = strdup( "RAL" );

    pMUD_desc->title = strdup( pRalHeader->rtitle );

    pMUD_desc->sample = NULL;

    strncpy( tempString, &pRalHeader->rtitle[11], 8 );
    strcat( tempString, " K" );
    pMUD_desc->temperature = strdup( tempString );

    strncpy( tempString, &pRalHeader->rtitle[20], 8 );
    strcat( tempString, " G" );
    pMUD_desc->field = strdup( tempString );

    if( pRalHeader->rtitle[28] == 't' )
    {
        strncpy( tempString, "transverse" );
    }
    elseif( pRalHeader->rtitle[28] == 'l' )
    {
        strncpy( tempString, "longitudinal" );
    }
    elseif( strncmp( &pRalHeader->rtitle[30], "NONE", 4 ) == 0 )
    {
        strncpy( tempString, "none" );
    }
    else
    {    
        strncpy( tempString, &pRalHeader->rtitle[30], 10 );
    }
    pMUD_desc->orient = strdup( tempString );

    pMUD_desc->apparatus = NULL;

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
    int i;
    int j;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    UINT32 ralData[2048];
    UINT32 ralData2[2048];
    int num;
    int size;
    int ralSize;
    int num_blocks;

    for( i = 0; i < pRalHeader->numhis; i++ )
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
	 *  Read the histogram
	 */
        size = 4*pRalHeader->histlen;

	num = fread( ralData, size, 1, fin );
	if( num != 1 ) 
	{
	    fprintf( stderr, "unexpected end-of-file in input file\n" );
	    goto error;
	}

        /*
         *  Handle RG mode
         */
        if( pRalHeader->rgmode )
        {
	    num = fread( ralData2, size, 1, fin );
	    if( num != 1 ) 
	    {
	        fprintf( stderr, "unexpected end-of-file in input file\n" );
	        goto error;
	    }

            /*
             *  Add R and G histograms together
             */
            for( j = 0; j < pRalHeader->histlen; j++ )
            {
                ralData[j] += ralData2[j];
            }
        }

	pMUD_histDat->pData = (char*)zalloc( size );
	if( pMUD_histDat->pData == NULL )
	{
	    goto error;
	}

	bcopy( pMUD_histDat->pData, ralData, size );

        status = RAL2MUD_hist( pRalHeader, pMUD_histHdr, pMUD_histDat );

	MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
	MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
    }

    return( SUCCESS );
error:
    MUD_free( pMUD_histHdr );
    MUD_free( pMUD_histDat );
    return( FAILURE );
}


int
TRI2MUD_hist( pRalHeader, 
	      pMUD_histHdr, 
	      pMUD_histDat )
    RAL_HEADER* pRalHeader;
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

    pMUD_histHdr->histType = MUD_SEC_RAL_HIST_ID;
    pMUD_histHdr->nEvents = _swap32( pTmf_h_recd->u.h.nevtot );
    pMUD_histHdr->nBins = pTmf_h_recd->u.h.length;
    if( ( pTmf_h_recd->u.h.ntpbin >= 1 ) &&
	( pTmf_h_recd->u.h.ntpbin <= 15 ) )
    {
	/*
	 *  TDC code (1-15)
	 */
	pMUD_histHdr->fsPerBin = 78125*pow( (double)2.0, 
					 (double)pTmf_h_recd->u.h.ntpbin );
    }
    else
    {
	/*
	 *  ?? Old files don't use TDC code 
	 *  Looks like it's just straight picoseconds per bin
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

        h = (UINT16*)pTmf_h_data;
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


