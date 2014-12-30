/*
 *  tri2spreadsheet.c -- Conversion of TRIUMF-TD to spreadsheet readable 
 *
 *  Revision history:
 *   21-Mar-1994  TW  Initial version
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"
#include "trid_fmt.h"

static int TRI2spreadsheet_hists _ANSI_ARGS_(( FILE *fin, char *outFile, TMF_F_HDR *pTmf_f_hdr ));


int
TRI2spreadsheet_convert( inFile, outFile_pref )
    char* inFile;
    char* outFile_pref;
{
    int status;
    FILE* fin;
    FILE* fout;
    TMF_F_HDR* pTmf_f_hdr;

    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
      fprintf( stderr, "failed to open file \"%s\"\n", inFile );
      return( FAILURE );
    }

    pTmf_f_hdr = (TMF_F_HDR*)zalloc( sizeof( TMF_F_HDR ) );

    TRI_readHdr( fin, pTmf_f_hdr );

    status = TRI2spreadsheet_hists( fin, outFile_pref, pTmf_f_hdr );

    fclose( fin );
    _free( pTmf_f_hdr );

    return( status );
}


static int
TRI2spreadsheet_hists( fin, outFile, pTmf_f_hdr )
    FILE* fin;
    char* outFile;
    TMF_F_HDR* pTmf_f_hdr;
{
    int i, j;
    TMF_H_RECD tmf_h_recd;
    char* pTmf_h_data;
    FILE* fout;
    int num;
    int size;
    int num_blocks;
    INT16* ps;

    pTmf_h_data = NULL;

    fout = MUD_openOutput( outFile );
    if( fout == NULL ) 
    {
      fprintf( stderr, "failed to open output file \"%s\"\n", outFile );
      return( FAILURE );
    }

    for( i = 0; i < pTmf_f_hdr->mhists; i++ )
    {
        TRI_readHistHdr( fin, &tmf_h_recd );

	size = 2*tmf_h_recd.u.h.length;

	/* 
	 *  Allocate this 512 bytes too much because
	 *  histograms are aligned on 512 block boundaries
	 *  (i.e. there is up to 511 bytes more than
	 *  the size of the histogram and header)
	 */
	pTmf_h_data = (char*)zalloc( size + 512 );
	if( pTmf_h_data == NULL )
	{
	    fprintf( stderr, "failed to malloc pTmf_h_data\n" );
	    goto error;
	}

	bcopy( &tmf_h_recd.u.data[32], pTmf_h_data, 512-64 );

	/*
	 *  Read the rest of the blocks of the histogram
	 */
	num_blocks = ((int)(( ( 64 + size ) + 511 )/512 ))-1;
	num = fread( &pTmf_h_data[448], 512, num_blocks, fin );
	if( num != num_blocks ) 
	{
	    fprintf( stderr, "unexpected end-of-file in input file\n" );
	    goto error;
	}

        /*
         *  Decode the histograms
         *  This is needed only to handle byte ordering
         */
        ps = (INT16*)pTmf_h_data;
        for( j = 0; j < tmf_h_recd.u.h.length; j++ )
        {
          bdecode_2( ps, ps++ );
        }

        /*
         *  Do the write
         */
        fprintf( fout, "Histogram %d - %d bins\n", i+1, tmf_h_recd.u.h.length );

	for( j = 0; j < tmf_h_recd.u.h.length; j++ )
	{
	    fprintf( fout, "%u\n", ((UINT16*)pTmf_h_data)[j] );
	}

	_free( pTmf_h_data );
    }

    fclose( fout );
    return( SUCCESS );

error:
    if( fout != NULL ) fclose( fout );
    if( pTmf_h_data != NULL ) free( pTmf_h_data );
    return( FAILURE );
}


