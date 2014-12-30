/*
 *  combine.c -- Combine two mud files (add or subtract)
 *
 *   Copyright (C) 1994-2009 TRIUMF (Vancouver, Canada)
 *
 *   Author: D. Arseneau
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
 *  03-Jul-2001  DA  Original
 *
 * $Log: combine.c,v $
 * Revision 1.4  2010/08/26 00:20:55  asnd
 * Release under the GNU LGPL (Yes, LGPL)
 *
 * Revision 1.3  2009/08/23 07:59:03  asnd
 * Fix wrong bytes-per-bin for time-integral.
 *
 * Revision 1.2  2009/08/19 05:26:28  asnd
 * Improve handling of mismatched logged vars, avoiding a possible
 * memory access error.
 *
 * Revision 1.1  2001/07/04 04:52:00  asnd
 * Add combine command to add/subtract/concatenate runs
 *
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"



static int combine_GEN_RUN_DESC _ANSI_ARGS_(( MUD_SEC_GEN_RUN_DESC* pMUD_indesc, MUD_SEC_GEN_RUN_DESC* pMUD_adddesc, MUD_SEC_GEN_RUN_DESC* pMUD_outdesc, int iAddSub ));
static int combine_TRI_TI_RUN_DESC _ANSI_ARGS_(( MUD_SEC_TRI_TI_RUN_DESC* pMUD_indesc, MUD_SEC_TRI_TI_RUN_DESC* pMUD_adddesc, MUD_SEC_TRI_TI_RUN_DESC* pMUD_outdesc, int iAddSub ));
static int combine_SCALER _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_inscalGrp, MUD_SEC_GRP* pMUD_addscalGrp, MUD_SEC_GRP* pMUD_outscalGrp, int iAddSub ));
static int combine_TD_HIST _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_inhistGrp, MUD_SEC_GRP* pMUD_addhistGrp, MUD_SEC_GRP* pMUD_outhistGrp, int iAddSub ));
static int combine_TRI_TI_HIST _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_inhistGrp, MUD_SEC_GRP* pMUD_addhistGrp, MUD_SEC_GRP* pMUD_outhistGrp, int iAddSub ));
static int combine_CMT _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_incmtGrp, MUD_SEC_GRP* pMUD_addcmtGrp, MUD_SEC_GRP* pMUD_outcmtGrp, int iAddSub ));
static int combine_IND_VAR _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_inindVarGrp, MUD_SEC_GRP* pMUD_addindVarGrp, MUD_SEC_GRP* pMUD_outindVarGrp, int iAddSub ));
static int combine_IND_VAR_ARR _ANSI_ARGS_(( MUD_SEC_GRP* pMUD_inindVarGrp, MUD_SEC_GRP* pMUD_addindVarGrp, MUD_SEC_GRP* pMUD_outindVarGrp, int iAddSub ));


int
combine_mud (inFile, addFile, outFile, iAddSub )
    char* inFile;
    char* addFile;
    char* outFile;
    int   iAddSub;
{
    int status;
    UINT32 run_fmt_ID;
    FILE* fin  = NULL;
    FILE* fout = NULL;

    MUD_SEC_GRP* pMUD_infileGrp = NULL;
    MUD_SEC_GRP* pMUD_addfileGrp = NULL;
    MUD_SEC_GRP* pMUD_outfileGrp = NULL;

    MUD_SEC_GEN_RUN_DESC* pMUD_indesc = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_adddesc = NULL;
    MUD_SEC_GEN_RUN_DESC* pMUD_outdesc = NULL;

    MUD_SEC_TRI_TI_RUN_DESC* pMUD_indescTI = NULL;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_adddescTI = NULL;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_outdescTI = NULL;

    MUD_SEC_GRP* pMUD_inscalGrp = NULL;
    MUD_SEC_GRP* pMUD_addscalGrp = NULL;
    MUD_SEC_GRP* pMUD_outscalGrp = NULL;

    MUD_SEC_GRP* pMUD_inhistGrp = NULL;
    MUD_SEC_GRP* pMUD_addhistGrp = NULL;
    MUD_SEC_GRP* pMUD_outhistGrp = NULL;

    MUD_SEC_GRP* pMUD_inarrGrp = NULL;
    MUD_SEC_GRP* pMUD_addarrGrp = NULL;
    MUD_SEC_GRP* pMUD_outarrGrp = NULL;

    MUD_SEC_GRP* pMUD_incmtGrp = NULL;
    MUD_SEC_GRP* pMUD_addcmtGrp = NULL;
    MUD_SEC_GRP* pMUD_outcmtGrp = NULL;

    MUD_SEC_GRP* pMUD_inivarGrp = NULL;
    MUD_SEC_GRP* pMUD_addivarGrp = NULL;
    MUD_SEC_GRP* pMUD_outivarGrp = NULL;
    MUD_SEC_GRP* pMUD_outivarArrGrp = NULL;

    UINT32 indVarType;

    /*
     *  Read the files
     */
    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", inFile );
	return( FAILURE ); 
    }

    pMUD_infileGrp = MUD_readFile( fin );
    if( pMUD_infileGrp == NULL )
    {
	printf( "error while reading file %s\n", inFile );
	goto error;
    }

    fclose( fin );

    fin = MUD_openInput( addFile );
    if( fin == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", inFile );
	goto error; 
    }

    pMUD_addfileGrp = MUD_readFile( fin );
    if( pMUD_addfileGrp == NULL )
    {
	printf( "error while reading file %s\n", addFile );
	goto error;
    }

    fclose( fin );

    run_fmt_ID = MUD_instanceID( pMUD_infileGrp );
    if( MUD_instanceID( pMUD_addfileGrp ) != run_fmt_ID ||
        ( run_fmt_ID != MUD_FMT_TRI_TD_ID && run_fmt_ID != MUD_FMT_TRI_TI_ID ) )
    {
	printf( "error: wrong format ID\n" );
	goto error;
    }

    pMUD_outfileGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, run_fmt_ID );
    if( pMUD_outfileGrp == NULL ) 
    {
	fprintf( stderr, "failed to malloc pMUD_outfileGrp\n" );
	goto error;
    }


    /*
     *  Combine the run descriptions
     */
    switch ( run_fmt_ID )
    {
      case MUD_FMT_TRI_TD_ID:
        pMUD_indesc = MUD_search( pMUD_infileGrp->pMem, 
                              MUD_SEC_GEN_RUN_DESC_ID, (UINT32)1, 
                              (UINT32)0 );
        pMUD_adddesc = MUD_search( pMUD_addfileGrp->pMem, 
                              MUD_SEC_GEN_RUN_DESC_ID, (UINT32)1, 
                              (UINT32)0 );
        if( pMUD_indesc == NULL || pMUD_adddesc == NULL )
        {
            printf( "error: bad file format: run description not found\n" );
            goto error;
        }
        pMUD_outdesc = (MUD_SEC_GEN_RUN_DESC*)MUD_new( MUD_SEC_GEN_RUN_DESC_ID,
                                                       (UINT32)1 );
        if( pMUD_outdesc == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_outdesc\n" );
            goto error;
        }

        status = combine_GEN_RUN_DESC( pMUD_indesc, pMUD_adddesc, 
                                       pMUD_outdesc, iAddSub);
        if( _failure( status ) ) 
        {
            goto error;
        }
        break;

      case MUD_FMT_TRI_TI_ID:
        pMUD_indescTI = MUD_search( pMUD_infileGrp->pMem, 
                              MUD_SEC_TRI_TI_RUN_DESC_ID, (UINT32)1, 
                              (UINT32)0 );
        pMUD_adddescTI = MUD_search( pMUD_addfileGrp->pMem, 
                               MUD_SEC_TRI_TI_RUN_DESC_ID, (UINT32)1, 
                               (UINT32)0 );
        if( pMUD_indescTI == NULL || pMUD_adddescTI == NULL )
        {
            printf( "error: bad file format: run description not found\n" );
            goto error;
        }
        pMUD_outdescTI = (MUD_SEC_TRI_TI_RUN_DESC*)MUD_new( MUD_SEC_TRI_TI_RUN_DESC_ID, 
                                                      (UINT32)1 );
        if( pMUD_outdescTI == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_outdescTI\n" );
            goto error;
        }

        status = combine_TRI_TI_RUN_DESC( pMUD_indescTI, pMUD_adddescTI, 
                                          pMUD_outdescTI, iAddSub);
        if( _failure( status ) )
        {
          goto error;
        }
        break;
    }

    if( _failure(status) )
    {
        goto error;
    }

    /*
     *  Combine the scalers
     */
    pMUD_inscalGrp = MUD_search( pMUD_infileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_SCALER_ID, 
			       (UINT32)0 );
    pMUD_addscalGrp = MUD_search( pMUD_addfileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_SCALER_ID, 
			       (UINT32)0 );

    if( pMUD_inscalGrp != NULL )
    {
#ifdef DEBUG
        printf( "found scaler group\n" );
#endif /* DEBUG */

        pMUD_outscalGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, 
                                                 MUD_GRP_TRI_TD_SCALER_ID );
        if( pMUD_outscalGrp == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_outscalGrp\n" );
            goto error;
        }
        status = combine_SCALER( pMUD_inscalGrp, pMUD_addscalGrp, 
                                 pMUD_outscalGrp, iAddSub);
        if( _failure( status ) ) 
        {
            goto error;
        }
    }


    /*
     *  Combine the (TD) histograms
     */
    pMUD_inhistGrp = MUD_search( pMUD_infileGrp->pMem, 
                                 MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID, 
                                 (UINT32)0 );
    pMUD_addhistGrp = MUD_search( pMUD_addfileGrp->pMem, 
                                  MUD_SEC_GRP_ID, MUD_GRP_TRI_TD_HIST_ID, 
                                  (UINT32)0 );
    /*
     *  Enforce presence of TD histograms in TD runs; allow them in TI runs!
     */
    if( run_fmt_ID == MUD_FMT_TRI_TD_ID &&
        ( pMUD_inhistGrp == NULL || pMUD_addhistGrp == NULL ) )
    {
        printf( "error: bad file format: histogram group not found\n" );
        goto error;
    }
    
    if( pMUD_inhistGrp != NULL )
    {
        pMUD_outhistGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, 
                                                 MUD_GRP_TRI_TD_HIST_ID );
        if( pMUD_outhistGrp == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_outhistGrp\n" );
            goto error;
        }
        
        status = combine_TD_HIST( pMUD_inhistGrp, pMUD_addhistGrp, 
                                  pMUD_outhistGrp, iAddSub );
        if( _failure( status ) )
        {
            goto error;
        }
    }

    /*
     *  Combine the TI data arrays, if desired
     */
    pMUD_inarrGrp = MUD_search( pMUD_infileGrp->pMem, 
                                MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID, 
                                (UINT32)0 );
    pMUD_addarrGrp = MUD_search( pMUD_addfileGrp->pMem, 
                                 MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID, 
                                 (UINT32)0 );
    /*
     *  Enforce presence of TI data arrays in TI runs
     */
    if( run_fmt_ID == MUD_FMT_TRI_TI_ID &&
        ( pMUD_inarrGrp == NULL || pMUD_addarrGrp == NULL ) )
    {
        printf( "error: bad file format: data array group not found\n" );
        goto error;
    }
    
    if( pMUD_inarrGrp != NULL )
    {
        pMUD_outarrGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID );
        if( pMUD_outarrGrp == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_outarrGrp\n" );
            goto error;
        }
        
        status = combine_TRI_TI_HIST( pMUD_inarrGrp, pMUD_addarrGrp, 
                                      pMUD_outarrGrp, iAddSub );
        if( _failure( status ) )
        {
            goto error;
        }
    }

    /*
     *  Combine comments if they exist
     */
    pMUD_incmtGrp = MUD_search( pMUD_infileGrp->pMem, 
                                MUD_SEC_GRP_ID, MUD_GRP_CMT_ID, 
                                (UINT32)0 );
    pMUD_addcmtGrp = MUD_search( pMUD_addfileGrp->pMem, 
                                 MUD_SEC_GRP_ID, MUD_GRP_CMT_ID, 
                                 (UINT32)0 );
    if( pMUD_incmtGrp != NULL || pMUD_addcmtGrp != NULL )
    {
        pMUD_outcmtGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_CMT_ID );
        if( pMUD_outcmtGrp == NULL )
        {
            fprintf( stderr, "failed to malloc pMUD_outcmtGrp\n" );
            goto error;
        }

        status = combine_CMT( pMUD_incmtGrp, pMUD_addcmtGrp, pMUD_outcmtGrp, iAddSub );
        if( _failure( status ) )
        {
            goto error;
        }
    }

    /*
     *  Combine the independent variable statistics
     */

    pMUD_inivarGrp = MUD_search( pMUD_infileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ID, (UINT32)0 );
    if( pMUD_inivarGrp != NULL )
    {
        pMUD_addivarGrp = MUD_search( pMUD_addfileGrp->pMem,
			       MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ID, (UINT32)0 );
        if( pMUD_addivarGrp != NULL )
        {
            pMUD_outivarGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ID );

            if( pMUD_outivarGrp == NULL )
            {
                fprintf( stderr, "failed to malloc pMUD_outivarGrp\n" );
                goto error;
            }

            status = combine_IND_VAR( pMUD_inivarGrp, pMUD_addivarGrp, 
                                  pMUD_outivarGrp, iAddSub );
            if( _failure( status ) )
            {
                goto error;
            }
        }
    }

    pMUD_inivarGrp = MUD_search( pMUD_infileGrp->pMem, 
			       MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ARR_ID, (UINT32)0 );
    if( pMUD_inivarGrp != NULL )
    {
        pMUD_addivarGrp = MUD_search( pMUD_addfileGrp->pMem,
			       MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ARR_ID, (UINT32)0 );
        if( pMUD_addivarGrp != NULL )
        {
            pMUD_outivarArrGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ARR_ID );

            if( pMUD_outivarArrGrp == NULL )
            {
                fprintf( stderr, "failed to malloc pMUD_outivarArrGrp\n" );
                goto error;
            }

            status = combine_IND_VAR_ARR( pMUD_inivarGrp, pMUD_addivarGrp, 
                                  pMUD_outivarArrGrp, iAddSub );
            if( _failure( status ) )
            {
                goto error;
            }
        }
    }


    /*
     *  Assemble groups at end, after creating all of them.  That way,
     *  if there is an error we know we can safely free them each 
     *  individually without corrupting pMUD_outfileGrp.  We assemble 
     *  TD and TI runs differently 
     */
    switch ( run_fmt_ID )
    {
      case MUD_FMT_TRI_TD_ID:
        if( pMUD_outdesc    != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outdesc );
        if( pMUD_outscalGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outscalGrp );
        if( pMUD_outhistGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outhistGrp );
        if( pMUD_outcmtGrp  != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outcmtGrp );
        if( pMUD_outivarGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outivarGrp );
        break;

      case MUD_FMT_TRI_TI_ID:
        if( pMUD_outdescTI  != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outdescTI );
        if( pMUD_outarrGrp  != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outarrGrp );
        if( pMUD_outivarArrGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outivarArrGrp );
        if( pMUD_outivarGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outivarGrp );
        if( pMUD_outcmtGrp  != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outcmtGrp );
        if( pMUD_outhistGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outhistGrp );
        if( pMUD_outscalGrp != NULL ) MUD_addToGroup( pMUD_outfileGrp, pMUD_outscalGrp );
        break;
    }

    /*
     *  Write the output mud file
     */
    fout = MUD_openOutput( outFile );
    if( fout == NULL ) 
    {
	fprintf( stderr, "failed to open file \"%s\"\n", outFile );
	goto out_error;
    }

    MUD_writeFile( fout, pMUD_outfileGrp );

    fclose( fout );

    MUD_free( pMUD_infileGrp );
    MUD_free( pMUD_addfileGrp );
    MUD_free( pMUD_outfileGrp );

    return( SUCCESS );

 error:

    MUD_free( pMUD_outivarGrp );
    MUD_free( pMUD_outarrGrp );
    MUD_free( pMUD_outhistGrp );
    MUD_free( pMUD_outcmtGrp );
    MUD_free( pMUD_outscalGrp );
    MUD_free( pMUD_outdescTI );
    MUD_free( pMUD_outdesc );
 out_error:
    if( fin  != NULL ) fclose( fin );
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_infileGrp );
    MUD_free( pMUD_addfileGrp );
    MUD_free( pMUD_outfileGrp );

    if( _success( status ) ) status = FAILURE;

    return( status );

}


/*      
 *     Combine TD run headers.  
 *     Mainly take the information form the first run, ignoring the
 *     second, but do combine the timeBegin, timeEnd, elapsedSec.
 */

int
combine_GEN_RUN_DESC( pMUD_indesc, pMUD_adddesc, pMUD_outdesc, iAddSub)
    MUD_SEC_GEN_RUN_DESC* pMUD_indesc;
    MUD_SEC_GEN_RUN_DESC* pMUD_adddesc;
    MUD_SEC_GEN_RUN_DESC* pMUD_outdesc;
    int   iAddSub;
{
    pMUD_outdesc->runNumber = pMUD_indesc->runNumber ;
    pMUD_outdesc->exptNumber = pMUD_indesc->exptNumber ;

    pMUD_outdesc->timeBegin = _min( pMUD_indesc->timeBegin,
                                    pMUD_adddesc->timeBegin );
    pMUD_outdesc->timeEnd = _max( pMUD_indesc->timeEnd,
                                  pMUD_adddesc->timeEnd );
    pMUD_outdesc->elapsedSec = pMUD_indesc->elapsedSec 
                             + pMUD_adddesc->elapsedSec ;

    pMUD_outdesc->title = strdup( pMUD_indesc->title );
    pMUD_outdesc->lab = strdup( pMUD_indesc->lab );
    pMUD_outdesc->area = strdup( pMUD_indesc->area );
    pMUD_outdesc->method = strdup( pMUD_indesc->method );
    pMUD_outdesc->apparatus = strdup( pMUD_indesc->apparatus );
    pMUD_outdesc->insert = strdup( pMUD_indesc->insert );
    pMUD_outdesc->sample = strdup( pMUD_indesc->sample );
    pMUD_outdesc->orient = strdup( pMUD_indesc->orient );
    pMUD_outdesc->das = strdup( pMUD_indesc->das );
    pMUD_outdesc->experimenter = strdup( pMUD_indesc->experimenter );
    pMUD_outdesc->temperature = strdup( pMUD_indesc->temperature );
    pMUD_outdesc->field = strdup( pMUD_indesc->field );

    return( SUCCESS );
}

/*      
 *     Combine TI (ImuSR) run headers.  
 *     Mainly take the information form the first run, ignoring the
 *     second, but do combine the timeBegin, timeEnd, elapsedSec.
 */

int
combine_TRI_TI_RUN_DESC( pMUD_indesc, pMUD_adddesc, pMUD_outdesc, iAddSub)
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_indesc;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_adddesc;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_outdesc;
    int   iAddSub;
{
    pMUD_outdesc->runNumber = pMUD_indesc->runNumber ;
    pMUD_outdesc->exptNumber = pMUD_indesc->exptNumber ;

    pMUD_outdesc->timeBegin = _min( pMUD_indesc->timeBegin,
                                    pMUD_adddesc->timeBegin );
    pMUD_outdesc->timeEnd = _max( pMUD_indesc->timeEnd,
                                  pMUD_adddesc->timeEnd );
    pMUD_outdesc->elapsedSec = pMUD_indesc->elapsedSec 
                             + pMUD_adddesc->elapsedSec ;

    pMUD_outdesc->title = strdup( pMUD_indesc->title );
    pMUD_outdesc->lab = strdup( pMUD_indesc->lab );
    pMUD_outdesc->area = strdup( pMUD_indesc->area );
    pMUD_outdesc->method = strdup( pMUD_indesc->method );
    pMUD_outdesc->apparatus = strdup( pMUD_indesc->apparatus );
    pMUD_outdesc->insert = strdup( pMUD_indesc->insert );
    pMUD_outdesc->sample = strdup( pMUD_indesc->sample );
    pMUD_outdesc->orient = strdup( pMUD_indesc->orient );
    pMUD_outdesc->das = strdup( pMUD_indesc->das );
    pMUD_outdesc->experimenter = strdup( pMUD_indesc->experimenter );
    pMUD_outdesc->subtitle = strdup( pMUD_indesc->subtitle );
    pMUD_outdesc->comment1 = strdup( pMUD_indesc->comment1 );
    pMUD_outdesc->comment2 = strdup( pMUD_indesc->comment2 );
    pMUD_outdesc->comment3 = strdup( pMUD_indesc->comment3 );

    return( SUCCESS );
}


int
combine_SCALER( pMUD_inscalGrp, pMUD_addscalGrp, pMUD_outscalGrp, iAddSub)
    MUD_SEC_GRP* pMUD_inscalGrp;
    MUD_SEC_GRP* pMUD_addscalGrp;
    MUD_SEC_GRP* pMUD_outscalGrp;
    int   iAddSub;
{
    int i;
    int n;
    BOOL haveBoth;
    BOOL didWarn = FALSE;
    MUD_SEC_GEN_SCALER* pMUD_inscal;
    MUD_SEC_GEN_SCALER* pMUD_addscal;
    MUD_SEC_GEN_SCALER* pMUD_outscal;

    /* 
     *  Check for no scalers
     */
    if( pMUD_inscalGrp == NULL )
    {
        printf( "error: bad file format: no scalers found\n" );
        return( FAILURE );
    }

    /* 
     *  Check for no scalers to add.
     */
    if( pMUD_addscalGrp == NULL )
    {
        haveBoth = FALSE;
        n = pMUD_inscalGrp->num;
    }
    else
    {
        haveBoth = TRUE;
        if( pMUD_inscalGrp->num != pMUD_addscalGrp->num )
        {
            printf( "warning: runs have different numbers of scalers\n" );
        }
        n = _min( pMUD_inscalGrp->num, pMUD_addscalGrp->num );
    }
    /*
     *  Scaler totals & labels
     */
    for( i = 0; i < n; i++ )
    {
        pMUD_inscal = MUD_search( pMUD_inscalGrp->pMem, 
				MUD_SEC_GEN_SCALER_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_inscal == NULL ) 
	{
	    printf( "error: bad file format: scaler #%d not found\n", i+1 );
	    goto error;
	}

	pMUD_outscal = (MUD_SEC_GEN_SCALER*)MUD_new( MUD_SEC_GEN_SCALER_ID, i+1 );
	if( pMUD_outscal == NULL ) 
	{
	    fprintf( stderr, "failed to malloc pMUD_outscal\n" );
	    goto error;
	}

        if( haveBoth )
        {
            pMUD_addscal = MUD_search( pMUD_addscalGrp->pMem, 
				MUD_SEC_GEN_SCALER_ID, (UINT32)(i+1), 
				(UINT32)0 );
            if( pMUD_addscal == NULL ) 
            {
                printf( "error: bad file format: scaler #%d not found\n", i+1 );
                goto error;
            }
            if( strncmp( pMUD_inscal->label, pMUD_addscal->label, 10 )
                && !didWarn )
            {
                printf( "warning: scaler labels do not match\n" );
                didWarn = TRUE;
            }
            /* 
             * we add or subtract scaler totals based on iAddSub, but don't do
             * pMUD_inscal->counts[0] + iAddSub * pMUD_addscal->counts[0];
             * because scalers are UINT32, and we don't want to reduce the 
             * total range.
             */
            if( iAddSub > 0 )
            {
                pMUD_outscal->counts[0] = 
                         pMUD_inscal->counts[0] + pMUD_addscal->counts[0];
            }
            else
            {
                pMUD_outscal->counts[0] = 
                         pMUD_inscal->counts[0] - pMUD_addscal->counts[0];
            }
        }
        pMUD_outscal->counts[1] = pMUD_inscal->counts[1] ;
	pMUD_outscal->label = strdup( pMUD_inscal->label );

	MUD_addToGroup( pMUD_outscalGrp, pMUD_outscal );
    }

    return( SUCCESS );

 error:

    MUD_free( pMUD_outscal );
    return( FAILURE );

}

int
combine_TD_HIST( pMUD_inhistGrp, pMUD_addhistGrp, pMUD_outhistGrp, iAddSub )
    MUD_SEC_GRP* pMUD_inhistGrp;
    MUD_SEC_GRP* pMUD_addhistGrp;
    MUD_SEC_GRP* pMUD_outhistGrp;
    int   iAddSub;
{
    int i, j ;
    int nh ;
    int nb = 0;
    MUD_SEC_GEN_HIST_HDR* pMUD_inhistHdr;
    MUD_SEC_GEN_HIST_HDR* pMUD_addhistHdr;
    MUD_SEC_GEN_HIST_HDR* pMUD_outhistHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_inhistDat;
    MUD_SEC_GEN_HIST_DAT* pMUD_addhistDat;
    MUD_SEC_GEN_HIST_DAT* pMUD_outhistDat;
    UINT32* pinHistData = NULL;
    UINT32* paddHistData = NULL;
    UINT32* pinj;
    UINT32* paddj;

    /* 
     *  Check for no histograms
     */
    if( pMUD_inhistGrp == NULL || pMUD_addhistGrp == NULL )
    {
        printf( "error: bad file format: no hisograms found\n" );
        return( FAILURE );
    }
    if( pMUD_inhistGrp->num != pMUD_addhistGrp->num )
    {
        goto incompatible;
    }
    nh = (pMUD_inhistGrp->num) / 2 ;

    /*
     *  Loop over histograms
     */

    for( i = 0; i < nh ; i++ )
    {
        /*
         *   Read the hist headers
         */
        pMUD_inhistHdr = MUD_search( pMUD_inhistGrp->pMem, 
				MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_inhistHdr == NULL ) goto nohisti;

        pMUD_addhistHdr = MUD_search( pMUD_addhistGrp->pMem, 
				MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_addhistHdr == NULL ) goto nohisti;

	pMUD_inhistDat = MUD_search( pMUD_inhistGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(i+1), 
				   (UINT32)0 );
	if( pMUD_inhistDat == NULL ) goto nohisti;

	pMUD_addhistDat = MUD_search( pMUD_addhistGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(i+1), 
				   (UINT32)0 );
	if( pMUD_addhistDat == NULL ) goto nohisti;

        if( pMUD_inhistHdr->histType != pMUD_addhistHdr->histType ||
            pMUD_inhistHdr->nBins    != pMUD_addhistHdr->nBins    ||
            pMUD_inhistHdr->fsPerBin != pMUD_addhistHdr->fsPerBin )
        {
            goto incompatible;
        }
        /*
         *   Declare the output mud sections
         */
	pMUD_outhistHdr = (MUD_SEC_GEN_HIST_HDR*)MUD_new( MUD_SEC_GEN_HIST_HDR_ID, i+1 );
	pMUD_outhistDat = (MUD_SEC_GEN_HIST_DAT*)MUD_new( MUD_SEC_GEN_HIST_DAT_ID, i+1 );
        if( pMUD_outhistHdr == NULL || pMUD_outhistDat == NULL )
        {
            goto noallochist;
        }

        /*
         *   Allocate / reallocate internal histogram buffers.
         *   Usually, all hists are the same size, so the buffers
         *   will be allocated just once for a run.
         */
        if( nb != pMUD_inhistHdr->nBins )
        {
            nb = pMUD_inhistHdr->nBins;

            _free( pinHistData );
            _free( paddHistData );
            
            pinHistData = (UINT32*)zalloc( 4*nb );
            paddHistData = (UINT32*)zalloc( 4*nb );

            if( pinHistData == NULL || paddHistData == NULL )
            {
                goto noallochist;
            }
        }

        /*
         *   Get hist data and add/subtract them.
         */
	MUD_SEC_GEN_HIST_unpack( nb, pMUD_inhistHdr->bytesPerBin, 
            pMUD_inhistDat->pData, 4, pinHistData );

	MUD_SEC_GEN_HIST_unpack( nb, pMUD_addhistHdr->bytesPerBin,
            pMUD_addhistDat->pData, 4, paddHistData );

        pinj = pinHistData;
        paddj = paddHistData;

        for( j = 0; j < nb; j++ )
        {
            *paddj++ = *paddj * iAddSub + *pinj++ ;
        }

        /*
         *  Combine the header info (mostly copy from "in" run)
         */
        pMUD_outhistHdr->histType = pMUD_inhistHdr->histType ;
        pMUD_outhistHdr->nBins = pMUD_inhistHdr->nBins ;
        pMUD_outhistHdr->bytesPerBin = 
           _max( pMUD_inhistHdr->bytesPerBin, pMUD_addhistHdr->bytesPerBin );
        pMUD_outhistHdr->fsPerBin = pMUD_inhistHdr->fsPerBin ;
        pMUD_outhistHdr->t0_ps = pMUD_inhistHdr->t0_ps ;
        pMUD_outhistHdr->t0_bin = pMUD_inhistHdr->t0_bin ;
        pMUD_outhistHdr->goodBin1 = pMUD_inhistHdr->goodBin1 ;
        pMUD_outhistHdr->goodBin2 = pMUD_inhistHdr->goodBin2 ;
        pMUD_outhistHdr->bkgd1 = pMUD_inhistHdr->bkgd1 ;
        pMUD_outhistHdr->bkgd2 = pMUD_inhistHdr->bkgd2 ;
        pMUD_outhistHdr->title = strdup( pMUD_inhistHdr->title ) ;

        /*
         *  We take care to keep the full range of the total-counts
         *  in the header, but don't worry about single bins with 
         *  more than 2E9 counts.
         */ 
        if( iAddSub > 0 )
        {
            pMUD_outhistHdr->nEvents = pMUD_inhistHdr->nEvents + 
                       pMUD_addhistHdr->nEvents ;
        }
        else
        {
            pMUD_outhistHdr->nEvents = pMUD_inhistHdr->nEvents - 
                       pMUD_addhistHdr->nEvents ;
        }

        /*
         *  Save the combined header & data.  
         */

        if( (pMUD_outhistDat->pData = (char*)zalloc( 4*nb )) == NULL )
        {
            goto noallochist;
        }

        pMUD_outhistHdr->nBytes = MUD_SEC_GEN_HIST_pack( nb, 4, paddHistData, 
		    pMUD_outhistHdr->bytesPerBin, pMUD_outhistDat->pData );

        pMUD_outhistDat->nBytes = pMUD_outhistHdr->nBytes;

	MUD_addToGroup( pMUD_outhistGrp, pMUD_outhistHdr );
	MUD_addToGroup( pMUD_outhistGrp, pMUD_outhistDat );

    }

    _free( pinHistData );
    _free( paddHistData );
    return( SUCCESS );

 incompatible:
    printf( "error: histograms are incompatible\n" );
    goto fail;

 noallochist:
    printf( "error: could not allocate space for histograms\n" );
    goto fail;

 nohisti:
    printf( "error: bad file format: histogram #%d not found\n", i+1 );
    goto fail;

 fail:
    _free( pinHistData );
    _free( paddHistData );
    MUD_free( pMUD_outhistHdr );
    MUD_free( pMUD_outhistDat );

    return( FAILURE );
}


/*   
 *  Combine TI data arrays ("histograms", though they're not)
 *  by concatenation.  This is just slightly different from
 *  combine_TD_HIST
 */
int
combine_TRI_TI_HIST( pMUD_inhistGrp, pMUD_addhistGrp, pMUD_outhistGrp, iAddSub )
    MUD_SEC_GRP* pMUD_inhistGrp;
    MUD_SEC_GRP* pMUD_addhistGrp;
    MUD_SEC_GRP* pMUD_outhistGrp;
    int   iAddSub;
{
    int i ;
    int nh ;
    int nb = 0;
    MUD_SEC_GEN_HIST_HDR* pMUD_inhistHdr;
    MUD_SEC_GEN_HIST_HDR* pMUD_addhistHdr;
    MUD_SEC_GEN_HIST_HDR* pMUD_outhistHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_inhistDat;
    MUD_SEC_GEN_HIST_DAT* pMUD_addhistDat;
    MUD_SEC_GEN_HIST_DAT* pMUD_outhistDat;
    UINT32* poutHistData = NULL;

    /*
     *  Check for no histograms
     */
    if( pMUD_inhistGrp == NULL || pMUD_addhistGrp == NULL )
    {
        printf( "error: bad file format: no scaler arrays found\n" );
        return( FAILURE );
    }
    if( pMUD_inhistGrp->num != pMUD_addhistGrp->num )
    {
        goto incompatible;
    }
    nh = (pMUD_inhistGrp->num) / 2 ;

    /*
     *  Loop over histograms
     */

    for( i = 0; i < nh ; i++ )
    {
        /*
         *   Read the hist headers
         */
        pMUD_inhistHdr = MUD_search( pMUD_inhistGrp->pMem, 
				MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_inhistHdr == NULL ) goto nohisti;

        pMUD_addhistHdr = MUD_search( pMUD_addhistGrp->pMem, 
				MUD_SEC_GEN_HIST_HDR_ID, (UINT32)(i+1), 
				(UINT32)0 );
	if( pMUD_addhistHdr == NULL ) goto nohisti;

	pMUD_inhistDat = MUD_search( pMUD_inhistGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(i+1), 
				   (UINT32)0 );
	if( pMUD_inhistDat == NULL ) goto nohisti;

	pMUD_addhistDat = MUD_search( pMUD_addhistGrp->pMem, 
				   MUD_SEC_GEN_HIST_DAT_ID, (UINT32)(i+1), 
				   (UINT32)0 );
	if( pMUD_addhistDat == NULL ) goto nohisti;

        if( pMUD_inhistHdr->histType != pMUD_addhistHdr->histType )
        {
            goto incompatible;
        }
        /*
         *   Declare the output mud sections
         */
	pMUD_outhistHdr = (MUD_SEC_GEN_HIST_HDR*)MUD_new( MUD_SEC_GEN_HIST_HDR_ID, i+1 );
	pMUD_outhistDat = (MUD_SEC_GEN_HIST_DAT*)MUD_new( MUD_SEC_GEN_HIST_DAT_ID, i+1 );
        if( pMUD_outhistHdr == NULL || pMUD_outhistDat == NULL )
        {
            goto noallochist;
        }

        /*
         *   Allocate / reallocate internal histogram buffers.
         *   Usually, all hists are the same size, so the buffers
         *   will be allocated just once for a run.
         */
        if( nb != pMUD_inhistHdr->nBins + pMUD_addhistHdr->nBins )
        {
            nb = pMUD_inhistHdr->nBins + pMUD_addhistHdr->nBins ;

            _free( poutHistData );
            
            poutHistData = (UINT32*)zalloc( 4*nb );

            if( poutHistData == NULL )
            {
                goto noallochist;
            }
        }

        /*
         *   Get hist data and concatenate them (ignoring iAddSub)
         */
	MUD_SEC_GEN_HIST_unpack( pMUD_inhistHdr->nBins, pMUD_inhistHdr->bytesPerBin, 
            pMUD_inhistDat->pData, 4, poutHistData );

	MUD_SEC_GEN_HIST_unpack( pMUD_addhistHdr->nBins, pMUD_addhistHdr->bytesPerBin,
            pMUD_addhistDat->pData, 4, (poutHistData + pMUD_inhistHdr->nBins) );

        /*
         *  Copy the hist header info from "in" run
         */
        pMUD_outhistHdr->histType = pMUD_inhistHdr->histType ;
        pMUD_outhistHdr->nBins = pMUD_inhistHdr->nBins ;
        pMUD_outhistHdr->bytesPerBin = 4;
        pMUD_outhistHdr->fsPerBin = pMUD_inhistHdr->fsPerBin ;
        pMUD_outhistHdr->t0_ps = pMUD_inhistHdr->t0_ps ;
        pMUD_outhistHdr->t0_bin = pMUD_inhistHdr->t0_bin ;
        pMUD_outhistHdr->goodBin1 = pMUD_inhistHdr->goodBin1 ;
        pMUD_outhistHdr->goodBin2 = pMUD_inhistHdr->goodBin2 ;
        pMUD_outhistHdr->bkgd1 = pMUD_inhistHdr->bkgd1 ;
        pMUD_outhistHdr->bkgd2 = pMUD_inhistHdr->bkgd2 ;
        pMUD_outhistHdr->title = strdup( pMUD_inhistHdr->title ) ;
        pMUD_outhistHdr->nEvents = pMUD_inhistHdr->nEvents + pMUD_addhistHdr->nEvents ;

        /*
         *  Save the combined header & data
         */
        if( (pMUD_outhistDat->pData = (char*)zalloc( 4*nb )) == NULL )
          goto noallochist;

        pMUD_outhistHdr->nBytes = MUD_SEC_GEN_HIST_pack( nb, 4, poutHistData, 
		    pMUD_outhistHdr->bytesPerBin, pMUD_outhistDat->pData );
        pMUD_outhistDat->nBytes = pMUD_outhistHdr->nBytes;

	MUD_addToGroup( pMUD_outhistGrp, pMUD_outhistHdr );
	MUD_addToGroup( pMUD_outhistGrp, pMUD_outhistDat );

    }

    _free( poutHistData );
    return( SUCCESS );

 incompatible:
    printf( "error: histograms are incompatible\n" );
    goto fail;

 noallochist:
    printf( "error: could not allocate space for histograms\n" );
    goto fail;

 nohisti:
    printf( "error: bad file format: histogram #%d not found\n", i+1 );
    goto fail;

 fail:
    _free( poutHistData );
    MUD_free( pMUD_outhistHdr );
    MUD_free( pMUD_outhistDat );

    return( FAILURE );
}


/*
 *  Combine comments, ... not written yet
 */
int
combine_CMT( pMUD_incmtGrp, pMUD_addcmtGrp, pMUD_outcmtGrp, iAddSub )
    MUD_SEC_GRP* pMUD_incmtGrp;
    MUD_SEC_GRP* pMUD_addcmtGrp;
    MUD_SEC_GRP* pMUD_outcmtGrp;
    int iAddSub;
{
    MUD_SEC_GRP* pMUD_cmtGrp;
    MUD_SEC_CMT* pMUD_incmt;
    MUD_SEC_CMT* pMUD_outcmt;
    int i, j, num, offset;

    num = 0;

    /*
     *  Do the "in" comments, then the "add" comments.
     */
    for( j=0; j<2; j++)
    {
        pMUD_cmtGrp = ( j == 0 ? pMUD_incmtGrp : pMUD_addcmtGrp ) ;
        offset = num;

        if( pMUD_cmtGrp == NULL ) continue;

        for( i = 0; i < pMUD_cmtGrp->num ; i++ )
        {
            
            pMUD_incmt = MUD_search( pMUD_cmtGrp->pMem, 
				MUD_SEC_CMT_ID, (UINT32)(i+1), 
				(UINT32)0 );

            if( pMUD_incmt != NULL ) 
            {
                pMUD_outcmt = (MUD_SEC_CMT*)MUD_new( MUD_SEC_CMT_ID, num );
                if( pMUD_outcmt == NULL )
                {
                    printf( "error: could not allocate space for comments\n" );
                    return( FAILURE );
                }

                num++;
                pMUD_outcmt->time = pMUD_incmt->time ;
                pMUD_outcmt->ID = pMUD_incmt->ID + offset ;
                pMUD_outcmt->prevReplyID = pMUD_incmt->prevReplyID + 
                           (pMUD_incmt->prevReplyID == 0 ? 0 : offset) ;
                pMUD_outcmt->nextReplyID = pMUD_incmt->nextReplyID +
                           (pMUD_incmt->nextReplyID == 0 ? 0 : offset) ;
                pMUD_outcmt->author = strdup( pMUD_incmt->author ) ;
                pMUD_outcmt->title = strdup( pMUD_incmt->title ) ;
                pMUD_outcmt->comment = strdup( pMUD_incmt->comment ) ;

                MUD_addToGroup( pMUD_outcmtGrp, pMUD_outcmt );
            }

        } /* next comment i */

    } /* next run j */

    return( SUCCESS );

}


/*
 *  Combine logged variable information by:
 *  1) If only summaries are present, crudely approximate combined mean,
 *     std.dev. and skew; give correct min & max: combine_IND_VAR
 *  2) If arrays of logged values are given, concatenate the arrays, and
 *     calculate a new summary based on the values: combine_IND_VAR_ARR
 */
int
combine_IND_VAR( pMUD_inindVarGrp, pMUD_addindVarGrp, pMUD_outindVarGrp, iAddSub )
    MUD_SEC_GRP* pMUD_inindVarGrp;
    MUD_SEC_GRP* pMUD_addindVarGrp;
    MUD_SEC_GRP* pMUD_outindVarGrp;
    int iAddSub;
{
    MUD_SEC_GEN_IND_VAR* pMUD_inindVarHdr;
    MUD_SEC_GEN_IND_VAR* pMUD_addindVarHdr;
    MUD_SEC_GEN_IND_VAR* pMUD_outindVarHdr;
    int num;
    int nvout = 0;
    double d, s;
    int i, j;

    if( pMUD_inindVarGrp == NULL || pMUD_addindVarGrp == NULL )
    {
        return( FAILURE );
    }

    /*
     *  Loop over independent variables
     */

    for( i = 0; i < pMUD_inindVarGrp->num ; i++ )
    {
        /*
         *   Read the sections
         */
        pMUD_inindVarHdr = MUD_search( pMUD_inindVarGrp->pMem,
				MUD_SEC_GEN_IND_VAR_ID, (UINT32)(i+1),
				(UINT32)0 );

        pMUD_addindVarHdr = MUD_search( pMUD_addindVarGrp->pMem,
				MUD_SEC_GEN_IND_VAR_ID, (UINT32)(i+1),
				(UINT32)0 );

        if( pMUD_inindVarHdr == NULL || pMUD_addindVarHdr == NULL ) continue;

        if( strncmp( pMUD_inindVarHdr->name, pMUD_addindVarHdr->name, 16 ) ||
            strncmp( pMUD_inindVarHdr->description, pMUD_addindVarHdr->description, 16 ) ||
            strncmp( pMUD_inindVarHdr->units, pMUD_addindVarHdr->units, 16 ) )
        {
            printf( "warning: logged variables (%s, %s) could not be combined\n",
		    pMUD_inindVarHdr->name, pMUD_addindVarHdr->name);
            continue;
        }

        nvout++;
        pMUD_outindVarHdr = (MUD_SEC_GEN_IND_VAR*)MUD_new( MUD_SEC_GEN_IND_VAR_ID, nvout );
        if( pMUD_outindVarHdr == NULL )
        {
            goto noalloc;
        }

        pMUD_outindVarHdr->name = strdup( pMUD_inindVarHdr->name ) ;
        pMUD_outindVarHdr->description = strdup( pMUD_inindVarHdr->description ) ;
        pMUD_outindVarHdr->units = strdup( pMUD_inindVarHdr->units ) ;

        /*
         *  Roughly combine statistics from two runs
         */
        pMUD_outindVarHdr->low = _min( pMUD_inindVarHdr->low, pMUD_addindVarHdr->low );
        pMUD_outindVarHdr->high = _max( pMUD_inindVarHdr->high, pMUD_addindVarHdr->high );
        pMUD_outindVarHdr->mean = ( pMUD_inindVarHdr->mean + pMUD_addindVarHdr->mean )/2.0 ;
        d = ( pMUD_inindVarHdr->mean - pMUD_addindVarHdr->mean )/2.0 ;
        s = ( pMUD_inindVarHdr->stddev + pMUD_addindVarHdr->stddev )/2.0 ;
        pMUD_outindVarHdr->stddev = sqrt( d*d + s*s );
        pMUD_outindVarHdr->skewness = ( pMUD_inindVarHdr->skewness + 
                                        pMUD_addindVarHdr->skewness )/2.0 ;

        MUD_addToGroup( pMUD_outindVarGrp, pMUD_outindVarHdr );
	pMUD_outindVarHdr = NULL;

    }   /*   end loop over variables  */

    return( SUCCESS );

 noalloc:
    printf( "error: could not allocate space for variables\n" );
    goto fail;

 fail:
    MUD_free( pMUD_outindVarHdr );

    return( FAILURE );
}


int
combine_IND_VAR_ARR( pMUD_inindVarGrp, pMUD_addindVarGrp, pMUD_outindVarGrp, iAddSub )
    MUD_SEC_GRP* pMUD_inindVarGrp;
    MUD_SEC_GRP* pMUD_addindVarGrp;
    MUD_SEC_GRP* pMUD_outindVarGrp;
    int iAddSub;
{
    MUD_SEC_GEN_IND_VAR* pMUD_inindVarHdr;
    MUD_SEC_GEN_IND_VAR* pMUD_addindVarHdr;
    MUD_SEC_GEN_IND_VAR* pMUD_outindVarHdr;
    MUD_SEC_GEN_ARRAY* pMUD_inindVarDat;
    MUD_SEC_GEN_ARRAY* pMUD_addindVarDat;
    MUD_SEC_GEN_ARRAY* pMUD_outindVarDat;
    float* poutindVarData = NULL;
    float* poutj = NULL;
    int num, nvar;
    int nvout = 0;
    double sum, sum2, sum3, val, dn, mean, stddev, d, s;
    int i, j;


    if( pMUD_inindVarGrp == NULL || pMUD_addindVarGrp == NULL )
    {
        return( FAILURE );
    }

    /*
     *  Loop over independent variables
     */
    nvar = pMUD_inindVarGrp->num / 2 ;

    for( i = 0; i < nvar ; i++ )
    {
        /*
         *   Read the pair of sections
         */
        pMUD_inindVarHdr = MUD_search( pMUD_inindVarGrp->pMem, 
				MUD_SEC_GEN_IND_VAR_ID, (UINT32)(i+1), 
				(UINT32)0 );

        pMUD_inindVarDat = MUD_search( pMUD_inindVarGrp->pMem, 
                                MUD_SEC_GEN_ARRAY_ID, (UINT32)(i+1), 
                                (UINT32)0 );

	if( pMUD_inindVarHdr == NULL || pMUD_inindVarDat == NULL ) goto novari;

        pMUD_addindVarHdr = MUD_search( pMUD_addindVarGrp->pMem, 
				MUD_SEC_GEN_IND_VAR_ID, (UINT32)(i+1), 
				(UINT32)0 );

        pMUD_addindVarDat = MUD_search( pMUD_addindVarGrp->pMem, 
				MUD_SEC_GEN_ARRAY_ID, (UINT32)(i+1), 
				(UINT32)0 );

        if( pMUD_addindVarHdr == NULL || pMUD_addindVarDat == NULL )
        {
            printf( "warning: logged variables could not be combined\n" );
            break;
        }
        if( strncmp( pMUD_inindVarHdr->name, pMUD_addindVarHdr->name, 16 ) ||
            strncmp( pMUD_inindVarHdr->description, pMUD_addindVarHdr->description, 16 ) ||
            strncmp( pMUD_inindVarHdr->units, pMUD_addindVarHdr->units, 16 ) )
        {
            printf( "warning: logged variables could not be combined\n" );
            continue;
        }
        if( pMUD_inindVarDat->elemSize != 4 ||
            pMUD_addindVarDat->elemSize != 4 ||
            pMUD_inindVarDat->type != 2 ||
            pMUD_addindVarDat->type != 2 )
        {
            printf( "warning: unusable logged variable\n" );
            continue;
        }

        /*
         *   Allocate output sections
         */
        nvout++;
        pMUD_outindVarHdr = (MUD_SEC_GEN_IND_VAR*)MUD_new( MUD_SEC_GEN_IND_VAR_ID, nvout );
        pMUD_outindVarDat = (MUD_SEC_GEN_ARRAY*)MUD_new( MUD_SEC_GEN_ARRAY_ID, nvout );
        if( pMUD_outindVarHdr == NULL || pMUD_outindVarDat == NULL )
        {
            goto noalloc;
        }

        num = pMUD_inindVarDat->num + pMUD_addindVarDat->num ;
            
        poutindVarData = (float*)zalloc( 4*num );
        
        if( poutindVarData == NULL )
        {
            goto noalloc;
        }

        pMUD_outindVarHdr->name = strdup( pMUD_inindVarHdr->name ) ;
        pMUD_outindVarHdr->description = strdup( pMUD_inindVarHdr->description ) ;
        pMUD_outindVarHdr->units = strdup( pMUD_inindVarHdr->units ) ;


        /*
         *   Get array data and concatenate them (ignoring iAddSub)
         */
        poutj = poutindVarData;
        pMUD_outindVarDat->pData = (caddr_t) poutj;
        for( j = 0; j < pMUD_inindVarDat->num; j++ )
        {
            *poutj++ = ((float*)pMUD_inindVarDat->pData)[j];
        }
        for( j = 0; j < pMUD_addindVarDat->num; j++ )
        {
            *poutj++ = ((float*)pMUD_addindVarDat->pData)[j];
        }
        pMUD_outindVarDat->num = num;
        pMUD_outindVarDat->elemSize = 4;   /* 4 bytes/bin */
        pMUD_outindVarDat->type = 2;       /* 2=real */
        pMUD_outindVarDat->hasTime = 0;    /* no time data */
        pMUD_outindVarDat->pTime = NULL;
        pMUD_outindVarDat->nBytes = num * pMUD_outindVarDat->elemSize;
        /*
         *  Calculate statistics from combined array
         */
        if( num > 0 )
        {
            sum = sum2 = sum3 = 0.0;
            pMUD_outindVarHdr->low = pMUD_outindVarHdr->high = 
                    ((REAL32*)pMUD_outindVarDat->pData)[0];
            for( j = 0; j < num; j++ )
            {
                val = (double)((REAL32*)pMUD_outindVarDat->pData)[j];
                pMUD_outindVarHdr->low  = _min( pMUD_outindVarHdr->low, val );
                pMUD_outindVarHdr->high = _max( pMUD_outindVarHdr->high, val );
                sum += val;
                sum2 += val*val;
                sum3 += val*val*val;
            }
            dn = (double)num;
            mean = pMUD_outindVarHdr->mean = sum/dn;
            stddev = pMUD_outindVarHdr->stddev = ( num == 1 ) ? 0.0 :
                         sqrt( fabs( ( sum2 - sum*sum/dn )/( num - 1 ) ) );
            pMUD_outindVarHdr->skewness = ( stddev == 0.0 ) ? 0.0 :
                       ( sum3 - 3.0*mean*sum2 + 2.0*sum*sum*sum/(dn*dn) ) /
                             ( dn*stddev*stddev*stddev );
        }
        /* 
         *  Put combined stats into output group
         */            
        MUD_addToGroup( pMUD_outindVarGrp, pMUD_outindVarHdr );
        MUD_addToGroup( pMUD_outindVarGrp, pMUD_outindVarDat );
        poutindVarData = NULL;

    }   /*   end loop over variables  */

    _free( poutindVarData );
    return( SUCCESS );

 noalloc:
    printf( "error: could not allocate space for variables\n" );
    goto fail;

 novari:
    printf( "error: bad file format: logged variables not found\n" );
    goto fail;

 fail:
    _free( poutindVarData );
    MUD_free( pMUD_outindVarHdr );
    MUD_free( pMUD_outindVarDat );

    return( FAILURE );
}

