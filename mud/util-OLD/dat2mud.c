/*
 *  dat2mud.c -- Conversion of TRIUMF-TI to MSR-TI
 *		 for µSR Data Format Utility
 *
 *  Revision history:
 *   26-Jan-1994  TW  Initial version
 *   15-Feb-1994  TW  Split ...GEN_HIST to ...GEN_HIST_HDR
 *                       and ...GEN_HIST_DAT
 *   17-Feb-1994  TW  Groups with member index
 *   22-Feb-1996  TW  Convert independent variables, experimenter and
 *                    exptNumber.  Convert nvers=7,8 or 9.
 *   27-Feb-1996  TW  nvers=10, exptNumber is integer.
 *   06-Mar-1996  TW  nvers=11, camp block aligned
 *   23-Jul-1999  DA  Fix bugs
 *          2000  DA  put time-string interpretation in mu_interp_time
 *   20-Aug-2001  DA  checks on input-file validity
 *   31-Aug-2001  DA  Use specified experimental area
 *   15-Nov-2001  DA  Allow more than 3 Aux scalers, pushing camp vars back 
 */

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "mud_util.h"
#include "trii_fmt.h"

int getMonth _ANSI_ARGS_(( char *s ));
static int DAT_readHdr _ANSI_ARGS_(( FILE* fin, IMUSR_HDR* pImusr_hdr ));
static int DAT_readCamp _ANSI_ARGS_(( FILE* fin, IMUSR_CAMP_BLOCK* pImusr_blk, int num ));
static int DAT_readCampV8 _ANSI_ARGS_(( FILE* fin, IMUSR_CAMP_BLOCK_V8* pImusr_blk, int num ));
static int DAT2MUD_runDesc _ANSI_ARGS_(( IMUSR_HDR *pImusr_hdr, MUD_SEC_TRI_TI_RUN_DESC *pMUD_desc, char* exptnumber, char* exptmember, char* exptarea ));
static int DAT2MUD_hists _ANSI_ARGS_(( FILE *fin, IMUSR_HDR *pImusr_hdr, MUD_SEC_GRP *pMUD_histGrp, MUD_SEC_GRP *pMUD_indVarGrp ));
static char *getHistType _ANSI_ARGS_(( int i ));


static int
DAT_readHdr( fin, pImusr_hdr )
    FILE* fin;
    IMUSR_HDR* pImusr_hdr;
{
    char buf[512];
    char* p;
    int n;

    if( ( n = fread( buf, 512, 1, fin ) ) == 0 ) return( 0 );
    p = buf;
    bdecode_2( p, &pImusr_hdr->headtype ); p += 2;
    bdecode_2( p, &pImusr_hdr->runno ); p += 2;
    bdecode_2( p, &pImusr_hdr->begindata ); p += 2;
    bdecode_2( p, &pImusr_hdr->npoints_1 ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipdat1 ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipdat2 ); p += 2;
    bdecode_2( p, &pImusr_hdr->iptitl ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipdesc ); p += 2;
    bdecode_2( p, &pImusr_hdr->nvers ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipsubtit ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipcomment1 ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipcomment2 ); p += 2;
    bdecode_2( p, &pImusr_hdr->ipcomment3 ); p += 2;
    bdecode_2( p, &pImusr_hdr->nescalers ); p += 2;
    bdecode_2( p, &pImusr_hdr->nascalers ); p += 2;
    bdecode_2( p, &pImusr_hdr->currentblk ); p += 2;
    bdecode_4( p, &pImusr_hdr->last_dac ); p += 4;
    bdecode_2( p, &pImusr_hdr->ncamp ); p += 2;
    bdecode_2( p, &pImusr_hdr->exptNumber ); p += 2;
    bdecode_2( p, &pImusr_hdr->len_experimenter ); p += 2;
    bdecode_obj( p, pImusr_hdr->experimenter, 18 ); p += 18;
    bdecode_2( p, &pImusr_hdr->lendatestrt ); p += 2;
    bdecode_obj( p, pImusr_hdr->dattimstart, 18 ); p += 18;
    bdecode_2( p, &pImusr_hdr->lendatend ); p += 2;
    bdecode_obj( p, pImusr_hdr->dattimeend, 18 ); p += 18;
    bdecode_2( p, &pImusr_hdr->lentitle ); p += 2;
    bdecode_obj( p, pImusr_hdr->title, 80 ); p += 80;
    bdecode_2( p, &pImusr_hdr->lendatadesc ); p += 2;
    bdecode_obj( p, pImusr_hdr->datadescfor, 2 ); p += 2;
    bdecode_2( p, &pImusr_hdr->datadesclen ); p += 2;
    bdecode_2( p, &pImusr_hdr->lensubtitle ); p += 2;
    bdecode_obj( p, pImusr_hdr->subtitle, 80 ); p += 80;
    bdecode_2( p, &pImusr_hdr->lencomment1 ); p += 2;
    bdecode_obj( p, pImusr_hdr->comment1, 78 ); p += 78;
    bdecode_2( p, &pImusr_hdr->lencomment2 ); p += 2;
    bdecode_obj( p, pImusr_hdr->comment2, 78 ); p += 78;
    bdecode_2( p, &pImusr_hdr->lencomment3 ); p += 2;
    bdecode_obj( p, pImusr_hdr->comment3, 78 ); p += 78;
    bdecode_2( p, &pImusr_hdr->lasthdrword ); p += 2;

/*
 *  check if data is valid integral muSR file
 */
    if( pImusr_hdr->headtype != -1 ||
        pImusr_hdr->begindata < 250 ||
        pImusr_hdr->ipdat1 < 10 || pImusr_hdr->ipdat1 > 512 ||
        pImusr_hdr->ipdat2 < 10 || pImusr_hdr->ipdat2 > 512 ||
        pImusr_hdr->iptitl < 10 || pImusr_hdr->iptitl > 512 ||
        pImusr_hdr->ipdesc < 10 || pImusr_hdr->ipdesc > 512 )
    {
        return( 0 );
    }

/*  
 *  Unended runs are flaged by zero for end-time length.  Ignore.
 */
    if( pImusr_hdr->lendatend < 1 ) pImusr_hdr->lendatend = 
		pImusr_hdr->lendatestrt;
/*   
 *  String lengths were in words for versions 2-6
 */
    if( pImusr_hdr->nvers < 7 && pImusr_hdr->nvers > 1 ) 
    {
	pImusr_hdr->lentitle = pImusr_hdr->lentitle * 2;
        pImusr_hdr->lendatestrt = pImusr_hdr->lendatestrt * 2;
        pImusr_hdr->lendatend = pImusr_hdr->lendatend * 2;
    }

    return( n );
}

static int
DAT_readCamp( fin, pImusr_blk, num )
    FILE* fin;
    IMUSR_CAMP_BLOCK* pImusr_blk;
    int num;
{
    char buf[512];
    char* p;
    int i;
    int n;

    if( ( n = fread( buf, 512, 1, fin ) ) == 0 ) return( 0 );
    p = buf;
    for( i = 0; i < num; i++ )
    {
      bdecode_2( p, &pImusr_blk->var[i].type ); p += 2;
      bdecode_2( p, &pImusr_blk->var[i].len_path ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].path, 50 ); p += 50;
      bdecode_2( p, &pImusr_blk->var[i].len_title ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].title, 19 ); p += 19;
      bdecode_2( p, &pImusr_blk->var[i].len_units ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].units, 8 ); p += 8;
    }
      

    return( n );
}

static int
DAT_readCampV8( fin, pImusr_blk, num )
    FILE* fin;
    IMUSR_CAMP_BLOCK_V8* pImusr_blk;
    int num;
{
    char buf[512];
    char* p;
    int i;
    int n;

    if( ( n = fread( buf, 512, 1, fin ) ) == 0 ) return( 0 );
    p = buf;
    for( i = 0; i < num; i++ )
    {
      bdecode_4( p, &pImusr_blk->var[i].type ); p += 4;
      bdecode_2( p, &pImusr_blk->var[i].len_path ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].path, 114 ); p += 114;
      bdecode_2( p, &pImusr_blk->var[i].len_title ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].title, 31 ); p += 31;
      bdecode_2( p, &pImusr_blk->var[i].len_units ); p += 2;
      bdecode_obj( p, pImusr_blk->var[i].units, 15 ); p += 15;
    }

    return( n );
}

int
DAT2MUD_convert( inFile, outFile, exptnumber, exptmember, exptarea )
    char* inFile;
    char* outFile;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    FILE* fin;
    FILE* fout;
    IMUSR_HDR* pImusr_hdr;
    MUD_SEC_GRP* pMUD_fileGrp;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_desc;
    MUD_SEC_GRP* pMUD_histGrp;
    MUD_SEC_GRP* pMUD_indVarGrp;

    fin = MUD_openInput( inFile );
    if( fin == NULL ) 
    {
        fprintf( stderr, "failed to open file \"%s\"\n", inFile );
        return( FAILURE );
    }

    pMUD_fileGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_FMT_TRI_TI_ID );

    /*
     *  Convert the run description
     */
    pMUD_desc = (MUD_SEC_TRI_TI_RUN_DESC*)MUD_new( MUD_SEC_TRI_TI_RUN_DESC_ID, 1 );

    pImusr_hdr = (IMUSR_HDR*)zalloc( sizeof( IMUSR_HDR ) );
    if( DAT_readHdr( fin, pImusr_hdr ) == 0 )
    {
        fprintf( stderr, "input \"%s\" is not a readable IµSR dat file\n", inFile );
        fclose( fin );
        MUD_free( pMUD_fileGrp );
        _free( pImusr_hdr );
        return( FAILURE );
    }

    DAT2MUD_runDesc( pImusr_hdr, pMUD_desc, exptnumber, exptmember, exptarea );

    /*
     *  Convert the histograms and independent variables
     */
    pMUD_histGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_TRI_TI_HIST_ID );
    if( pImusr_hdr->ncamp > 0 )
    {
      pMUD_indVarGrp = (MUD_SEC_GRP*)MUD_new( MUD_SEC_GRP_ID, MUD_GRP_GEN_IND_VAR_ARR_ID );
    }
    else pMUD_indVarGrp = NULL;

    DAT2MUD_hists( fin, pImusr_hdr, pMUD_histGrp, pMUD_indVarGrp );

    fclose( fin );

    /*
     *  Assemble the first level sections
     */
    MUD_addToGroup( pMUD_fileGrp, pMUD_desc );
    MUD_addToGroup( pMUD_fileGrp, pMUD_histGrp );
    if( pImusr_hdr->ncamp > 0 )
    {
      MUD_addToGroup( pMUD_fileGrp, pMUD_indVarGrp );
    }

    /*
     *  Do the write
     */
    fout = MUD_openOutput( outFile );
    if( fout == NULL ) return( FAILURE );

    MUD_writeFile( fout, pMUD_fileGrp );

    fclose( fout );

    /*
     *  Free malloc'ed mem
     */
    MUD_free( pMUD_fileGrp );
    _free( pImusr_hdr );

    return( SUCCESS );
}


int
getMonth( s )
    char* s;
{
    int i;
    char month[4];
    static char *months[] = { "jan", "feb", "mar", "apr", "may", "jun", 
                              "jul", "aug", "sep", "oct", "nov", "dec" };

    strncpy( month, s, 3 );
    month[3] = '\0';
    stolower( month );

    for( i = 0; i < 12; i++ )
	if( strncmp( month, months[i], 3 ) == 0 ) return( i );

    return( -1 );
}

void 
mu_interp_time( time_t* ptimval_out, char in_date[], char in_time[] )
{
    INT32 tempTime[6], tempTime1[6];
    if( in_date[2] != '-' )
    {     /*  e.g.,  " 3MAY1999 16:53:00"   */
	tempTime[0] = 1000*(in_date[5]-48) +
		      100*(in_date[6]-48) + 
		      10*(in_date[7]-48) + 
		      (in_date[8]-48) - 1900;
	tempTime[1] = getMonth( &in_date[2] ) + 1;
    }
    else
    {      /*  e.g.,  "04-JUN-89.23:26:45"  (. is null) */
	tempTime[0] = 10*(in_date[7]-48) + 
		      (in_date[8]-48);
	if( tempTime[0] < 70 ) tempTime[0] += 100;
	tempTime[1] = getMonth( &in_date[3] ) + 1;
    }
    tempTime[2] = in_date[1]-48 ;
    if( in_date[0] != ' ' ) tempTime[2] += 10*(in_date[0]-48);

    tempTime[3] = 10*(in_time[0]-48) + (in_time[1]-48);
    tempTime[4] = 10*(in_time[3]-48) + (in_time[4]-48);
    tempTime[5] = 10*(in_time[6]-48) + (in_time[7]-48);

    GMF_MKTIME( ptimval_out, tempTime );
    return;
}

static int
DAT2MUD_runDesc( pImusr_hdr, pMUD_desc,
                 exptnumber, exptmember, exptarea )
    IMUSR_HDR* pImusr_hdr;
    MUD_SEC_TRI_TI_RUN_DESC* pMUD_desc;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    int i;
    INT32 tempTime[6];
    INT32 tempTime1[6];
    char tempString[256];
    int expt;
    char* p;

    pMUD_desc->runNumber = pImusr_hdr->runno;

    if( exptnumber[0] != '\0' )
    {
      expt = strtol( exptnumber, &p, 0 );
      if( *p == 0 ) pMUD_desc->exptNumber = expt;
    }
    else 
    {
      pMUD_desc->exptNumber = pImusr_hdr->exptNumber;
    }

    if( exptmember[0] != '\0' )
    {
      pMUD_desc->experimenter = strdup( exptmember );
    }
    else if( pImusr_hdr->len_experimenter > 0 )
    {
      strncpy( tempString, pImusr_hdr->experimenter, 
               pImusr_hdr->len_experimenter );
      tempString[pImusr_hdr->len_experimenter] = '\0';
      pMUD_desc->experimenter = strdup( tempString );
    }


    /*
     *  Assemble time info
     */
    mu_interp_time( (time_t*)&pMUD_desc->timeBegin, 
		    &pImusr_hdr->dattimstart[0], &pImusr_hdr->dattimstart[10] );
    mu_interp_time( (time_t*)&pMUD_desc->timeEnd, 
		    &pImusr_hdr->dattimeend[0], &pImusr_hdr->dattimeend[10] );

    pMUD_desc->elapsedSec = pMUD_desc->timeEnd - pMUD_desc->timeBegin;

    strncpy( tempString, pImusr_hdr->title, pImusr_hdr->lentitle );
    tempString[pImusr_hdr->lentitle] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->title = strdup( tempString );

    pMUD_desc->lab = strdup( "TRIUMF" );

    if( *exptarea ) pMUD_desc->area = strdup( exptarea );
    else if( pMUD_desc->runNumber < 5000 ) pMUD_desc->area = strdup( "M20" );
    else if( pMUD_desc->runNumber < 10000 ) pMUD_desc->area = strdup( "M15" );
    else if( pMUD_desc->runNumber < 15000 ) pMUD_desc->area = strdup( "M13" );
    else if( pMUD_desc->runNumber < 20000 ) pMUD_desc->area = strdup( "M9" );
    else if( pMUD_desc->runNumber < 25000 ) pMUD_desc->area = strdup( "DASDEV" );
    else if( pMUD_desc->runNumber >= 30001 && pMUD_desc->runNumber < 35000 )
        pMUD_desc->area = strdup( "TEST" );

    pMUD_desc->method = strdup( "TI-µSR" );
    pMUD_desc->das = strdup( "MODAS" );

    strncpy( tempString, pImusr_hdr->subtitle, pImusr_hdr->lensubtitle );
    tempString[pImusr_hdr->lensubtitle] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->subtitle = strdup( tempString );

    strncpy( tempString, pImusr_hdr->comment1, pImusr_hdr->lencomment1 );
    tempString[pImusr_hdr->lencomment1] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->comment1 = strdup( tempString );

    strncpy( tempString, pImusr_hdr->comment2, pImusr_hdr->lencomment2 );
    tempString[pImusr_hdr->lencomment2] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->comment2 = strdup( tempString );

    strncpy( tempString, pImusr_hdr->comment3, pImusr_hdr->lencomment3 );
    tempString[pImusr_hdr->lencomment3] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->comment3 = strdup( tempString );

    return( SUCCESS );
}


static char*
getHistType( i )
    int i;
{
    static char *types[] = { "DAC","F+","F-","B+","B-","Mu","CLOC",
		             "AUX1","AUX2","AUX3","AUX4","AUX5",
			     "AUX6","AUX7","AUX8","AUX9" };
    return( ( i > 0 || i < 17 ) ? types[i-1] : NULL );
}


static int
DAT2MUD_hists( fin, pImusr_hdr, 
               pMUD_histGrp, pMUD_indVarGrp )
    FILE* fin;
    IMUSR_HDR* pImusr_hdr;
    MUD_SEC_GRP* pMUD_histGrp;
    MUD_SEC_GRP* pMUD_indVarGrp;
{
    int h, b, d, p, i, j;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    MUD_SEC_GEN_IND_VAR* pMUD_indVarHdr;
    MUD_SEC_GEN_ARRAY* pMUD_indVarDat;
    IMUSR_BLK* pImusr_blk;
    IMUSR_CAMP_BLOCK imusr_camp_blk;
    IMUSR_CAMP_BLOCK_V8 imusr_camp_blk_v8;
    int nScalers, nPackBlock, nBlocks;
    int indVarOffset;
    double val, sum, sum2, sum3, mean, stddev;
    int num;
    float f;

    if( pImusr_hdr->nvers < 5 )
	nScalers = 8;
    else
	nScalers = 16;

    nPackBlock = 128/nScalers;
    nBlocks = ( pImusr_hdr->npoints_1 - 1 + ( nPackBlock - 1 ) )/nPackBlock;
    pImusr_blk = (IMUSR_BLK*)zalloc( nBlocks*512 );
    fread( pImusr_blk, 512, nBlocks, fin );
    if( pImusr_hdr->ncamp > 0 )
    {
      /*
       *  Maintain backwards compatibility with nvers=7,8
       */
      if( pImusr_hdr->nvers >= 9 )
      {
        DAT_readCamp( fin, &imusr_camp_blk, pImusr_hdr->ncamp );
      }
      else
      {
        DAT_readCampV8( fin, &imusr_camp_blk_v8, pImusr_hdr->ncamp );
      }
    }
    if( ( pImusr_hdr->nescalers == 0 ) && ( pImusr_hdr->nascalers == 0 ) )
    {
	pImusr_hdr->nescalers = 6 ;
	pImusr_hdr->nascalers = nScalers - ( pImusr_hdr->nescalers + 1 ) ;
    }

    for( h = 0; h < 7 + pImusr_hdr->nascalers; h++ )
    {
	if( ( h >= 1 + pImusr_hdr->nescalers ) && ( h < 7 ) ) continue;

	pMUD_histHdr = (MUD_SEC_GEN_HIST_HDR*)MUD_new( MUD_SEC_GEN_HIST_HDR_ID, h+1 );
	pMUD_histDat = (MUD_SEC_GEN_HIST_DAT*)MUD_new( MUD_SEC_GEN_HIST_DAT_ID, h+1 );

 	/*
 	 *  Allocate space for the array (4 bytes/bin, no packing)
	 */
	pMUD_histDat->pData = (caddr_t)zalloc( 
				4*( pImusr_hdr->npoints_1 - 1 ) );

	for( d = 0; d < pImusr_hdr->npoints_1 - 1; d++ )
	{
	    b = d / nPackBlock;
	    p = d % nPackBlock;

            /* 
             *  No need to decode/encode
             */
            bcopy( &pImusr_blk[b].packet[p].data[h],
                   &((UINT32*)pMUD_histDat->pData)[d], 4 );
	}

        pMUD_histHdr->histType = MUD_SEC_TRI_TI_HIST_ID;
        pMUD_histHdr->nBins = pImusr_hdr->npoints_1 - 1;
        pMUD_histHdr->nEvents = pMUD_histHdr->nBins;
        pMUD_histHdr->bytesPerBin = 4;
        pMUD_histHdr->nBytes = pMUD_histHdr->bytesPerBin * pMUD_histHdr->nBins;
        pMUD_histHdr->title = strdup( getHistType( MUD_instanceID( pMUD_histHdr ) ) );

        pMUD_histDat->nBytes = pMUD_histHdr->nBytes;

	MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
	MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
    }

    /*
     *  Now do the independent variables
     */
    indVarOffset = _max( ( pImusr_hdr->nvers >= 9 ? 10 : 13 ),
                         ( 7 + pImusr_hdr->nascalers ) );

    for( h = indVarOffset; h < 16 && h < indVarOffset + pImusr_hdr->ncamp; h++ )
    {
    	i = h-indVarOffset;
	pMUD_indVarHdr = (MUD_SEC_GEN_IND_VAR*)MUD_new( MUD_SEC_GEN_IND_VAR_ID, i+1 );
	pMUD_indVarDat = (MUD_SEC_GEN_ARRAY*)MUD_new( MUD_SEC_GEN_ARRAY_ID, i+1 );

 	/*
 	 *  Allocate space for the array (4 bytes/bin, no packing)
	 */
	pMUD_indVarDat->pData = (caddr_t)zalloc( 
                                  4*( pImusr_hdr->npoints_1 - 1 ) );

	for( d = 0; d < pImusr_hdr->npoints_1 - 1; d++ )
	{
	    b = d / nPackBlock;
	    p = d % nPackBlock;

            /*
             *  Handle floats across architectures
             */
            bdecode_float( (char*)&pImusr_blk[b].packet[p].data[h], 
                           &((float*)pMUD_indVarDat->pData)[d] );
	}

	pMUD_indVarDat->num = pImusr_hdr->npoints_1 - 1;
	pMUD_indVarDat->elemSize = 4;   /* 4 bytes/bin */
	pMUD_indVarDat->type = 2;       /* 2=real */
	pMUD_indVarDat->hasTime = 0;    /* no time data */
	pMUD_indVarDat->pTime = NULL;
        pMUD_indVarDat->nBytes = pMUD_indVarDat->num*pMUD_indVarDat->elemSize;

    	if( pImusr_hdr->nvers >= 9 )
        {
          pMUD_indVarHdr->name = strndup( imusr_camp_blk.var[i].path,
                                          imusr_camp_blk.var[i].len_path );
          pMUD_indVarHdr->description = strndup( imusr_camp_blk.var[i].title,
                                          imusr_camp_blk.var[i].len_title );
          pMUD_indVarHdr->units = strndup( imusr_camp_blk.var[i].units,
                                          imusr_camp_blk.var[i].len_units );
        }
	else
	{
          pMUD_indVarHdr->name = strndup( imusr_camp_blk_v8.var[i].path,
                                          imusr_camp_blk_v8.var[i].len_path );
          pMUD_indVarHdr->description = strndup( imusr_camp_blk_v8.var[i].title,
                                          imusr_camp_blk_v8.var[i].len_title );
          pMUD_indVarHdr->units = strndup( imusr_camp_blk_v8.var[i].units,
                                          imusr_camp_blk_v8.var[i].len_units );
	}

    	trimBlanks( pMUD_indVarHdr->name, pMUD_indVarHdr->name );
    	trimBlanks( pMUD_indVarHdr->description, pMUD_indVarHdr->description );
    	trimBlanks( pMUD_indVarHdr->units, pMUD_indVarHdr->units );

	/*
         *  Calculate statistics
 	 */
	num = pMUD_indVarDat->num;
	if( num > 0 )
        {
	  sum = sum2 = sum3 = 0.0;
	  pMUD_indVarHdr->low = pMUD_indVarHdr->high = 
                                    ((REAL32*)pMUD_indVarDat->pData)[0];
	  for( j = 0; j < num; j++ )
	  {
	    val = (double)((REAL32*)pMUD_indVarDat->pData)[j];
	    pMUD_indVarHdr->low = _min( pMUD_indVarHdr->low, val );
	    pMUD_indVarHdr->high = _max( pMUD_indVarHdr->high, val );
	    sum += val;
	    sum2 += pow( val, 2.0 );
	    sum3 += pow( val, 3.0 );
	  }
	  mean = pMUD_indVarHdr->mean = sum/num;
	  stddev = pMUD_indVarHdr->stddev = ( num == 1 ) ? 0.0 :
                     sqrt( fabs( ( sum2 - pow( sum, 2.0 )/num )/( num - 1 ) ) );
	  pMUD_indVarHdr->skewness = ( stddev == 0.0 ) ? 0.0 :
              ( sum3 - 3.0*mean*sum2 + 
                 2.0*pow( sum, 3.0 )/pow( ((double)num), 2.0 ) )/
              ( ((double)num)*pow( stddev, 3.0 ) );

        }

	MUD_addToGroup( pMUD_indVarGrp, pMUD_indVarHdr );
	MUD_addToGroup( pMUD_indVarGrp, pMUD_indVarDat );
    }

    _free( pImusr_blk );

    return( SUCCESS );
}


