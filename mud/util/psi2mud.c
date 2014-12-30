/*
    You may use or modify this code for any use as long as this entire
    notice is retained:

    psi2mud.c: 
      This file provides functions to read time-differential muSR data
    files in the .BIN format of the Paul Scherrer Institute (PSI),
    convert the contents to  Triumf MUD_UTIL internals and create a MUD
    data file using MUD library routines. This file and psi_fmt.h are
    parts of a modified version of the program MUD_UTIL from the TRIUMF
    laboratory. This modified version adds the command "psi2mud" and
    is known as MUDUTIL-GDM to distinguish it from the original and
    other modified versions.

    Copyright (C) 2002 Gerald D. Morris, gmorris@triumf.ca 
                                         (formerly gmorris@lanl.gov)
					 
    Version 1.0 2002.8.14 (Los Alamos National Lab unlimited release LA-CC 02-055)
    
    
    Released under the GNU LGPL - see http://www.gnu.org/licenses

      This SOFTWARE has been authored by an employee or employees of the
    University of California, operator of the Los Alamos National Laboratory
    under Contract No. W-7405-ENG-36 with the U.S. Department of Energy. The
    U.S. Government has rights to use, reproduce, and distribute this SOFTWARE.
    Neither the Government nor the University makes any warranty, express or
    implied, or assumes any liability or responsibility for the use of this
    SOFTWARE. If SOFTWARE is modified to produce derivative works, such modified
    SOFTWARE should be clearly marked, so as not to confuse it with the version
    available from LANL.
      Additionally, this program is free software; you can distribute it and/or
    modify it under the terms of the Lesser GNU General Public License as published
    by the Free Software Foundation; either version 2 of the License, or any later
    version. Accordingly, this program is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General
    Public License for more details.
*/


#include <stdio.h>
#include <math.h>
#include <string.h>
#include "mud_util.h"
#include "psi_fmt.h"


static int PSI2MUD_runDesc  _ANSI_ARGS_(( PSI_INFO_HDR *pPSI_info_hdr, MUD_SEC_GEN_RUN_DESC *pMUD_desc, 
                                        char* exptnumber, char* exptmember, char* exptarea ));
static int PSI2MUD_scalers  _ANSI_ARGS_(( PSI_INFO_HDR *pPSI_info_hdr , MUD_SEC_GRP *pMUD_scalGrp ));
static int PSI2MUD_hists  _ANSI_ARGS_(( FILE *fin , PSI_INFO_HDR *pPSI_info_hdr , MUD_SEC_GRP *pMUD_histGrp ));
static int PSI2MUD_hist  _ANSI_ARGS_(( PSI_INFO_HDR *pPSI_info_hdr, int nhist, int* PSI_data, 
                                    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr, MUD_SEC_GEN_HIST_DAT* pMUD_histDat ));

static UINT32 PSI2MUD_BinSizes[32] =
    {    78125,   156250,   312500,    625000,   1250000,   2500000,    5000000,   10000000,
      20000000, 40000000, 80000000, 160000000, 320000000, 640000000, 1280000000, 2560000000u,
            16,       17,       18,    390625,    781250,   1562500,    3125000,    6250000,
      12500000, 25000000, 50000000, 100000000, 200000000, 400000000,  800000000, 1600000000};

double promptgetval(double, char *);

int PSI2MUD_convert( inFile, outFile, exptnumber, exptmember, exptarea )
    char* inFile;
    char* outFile;
    char* exptnumber;
    char* exptmember;
    char* exptarea;
{
    int status;
    FILE* fin = NULL;
    FILE* fout = NULL;
    PSI_INFO_HDR PSI_info_hdr;
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

    PSI_readHdr( fin, &PSI_info_hdr );           // Get run information from a PSI header.

    status = PSI2MUD_runDesc( &PSI_info_hdr, pMUD_desc, exptnumber, exptmember, exptarea );
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

    status = PSI2MUD_scalers( &PSI_info_hdr, pMUD_scalGrp );
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

    status = PSI2MUD_hists( fin, &PSI_info_hdr, pMUD_histGrp );
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
    if( fin != NULL ) fclose( fin );            // fixed so it doesn't dump core w/ non-existent input files. -gdm
    if( fout != NULL ) fclose( fout );
    MUD_free( pMUD_desc );
    MUD_free( pMUD_scalGrp );
    MUD_free( pMUD_histGrp );
    MUD_free( pMUD_fileGrp );
    return( FAILURE );
}

/****************************************************************************/


int PSI_readHdr( fin, pPSI_info_hdr )
  FILE* fin;
  PSI_INFO_HDR* pPSI_info_hdr;
{
  char buf[1024];
  char tempString[256];
  char *p;
  int i, j, k, count, tbin0,tbin1,tbin2, tbintemp, modbins;
  int runnumber;
  float  fv;
  float *fp;
  unsigned int *ubuf;
  int verbosemode, editmode;                 // for diagnostics - touch these files to turn on mode.
  
  FILE *fpv;
  
  count = fread( buf, 1024, 1, fin );       // read the 1kB header
  if( count == 0 ) return( 0 );

  editmode=0;
  verbosemode=0;
  
  if( (fpv=fopen("editmode","r"))!=NULL){
      editmode=1;
      fclose(fpv);
  }
  if( (fpv=fopen("verbosemode","r"))!=NULL){
      verbosemode=1;
      fclose(fpv);
//      printf("\n\n *** MUDUTIL-GDM - 2002.8.14 - Send comments & bug reports to Gerald Morris gmorris@lanl.gov \n");
      printf("\n\n *** MUD_UTIL_GDM - 2005.6.20 - Send comments & bug reports to Gerald Morris gmorris@triumf.ca \n");
  }
    
  p = buf;
  bdecode_obj( p, pPSI_info_hdr->FMT_ID, 2 );
  strncpy( tempString, pPSI_info_hdr->FMT_ID, 2 );
  tempString[3]='\0';
  if(verbosemode == 1) printf("FMT_ID = %s \n", tempString);

  p = buf+2;
  bdecode_2( p, &pPSI_info_hdr->KDTRES ); 
  if(verbosemode == 1) printf("KDTRES = %i \n", pPSI_info_hdr->KDTRES);
  
  p = buf+4;
  bdecode_2( p, &pPSI_info_hdr->KDOFTI ); 
  if(verbosemode == 1) printf("KDOFTI= %i \n", pPSI_info_hdr->KDOFTI);

  p = buf+6;
  bdecode_2( p, &pPSI_info_hdr->NRUN );   
  runnumber = pPSI_info_hdr->NRUN;
  if(verbosemode == 1) printf("Nrun = %i \n", runnumber);

  p = buf+8;
  bdecode_obj( p, pPSI_info_hdr->PATCH, 16 ); 
  if(verbosemode == 1) printf("PATCH = %s \n", pPSI_info_hdr->PATCH);

  p = buf+28;
  bdecode_2( p, &pPSI_info_hdr->LENHIS );   
  if(verbosemode == 1) printf("LENHIS = %i \n", pPSI_info_hdr->LENHIS);

  p = buf+30;
  bdecode_2( p, &pPSI_info_hdr->NUMHIS );   
  if(verbosemode == 1) printf("NUMHIS = %i \n", pPSI_info_hdr->NUMHIS);

  p = buf+46;
  bdecode_obj( p, pPSI_info_hdr->NHM_B, 2 ); 

  p = buf+48;
  bdecode_2( p, &pPSI_info_hdr->IBR );
  if(verbosemode == 1) printf("Branch %i \n", pPSI_info_hdr->IBR );

  p = buf+50;
  bdecode_2( p, &pPSI_info_hdr->ICR );   
  if(verbosemode == 1) printf("CAMAC crate %i \n", pPSI_info_hdr->ICR );

  p = buf+52;
  bdecode_2( p, &pPSI_info_hdr->NTD );   
  if(verbosemode == 1) printf("TDC Station %i \n", pPSI_info_hdr->NTD );

  p = buf+54;
  bdecode_obj( p, pPSI_info_hdr->NHM_A, 2*1 ); 
  strncpy( tempString, pPSI_info_hdr->NHM_A, 2 );
  tempString[2]='\0';
  if(verbosemode == 1) printf("CAMAC station of 1st and 2nd histogram memories NHM_A = %s \n", tempString);

  p = buf+56;
  bdecode_obj( p, pPSI_info_hdr->HMTYPE, 3*1 ); 
  strncpy( tempString, pPSI_info_hdr->HMTYPE, 3 );
  tempString[3]='\0';
  if(verbosemode == 1) printf("HMTYPE = %s \n", tempString);

  p = buf+60;
  bdecode_obj( p, pPSI_info_hdr->MONDEV, 12*1 ); 
  strncpy( tempString, pPSI_info_hdr->MONDEV, 12 );
  tempString[11]='\0';
  if(verbosemode == 1) printf("MON_DEV = %s \n", tempString);

//  p = buf+72;
//  p = buf+88;

  p = buf+104;
  for(i=0; i<=3; i++){
		bdecode_4( p,  &(pPSI_info_hdr->MON_LST[i]) );
    if(verbosemode == 1) printf("T[%i] = %f K\n", i, pPSI_info_hdr->MON_LST[i] );
  }

  p = buf+128;
  bdecode_2( p, &pPSI_info_hdr->NUMDAF ); 
  if(verbosemode == 1) printf("NUMDAF = %i \n", pPSI_info_hdr->NUMDAF);

  p = buf+130;
  bdecode_2( p, &pPSI_info_hdr->LENDAF ); 
  if(verbosemode == 1) printf("LENDAF = %i \n", pPSI_info_hdr->LENDAF);

  p =buf+132;
  bdecode_2( p, &pPSI_info_hdr->KDAFHI ); 
  if(verbosemode == 1) printf("KDAFHI = %i \n", pPSI_info_hdr->KDAFHI);

  p =buf+134;
  bdecode_2( p, &pPSI_info_hdr->KHIDAF ); 
  if(verbosemode == 1) printf("KHIDAF = %i \n", pPSI_info_hdr->KHIDAF);

  p = buf+138;
  bdecode_obj( p, pPSI_info_hdr->TITLE, 40 ); 
  strncpy( tempString, pPSI_info_hdr->TITLE, 40 );
  tempString[39]='\0';
  if(verbosemode == 1) printf("RUN TITLE = %s \n", tempString);

  p = buf+178;
  bdecode_obj( p, pPSI_info_hdr->SETUP, 10 ); 
  strncpy( tempString, pPSI_info_hdr->SETUP, 10 );
  tempString[10]='\0';
  if(verbosemode == 1) printf("Setup = %s \n", tempString);

  p = buf+218;  
  bdecode_obj( p, pPSI_info_hdr->DATE1, 9);
  strncpy( tempString, pPSI_info_hdr->DATE1, 9);
  tempString[9]='\0';
  if(verbosemode == 1) printf("Date1 = %s \n", tempString);
  
  p = buf+227;  
  bdecode_obj( p, pPSI_info_hdr->DATE2, 9);
  strncpy( tempString, pPSI_info_hdr->DATE2, 9);
  tempString[9]='\0';
  if(verbosemode == 1) printf("Date2 = %s \n", tempString);

  p = buf+236;
  bdecode_obj( p, pPSI_info_hdr->TIME1, 8);
  strncpy( tempString, pPSI_info_hdr->TIME1, 8);
  tempString[8]='\0';
  if(verbosemode == 1) printf("Time1 = %s \n", tempString);

  p = buf+244;
  bdecode_obj( p, pPSI_info_hdr->TIME2, 8);
  strncpy( tempString, pPSI_info_hdr->TIME2, 8);
  tempString[8]='\0';
  if(verbosemode == 1) printf("Time2 = %s \n", tempString);

  p = buf+296;
  bdecode_obj( p, pPSI_info_hdr->CNTOLD, 16*4 );
  if(verbosemode == 1) printf("Totals per hist = ");
  if(verbosemode == 1) for(i=0; i<=15; i++) printf("%8li ", pPSI_info_hdr->CNTOLD[i]);
  if(verbosemode == 1) printf("\n");

  p = buf+360;
  bdecode_obj( p, pPSI_info_hdr->I4SCAL_B, 12*4 );
  if(verbosemode == 1) printf("I4SCAL_B =");
  if(verbosemode == 1) for(i=0; i<=11; i++) printf("%10li ", pPSI_info_hdr->I4SCAL_B[i]);
  if(verbosemode == 1) printf("\n");
 
  p = buf+424;
  bdecode_4( p, &pPSI_info_hdr->TOTOLD );
  if(verbosemode == 1) printf("TOTOLD = %li \n", pPSI_info_hdr->TOTOLD);  

  p = buf+458;
  bdecode_obj( p, pPSI_info_hdr->NT0, 16*2 );
  if(verbosemode == 1) printf("NT0 =");
  if(verbosemode == 1) for(i=0; i<=15; i++) printf("%6i ", pPSI_info_hdr->NT0[i]);
  if(verbosemode == 1) printf("\n");

  p = buf+490;
  bdecode_obj( p, pPSI_info_hdr->NTINI, 16*2 );
  if(verbosemode == 1) printf("NT1 =");
  if(verbosemode == 1) for(i=0; i<=15; i++) printf("%6i ", pPSI_info_hdr->NTINI[i]);
  if(verbosemode == 1) printf("\n");

  p = buf+522;
  bdecode_obj( p, pPSI_info_hdr->NTFIN, 16*2 );
  if(verbosemode == 1) printf("NT2 =");
  if(verbosemode == 1) for(i=0; i<=15; i++) printf("%6i ", pPSI_info_hdr->NTFIN[i]);
  if(verbosemode == 1) printf("\n");

/* NOTE  temporary tbin0,1,2 are needed below because PSI format stores short 
   ints,  not 32-bit int read by scanf.
*/
  
  modbins=0;                                      // flag set to 1 if any markers are changed.
  
  if(editmode){
       printf("Edit mode is ON.  Present values of NT0,1,2 :\n");
       printf("NT0:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NT0[i]);
       printf("\n");
       printf("NT1:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NTINI[i]);
       printf("\n");       
       printf("NT2:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NTFIN[i]);
       printf("\n");

  }
  
  for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++){
    tbin0 = pPSI_info_hdr->NT0[i];
    if(tbin0 == 0 || editmode){
      printf(" \n PSI Data file says NT0[%i] = %5i .", i+1, tbin0);
      tbintemp=tbin0;
      tbin0 = (int) promptgetval((double) tbin0, " Replace with ");
      pPSI_info_hdr->NT0[i] = tbin0;                    // implicit convert to short int
      if(tbin0!=tbintemp) modbins=1;
    }
    
    tbin1 = pPSI_info_hdr->NTINI[i];
    if(tbin1 == 0 || tbin1<=tbin0 || editmode){
      if( tbin1<=tbin0) printf("\n NT1 <= NT0 !!!");
      printf(" \n PSI Data file says NT1[%i] = %5i .", i+1, tbin1);
      tbintemp=tbin1;
      tbin1 = (int) promptgetval((double) tbin1, " Replace with ");
      pPSI_info_hdr->NTINI[i] = tbin1;
      if(tbin1!=tbintemp) modbins=1;
    }
    
    tbin2 = pPSI_info_hdr->NTFIN[i];
    if(tbin2 == 0 || tbin2<=tbin1 || editmode){
      if( tbin2<=tbin1) printf("\n NT2 <= NT1 !!!");
      printf(" \n PSI Data file says NT2[%i] = %5i .", i+1, tbin2);
      tbintemp=tbin2;
      tbin2 = (int) promptgetval((double) tbin2, " Replace with ");
      pPSI_info_hdr->NTFIN[i] = tbin2;
      if(tbin2!=tbintemp) modbins=1;
    }
  }
  
  if(modbins){
       printf("\nSome NT0,1,2's were changed. Output file will have:\n");
       printf("NT0:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NT0[i]);
       printf("\n");
       printf("NT1:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NTINI[i]);
       printf("\n");       
       printf("NT2:");
       for(i=0; i<=pPSI_info_hdr->NUMHIS -1; i++) printf("%5i ", pPSI_info_hdr->NTFIN[i]);
       printf("\n");

  }
  

  p = buf+554;
  bdecode_obj( p, pPSI_info_hdr->SCALA_B, 48 );
  if(verbosemode == 1) printf("Scalar labels (7-18): ");
  if(verbosemode == 1){
      for(i=0; i<=11; i++){
   			for(j=0; j<=3; j++) printf("%c", pPSI_info_hdr->SCALA_B[4*i+j] );
   			printf(", ");
      }
  }
  if(verbosemode == 1) printf("\n");
  
  p = buf+642;
  bdecode_obj( p, pPSI_info_hdr->SCTYPE, 5);
  strncpy( tempString, pPSI_info_hdr->SCTYPE, 5);
  tempString[5]='\0';
  if(verbosemode == 1) printf("Scalar type = %s \n", tempString);

  p = buf+648;
  bdecode_2( p, &pPSI_info_hdr->IFTYPE );
  if(verbosemode == 1) if(pPSI_info_hdr->IFTYPE == 6) printf("Type of CAMAC Interface = %i : SCI-2280 \n", pPSI_info_hdr->IFTYPE);
  if(verbosemode == 1) if(pPSI_info_hdr->IFTYPE == 9) printf("Type of CAMAC Interface = %i : CCP      \n", pPSI_info_hdr->IFTYPE);

  p = buf+650;
  bdecode_2( p, &pPSI_info_hdr->NIVG );
  if(verbosemode == 1) printf("NVIG = %i \n", pPSI_info_hdr->NIVG );

  p = buf+670;
  bdecode_obj( p, pPSI_info_hdr->I4SCAL_A, 6*4 );
  if(verbosemode == 1) printf("I4SCAL_A (1-6): ");
  if(verbosemode == 1) for(i=0; i<=5; i++) printf("%10li ", pPSI_info_hdr->I4SCAL_A[i]);
  if(verbosemode == 1) printf("\n");

  p= buf+694;
  bdecode_obj( p, pPSI_info_hdr->NSC, 3*2 );
  if(verbosemode == 1) printf("CAMAC station of the singles scalers NSC = ");
  if(verbosemode == 1) for(i=0; i<=2; i++) printf("%3li ", pPSI_info_hdr->NSC[i]);
  if(verbosemode == 1) printf("\n");

  p = buf+712;
  bdecode_4( p, &pPSI_info_hdr->MON_NV );
  if(verbosemode == 1) printf("Number of measurements used for TEMPER and TEMDEV = %i \n", pPSI_info_hdr->MON_NV);

  p = buf+716;
  for(i=0; i<=3; i++){
  	bdecode_4( p,  &(pPSI_info_hdr->TEMPER[i]) );
    p=p+4;
  }
  p = buf+738;
  for(i=0; i<=3; i++){
  	bdecode_4( p,  &(pPSI_info_hdr->TEMDEV[i]) );
    p=p+4;
  }
  if(verbosemode == 1){
    for(i=0; i<=3; i++){
    printf("Avg T[%i] = %f +/- %f K\n", i, pPSI_info_hdr->TEMPER[i], pPSI_info_hdr->TEMDEV[i] );
    }
  }
  
  p = buf+770;
  bdecode_2(p , &pPSI_info_hdr->NIO );
  if(verbosemode == 1) printf("CAMAC station of the IO506 = %i \n", pPSI_info_hdr->NIO );

//  p = buf+792;

  p = buf+860;
  bdecode_obj( p, pPSI_info_hdr->SUBTITLE, 62 );
  strncpy( tempString, pPSI_info_hdr->SUBTITLE, 62 );
  tempString[61]='\0';
  if(verbosemode == 1) printf("Run Subtitle = %s \n", tempString);

  p = buf+924;
  bdecode_obj( p, pPSI_info_hdr->SCALA_A, 24 );
  if(verbosemode == 1) printf("Scalar labels (1-6): ");
  if(verbosemode == 1) {
		for(i=0; i<=5; i++){
    	for(j=0; j<=3; j++) printf("%c", pPSI_info_hdr->SCALA_A[4*i+j] );
      printf(", ");
    }
  }
  if(verbosemode == 1) printf("\n");

  p = buf+948;
  bdecode_obj( p, pPSI_info_hdr->HISLA, 64 );
  if(verbosemode == 1) printf("Histogram labels : ");
  if(verbosemode == 1){ for(i=0; i<=15; i++){
   for(j=0; j<=3; j++) printf("%c", pPSI_info_hdr->HISLA[4*i+j] );
                      printf(", ");
   }
  }
  if(verbosemode == 1) printf("\n");

/* Get actual time per bin from data file, in microseconds. If non-zero this supercedes KDTRES */
  p = buf+1012;
  bdecode_4( p,  &(pPSI_info_hdr->BINWIX) );
  if(verbosemode == 1) {
		printf("Time per bin (BINWIX) = %E usec ",   pPSI_info_hdr->BINWIX );
//    if( pPSI_info_hdr->BINWIX == 0.0) printf("(time/bin is from TDC code KDTRES )");
//    if( pPSI_info_hdr->BINWIX != 0.0) printf("(supercedes TDC code KDTRES)" );
    printf(" \n");
  }
  return( count );
}

static int
PSI2MUD_runDesc( pPSI_info_hdr, pMUD_desc,
                 exptnumber, exptmember, exptarea )
    PSI_INFO_HDR* pPSI_info_hdr;
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

    pMUD_desc->runNumber = pPSI_info_hdr->NRUN;

    expt = strtol( exptnumber, &p, 0 );
    if( *p == 0 ) pMUD_desc->exptNumber = expt;

    pMUD_desc->experimenter = strdup( exptmember );

// Sort out times and dates, and compute elapsed run time: 
    mu_interp_time( &pMUD_desc->timeBegin, pPSI_info_hdr->DATE1, pPSI_info_hdr->TIME1 );
    mu_interp_time( &pMUD_desc->timeEnd, pPSI_info_hdr->DATE2, pPSI_info_hdr->TIME2 );
    pMUD_desc->elapsedSec =  pMUD_desc->timeEnd - pMUD_desc->timeBegin;

    pMUD_desc->method = strdup( "TD-µSR" );
    pMUD_desc->lab = strdup( "PSI" );
    pMUD_desc->das = strdup( "MODAS" );
    pMUD_desc->area = strdup( "PSI2MUD" );
    
    
    strncpy( tempString, &pPSI_info_hdr->TITLE[0], 40 );
    tempString[40] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->title = strdup( tempString );

    strncpy( tempString, &pPSI_info_hdr->TITLE[0], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->sample = strdup( tempString );

    strncpy( tempString, &pPSI_info_hdr->TITLE[10], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->temperature = strdup( tempString );

    strncpy( tempString, &pPSI_info_hdr->TITLE[20], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->field = strdup( tempString );

    strncpy( tempString, &pPSI_info_hdr->TITLE[30], 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->orient = strdup( tempString );

    strncpy( tempString, "         ", 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->apparatus = strdup( tempString );

    strncpy( tempString, "         ", 10 );
    tempString[10] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_desc->insert = strdup( tempString );

    return( SUCCESS );
}


static int
PSI2MUD_scalers( pPSI_info_hdr, pMUD_scalGrp )
    PSI_INFO_HDR* pPSI_info_hdr;
    MUD_SEC_GRP* pMUD_scalGrp;
{
    int status;
    char tempString[256];
    int i;
    MUD_SEC_GEN_SCALER* pMUD_scal;

    for( i = 0; i < 9  ; i++ )                 // note - PSI format allows for 18 scalars.
       {
        pMUD_scal = (MUD_SEC_GEN_SCALER*)MUD_new( MUD_SEC_GEN_SCALER_ID, i+1 );
        if( pMUD_scal == NULL ) 
        {
            fprintf( stderr, "failed to malloc pMUD_scal\n" );
            return( FAILURE );
        }

// PSI scalars are in two blocks:
        if(i<=5){
          pMUD_scal->counts[0] = pPSI_info_hdr->I4SCAL_A[i];
          strncpy( tempString, &pPSI_info_hdr->SCALA_A[4*i], 4 );
          tempString[4] = '\0';
          trimBlanks( tempString, tempString );
          pMUD_scal->label = strdup( tempString );

          MUD_addToGroup( pMUD_scalGrp, pMUD_scal );
        }
        if(i>=6){
          pMUD_scal->counts[0] = pPSI_info_hdr->I4SCAL_B[i-6] ;
          strncpy( tempString, &pPSI_info_hdr->SCALA_B[4*(i-6)], 4 );
          tempString[4] = '\0';
          trimBlanks( tempString, tempString );
          pMUD_scal->label = strdup( tempString );

          MUD_addToGroup( pMUD_scalGrp, pMUD_scal );
        }
    }
    return( SUCCESS );
}


static int
PSI2MUD_hists( fin, pPSI_info_hdr, pMUD_histGrp )
    FILE* fin;
    PSI_INFO_HDR* pPSI_info_hdr;
    MUD_SEC_GRP* pMUD_histGrp;
{
    int status;
    int i, j;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
    UINT16* ps;
    UINT16 s;
    int num;
    int size;
    int buf[257000];   // ok, not very elegant, but it works
    int nhist;

    for( i = 0; i <= pPSI_info_hdr->NUMHIS-1; i++ )
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

        size = pPSI_info_hdr->LENHIS * 2;
        pMUD_histDat->pData = (char*)zalloc( size+16 );
        if( pMUD_histDat->pData == NULL )
        {
            goto error;
        }

// Read one histogram:

//      fread( buf, pPSI_info_hdr->LENHIS *4, 1, fin );
        fread( buf, (pPSI_info_hdr->LENDAF * pPSI_info_hdr->KDAFHI)*4, 1, fin );
        status = PSI2MUD_hist( pPSI_info_hdr, i, buf, pMUD_histHdr, pMUD_histDat );
        
        if( _failure( status ) ) 
        {
            goto error;
        }
            
//        for( j = 0; j < pPSI_info_hdr->LENHIS; j++){
//           printf("%u %u %u \n", i, j, buf[j]);
//        }

        MUD_addToGroup( pMUD_histGrp, pMUD_histHdr );
        MUD_addToGroup( pMUD_histGrp, pMUD_histDat );
    }

    return( SUCCESS );
error:
//    free( pPSI_h_data );
    MUD_free( pMUD_histHdr );
    MUD_free( pMUD_histDat );
    return( FAILURE );
}


static int
PSI2MUD_hist( pPSI_info_hdr, nhist, PSI_data, pMUD_histHdr, pMUD_histDat )
    PSI_INFO_HDR *pPSI_info_hdr;
    int nhist;
    int *PSI_data;
    MUD_SEC_GEN_HIST_HDR* pMUD_histHdr;
    MUD_SEC_GEN_HIST_DAT* pMUD_histDat;
{
    int i, status;
    INT32 nt0, nt00, nt01, nmid, ncl;
    char tempString[32];
    unsigned int udata[257000];

    strncpy( tempString, &pPSI_info_hdr->HISLA[nhist*4], 4 );
    tempString[4] = '\0';
    trimBlanks( tempString, tempString );
    pMUD_histHdr->title = strdup( tempString );    

    pMUD_histHdr->histType = MUD_SEC_TRI_TD_HIST_ID;
    pMUD_histHdr->nEvents = (int) pPSI_info_hdr-> CNTOLD[nhist];
    pMUD_histHdr->nBins = pPSI_info_hdr->LENHIS;

// Figure out time per bin
    if( (pPSI_info_hdr->KDTRES >= 0) && (pPSI_info_hdr->KDTRES <= 31) && (pPSI_info_hdr->BINWIX == 0.0) ){
        pMUD_histHdr->fsPerBin = PSI2MUD_BinSizes[pPSI_info_hdr->KDTRES];
    }
    else     // if BINWIX!=0 then use it instead of KDTRES
    {
        pMUD_histHdr->fsPerBin = (unsigned)1000000000.0*pPSI_info_hdr->BINWIX;
    }
    
    pMUD_histHdr->bytesPerBin = 0;    // not sure why this works, but it does.

    nt0 = pPSI_info_hdr->NT0[nhist];

/*    if( nt0 == 0 )                    // if not defined, find NT0 with Ted's method from tri2mud.c
    {
        UINT16* h;
        for(i=0; i<= pPSI_info_hdr->LENHIS -1; i++){
           h[i] = (UINT16) PSI_data[i];              // convert 32-bit to 16-bit unsigned
        }
        nt0 = pPSI_info_hdr->LENHIS/2;
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
*/
    
    pMUD_histHdr->t0_bin = nt0;
    pMUD_histHdr->goodBin1 = pPSI_info_hdr->NTINI[nhist];
    pMUD_histHdr->goodBin2 = pPSI_info_hdr->NTFIN[nhist];
    pMUD_histHdr->t0_ps = 0.001*(pMUD_histHdr->fsPerBin)*( (double)nt0 - 0.5 );
    pMUD_histHdr->bkgd1 = 0;
    pMUD_histHdr->bkgd2 = 0;

    if( pMUD_histHdr->bkgd1 == 0 && pMUD_histHdr->bkgd2 == 0 && nt0 > 1 )
    {
        INT32 kmid, kb1, kb2;
        INT32 nb;
        double avg, diff, err, val;
        UINT16 h[257000];

        for(i=0; i<= pPSI_info_hdr->LENHIS -1; i++){
          h[i] = (UINT16) PSI_data[i];
        }
        kmid = (int) (0.8 * (float)nt0);
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
        if(kb1==-1){                              // could not find 5 sigma step at start of background range (probably few counts)
          for(i=0; i<=kmid; i++){                  // so simply find first non-zero bin and...
             if( h[i] != 0 ) break;
          }
          kb1= i;
        }
        kb1 = _min( kb1 + 5, kmid-1 );           // ... add 5 more bins just to be sure.

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

    for(i=0; i<= pPSI_info_hdr->LENHIS -1; i++){
       udata[i] = (UINT16) PSI_data[i];
//       if(PSI_data[i] >=65535) udata[i] = 65535;     // clip overflow
    }
#ifdef DEBUG
        printf( " Packing hist %ld, nBins=%ld\n", 
                            MUD_instanceID( pMUD_histHdr ), 
                            pMUD_histHdr->nBins );
#endif /* DEBUG */

    pMUD_histHdr->nBytes = MUD_SEC_GEN_HIST_pack( pMUD_histHdr->nBins, 4, udata,                                    
                    pMUD_histHdr->bytesPerBin, pMUD_histDat->pData );
    pMUD_histDat->nBytes = pMUD_histHdr->nBytes;

    return( SUCCESS );
}

/* Print out a string and present value of parameter x_old; return a new value or keep old value if
nothing entered.
*/
double promptgetval(double x_old, char prompt_str[])
{
    double x;
    char valstr[100];
    
    printf("%s[%g]: ", prompt_str, x_old);
    fgets(valstr,99,stdin);
    if (valstr[0] != '\n')
    {
      x=atof(valstr);
    }
    else x=x_old;
    
    return(x);
}
