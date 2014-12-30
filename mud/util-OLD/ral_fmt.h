
typedef struct {
/* i*2 */
/*   1 */      INT16 istfla;
/*   2 */      INT16 rescod;
/*   3 */      INT16 fill1[1];
/*   4 */      INT16 nrun;
/*   5-6 */    INT16 fill2[2];
/*   7-8 */    INT32  res_pscnds;
/*   9-14 */   INT16 fill3[6];
/*  15 */      INT16 histlen;
/*  16 */      INT16 numhis;
/*  17-64 */   INT16 fill4[48];
/*  65 */      INT16 numrec;
/*  66 */      INT16 lenrec;
/*  67 */      INT16 rgmode;
/*  68 */      INT16 nreads;
/*  69 */      INT16 nframes;
/*  70-89 */   char  rtitle[40];
/*  90-109 */  INT16 fill5[20];
/* 110-114 */  char  start_date[9];  
               char  fill6;
/* 115-119 */  char  stop_date[9];   
               char  fill7;
/* 120-123 */  char  start_time[8];
/* 124-127 */  char  stop_time[8];
/* 128-159 */  INT16 fill8[32];
/* 160-191 */  INT16 t0bin[32];
/* 192-223 */  INT16 tgood_begin[32];
/* 224-255 */  INT16 tgood_end[32];
/* 256-319 */  INT32  ntotal[32];
/* 320-430 */  INT16 fill9[111];
/* 431-461 */  char  comment[62];
/* 462-491 */  INT16 fill10[30];
/* 492-495 */  INT16 branch[4];
/* 496-499 */  INT16 crate[4];
/* 500-503 */  INT16 first[4];
/* 504-507 */  INT16 ndev[4];
/* 508-511 */  char  devtyp[2][4];  /* Error in READ_RAL ??? */
/* 512 */      INT16 ngroups;
} RAL_HEADER;

