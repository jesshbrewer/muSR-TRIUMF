
/* 
    psi_fmt.h:
      This file defines a structure for run header information of muSR data
    files in the .BIN format of the Paul Scherrer Institute (PSI).
    This file and psi2mud.c are parts of a modified version of the program
    MUD_UTIL from the TRIUMF laboratory. This modified version adds the
    command "psi2mud" and is known as MUDUTIL-GDM to distinguish it from
    the original and other modified versions.

    Gerald D. Morris, gmorris@triumf.ca

*/


typedef struct{                //      type  Byte offset  Purpose
     char      FMT_ID[2];    //      I*2     0          Flag identifying file format (= "1M"). See Note 5.1.
     INT16    KDTRES;       //      I*2     2          TDC time resolution code (0 - 15) if BINWIX is zero.
     INT16    KDOFTI;       //      I*2     4          TDC time overflow setting. The overflow occurs at
     INT16    NRUN;         //      I*2     6          Run Number.
     char      PATCH[16];    //      L*1     8          NIM/ECL patch routing for the counter telescopes
     INT16    LENHIS;       //      I*2    28          Length of a histogram (in bins).
     INT16    NUMHIS;       //      I*2    30          Number of histogram(s) in use (maximum = 16).
     char      NHM_B[2];     //      L*1    46          CAMAC station of 3rd and 4th histogram memories.
     INT16    IBR;          //      I*2    48          CAMAC branch.
     INT16    ICR;          //      I*2    50          CAMAC crate.
     INT16    NTD;          //      I*2    52          Left hand CAMAC station of TDC.
     char      NHM_A[2];     //      L*1    54          CAMAC station of 1st and 2nd histogram memories
     char      HMTYPE[3];    //      L*1    56          Type of histogram memory ("CES" or "LRS")
     char      MONDEV[12];   //      L*1    60          Type of temperature monitor (e.g. "KEITH_1992")
     REAL32   MON_LO[4];    //      R*4    72          "Temperature" lower limits.
     REAL32   MON_HI[4];    //      R*4    88          "Temperature" upper limits.
     REAL32   MON_LST[4];   //      R*4   104          Last "temperature" values read.
     INT16    NUMDAF;       //      I*2   128          Number of hist data records in file (NUMHIS*KDAFHI).
     INT16    LENDAF;       //      I*2   130          Length (in bins) of a hist data record ( < 4097).
     INT16    KDAFHI;       //      I*2   132          Number of histogram data records per histogram.
     INT16    KHIDAF;       //      I*2   134          Number of histograms per histogram record. For
     char      TITLE[40];    //      L*1   138          4*10 ASCII characters for Target/Temp./Field/Orient.
     char      SETUP[10];    //      L*1   178          Selected data acq mode (10 ASCII characters ).
     char      DATE1[9];     //      L*1   218          Date of run start (DD-MMM-YY).
     char      DATE2[9];     //      L*1   227          Date this file was written (DD-MMM-YY).
     char      TIME1[8];     //      L*1   236          Time of run start (HH:MM:SS)
     char      TIME2[8];     //      L*1   244          Time this file was written (HH:MM:SS).
     INT32    CNTOLD[16];   //      I*4   296          Number of events in each histogram.
     INT32    I4SCAL_B[12]; //      I*4   360          Contents of scalers 7 to 18 (including overflows).
     INT32    TOTOLD;       //      I*4   424          Total number of events in the histograms.
     INT16    NT0[16];      //      I*2   458          Zero time bin for each histogram.
     INT16    NTINI[16];    //      I*2   490          First good bin for each histogram.
     INT16    NTFIN[16];    //      I*2   522          Last  good bin for each histogram.
     char      SCALA_B[48];  //      L*1   554          Labels for scalers 7 to 18, 4 characters each.
     char      SCTYPE[5];    //      L*1   642          Type of singles' scaler ("S500 " or "S500A").
     INT16    IFTYPE;       //      I*2   648          Type of CAMAC Interface (6 = SCI-2280, 9 = CCP).
     INT16    NIVG;         //      I*2   650          Station number of camac interface.
     REAL32   DKSPER;       //      R*4   654          Period between disk saves during data acquisition.
     REAL32   MONPER;       //      R*4   658          Period between temperature monitor activations (was
     INT32    I4SCAL_A[6];  //      I*4   670          Contents of scalers 1 to 6 (including overflows).
     INT16    NSC[3];       //      I*2   694          CAMAC station of the singles' scalers.
     INT32    MON_NV;       //      I*4   712          Number of measurements used for TEMPER and TEMDEV.
     REAL32   TEMPER[4];    //      R*4   716          Mean temperatures (max. 4 sensors, undefined units).
     REAL32   TEMDEV[4];    //      R*4   738          Standard deviations of temperatures.
     INT16    NIO;          //      I*2   770          CAMAC station of the IO506.
     REAL32   REANT0[16];   //      R*4   792          Zero times. Supersede NT0 if non-zero
     char      SUBTITLE[62]; //      L*1   860          Run sub-title.
     char      SCALA_A[24];  //      L*1   924          Labels for scalers 1 to 6, 4 characters each.
     char      HISLA[64];    //      L*1   948          Labels for histograms,     4 characters each.
     REAL32   BINWIX;       //      R*4  1012          TDC res'n. Supersedes KDTRES if non-zero
} PSI_INFO_HDR;









