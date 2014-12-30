c
c  mud.f77 - FORTRAN (77) include file (no structures)
c 
c
c    Copyright (C) 1994-2010 TRIUMF (Vancouver, Canada)
c    
c    Authors: T. Whidden, D. Arseneau, S. Daviel
c    
c    Released under the GNU LGPL - see http://www.gnu.org/license
c
c    This program is free software; you can distribute it and/or modify it under 
c    the terms of the Lesser GNU General Public License as published by the Free 
c    Software Foundation; either version 2 of the License, or any later version. 
c    Accordingly, this program is distributed in the hope that it will be useful, 
c    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
c    or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License 
c    for more details.
c
c  Revision history:
c   v1.00  07-Feb-1994  [T. Whidden] Initial version
c   v2.00  15-dec-1999  SD Add section for BNMR
c   v2.01  22-Apr-2003  DA Add fMUD_openInOut,fMUD_openReadWrite, "external"

c
c  Lab identifiers
c
        integer*4  MUD_LAB_ALL_ID
        parameter( MUD_LAB_ALL_ID=	Z'01000000' )
        integer*4  MUD_LAB_TRI_ID
        parameter( MUD_LAB_TRI_ID=	Z'02000000' )
        integer*4  MUD_LAB_RAL_ID
        parameter( MUD_LAB_RAL_ID=	Z'03000000' )
        integer*4  MUD_LAB_PSI_ID
        parameter( MUD_LAB_PSI_ID=	Z'04000000' )
c
c  Format identifiers
c
        integer*4  MUD_FMT_ALL_ID
        parameter( MUD_FMT_ALL_ID=	Z'00010000'+MUD_LAB_ALL_ID )
        integer*4  MUD_FMT_GEN_ID
        parameter( MUD_FMT_GEN_ID=	Z'00020000'+MUD_LAB_ALL_ID )
        integer*4  MUD_FMT_TRI_TD_ID
        parameter( MUD_FMT_TRI_TD_ID=	Z'00010000'+MUD_LAB_TRI_ID )
        integer*4  MUD_FMT_TRI_TI_ID
        parameter( MUD_FMT_TRI_TI_ID=	Z'00020000'+MUD_LAB_TRI_ID )
        integer*4  MUD_FMT_TRI_BNMR_ID
        parameter( MUD_FMT_TRI_BNMR_ID=	Z'00030000'+MUD_LAB_TRI_ID )
c
c  ALL Format identifiers
c
        integer*4  MUD_SEC_ID
        parameter( MUD_SEC_ID=		Z'00000001'+MUD_FMT_ALL_ID )
        integer*4  MUD_SEC_FIXED_ID
        parameter( MUD_SEC_FIXED_ID=	Z'00000002'+MUD_FMT_ALL_ID )
        integer*4  MUD_SEC_GRP_ID
        parameter( MUD_SEC_GRP_ID=	Z'00000003'+MUD_FMT_ALL_ID )
        integer*4  MUD_SEC_EOF_ID
        parameter( MUD_SEC_EOF_ID=	Z'00000004'+MUD_FMT_ALL_ID )
        integer*4  MUD_SEC_CMT_ID
        parameter( MUD_SEC_CMT_ID=	Z'00000005'+MUD_FMT_ALL_ID )
    
        integer*4  MUD_GRP_CMT_ID
        parameter( MUD_GRP_CMT_ID=	Z'00000005'+MUD_FMT_ALL_ID )
c
c  GEN Format identifiers
c
        integer*4  MUD_SEC_GEN_RUN_DESC_ID
        parameter( MUD_SEC_GEN_RUN_DESC_ID= Z'00000001'+MUD_FMT_GEN_ID )
        integer*4  MUD_SEC_GEN_HIST_HDR_ID
        parameter( MUD_SEC_GEN_HIST_HDR_ID= Z'00000002'+MUD_FMT_GEN_ID )
        integer*4  MUD_SEC_GEN_HIST_DAT_ID
        parameter( MUD_SEC_GEN_HIST_DAT_ID= Z'00000003'+MUD_FMT_GEN_ID )
        integer*4  MUD_SEC_GEN_SCALER_ID
        parameter( MUD_SEC_GEN_SCALER_ID=   Z'00000004'+MUD_FMT_GEN_ID )
        integer*4  MUD_SEC_GEN_IND_VAR_ID
        parameter( MUD_SEC_GEN_IND_VAR_ID=  Z'00000005'+MUD_FMT_GEN_ID )
        integer*4  MUD_SEC_GEN_ARRAY_ID
        parameter( MUD_SEC_GEN_ARRAY_ID=
     +                                      Z'00000007'+MUD_FMT_GEN_ID )

        integer*4  MUD_GRP_GEN_HIST_ID
        parameter( MUD_GRP_GEN_HIST_ID=     Z'00000002'+MUD_FMT_GEN_ID )
        integer*4  MUD_GRP_GEN_SCALER_ID
        parameter( MUD_GRP_GEN_SCALER_ID=   Z'00000004'+MUD_FMT_GEN_ID )
        integer*4  MUD_GRP_GEN_IND_VAR_ID
        parameter( MUD_GRP_GEN_IND_VAR_ID=  Z'00000005'+MUD_FMT_GEN_ID )
        integer*4  MUD_GRP_GEN_IND_VAR_ARR_ID
        parameter( MUD_GRP_GEN_IND_VAR_ARR_ID=
     +                                      Z'00000006'+MUD_FMT_GEN_ID )

c TRI B-NMR Format identifiers
c
        integer*4  MUD_SEC_BNMR_RUN_DESC_ID
        parameter( MUD_SEC_BNMR_RUN_DESC_ID   =
     +		   MUD_FMT_TRI_BNMR_ID+Z'00000001' )

c
c  TRI_TD Format identifiers
c
        integer*4  MUD_SEC_TRI_TD_HIST_ID
        parameter( MUD_SEC_TRI_TD_HIST_ID   =
     +		   MUD_FMT_TRI_TD_ID+Z'00000002' )

        integer*4  MUD_GRP_TRI_TD_HIST_ID
        parameter( MUD_GRP_TRI_TD_HIST_ID   =
     +		   MUD_FMT_TRI_TD_ID+Z'00000002' )
        integer*4  MUD_GRP_TRI_TD_SCALER_ID
        parameter( MUD_GRP_TRI_TD_SCALER_ID =
     +		   MUD_FMT_TRI_TD_ID+Z'00000004' )
c
c  TRI_TI Format identifiers
c
        integer*4  MUD_SEC_TRI_TI_RUN_DESC_ID
        parameter( MUD_SEC_TRI_TI_RUN_DESC_ID =
     +		   MUD_FMT_TRI_TI_ID+Z'00000001' )
        integer*4  MUD_SEC_TRI_TI_HIST_ID
        parameter( MUD_SEC_TRI_TI_HIST_ID     =
     +		   MUD_FMT_TRI_TI_ID+Z'00000002' )

        integer*4  MUD_GRP_TRI_TI_HIST_ID
        parameter( MUD_GRP_TRI_TI_HIST_ID     =
     +		   MUD_FMT_TRI_TI_ID+Z'00000002' )
   



	integer*4 fMUD_openInput
	integer*4 fMUD_openOutput
	integer*4 fMUD_openInOut
	integer*4 fMUD_writeBegin
	integer*4 fMUD_writeEnd
	integer*4 fMUD_write
	integer*4 fMUD_readFile
	integer*4 fMUD_read
	integer*4 fMUD_search
	integer*4 fMUD_fseek
	integer*4 fMUD_rewind

	external fMUD_openInput
	external fMUD_openOutput
	external fMUD_openInOut
	external fMUD_writeBegin
	external fMUD_writeEnd
	external fMUD_write
	external fMUD_readFile
	external fMUD_read
	external fMUD_search
	external fMUD_fseek
	external fMUD_rewind

c
c       mud_friendly
c
	integer*4 fMUD_openRead
	integer*4 fMUD_openWrite
	integer*4 fMUD_openReadWrite
	integer*4 fMUD_closeRead
	integer*4 fMUD_closeWrite

	integer*4 fMUD_getRunDesc
	integer*4 fMUD_getExptNumber
	integer*4 fMUD_getRunNumber
	integer*4 fMUD_getElapsedSec
	integer*4 fMUD_getTimeBegin
	integer*4 fMUD_getTimeEnd
	integer*4 fMUD_getTitle
	integer*4 fMUD_getLab
	integer*4 fMUD_getArea
	integer*4 fMUD_getMethod
	integer*4 fMUD_getApparatus
	integer*4 fMUD_getInsert
	integer*4 fMUD_getSample
	integer*4 fMUD_getOrient
	integer*4 fMUD_getDas
	integer*4 fMUD_getExperimenter
	integer*4 fMUD_getTemperature
	integer*4 fMUD_getField
	integer*4 fMUD_getSubtitle
	integer*4 fMUD_getComment1
	integer*4 fMUD_getComment2
	integer*4 fMUD_getComment3

	integer*4 fMUD_setRunDesc
	integer*4 fMUD_setExptNumber
	integer*4 fMUD_setRunNumber
	integer*4 fMUD_setElapsedSec
	integer*4 fMUD_setTimeBegin
	integer*4 fMUD_setTimeEnd
	integer*4 fMUD_setTitle
	integer*4 fMUD_setLab
	integer*4 fMUD_setArea
	integer*4 fMUD_setMethod
	integer*4 fMUD_setApparatus
	integer*4 fMUD_setInsert
	integer*4 fMUD_setSample
	integer*4 fMUD_setOrient
	integer*4 fMUD_setDas
	integer*4 fMUD_setExperimenter
	integer*4 fMUD_setTemperature
	integer*4 fMUD_setField
	integer*4 fMUD_setSubtitle
	integer*4 fMUD_setComment1
	integer*4 fMUD_setComment2
	integer*4 fMUD_setComment3

	integer*4 fMUD_getComments
	integer*4 fMUD_getCommentPrev
	integer*4 fMUD_getCommentNext
	integer*4 fMUD_getCommentTime
	integer*4 fMUD_getCommentAuthor
	integer*4 fMUD_getCommentTitle
	integer*4 fMUD_getCommentBody

	integer*4 fMUD_setComments
	integer*4 fMUD_setCommentPrev
	integer*4 fMUD_setCommentNext
	integer*4 fMUD_setCommentTime
	integer*4 fMUD_setCommentAuthor
	integer*4 fMUD_setCommentTitle
	integer*4 fMUD_setCommentBody

	integer*4 fMUD_getHists
	integer*4 fMUD_getHistType
	integer*4 fMUD_getHistNumBytes
	integer*4 fMUD_getHistNumBins
	integer*4 fMUD_getHistBytesPerBin
	integer*4 fMUD_getHistFsPerBin
	integer*4 fMUD_getHistSecondsPerBin
	integer*4 fMUD_getHistT0_Ps
	integer*4 fMUD_getHistT0_Bin
	integer*4 fMUD_getHistGoodBin1
	integer*4 fMUD_getHistGoodBin2
	integer*4 fMUD_getHistBkgd1
	integer*4 fMUD_getHistBkgd2
	integer*4 fMUD_getHistNumEvents
	integer*4 fMUD_getHistTitle
	integer*4 fMUD_getHistData
	integer*4 fMUD_getHistTimeData

	integer*4 fMUD_setHists
	integer*4 fMUD_setHistType
	integer*4 fMUD_setHistNumBytes
	integer*4 fMUD_setHistNumBins
	integer*4 fMUD_setHistBytesPerBin
	integer*4 fMUD_setHistFsPerBin
	integer*4 fMUD_setHistSecondsPerBin
	integer*4 fMUD_setHistT0_Ps
	integer*4 fMUD_setHistT0_Bin
	integer*4 fMUD_setHistGoodBin1
	integer*4 fMUD_setHistGoodBin2
	integer*4 fMUD_setHistBkgd1
	integer*4 fMUD_setHistBkgd2
	integer*4 fMUD_setHistNumEvents
	integer*4 fMUD_setHistTitle
	integer*4 fMUD_setHistData
	integer*4 fMUD_setHistTimeData

	integer*4 fMUD_pack
	integer*4 fMUD_unpack

	integer*4 fMUD_getScalers
	integer*4 fMUD_getScalerLabel
	integer*4 fMUD_getScalerCounts

	integer*4 fMUD_setScalers
	integer*4 fMUD_setScalerLabel
	integer*4 fMUD_setScalerCounts

	integer*4 fMUD_getIndVars
	integer*4 fMUD_getIndVarLow
	integer*4 fMUD_getIndVarHigh
	integer*4 fMUD_getIndVarMean
	integer*4 fMUD_getIndVarStddev
	integer*4 fMUD_getIndVarSkewness
	integer*4 fMUD_getIndVarName
	integer*4 fMUD_getIndVarDescription
	integer*4 fMUD_getIndVarUnits
	integer*4 fMUD_getIndVarNumData
	integer*4 fMUD_getIndVarElemSize
	integer*4 fMUD_getIndVarDataType
	integer*4 fMUD_getIndVarHasTime
	integer*4 fMUD_getIndVarData
	integer*4 fMUD_getIndVarTimeData

	integer*4 fMUD_setIndVars
	integer*4 fMUD_setIndVarLow
	integer*4 fMUD_setIndVarHigh
	integer*4 fMUD_setIndVarMean
	integer*4 fMUD_setIndVarStddev
	integer*4 fMUD_setIndVarSkewness
	integer*4 fMUD_setIndVarName
	integer*4 fMUD_setIndVarDescription
	integer*4 fMUD_setIndVarUnits
	integer*4 fMUD_setIndVarNumData
	integer*4 fMUD_setIndVarElemSize
	integer*4 fMUD_setIndVarDataType
	integer*4 fMUD_setIndVarHasTime
	integer*4 fMUD_setIndVarData
	integer*4 fMUD_setIndVarTimeData
!
!       Now declare all functions external
!
	external fMUD_openRead
	external fMUD_openWrite
	external fMUD_openReadWrite
	external fMUD_closeRead
	external fMUD_closeWrite

	external fMUD_getRunDesc
	external fMUD_getExptNumber
	external fMUD_getRunNumber
	external fMUD_getElapsedSec
	external fMUD_getTimeBegin
	external fMUD_getTimeEnd
	external fMUD_getTitle
	external fMUD_getLab
	external fMUD_getArea
	external fMUD_getMethod
	external fMUD_getApparatus
	external fMUD_getInsert
	external fMUD_getSample
	external fMUD_getOrient
	external fMUD_getDas
	external fMUD_getExperimenter
	external fMUD_getTemperature
	external fMUD_getField
	external fMUD_getSubtitle
	external fMUD_getComment1
	external fMUD_getComment2
	external fMUD_getComment3

	external fMUD_setRunDesc
	external fMUD_setExptNumber
	external fMUD_setRunNumber
	external fMUD_setElapsedSec
	external fMUD_setTimeBegin
	external fMUD_setTimeEnd
	external fMUD_setTitle
	external fMUD_setLab
	external fMUD_setArea
	external fMUD_setMethod
	external fMUD_setApparatus
	external fMUD_setInsert
	external fMUD_setSample
	external fMUD_setOrient
	external fMUD_setDas
	external fMUD_setExperimenter
	external fMUD_setTemperature
	external fMUD_setField
	external fMUD_setSubtitle
	external fMUD_setComment1
	external fMUD_setComment2
	external fMUD_setComment3

	external fMUD_getComments
	external fMUD_getCommentPrev
	external fMUD_getCommentNext
	external fMUD_getCommentTime
	external fMUD_getCommentAuthor
	external fMUD_getCommentTitle
	external fMUD_getCommentBody

	external fMUD_setComments
	external fMUD_setCommentPrev
	external fMUD_setCommentNext
	external fMUD_setCommentTime
	external fMUD_setCommentAuthor
	external fMUD_setCommentTitle
	external fMUD_setCommentBody

	external fMUD_getHists
	external fMUD_getHistType
	external fMUD_getHistNumBytes
	external fMUD_getHistNumBins
	external fMUD_getHistBytesPerBin
	external fMUD_getHistFsPerBin
	external fMUD_getHistSecondsPerBin
	external fMUD_getHistT0_Ps
	external fMUD_getHistT0_Bin
	external fMUD_getHistGoodBin1
	external fMUD_getHistGoodBin2
	external fMUD_getHistBkgd1
	external fMUD_getHistBkgd2
	external fMUD_getHistNumEvents
	external fMUD_getHistTitle
	external fMUD_getHistData
	external fMUD_getHistTimeData

	external fMUD_setHists
	external fMUD_setHistType
	external fMUD_setHistNumBytes
	external fMUD_setHistNumBins
	external fMUD_setHistBytesPerBin
	external fMUD_setHistFsPerBin
	external fMUD_setHistSecondsPerBin
	external fMUD_setHistT0_Ps
	external fMUD_setHistT0_Bin
	external fMUD_setHistGoodBin1
	external fMUD_setHistGoodBin2
	external fMUD_setHistBkgd1
	external fMUD_setHistBkgd2
	external fMUD_setHistNumEvents
	external fMUD_setHistTitle
	external fMUD_setHistData
	external fMUD_setHistTimeData

	external fMUD_pack
	external fMUD_unpack

	external fMUD_getScalers
	external fMUD_getScalerLabel
	external fMUD_getScalerCounts

	external fMUD_setScalers
	external fMUD_setScalerLabel
	external fMUD_setScalerCounts

	external fMUD_getIndVars
	external fMUD_getIndVarLow
	external fMUD_getIndVarHigh
	external fMUD_getIndVarMean
	external fMUD_getIndVarStddev
	external fMUD_getIndVarSkewness
	external fMUD_getIndVarName
	external fMUD_getIndVarDescription
	external fMUD_getIndVarUnits
	external fMUD_getIndVarNumData
	external fMUD_getIndVarElemSize
	external fMUD_getIndVarDataType
	external fMUD_getIndVarHasTime
	external fMUD_getIndVarData
	external fMUD_getIndVarTimeData

	external fMUD_setIndVars
	external fMUD_setIndVarLow
	external fMUD_setIndVarHigh
	external fMUD_setIndVarMean
	external fMUD_setIndVarStddev
	external fMUD_setIndVarSkewness
	external fMUD_setIndVarName
	external fMUD_setIndVarDescription
	external fMUD_setIndVarUnits
	external fMUD_setIndVarNumData
	external fMUD_setIndVarElemSize
	external fMUD_setIndVarDataType
	external fMUD_setIndVarHasTime
	external fMUD_setIndVarData
	external fMUD_setIndVarTimeData

