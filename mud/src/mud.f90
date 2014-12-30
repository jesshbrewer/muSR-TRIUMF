!
!  mud.f90 - Modern Fortran (90/95) type declarations for mud
! 
!
!    Copyright (C) 2001-2010 TRIUMF (Vancouver, Canada)
!    D. Arseneau
!    
!    Released under the GNU LGPL - see http://www.gnu.org/licenses
!
!    This program is free software; you can distribute it and/or modify it under 
!    the terms of the Lesser GNU General Public License as published by the Free 
!    Software Foundation; either version 2 of the License, or any later version. 
!    Accordingly, this program is distributed in the hope that it will be useful, 
!    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
!    or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License 
!    for more details.
!
!  Revision history:
!   v1.0  25-Sep-2001   DA  Initial version (based on mud.finc)
!   v1.01 22-Apr-2003   DA  Add fMUD_openInOut, fMUD_openReadWrite, "external"
!

	integer mf_i4,mf_r8
        parameter (mf_i4 = selected_int_kind(9)) 
        parameter (mf_r8 = selected_real_kind(15))
!
!  Lab identifiers
!
        integer(mf_i4)  MUD_LAB_ALL_ID
        parameter( MUD_LAB_ALL_ID=	Z'01000000' )
        integer(mf_i4)  MUD_LAB_TRI_ID
        parameter( MUD_LAB_TRI_ID=	Z'02000000' )
        integer(mf_i4)  MUD_LAB_RAL_ID
        parameter( MUD_LAB_RAL_ID=	Z'03000000' )
        integer(mf_i4)  MUD_LAB_PSI_ID
        parameter( MUD_LAB_PSI_ID=	Z'04000000' )
!
!  Format identifiers
!
        integer(mf_i4)  MUD_FMT_ALL_ID
        parameter( MUD_FMT_ALL_ID=	Z'00010000'+MUD_LAB_ALL_ID )
        integer(mf_i4)  MUD_FMT_GEN_ID
        parameter( MUD_FMT_GEN_ID=	Z'00020000'+MUD_LAB_ALL_ID )
        integer(mf_i4)  MUD_FMT_TRI_TD_ID
        parameter( MUD_FMT_TRI_TD_ID=	Z'00010000'+MUD_LAB_TRI_ID )
        integer(mf_i4)  MUD_FMT_TRI_TI_ID
        parameter( MUD_FMT_TRI_TI_ID=	Z'00020000'+MUD_LAB_TRI_ID )
!
!  ALL Format identifiers
!
        integer(mf_i4)  MUD_SEC_ID
        parameter( MUD_SEC_ID=		Z'00000001'+MUD_FMT_ALL_ID )
        integer(mf_i4)  MUD_SEC_FIXED_ID
        parameter( MUD_SEC_FIXED_ID=	Z'00000002'+MUD_FMT_ALL_ID )
        integer(mf_i4)  MUD_SEC_GRP_ID
        parameter( MUD_SEC_GRP_ID=	Z'00000003'+MUD_FMT_ALL_ID )
        integer(mf_i4)  MUD_SEC_EOF_ID
        parameter( MUD_SEC_EOF_ID=	Z'00000004'+MUD_FMT_ALL_ID )
        integer(mf_i4)  MUD_SEC_CMT_ID
        parameter( MUD_SEC_CMT_ID=	Z'00000005'+MUD_FMT_ALL_ID )
    
        integer(mf_i4)  MUD_GRP_CMT_ID
        parameter( MUD_GRP_CMT_ID=	Z'00000005'+MUD_FMT_ALL_ID )
!
!  GEN Format identifiers
!
        integer(mf_i4)  MUD_SEC_GEN_RUN_DESC_ID
        parameter( MUD_SEC_GEN_RUN_DESC_ID= Z'00000001'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_SEC_GEN_HIST_HDR_ID
        parameter( MUD_SEC_GEN_HIST_HDR_ID= Z'00000002'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_SEC_GEN_HIST_DAT_ID
        parameter( MUD_SEC_GEN_HIST_DAT_ID= Z'00000003'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_SEC_GEN_SCALER_ID
        parameter( MUD_SEC_GEN_SCALER_ID=   Z'00000004'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_SEC_GEN_IND_VAR_ID
        parameter( MUD_SEC_GEN_IND_VAR_ID=  Z'00000005'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_SEC_GEN_ARRAY_ID
        parameter( MUD_SEC_GEN_ARRAY_ID=
     +                                      Z'00000007'+MUD_FMT_GEN_ID )

        integer(mf_i4)  MUD_GRP_GEN_HIST_ID
        parameter( MUD_GRP_GEN_HIST_ID=     Z'00000002'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_GRP_GEN_SCALER_ID
        parameter( MUD_GRP_GEN_SCALER_ID=   Z'00000004'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_GRP_GEN_IND_VAR_ID
        parameter( MUD_GRP_GEN_IND_VAR_ID=  Z'00000005'+MUD_FMT_GEN_ID )
        integer(mf_i4)  MUD_GRP_GEN_IND_VAR_ARR_ID
        parameter( MUD_GRP_GEN_IND_VAR_ARR_ID=
     +                                      Z'00000006'+MUD_FMT_GEN_ID )
!
!  TRI_TD Format identifiers
!
        integer(mf_i4)  MUD_SEC_TRI_TD_HIST_ID
        parameter( MUD_SEC_TRI_TD_HIST_ID   =
     +		   MUD_FMT_TRI_TD_ID+Z'00000002' )

        integer(mf_i4)  MUD_GRP_TRI_TD_HIST_ID
        parameter( MUD_GRP_TRI_TD_HIST_ID   =
     +		   MUD_FMT_TRI_TD_ID+Z'00000002' )
        integer(mf_i4)  MUD_GRP_TRI_TD_SCALER_ID
        parameter( MUD_GRP_TRI_TD_SCALER_ID =
     +		   MUD_FMT_TRI_TD_ID+Z'00000004' )
!
!  TRI_TI Format identifiers
!
        integer(mf_i4)  MUD_SEC_TRI_TI_RUN_DESC_ID
        parameter( MUD_SEC_TRI_TI_RUN_DESC_ID =
     +		   MUD_FMT_TRI_TI_ID+Z'00000001' )
        integer(mf_i4)  MUD_SEC_TRI_TI_HIST_ID
        parameter( MUD_SEC_TRI_TI_HIST_ID     =
     +		   MUD_FMT_TRI_TI_ID+Z'00000002' )

        integer(mf_i4)  MUD_GRP_TRI_TI_HIST_ID
        parameter( MUD_GRP_TRI_TI_HIST_ID     =
     +		   MUD_FMT_TRI_TI_ID+Z'00000002' )
   

	integer(mf_i4) fMUD_openInput
	integer(mf_i4) fMUD_openOutput
	integer(mf_i4) fMUD_openInOut
	integer(mf_i4) fMUD_writeBegin
	integer(mf_i4) fMUD_writeEnd
	integer(mf_i4) fMUD_write
	integer(mf_i4) fMUD_readFile
	integer(mf_i4) fMUD_read
	integer(mf_i4) fMUD_search
	integer(mf_i4) fMUD_fseek
	integer(mf_i4) fMUD_rewind

!
!       mud_friendly
!
	integer(mf_i4) fMUD_openRead
	integer(mf_i4) fMUD_openWrite
	integer(mf_i4) fMUD_openReadWrite
	integer(mf_i4) fMUD_closeRead
	integer(mf_i4) fMUD_closeWrite

	integer(mf_i4) fMUD_getRunDesc
	integer(mf_i4) fMUD_getExptNumber
	integer(mf_i4) fMUD_getRunNumber
	integer(mf_i4) fMUD_getElapsedSec
	integer(mf_i4) fMUD_getTimeBegin
	integer(mf_i4) fMUD_getTimeEnd
	integer(mf_i4) fMUD_getTitle
	integer(mf_i4) fMUD_getLab
	integer(mf_i4) fMUD_getArea
	integer(mf_i4) fMUD_getMethod
	integer(mf_i4) fMUD_getApparatus
	integer(mf_i4) fMUD_getInsert
	integer(mf_i4) fMUD_getSample
	integer(mf_i4) fMUD_getOrient
	integer(mf_i4) fMUD_getDas
	integer(mf_i4) fMUD_getExperimenter
	integer(mf_i4) fMUD_getTemperature
	integer(mf_i4) fMUD_getField
	integer(mf_i4) fMUD_getSubtitle
	integer(mf_i4) fMUD_getComment1
	integer(mf_i4) fMUD_getComment2
	integer(mf_i4) fMUD_getComment3

	integer(mf_i4) fMUD_setRunDesc
	integer(mf_i4) fMUD_setExptNumber
	integer(mf_i4) fMUD_setRunNumber
	integer(mf_i4) fMUD_setElapsedSec
	integer(mf_i4) fMUD_setTimeBegin
	integer(mf_i4) fMUD_setTimeEnd
	integer(mf_i4) fMUD_setTitle
	integer(mf_i4) fMUD_setLab
	integer(mf_i4) fMUD_setArea
	integer(mf_i4) fMUD_setMethod
	integer(mf_i4) fMUD_setApparatus
	integer(mf_i4) fMUD_setInsert
	integer(mf_i4) fMUD_setSample
	integer(mf_i4) fMUD_setOrient
	integer(mf_i4) fMUD_setDas
	integer(mf_i4) fMUD_setExperimenter
	integer(mf_i4) fMUD_setTemperature
	integer(mf_i4) fMUD_setField
	integer(mf_i4) fMUD_setSubtitle
	integer(mf_i4) fMUD_setComment1
	integer(mf_i4) fMUD_setComment2
	integer(mf_i4) fMUD_setComment3

	integer(mf_i4) fMUD_getComments
	integer(mf_i4) fMUD_getCommentPrev
	integer(mf_i4) fMUD_getCommentNext
	integer(mf_i4) fMUD_getCommentTime
	integer(mf_i4) fMUD_getCommentAuthor
	integer(mf_i4) fMUD_getCommentTitle
	integer(mf_i4) fMUD_getCommentBody

	integer(mf_i4) fMUD_setComments
	integer(mf_i4) fMUD_setCommentPrev
	integer(mf_i4) fMUD_setCommentNext
	integer(mf_i4) fMUD_setCommentTime
	integer(mf_i4) fMUD_setCommentAuthor
	integer(mf_i4) fMUD_setCommentTitle
	integer(mf_i4) fMUD_setCommentBody

	integer(mf_i4) fMUD_getHists
	integer(mf_i4) fMUD_getHistType
	integer(mf_i4) fMUD_getHistNumBytes
	integer(mf_i4) fMUD_getHistNumBins
	integer(mf_i4) fMUD_getHistBytesPerBin
	integer(mf_i4) fMUD_getHistFsPerBin
	integer(mf_i4) fMUD_getHistSecondsPerBin
	integer(mf_i4) fMUD_getHistT0_Ps
	integer(mf_i4) fMUD_getHistT0_Bin
	integer(mf_i4) fMUD_getHistGoodBin1
	integer(mf_i4) fMUD_getHistGoodBin2
	integer(mf_i4) fMUD_getHistBkgd1
	integer(mf_i4) fMUD_getHistBkgd2
	integer(mf_i4) fMUD_getHistNumEvents
	integer(mf_i4) fMUD_getHistTitle
	integer(mf_i4) fMUD_getHistData
	integer(mf_i4) fMUD_getHistTimeData

	integer(mf_i4) fMUD_setHists
	integer(mf_i4) fMUD_setHistType
	integer(mf_i4) fMUD_setHistNumBytes
	integer(mf_i4) fMUD_setHistNumBins
	integer(mf_i4) fMUD_setHistBytesPerBin
	integer(mf_i4) fMUD_setHistFsPerBin
	integer(mf_i4) fMUD_setHistSecondsPerBin
	integer(mf_i4) fMUD_setHistT0_Ps
	integer(mf_i4) fMUD_setHistT0_Bin
	integer(mf_i4) fMUD_setHistGoodBin1
	integer(mf_i4) fMUD_setHistGoodBin2
	integer(mf_i4) fMUD_setHistBkgd1
	integer(mf_i4) fMUD_setHistBkgd2
	integer(mf_i4) fMUD_setHistNumEvents
	integer(mf_i4) fMUD_setHistTitle
	integer(mf_i4) fMUD_setHistData
	integer(mf_i4) fMUD_setHistTimeData

	integer(mf_i4) fMUD_pack
	integer(mf_i4) fMUD_unpack

	integer(mf_i4) fMUD_getScalers
	integer(mf_i4) fMUD_getScalerLabel
	integer(mf_i4) fMUD_getScalerCounts

	integer(mf_i4) fMUD_setScalers
	integer(mf_i4) fMUD_setScalerLabel
	integer(mf_i4) fMUD_setScalerCounts

	integer(mf_i4) fMUD_getIndVars
	integer(mf_i4) fMUD_getIndVarLow
	integer(mf_i4) fMUD_getIndVarHigh
	integer(mf_i4) fMUD_getIndVarMean
	integer(mf_i4) fMUD_getIndVarStddev
	integer(mf_i4) fMUD_getIndVarSkewness
	integer(mf_i4) fMUD_getIndVarName
	integer(mf_i4) fMUD_getIndVarDescription
	integer(mf_i4) fMUD_getIndVarUnits
	integer(mf_i4) fMUD_getIndVarNumData
	integer(mf_i4) fMUD_getIndVarElemSize
	integer(mf_i4) fMUD_getIndVarDataType
	integer(mf_i4) fMUD_getIndVarHasTime
	integer(mf_i4) fMUD_getIndVarData
	integer(mf_i4) fMUD_getIndVarTimeData

	integer(mf_i4) fMUD_setIndVars
	integer(mf_i4) fMUD_setIndVarLow
	integer(mf_i4) fMUD_setIndVarHigh
	integer(mf_i4) fMUD_setIndVarMean
	integer(mf_i4) fMUD_setIndVarStddev
	integer(mf_i4) fMUD_setIndVarSkewness
	integer(mf_i4) fMUD_setIndVarName
	integer(mf_i4) fMUD_setIndVarDescription
	integer(mf_i4) fMUD_setIndVarUnits
	integer(mf_i4) fMUD_setIndVarNumData
	integer(mf_i4) fMUD_setIndVarElemSize
	integer(mf_i4) fMUD_setIndVarDataType
	integer(mf_i4) fMUD_setIndVarHasTime
	integer(mf_i4) fMUD_setIndVarData
	integer(mf_i4) fMUD_setIndVarTimeData

!
!       Declare functions to be functions (external)
!
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

!
!       mud_friendly
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


!***********************************************************************
!*
!*       Derived types (structures) 
!*
!***********************************************************************
!*
!*  Use these to declare, for example,
!*
!*  type(MUD_SEC_GEN_RUN_DESC) :: header
!*
!*  and use as
!*
!*  header%runnumber
!*
!***********************************************************************

        type MUD_CORE
            sequence
            integer(mf_i4)	pNext		!* pointer to next section *
            integer(mf_i4)	size
            integer(mf_i4)	secID		!* Ident of section type *
            integer(mf_i4)	instanceID
            integer(mf_i4)	sizeof
            integer(mf_i4)	proc
        end type
        

        type MUD_INDEX
            sequence
            integer(mf_i4)	pNext		!* pointer to next section *
            integer(mf_i4)	offset
            integer(mf_i4)	secID		!* Ident of section type *
            integer(mf_i4)	instanceID
        end type
        

        type MUD_SEC
            sequence
	    type(MUD_CORE) core
        end type
        
        
        type MUD_SEC_FIXED
            sequence
	    type(MUD_CORE) core
        
            integer(mf_i4)	fileSize
            integer(mf_i4)	formatID
        end type
        
        
        type MUD_SEC_GROUP
            sequence
	    type(MUD_CORE) core
            
            integer(mf_i4)	num		!* number of group members *
            integer(mf_i4)	memSize		!* total size of group *
            integer(mf_i4)	pMemIndex	!* pointer to member index list *
            integer(mf_i4)	pMem		!* pointer to list of group members *
	    integer(mf_i4)   pos
	    integer(mf_i4)   pParent
        end type
        
        
        type MUD_SEC_EOF
            sequence
	    type(MUD_CORE) core
        end type
        

        type MUD_SEC_CMT
            sequence
	    type(MUD_CORE) core

	    integer(mf_i4)  ID
	    integer(mf_i4)  prevReplyID
	    integer(mf_i4)  nextReplyID
	    integer(mf_i4)  time
	    integer(mf_i4)  pcsAuthor
	    integer(mf_i4)  pcsTitle
	    integer(mf_i4)  pcsComment
        end type
        

        type MUD_SEC_GEN_RUN_DESC
            sequence
	    type(MUD_CORE) core

	    integer(mf_i4)	exptNumber
	    integer(mf_i4)	runNumber
	    integer(mf_i4)	timeBegin
	    integer(mf_i4)	timeEnd
	    integer(mf_i4)	elapsedSec
	    integer(mf_i4)	pcsTitle
	    integer(mf_i4)	pcsLab
	    integer(mf_i4)	pcsArea
	    integer(mf_i4)	pcsMethod
	    integer(mf_i4)	pcsApparatus
	    integer(mf_i4)	pcsInsert
	    integer(mf_i4)	pcsSample
	    integer(mf_i4)	pcsOrient
	    integer(mf_i4)	pcsDas
	    integer(mf_i4)	pcsExperimenter
	    integer(mf_i4)	pcsTemperature
	    integer(mf_i4)	pcsField
        end type
        
        
	! Generic histogram header type
        type MUD_SEC_GEN_HIST_HDR
            sequence
	    type(MUD_CORE) core
        
	    integer(mf_i4)	histType
	    integer(mf_i4)	nBytes
	    integer(mf_i4)	nBins
	    integer(mf_i4)	bytesPerBin
	    integer(mf_i4)	fsPerBin
	    integer(mf_i4)	t0_ps
	    integer(mf_i4)	t0_bin
	    integer(mf_i4)	goodBin1
	    integer(mf_i4)	goodBin2
	    integer(mf_i4)	bkgd1
	    integer(mf_i4)	bkgd2
	    integer(mf_i4)	nEvents
	    integer(mf_i4)	pcsTitle
        end type
        
        
	! Generic histogram header type
        type MUD_SEC_GEN_HIST_DAT
            sequence
	    type(MUD_CORE) core
        
	    integer(mf_i4)	nBytes
	    integer(mf_i4)	pData		  !* pointer to the histogram data *
        end type
        
        
        type MUD_SEC_GEN_SCALER
            sequence
	    type(MUD_CORE) core
        
	    integer(mf_i4)	counts(2)
	    integer(mf_i4)	pcsLabel
        end type
        
        
        type MUD_SEC_GEN_IND_VAR
            sequence
	    type(MUD_CORE) core
        
	    real(mf_r8)	low
	    real(mf_r8)	high
	    real(mf_r8)	mean
	    real(mf_r8)	stddev
	    real(mf_r8)	skewness
	    integer(mf_i4)	pcsName
	    integer(mf_i4)	pcsDescription
	    integer(mf_i4)	pcsUnits
	end type
	        
        
        type MUD_SEC_GEN_ARRAY
            sequence
	    type(MUD_CORE) core
        
	    integer(mf_i4)   num
	    integer(mf_i4)   elemSize
	    integer(mf_i4)   type
	    integer(mf_i4)   hasTime
	    integer(mf_i4)   pData
	    integer(mf_i4)   pTime
	end type
	        
        
        type MUD_SEC_TRI_TI_RUN_DESC
            sequence
            type(MUD_CORE) core

	    integer(mf_i4)	exptNumber
	    integer(mf_i4)	runNumber
	    integer(mf_i4)	timeBegin
	    integer(mf_i4)	timeEnd
	    integer(mf_i4)	elapsedSec
	    integer(mf_i4)	pcsTitle
	    integer(mf_i4)	pcsLab
	    integer(mf_i4)	pcsArea
	    integer(mf_i4)	pcsMethod
	    integer(mf_i4)	pcsApparatus
	    integer(mf_i4)	pcsInsert
	    integer(mf_i4)	pcsSample
	    integer(mf_i4)	pcsOrient
	    integer(mf_i4)	pcsDas
	    integer(mf_i4)	pcsExperimenter
	    integer(mf_i4)	pcsSubtitle
	    integer(mf_i4)	pcsComment1
	    integer(mf_i4)	pcsComment2
	    integer(mf_i4)	pcsComment3
        end type
        
