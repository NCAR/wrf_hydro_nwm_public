/* -------------------------------------------------------------------------- */
/* Macros for ESMF logging                                                    */
/* -------------------------------------------------------------------------- */
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT
#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)

!-------------------------------------------------------------------------------
! Define ESMF real kind to match Appplications single/double precision
!-------------------------------------------------------------------------------
#if defined(REAL4)
#define ESMF_KIND_FIELD ESMF_KIND_R4
#define ESMF_KIND_COORD ESMF_KIND_R4
#define ESMF_TYPEKIND_FIELD ESMF_TYPEKIND_R4
#define ESMF_TYPEKIND_COORD ESMF_TYPEKIND_R4
#elif defined(REAL8)
#define ESMF_KIND_FIELD ESMF_KIND_R8
#define ESMF_KIND_COORD ESMF_KIND_R8
#define ESMF_TYPEKIND_FIELD ESMF_TYPEKIND_R8
#define ESMF_TYPEKIND_COORD ESMF_TYPEKIND_R8
#else
#define ESMF_KIND_FIELD ESMF_KIND_R4
#define ESMF_KIND_COORD ESMF_KIND_R8
#define ESMF_TYPEKIND_FIELD ESMF_TYPEKIND_R4
#define ESMF_TYPEKIND_COORD ESMF_TYPEKIND_R8
#endif

/* -------------------------------------------------------------------------- */
/* Define real kind for data passed through ESMF interface                    */
/* -------------------------------------------------------------------------- */
#if defined(ESMF_R8)
#define _ESMF_KIND_RX _ESMF_KIND_R8
#define ESMF_KIND_RX ESMF_KIND_R8
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R8
#else
#define _ESMF_KIND_RX _ESMF_KIND_R4
#define ESMF_KIND_RX ESMF_KIND_R4
#define ESMF_TYPEKIND_RX ESMF_TYPEKIND_R4
#endif

!------------------------------------------------------------------------------
! Macros for debugging
!------------------------------------------------------------------------------
#define DEBUG_ESMF_IMPORT___disabled
#define DEBUG_ESMF_EXPORT___disabled

!-------------------------------------------------------------------------------
! Define Missing Value
!-------------------------------------------------------------------------------

#define MISSINGVALUE 999999
#define UNINITIALIZED -9999

!-------------------------------------------------------------------------------
! Define Output Levels
!-------------------------------------------------------------------------------

#define VERBOSITY_LV0 0
#define VERBOSITY_LV1 1
#define VERBOSITY_LV2 255
#define VERBOSITY_LV3 1023

/* -------------------------------------------------------------------------- */
/* Macros for ESMF logging                                                    */
/* -------------------------------------------------------------------------- */
#ifndef FILENAME
#define FILENAME __FILE__
#endif
#define CONTEXT  line=__LINE__,file=FILENAME
#define PASSTHRU msg=ESMF_LOGERR_PASSTHRU,CONTEXT


/* -------------------------------------------------------------------------- */
/* Define macros for mask values                                              */
/* -------------------------------------------------------------------------- */
#define MASK_INLAND_WATER -1
#define MASK_WATER         0
#define MASK_LAND          1
#define MASK_FROZEN_WATER  2
#define MASK_FROZEN_LAND   3

