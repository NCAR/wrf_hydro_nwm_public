!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##################
      MODULE MODE_SNOW3L
!     ##################
!
!!****  *MODE_SNOW * - contains explicit snow (ISBA-ES) characteristics functions
!!                     for total liquid water holding capacity of a snow layer (m)
!!                     and the thermal heat capacity of a layer (J K-1 m-3)
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    direct calculation
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    Boone and Etchevers, J. HydroMeteor., 2001
!!
!!
!!    AUTHOR
!!    ------
!!	A. Boone       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        01/08/02
!!      V. Masson       01/2004  add snow grid computations
!!      V. Vionnet      06/2008 -Introduction of Crocus formulation to
!                       calculate maximum liquid water holding capacity
!!                              - New algorithm to compute snow grid :
!                       10 layers
!!                              - Routine to aggregate snow grain type
!                       from 2 layers
!!                              _ Routine to compute average grain
!                       type when snow depth< 3 cm.
!     S. Morin          02/2011 - Add routines for Crocus
!     A. Boone          02/2012 - Add optimization of do-loops.
!     C. Carmagnola     12/2012 - Add the case CSNOWMETAMO!='B92' in subroutine SNOW3LAVGRAIN and in function SNOW3LDIFTYP
!     M. Lafaysse       01/2013 - Remove SNOWCROWLIQMAX routines (not used)
!     M. Lafaysse       08/2013 - simplification of routine SNOW3LAVGRAIN (logical GDENDRITIC)
!     B. Decharme       07/2013 - SNOW3LGRID cleanning
!                                 New algorithm to compute snow grid for 6-L or 12-L
!     A. Boone          10/2014 - Added snow thermal conductivity routines
!     B. Decharme       01/2015 - Added optical snow grain size diameter
!----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
INTERFACE SNOW3LWLIQMAX
  MODULE PROCEDURE SNOW3LWLIQMAX_3D
  MODULE PROCEDURE SNOW3LWLIQMAX_2D
  MODULE PROCEDURE SNOW3LWLIQMAX_1D
END INTERFACE
!
INTERFACE SNOW3LHOLD
  MODULE PROCEDURE SNOW3LHOLD_3D
  MODULE PROCEDURE SNOW3LHOLD_2D
  MODULE PROCEDURE SNOW3LHOLD_1D
  MODULE PROCEDURE SNOW3LHOLD_0D
END INTERFACE
INTERFACE SNOWCROHOLD
  MODULE PROCEDURE SNOWCROHOLD_3D
  MODULE PROCEDURE SNOWCROHOLD_2D
  MODULE PROCEDURE SNOWCROHOLD_1D
  MODULE PROCEDURE SNOWCROHOLD_0D
END INTERFACE
!
INTERFACE SNOW3LSCAP
  MODULE PROCEDURE SNOW3LSCAP_3D
  MODULE PROCEDURE SNOW3LSCAP_2D
  MODULE PROCEDURE SNOW3LSCAP_1D
  MODULE PROCEDURE SNOW3LSCAP_0D
END INTERFACE
!
INTERFACE SNOW3L_MARBOUTY
  MODULE PROCEDURE SNOW3L_MARBOUTY
END INTERFACE
!
INTERFACE SNOW3LGRID
  MODULE PROCEDURE SNOW3LGRID_2D
  MODULE PROCEDURE SNOW3LGRID_1D
END INTERFACE
!
INTERFACE SNOW3LAGREG
  MODULE PROCEDURE SNOW3LAGREG
END INTERFACE
!
INTERFACE SNOW3LAVGRAIN
  MODULE PROCEDURE SNOW3LAVGRAIN
END INTERFACE
!
INTERFACE SNOW3LDIFTYP
  MODULE PROCEDURE SNOW3LDIFTYP
END INTERFACE
!
INTERFACE GET_MASS_HEAT
  MODULE PROCEDURE GET_MASS_HEAT
END INTERFACE
!
INTERFACE GET_DIAM
  MODULE PROCEDURE GET_DIAM
END INTERFACE
!
!
INTERFACE SNOW3LTHRM
  MODULE PROCEDURE SNOW3LTHRM
END INTERFACE
!
INTERFACE SNOW3LDOPT
  MODULE PROCEDURE SNOW3LDOPT_2D
  MODULE PROCEDURE SNOW3LDOPT_1D
  MODULE PROCEDURE SNOW3LDOPT_0D
END INTERFACE
!
INTERFACE SNOW3LALB
  MODULE PROCEDURE SNOW3LALB
END INTERFACE
!
!-------------------------------------------------------------------------------
 CONTAINS
!
!####################################################################
      FUNCTION SNOW3LWLIQMAX_3D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                                  :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_3D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:,:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:,:,:) = ZHOLDMAXR(:,:,:)*ZSNOWRHO(:,:,:)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_3D
!####################################################################
      FUNCTION SNOW3LWLIQMAX_2D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_2D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:,:) = ZHOLDMAXR(:,:)*ZSNOWRHO(:,:)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_2D
!####################################################################
      FUNCTION SNOW3LWLIQMAX_1D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                           XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_1D',0,ZHOOK_HANDLE)
ZSNOWRHO(:) = MIN(XRHOSMAX_ES, PSNOWRHO(:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:) = ZHOLDMAXR(:)*ZSNOWRHO(:)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_1D

!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOW3LHOLD_3D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                 :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_3D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:,:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:,:,:) = ZHOLDMAXR(:,:,:)*PSNOWDZ(:,:,:)*ZSNOWRHO(:,:,:)/XRHOLW
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_3D
!####################################################################
      FUNCTION SNOW3LHOLD_2D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_2D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:,:) = ZHOLDMAXR(:,:)*PSNOWDZ(:,:)*ZSNOWRHO(:,:)/XRHOLW
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_2D
!####################################################################
      FUNCTION SNOW3LHOLD_1D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,     &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                     :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_1D',0,ZHOOK_HANDLE)
ZSNOWRHO(:) = MIN(XRHOSMAX_ES, PSNOWRHO(:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*     &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:))/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:) = ZHOLDMAXR(:)*PSNOWDZ(:)*ZSNOWRHO(:)/XRHOLW
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_1D
!####################################################################
      FUNCTION SNOW3LHOLD_0D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,     &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)        :: PSNOWDZ, PSNOWRHO
!
REAL                    :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL                    :: ZHOLDMAXR, ZSNOWRHO
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_0D',0,ZHOOK_HANDLE)
ZSNOWRHO = MIN(XRHOSMAX_ES, PSNOWRHO)
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1) *     &
                             MAX(0.,XSNOWRHOHOLD-ZSNOWRHO)/XSNOWRHOHOLD
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX = ZHOLDMAXR*PSNOWDZ*ZSNOWRHO/XRHOLW
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_0D
!####################################################################
      FUNCTION SNOWCROHOLD_3D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
USE MODD_SNOW_PAR, ONLY : XPERCENTAGEPORE
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                 :: PSNOWDZ, PSNOWLIQ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWHOLDMAX
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus,
!taking into account the conversion between wet and dry density -
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_3D',0,ZHOOK_HANDLE)
PWHOLDMAX(:,:,:) = XPERCENTAGEPORE/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_3D
!####################################################################
      FUNCTION SNOWCROHOLD_2D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
USE MODD_SNOW_PAR, ONLY : XPERCENTAGEPORE
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWHOLDMAX
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus,
!taking into account the conversion between wet and dry density -
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_2D',0,ZHOOK_HANDLE)
PWHOLDMAX(:,:) = XPERCENTAGEPORE/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_2D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOWCROHOLD_1D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
USE MODD_SNOW_PAR, ONLY : XPERCENTAGEPORE
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                     :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: PWHOLDMAX
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus,
!taking into account the conversion between wet and dry density -
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_1D',0,ZHOOK_HANDLE)
PWHOLDMAX(:) = XPERCENTAGEPORE/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_1D
!####################################################################
      FUNCTION SNOWCROHOLD_0D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
USE MODD_SNOW_PAR, ONLY : XPERCENTAGEPORE
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)        :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL                    :: PWHOLDMAX
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus,
!taking into account the conversion between wet and dry density -
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_0D',0,ZHOOK_HANDLE)
PWHOLDMAX = XPERCENTAGEPORE/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_0D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOW3LSCAP_3D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                                  :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PSCAP
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_3D',0,ZHOOK_HANDLE)
PSCAP(:,:,:) = PSNOWRHO(:,:,:)*XCI      ! (J K-1 m-3)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_3D
!####################################################################
      FUNCTION SNOW3LSCAP_2D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PSCAP
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_2D',0,ZHOOK_HANDLE)
PSCAP(:,:) = PSNOWRHO(:,:)*XCI      ! (J K-1 m-3)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_2D
!####################################################################
      FUNCTION SNOW3LSCAP_1D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                   :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO))                  :: PSCAP
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_1D',0,ZHOOK_HANDLE)
PSCAP(:) = PSNOWRHO(:)*XCI      ! (J K-1 m-3)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_1D
!####################################################################
      FUNCTION SNOW3LSCAP_0D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)       :: PSNOWRHO
!
REAL                   :: PSCAP
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_0D',0,ZHOOK_HANDLE)
PSCAP = PSNOWRHO*XCI      ! (J K-1 m-3)
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_0D
!
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOW3L_MARBOUTY(PSNOWRHO,PSNOWTEMP,PGRADT) RESULT(PDANGL) 
!**** *ZDANGL* - CROISSANCE DES GRAINS NON DENDRITIQUES ET ANGULEUX .
!              - GROWTH RATES FOR NON DENDRITIC GRAINS WITH SPHERICITY=0 


!     OBJET.
!     ------

!**   INTERFACE.
!     ----------

!     *ZDANGL*(PST,PSRO,PGRADT)*

!        *PST*     -  TEMPERATURE DE LA STRATE DE NEIGE.
!        *PSRO*    -  MASSE VOLUMIQUE DE LA STRATE DE NEIGE.
!        *PGRADT*  -  GRADIENT DE TEMPERATURE AFFECTANT LA STRATE DE NEIGE.

!     METHODE.
!     --------
!     THE FUNCTION RETURN A VALUE BETWEEN 0 AND 1 WHICH IS USED IN THE DETERMINATION OF THE 
!     GROWTH RATE FOR THE CONSIDERED LAYER.
!     THIS VALUE (WITHOUT UNIT) IS THE PRODUCT OF 3 FUNCTIONS (WHICH HAVE THEIR SOLUTIONS BETWEEN 0 AND 1) :
!     F(TEMPERATURE) * H(DENSITY) * G(TEMPERATURE GRADIENT)

!     EXTERNES.
!     ---------

!     REFERENCES.
!     -----------
!        MARBOUTY D (1980) AN EXPERIMENTAL STUDY OF TEMPERATURE GRADIENT 
!                          METAMORPHISM J GLACIOLOGY

!     AUTEURS.
!     --------
!        DOMINIQUE MARBOUTY (FMARBO/GMARBO/HMARBO).

!     MODIFICATIONS.
!     --------------
!        08/95: YANNICK DANIELOU - CODAGE A LA NORME DOCTOR.
!        03/06: JM WILLEMET      - F90 AND SI UNITS
!        01/08: JM WILLEMET      - ERROR ON THE FORMULATION OF G FUNCTION (WITH GRADIENT) IS CORRECTED 

USE MODD_CSTS, ONLY : XTT
USE MODD_SNOW_METAMO
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!     DECLARATIONS.
!     -------------
!
REAL ,INTENT(IN) :: PSNOWTEMP, PSNOWRHO, PGRADT
!
REAL             :: PDANGL
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3L_MARBOUTY',0,ZHOOK_HANDLE)
!
PDANGL = 0.0
!
! INFLUENCE DE LA TEMPERATURE /TEMPERATURE INFLUENCE.
IF( PSNOWTEMP>=XTT-XVTANG1 ) THEN
  !
  IF ( PSNOWTEMP>=XTT-XVTANG2 ) THEN
    PDANGL = XVTANG4 + XVTANG5 * (XTT-PSNOWTEMP) / XVTANG6
  ELSEIF( PSNOWTEMP>=XTT-XVTANG3 ) THEN
    PDANGL = XVTANG7 - XVTANG8 * (XTT-XVTANG2-PSNOWTEMP) / XVTANG9
  ELSE
    PDANGL = XVTANGA - XVTANGB * (XTT-XVTANG3-PSNOWTEMP) / XVTANGC
  ENDIF
  !
  ! INFLUENCE DE LA MASSE VOLUMIQUE / DENSITY INFLUENCE.
  IF ( PSNOWRHO<=XVRANG1 ) THEN
    !
    IF ( PSNOWRHO>XVRANG2 ) THEN
      PDANGL = PDANGL * ( 1. - (PSNOWRHO-XVRANG2)/(XVRANG1-XVRANG2) )
    ENDIF
    !
    ! INFLUENCE DU GRADIENT DE TEMPERATURE / TEMPERATURE GRADIENT INFLUENCE.
    IF ( PGRADT<=XVGANG1 ) THEN
      !
      IF ( PGRADT<=XVGANG2 ) THEN
        PDANGL = PDANGL * XVGANG5 * (PGRADT-XVGANG6)/(XVGANG2-XVGANG6)
      ELSEIF( PGRADT<=XVGANG3 ) THEN
        PDANGL = PDANGL * ( XVGANG7 + XVGANG8 * (PGRADT-XVGANG2)/(XVGANG3-XVGANG2) )
      ELSEIF( PGRADT<=XVGANG4 )THEN
        PDANGL = PDANGL * ( XVGANG9 + XVGANGA * (PGRADT-XVGANG3)/(XVGANG4-XVGANG3) )
      ELSE
        PDANGL = PDANGL * ( XVGANGB + XVGANGC * (PGRADT-XVGANG4)/(XVGANG1-XVGANG4) )
      ENDIF
      !
    ENDIF
    !
  ELSE
    !
    PDANGL = 0.
    !
  ENDIF
  !
ELSE
  !
  PDANGL = 0.
  !
ENDIF
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3L_MARBOUTY',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3L_MARBOUTY
!
!####################################################################
!####################################################################
!####################################################################
!
      SUBROUTINE SNOW3LGRID_2D(PSNOWDZ,PSNOW,PSNOWDZ_OLD)
!
!!    PURPOSE
!!    -------
!     Once during each time step, update grid to maintain
!     grid proportions. Similar to approach of Lynch-Steiglitz,
!     1994, J. Clim., 7, 1842-1855. Corresponding mass and
!     heat adjustments made directly after the call to this
!     routine. 3 grid configurations:
!     1) for very thin snow, constant grid spacing
!     2) for intermediate thicknesses, highest resolution at soil/snow
!        interface and at the snow/atmosphere interface
!     3) for deep snow, vertical resoution finest at snow/atmosphere
!        interface (set to a constant value) and increases with snow depth.
!        Second layer can't be more than an order of magnitude thicker
!        than surface layer.
!
!
!USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XSNOWCRITD, XUNDEF
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:  ), INTENT(IN )           :: PSNOW
REAL, DIMENSION(:,:), INTENT(OUT)           :: PSNOWDZ
REAL, DIMENSION(:,:), INTENT(IN ), OPTIONAL :: PSNOWDZ_OLD
!
!*      0.1    declarations of local variables
!
INTEGER                           :: JJ, JI
!
INTEGER                           :: INLVLS, INI
!
REAL,     DIMENSION(SIZE(PSNOW))  :: ZWORK
!
LOGICAL , DIMENSION(SIZE(PSNOW))  :: GREGRID

! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF1  = (/0.25, 0.50, 0.25/)
REAL, PARAMETER, DIMENSION(2)     :: ZSGCOEF2  = (/0.05, 0.34/)
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF   = (/0.3, 0.4, 0.3/)
!
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: ZSNOWTRANS = 0.20                ! (m)
!
! Minimum snow depth by layer for 6-L or 12-L configuration :
!
REAL, PARAMETER                   ::  ZDZ1=0.01
REAL, PARAMETER                   ::  ZDZ2=0.05
REAL, PARAMETER                   ::  ZDZ3=0.15
REAL, PARAMETER                   ::  ZDZ4=0.50
REAL, PARAMETER                   ::  ZDZ5=1.00
REAL, PARAMETER                   ::  ZDZN0=0.02
REAL, PARAMETER                   ::  ZDZN1=0.1
REAL, PARAMETER                   ::  ZDZN2=0.5
REAL, PARAMETER                   ::  ZDZN3=1.0
!
REAL, PARAMETER                   ::  ZCOEF1 = 0.5
REAL, PARAMETER                   ::  ZCOEF2 = 1.5
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',0,ZHOOK_HANDLE)
!
INLVLS = SIZE(PSNOWDZ(:,:),2)
INI    = SIZE(PSNOWDZ(:,:),1)
!
ZWORK  (:) = 0.0
GREGRID(:) = .TRUE.
!
! 1. Calculate current grid for 3-layer (default) configuration):
! ---------------------------------------------------------------
! Based on formulation of Lynch-Stieglitz (1994)
! except for 3 modifications:
! i) smooth transition here at ZSNOWTRANS
! ii) constant ratio for very thin snow:
! iii) ratio of layer 2 to surface layer <= 10
!
IF(INLVLS == 1)THEN
!
  DO JI=1,INI
     PSNOWDZ(JI,1)  = PSNOW(JI)
  ENDDO
!
ELSEIF(INLVLS == 3)THEN
!
   WHERE(PSNOW <= XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,3) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,2) = PSNOW(:) - PSNOWDZ(:,1) - PSNOWDZ(:,3)
   END WHERE
!
   WHERE(PSNOW <= ZSNOWTRANS .AND. PSNOW > XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = PSNOW(:)*ZSGCOEF1(1)
      PSNOWDZ(:,2) = PSNOW(:)*ZSGCOEF1(2)
      PSNOWDZ(:,3) = PSNOW(:)*ZSGCOEF1(3)
   END WHERE
!
   WHERE(PSNOW > ZSNOWTRANS)
      PSNOWDZ(:,1) = ZSGCOEF2(1)
      PSNOWDZ(:,2) = (PSNOW(:)-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
!
! When using simple finite differences, limit the thickness
! factor between the top and 2nd layers to at most 10
!
      PSNOWDZ(:,2) = MIN(10*ZSGCOEF2(1),  PSNOWDZ(:,2))
      PSNOWDZ(:,3) = PSNOW(:) - PSNOWDZ(:,2) - PSNOWDZ(:,1)
   END WHERE
!
!
! 2. Calculate current grid for 6-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 6)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,6) < ZCOEF1 * MIN(ZDZN1,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,6) > ZCOEF2 * MIN(ZDZN1,PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,6) = MIN(ZDZN1,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:)     = PSNOW(:) - PSNOWDZ(:,1) - PSNOWDZ(:,2) - PSNOWDZ(:,6)
       PSNOWDZ(:,3) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,4) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,5) = ZWORK(:)*ZSGCOEF(3)
!      layer 3 tickness >= layer 2 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,3)-PSNOWDZ(:,2))
       PSNOWDZ(:,3)=PSNOWDZ(:,3)-ZWORK(:)
       PSNOWDZ(:,4)=PSNOWDZ(:,4)+ZWORK(:)
!      layer 5 tickness >= layer 6 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,5)-PSNOWDZ(:,6))
       PSNOWDZ(:,5)=PSNOWDZ(:,5)-ZWORK(:)
       PSNOWDZ(:,4)=PSNOWDZ(:,4)+ZWORK(:)
  ENDWHERE
!
! 3. Calculate current grid for 9-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 9)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,9) < ZCOEF1 * MIN(ZDZN0,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,9) > ZCOEF2 * MIN(ZDZN0,PSNOW(:)/INLVLS)
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3) = MIN(ZDZ3,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,9)= MIN(ZDZN0,PSNOW(:)/INLVLS)
       PSNOWDZ(:,8)= MIN(ZDZN1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,7)= MIN(ZDZN2,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:) = PSNOW(:) - PSNOWDZ(:, 1) - PSNOWDZ(:, 2) - PSNOWDZ(:, 3) &
                           - PSNOWDZ(:, 7) - PSNOWDZ(:, 8) - PSNOWDZ(:, 9)
       PSNOWDZ(:,4) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,5) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,6) = ZWORK(:)*ZSGCOEF(3)
!      layer 4 tickness >= layer 3 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,4)-PSNOWDZ(:,3))
       PSNOWDZ(:,4)=PSNOWDZ(:,4)-ZWORK(:)
       PSNOWDZ(:,5)=PSNOWDZ(:,5)+ZWORK(:)
!      layer 6 tickness >= layer 7 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,6)-PSNOWDZ(:,7))
       PSNOWDZ(:,6)=PSNOWDZ(:,6)-ZWORK(:)
       PSNOWDZ(:,5)=PSNOWDZ(:,5)+ZWORK(:)
  ENDWHERE
!
! 4. Calculate current grid for 12-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 12)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:, 1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:, 2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,12) < ZCOEF1 * MIN(ZDZN0,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,12) > ZCOEF2 * MIN(ZDZN0,PSNOW(:)/INLVLS) 
  ENDIF
!
  WHERE(GREGRID(:))
!      top layers
       PSNOWDZ(:,1) = MIN(ZDZ1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2) = MIN(ZDZ2,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3) = MIN(ZDZ3,PSNOW(:)/INLVLS)
       PSNOWDZ(:,4) = MIN(ZDZ4,PSNOW(:)/INLVLS)
       PSNOWDZ(:,5) = MIN(ZDZ5,PSNOW(:)/INLVLS)
!      last layers
       PSNOWDZ(:,12)= MIN(ZDZN0,PSNOW(:)/INLVLS)
       PSNOWDZ(:,11)= MIN(ZDZN1,PSNOW(:)/INLVLS)
       PSNOWDZ(:,10)= MIN(ZDZN2,PSNOW(:)/INLVLS)
       PSNOWDZ(:, 9)= MIN(ZDZN3,PSNOW(:)/INLVLS)
!      remaining snow for remaining layers
       ZWORK(:) = PSNOW(:) - PSNOWDZ(:, 1) - PSNOWDZ(:, 2) - PSNOWDZ(:, 3) &
                           - PSNOWDZ(:, 4) - PSNOWDZ(:, 5) - PSNOWDZ(:, 9) &
                           - PSNOWDZ(:,10) - PSNOWDZ(:,11) - PSNOWDZ(:,12)
       PSNOWDZ(:,6) = ZWORK(:)*ZSGCOEF(1)
       PSNOWDZ(:,7) = ZWORK(:)*ZSGCOEF(2)
       PSNOWDZ(:,8) = ZWORK(:)*ZSGCOEF(3)
!      layer 6 tickness >= layer 5 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,6)-PSNOWDZ(:,5))
       PSNOWDZ(:,6)=PSNOWDZ(:,6)-ZWORK(:)
       PSNOWDZ(:,7)=PSNOWDZ(:,7)+ZWORK(:)
!      layer 8 tickness >= layer 9 tickness
       ZWORK(:)=MIN(0.0,PSNOWDZ(:,8)-PSNOWDZ(:,9))
       PSNOWDZ(:,8)=PSNOWDZ(:,8)-ZWORK(:)
       PSNOWDZ(:,7)=PSNOWDZ(:,7)+ZWORK(:)
  ENDWHERE
!
! 4. Calculate other non-optimized grid :
! ---------------------------------------
!
ELSEIF(INLVLS<10.AND.INLVLS/=3.AND.INLVLS/=6.AND.INLVLS/=9) THEN
!
  DO JJ=1,INLVLS
     DO JI=1,INI
        PSNOWDZ(JI,JJ)  = PSNOW(JI)/INLVLS
     ENDDO
  ENDDO
!
  PSNOWDZ(:,INLVLS) = PSNOWDZ(:,INLVLS) + (PSNOWDZ(:,1) - MIN(0.05, PSNOWDZ(:,1)))
  PSNOWDZ(:,1)      = MIN(0.05, PSNOWDZ(:,1))
!
ELSE !(INLVLS>=10 and /=12)
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID(:) = PSNOWDZ_OLD(:,     1) < ZCOEF1 * MIN(ZDZ1         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     1) > ZCOEF2 * MIN(ZDZ1         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     2) < ZCOEF1 * MIN(ZDZ2         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,     2) > ZCOEF2 * MIN(ZDZ2         ,PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,INLVLS) < ZCOEF1 * MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS) .OR. &
               & PSNOWDZ_OLD(:,INLVLS) > ZCOEF2 * MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS) 
  ENDIF
!
  WHERE(GREGRID(:))
       PSNOWDZ(:,1     ) = MIN(ZDZ1         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,2     ) = MIN(ZDZ2         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,3     ) = MIN(ZDZ3         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,4     ) = MIN(ZDZ4         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,5     ) = MIN(ZDZ5         ,PSNOW(:)/INLVLS)
       PSNOWDZ(:,INLVLS) = MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS)
  ENDWHERE
!
  DO JJ=6,INLVLS-1,1
     DO JI=1,INI
        IF(GREGRID(JI))THEN
          ZWORK(JI) = PSNOWDZ(JI,1)+PSNOWDZ(JI,2)+PSNOWDZ(JI,3)+PSNOWDZ(JI,4)+PSNOWDZ(JI,5)
          PSNOWDZ(JI,JJ) = (PSNOW(JI)-ZWORK(JI)-PSNOWDZ(JI,INLVLS))/(INLVLS-6) 
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(PSNOW(JI)==XUNDEF)THEN
         PSNOWDZ(JI,JJ) = XUNDEF
      ELSEIF(.NOT.GREGRID(JI))THEN
         PSNOWDZ(JI,JJ)=PSNOWDZ_OLD(JI,JJ)
      ENDIF
   ENDDO
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LGRID_2D
!####################################################################
!####################################################################
!####################################################################
!
      SUBROUTINE SNOW3LGRID_1D(PSNOWDZ,PSNOW,PSNOWDZ_OLD)
!
!!    PURPOSE
!!    -------
!     Once during each time step, update grid to maintain
!     grid proportions. Similar to approach of Lynch-Steiglitz,
!     1994, J. Clim., 7, 1842-1855. Corresponding mass and
!     heat adjustments made directly after the call to this
!     routine. 3 grid configurations:
!     1) for very thin snow, constant grid spacing
!     2) for intermediate thicknesses, highest resolution at soil/snow
!        interface and at the snow/atmosphere interface
!     3) for deep snow, vertical resoution finest at snow/atmosphere
!        interface (set to a constant value) and increases with snow depth.
!        Second layer can't be more than an order of magnitude thicker
!        than surface layer.
!
!
!USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XSNOWCRITD, XUNDEF
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN )           :: PSNOW
REAL, DIMENSION(:), INTENT(OUT)           :: PSNOWDZ
REAL, DIMENSION(:), INTENT(IN ), OPTIONAL :: PSNOWDZ_OLD
!
!*      0.1    declarations of local variables
!
INTEGER JJ
!
INTEGER                           :: INLVLS
!
REAL                              :: ZWORK
!
! modif_EB pour maillage
LOGICAL                           :: GREGRID

! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF1  = (/0.25, 0.50, 0.25/)
REAL, PARAMETER, DIMENSION(2)     :: ZSGCOEF2  = (/0.05, 0.34/)
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF   = (/0.3, 0.4, 0.3/)
!
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: ZSNOWTRANS  = 0.20                ! (m)
!
! Minimum snow depth by layer for 6-L or 12-L configuration :
!
REAL, PARAMETER                   ::  ZDZ1=0.01
REAL, PARAMETER                   ::  ZDZ2=0.05
REAL, PARAMETER                   ::  ZDZ3=0.15
REAL, PARAMETER                   ::  ZDZ4=0.50
REAL, PARAMETER                   ::  ZDZ5=1.00
REAL, PARAMETER                   ::  ZDZN0=0.02
REAL, PARAMETER                   ::  ZDZN1=0.1
REAL, PARAMETER                   ::  ZDZN2=0.5
REAL, PARAMETER                   ::  ZDZN3=1.0
!
REAL, PARAMETER                   ::  ZCOEF1 = 0.5
REAL, PARAMETER                   ::  ZCOEF2 = 1.5
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_1D',0,ZHOOK_HANDLE)
!
INLVLS = SIZE(PSNOWDZ(:),1)
!
GREGRID = .TRUE.
!
! 1. Calculate current grid for 3-layer (default) configuration):
! ---------------------------------------------------------------
! Based on formulation of Lynch-Stieglitz (1994)
! except for 3 modifications:
! i) smooth transition here at ZSNOWTRANS
! ii) constant ratio for very thin snow:
! iii) ratio of layer 2 to surface layer <= 10
!
IF(INLVLS == 1)THEN
!
  PSNOWDZ(1) = PSNOW
!
ELSEIF(INLVLS == 3)THEN
!
   IF(PSNOW <= XSNOWCRITD+0.01)THEN
      PSNOWDZ(1) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(3) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(2) = PSNOW - PSNOWDZ(1) - PSNOWDZ(3)
   ENDIF
!
   IF(PSNOW <= ZSNOWTRANS .AND. PSNOW > XSNOWCRITD+0.01)THEN
      PSNOWDZ(1) = PSNOW*ZSGCOEF1(1)
      PSNOWDZ(2) = PSNOW*ZSGCOEF1(2)
      PSNOWDZ(3) = PSNOW*ZSGCOEF1(3)
   ENDIF
!
   IF(PSNOW > ZSNOWTRANS)THEN
      PSNOWDZ(1) = ZSGCOEF2(1)
      PSNOWDZ(2) = (PSNOW-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
!
! When using simple finite differences, limit the thickness
! factor between the top and 2nd layers to at most 10
!
      PSNOWDZ(2) = MIN(10*ZSGCOEF2(1),  PSNOWDZ(2))
      PSNOWDZ(3) = PSNOW - PSNOWDZ(2) - PSNOWDZ(1)
   END IF
!
!
! 2. Calculate current grid for 6-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 6)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID    = PSNOWDZ_OLD(1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(6) < ZCOEF1 * MIN(ZDZN1,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(6) > ZCOEF2 * MIN(ZDZN1,PSNOW/INLVLS)
  ENDIF
!
  IF(GREGRID)THEN
!      top layers
       PSNOWDZ(1) = MIN(ZDZ1,PSNOW/INLVLS)
       PSNOWDZ(2) = MIN(ZDZ2,PSNOW/INLVLS)
!      last layers
       PSNOWDZ(6) = MIN(ZDZN1,PSNOW/INLVLS)
!      remaining snow for remaining layers
       ZWORK      = PSNOW - PSNOWDZ(1) - PSNOWDZ(2) - PSNOWDZ(6)
       PSNOWDZ(3) = ZWORK*ZSGCOEF(1)
       PSNOWDZ(4) = ZWORK*ZSGCOEF(2)
       PSNOWDZ(5) = ZWORK*ZSGCOEF(3)
!      layer 3 tickness >= layer 2 tickness
       ZWORK=MIN(0.0,PSNOWDZ(3)-PSNOWDZ(2))
       PSNOWDZ(3)=PSNOWDZ(3)-ZWORK
       PSNOWDZ(4)=PSNOWDZ(4)+ZWORK
!      layer 5 tickness >= layer 6 tickness
       ZWORK=MIN(0.0,PSNOWDZ(5)-PSNOWDZ(6))
       PSNOWDZ(5)=PSNOWDZ(5)-ZWORK
       PSNOWDZ(4)=PSNOWDZ(4)+ZWORK
  ENDIF
!
! 3. Calculate current grid for 9-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 9)THEN
!
! critere a satisfaire pour remaillage
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID    = PSNOWDZ_OLD(1) < ZCOEF1 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(1) > ZCOEF2 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2) < ZCOEF1 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2) > ZCOEF2 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(9) < ZCOEF1 * MIN(ZDZN0,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(9) > ZCOEF2 * MIN(ZDZN0,PSNOW/INLVLS)
  ENDIF
!
  IF(GREGRID)THEN
!      top layers
       PSNOWDZ(1) = MIN(ZDZ1,PSNOW/INLVLS)
       PSNOWDZ(2) = MIN(ZDZ2,PSNOW/INLVLS)
       PSNOWDZ(3) = MIN(ZDZ3,PSNOW/INLVLS)
!      last layers
       PSNOWDZ(9)= MIN(ZDZN0,PSNOW/INLVLS)
       PSNOWDZ(8)= MIN(ZDZN1,PSNOW/INLVLS)
       PSNOWDZ(7)= MIN(ZDZN2,PSNOW/INLVLS)
!      remaining snow for remaining layers
       ZWORK = PSNOW - PSNOWDZ( 1) - PSNOWDZ( 2) - PSNOWDZ( 3) &
                     - PSNOWDZ( 7) - PSNOWDZ( 8) - PSNOWDZ( 9)
       PSNOWDZ(4) = ZWORK*ZSGCOEF(1)
       PSNOWDZ(5) = ZWORK*ZSGCOEF(2)
       PSNOWDZ(6) = ZWORK*ZSGCOEF(3)
!      layer 4 tickness >= layer 3 tickness
       ZWORK=MIN(0.0,PSNOWDZ(4)-PSNOWDZ(3))
       PSNOWDZ(4)=PSNOWDZ(4)-ZWORK
       PSNOWDZ(5)=PSNOWDZ(5)+ZWORK
!      layer 6 tickness >= layer 7 tickness
       ZWORK=MIN(0.0,PSNOWDZ(6)-PSNOWDZ(7))
       PSNOWDZ(6)=PSNOWDZ(6)-ZWORK
       PSNOWDZ(5)=PSNOWDZ(5)+ZWORK
  ENDIF
!
! 4. Calculate current grid for 12-layer :
! ---------------------------------------------------------------
!
ELSEIF(INLVLS == 12)THEN
!
! modif_EB pour maillage
! critere a satisfaire pour remaillage
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID    = PSNOWDZ_OLD(1)  < ZCOEF1 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(1)  > ZCOEF2 * MIN(ZDZ1 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2)  < ZCOEF1 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(2)  > ZCOEF2 * MIN(ZDZ2 ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(12) < ZCOEF1 * MIN(ZDZN0,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(12) > ZCOEF2 * MIN(ZDZN0,PSNOW/INLVLS)
  ENDIF
!
  IF (GREGRID)THEN
!    top layers
     PSNOWDZ(1) = MIN(ZDZ1,PSNOW/INLVLS)
     PSNOWDZ(2) = MIN(ZDZ2,PSNOW/INLVLS)
     PSNOWDZ(3) = MIN(ZDZ3,PSNOW/INLVLS)
     PSNOWDZ(4) = MIN(ZDZ4,PSNOW/INLVLS)
     PSNOWDZ(5) = MIN(ZDZ5,PSNOW/INLVLS)
!    last layers
     PSNOWDZ(12)= MIN(ZDZN0,PSNOW/INLVLS)
     PSNOWDZ(11)= MIN(ZDZN1,PSNOW/INLVLS)
     PSNOWDZ(10)= MIN(ZDZN2,PSNOW/INLVLS)
     PSNOWDZ( 9)= MIN(ZDZN3,PSNOW/INLVLS)
!    remaining snow for remaining layers
     ZWORK = PSNOW - PSNOWDZ( 1) - PSNOWDZ( 2) - PSNOWDZ( 3) &
                   - PSNOWDZ( 4) - PSNOWDZ( 5) - PSNOWDZ( 9) &
                   - PSNOWDZ(10) - PSNOWDZ(11) - PSNOWDZ(12)
     PSNOWDZ(6) = ZWORK*ZSGCOEF(1)
     PSNOWDZ(7) = ZWORK*ZSGCOEF(2)
     PSNOWDZ(8) = ZWORK*ZSGCOEF(3)
!    layer 6 tickness >= layer 5 tickness
     ZWORK=MIN(0.0,PSNOWDZ(6)-PSNOWDZ(5))
     PSNOWDZ(6)=PSNOWDZ(6)-ZWORK
     PSNOWDZ(7)=PSNOWDZ(7)+ZWORK
!    layer 8 tickness >= layer 9 tickness
     ZWORK=MIN(0.0,PSNOWDZ(8)-PSNOWDZ(9))
     PSNOWDZ(8)=PSNOWDZ(8)-ZWORK
     PSNOWDZ(7)=PSNOWDZ(7)+ZWORK
  ENDIF
!
! 4. Calculate other non-optimized grid to allow CROCUS PREP :
! ------------------------------------------------------------
!
ELSE IF(INLVLS<10.AND.INLVLS/=3.AND.INLVLS/=6.AND.INLVLS/=9) THEN
!
  DO JJ=1,INLVLS
     PSNOWDZ(JJ)  = PSNOW/INLVLS
  ENDDO
!
  PSNOWDZ(INLVLS) = PSNOWDZ(INLVLS) + (PSNOWDZ(1) - MIN(0.05, PSNOWDZ(1)))
  PSNOWDZ(1)      = MIN(0.05, PSNOWDZ(1))
!
ELSE
!
  IF(PRESENT(PSNOWDZ_OLD))THEN
    GREGRID    = PSNOWDZ_OLD(     1) < ZCOEF1 * MIN(ZDZ1      ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(     1) > ZCOEF2 * MIN(ZDZ1      ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(     2) < ZCOEF1 * MIN(ZDZ2      ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(     2) > ZCOEF2 * MIN(ZDZ2      ,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(INLVLS) < ZCOEF1 * MIN(0.05*PSNOW,PSNOW/INLVLS) .OR. &
               & PSNOWDZ_OLD(INLVLS) > ZCOEF2 * MIN(0.05*PSNOW,PSNOW/INLVLS) 
  ENDIF
!
  IF (GREGRID)THEN
     PSNOWDZ(     1) = MIN(ZDZ1      ,PSNOW/INLVLS)
     PSNOWDZ(     2) = MIN(ZDZ2      ,PSNOW/INLVLS)
     PSNOWDZ(     3) = MIN(ZDZ3      ,PSNOW/INLVLS)
     PSNOWDZ(     4) = MIN(ZDZ4      ,PSNOW/INLVLS)
     PSNOWDZ(     5) = MIN(ZDZ5      ,PSNOW/INLVLS)
     PSNOWDZ(INLVLS) = MIN(0.05*PSNOW,PSNOW/INLVLS)
     ZWORK = SUM(PSNOWDZ(1:5))
     DO JJ=6,INLVLS-1,1
        PSNOWDZ(JJ) = (PSNOW - ZWORK -PSNOWDZ(INLVLS))/(INLVLS-6)
     END DO
  ENDIF
!
ENDIF
!
DO JJ=1,INLVLS
  IF(PSNOW==XUNDEF)THEN
    PSNOWDZ(JJ) = XUNDEF
  ELSEIF(.NOT.GREGRID)THEN
    PSNOWDZ(JJ) = PSNOWDZ_OLD(JJ)
  ENDIF
END DO
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_1D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LGRID_1D
!
!###################################################################################
!###################################################################################
SUBROUTINE SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1, PSNOWGRAN2, &
                       PSNOWHIST,PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN,     &
                       KL1,KL2,PSNOWDDZ                        )
!
USE MODD_SNOW_METAMO
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!       0.1 declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWDDZ
!
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST
REAL, DIMENSION(:), INTENT(OUT) :: PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN                                              
!
INTEGER, INTENT(IN) :: KL1  ! Indice couche de reference (i)
INTEGER, INTENT(IN) :: KL2 ! Indice de la couche (i-1 ou i+1) dont une
                                ! partie est aggregee à la couche (i)
!
!       0.2 declaration of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWRHO
REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZDIAMD,ZDIAMV,ZSPHERD,ZSPHERV,&
                                     ZDIAMN,ZSPHERN,ZDENT
!
REAL :: ZDELTA, ZCOMP
!
INTEGER :: IDENT, IVIEU, IL
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG',0,ZHOOK_HANDLE)
!
IF( KL1<KL2 ) THEN
  ZDELTA = 0.0
  IL = KL1
ELSE
  ZDELTA = 1.0
  IL = KL2
ENDIF
!
! Mean Properties
!
!       1. History
!
IF ( PSNOWHIST(KL1)/=PSNOWHIST(KL2) ) THEN
  PSNOWHISTN(KL1) = 0.0
ENDIF
!
!       2. New grain types
!
!       2.1 Same grain type
!
IF (  PSNOWGRAN1(KL1)*PSNOWGRAN1(KL2)>0.0 .OR. &
    ( PSNOWGRAN1(KL1)==0.0 .AND. PSNOWGRAN1(KL2)>=0.0 ) .OR. &
    ( PSNOWGRAN1(KL2)==0.0 .AND. PSNOWGRAN1(KL1)>=0.0 ) ) THEN
  !
  !code original vincent          PSNOWGRAN1N(KL1)=(PSNOWGRAN1(KL1)*PSNOWRHO(KL1)&
  !code original vincent        *(PSNOWDZN(KL1)-(1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA*&
  !code original vincent        ABS(PSNOWDDZ(KL2)))+PSNOWGRAN1(KL2)*                   &
  !code original vincent        PSNOWRHO(KL2)*((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+        &   
  !code original vincent        ZDELTA*ABS(PSNOWDDZ(KL2))))/((PSNOWDZN(KL1)-(1.0-ZDELTA)&
  !code original vincent        *ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*        &
  !code original vincent        PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+        &   
  !code original vincent        ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))
  !code original vincent !
  !code original vincent          PSNOWGRAN2N(KL1)=(PSNOWGRAN2(KL1)*PSNOWRHO(KL1) &
  !code original vincent        *(PSNOWDZN(KL1)-(1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA* &
  !code original vincent        ABS(PSNOWDDZ(KL2)))+PSNOWGRAN2(KL2)*                   &
  !code original vincent        PSNOWRHO(KL2)*((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))        &
  !code original vincent        +ZDELTA*ABS(PSNOWDDZ(KL2))))/((PSNOWDZN(KL1)-(1.0-ZDELTA)&
  !code original vincent        *ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*        &
  !code original vincent        PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+        &   
  !code original vincent        ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))
  !
  !plm
  CALL GET_AGREG(KL1,KL2,PSNOWGRAN1(KL1),PSNOWGRAN1(KL2),PSNOWGRAN1N(KL1))
  !
  CALL GET_AGREG(KL1,KL2,PSNOWGRAN2(KL1),PSNOWGRAN2(KL2),PSNOWGRAN2N(KL1))
  !
  !plm
  !
ELSE
  !
  !       2.2 Different types
  !
  IF ( PSNOWGRAN1(KL1)<0.0 ) THEN
    IDENT = KL1
    IVIEU = KL2
  ELSE
    IDENT = KL2
    IVIEU = KL1
  ENDIF
  !
  ZDIAMD (KL1) = - PSNOWGRAN1(IDENT)/XGRAN * XDIAET + ( 1.0 + PSNOWGRAN1(IDENT)/XGRAN ) * &
                 ( PSNOWGRAN2(IDENT)/XGRAN * XDIAGF + ( 1.0 - PSNOWGRAN2(IDENT)/XGRAN ) * XDIAFP )
  !
  ZSPHERD(KL1) = PSNOWGRAN2(IDENT)/XGRAN
  ZDIAMV (KL1) = PSNOWGRAN2(IVIEU)
  ZSPHERV(KL1) = PSNOWGRAN1(IVIEU)/XGRAN
  !IF(KL1==1)THEN
  !write(*,*) 'ZDD1',ZDIAMD(1),'ZSD1',ZSPHERD(1)
  !write(*,*) 'ZDV1',ZDIAMV(1),'ZSV1',ZSPHERV(1)
  !ENDIF
  !
  IF ( IDENT==KL1 ) THEN
    !code original vincent        ZDIAMN(KL1)= (ZDIAMD(KL1)*PSNOWRHO(IDENT)*&
    !code original vincent            (PSNOWDZN(IDENT)-(1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA*      &
    !code original vincent            ABS(PSNOWDDZ(KL2)))+ZDIAMV(KL1)*PSNOWRHO(IVIEU)*(       &
    !code original vincent            (1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2))))/&
    !code original vincent            ((PSNOWDZN(KL1)-(1.0-ZDELTA)*                                    &
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*            &
    !code original vincent            PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+          &   
    !code original vincent            ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))
    !
    !plm
    CALL GET_AGREG(IDENT,IVIEU,ZDIAMD(KL1),ZDIAMV(KL1),ZDIAMN(KL1))
    !
    !plm
    !
    !code original vincent        ZSPHERN(KL1)= (ZSPHERD(KL1)*PSNOWRHO(IDENT)*&
    !code original vincent            (PSNOWDZN(IDENT)-(1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA*      &
    !code original vincent            ABS(PSNOWDDZ(KL2)))+ZSPHERV(KL1)*PSNOWRHO(IVIEU)*(       &
    !code original vincent            (1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2))))/&
    !code original vincent            ((PSNOWDZN(KL1)-(1.0-ZDELTA)*                                    &
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*            &
    !code original vincent            PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+          &   
    !code original vincent            ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))

    !plm
    CALL GET_AGREG(IDENT,IVIEU,ZSPHERD(KL1),ZSPHERV(KL1),ZSPHERN(KL1))   
    !plm
    !
  ELSE
    !code original vincent        ZDIAMN(KL1)= (ZDIAMD(KL1)*PSNOWRHO(IDENT)*&
    !code original vincent            ((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+ZDELTA*ABS(PSNOWDDZ(KL2)))&
    !code original vincent            +ZDIAMV(KL1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-(1.0-ZDELTA)*  & 
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2))))/&
    !code original vincent            ((PSNOWDZN(KL1)-(1.0-ZDELTA)*                          &
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*    &
    !code original vincent            PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+   &
    !code original vincent            ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))
    !code original vincent!
    !code original vincent         ZSPHERN(KL1)= (ZSPHERD(KL1)*PSNOWRHO(IDENT)*&
    !code original vincent            ((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+ZDELTA*ABS(PSNOWDDZ(KL2)))&
    !code original vincent            +ZSPHERV(KL1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-(1.0-ZDELTA)* & 
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2))))/&
    !code original vincent            ((PSNOWDZN(KL1)-(1.0-ZDELTA)*                                    &
    !code original vincent            ABS(PSNOWDDZ(KL1))-ZDELTA*ABS(PSNOWDDZ(KL2)))*            &
    !code original vincent            PSNOWRHO(KL1)+((1.0-ZDELTA)*ABS(PSNOWDDZ(KL1))+          &   
    !code original vincent            ZDELTA*ABS(PSNOWDDZ(KL2)))*PSNOWRHO(KL2))
    !plm
    !
    CALL GET_AGREG(IVIEU,IDENT,ZDIAMV(KL1),ZDIAMD(KL1),ZDIAMN(KL1))
    !
    CALL GET_AGREG(IVIEU,IDENT,ZSPHERV(KL1),ZSPHERD(KL1),ZSPHERN(KL1))
    !plm
    !
  ENDIF
  !
  ZCOMP = ZSPHERN(KL1) * XDIAGF + ( 1.-ZSPHERN(KL1) ) * XDIAFP
  IF( ZDIAMN(KL1) < ZCOMP ) THEN
    !
    ZDENT(KL1) = ( ZDIAMN(KL1) - ZCOMP ) / ( XDIAET - ZCOMP )
    !IF(KL1==1) write(*,*) 'ZDENT',ZDENT(1)
    PSNOWGRAN1N(KL1) = - XGRAN * ZDENT  (KL1)
    PSNOWGRAN2N(KL1) =   XGRAN * ZSPHERN(KL1)
    !
  ELSE
    !
    PSNOWGRAN1N(KL1) = XGRAN * ZSPHERN(KL1)
    PSNOWGRAN2N(KL1) = ZDIAMN(KL1)
    !
  ENDIF
  !
ENDIF
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG',1,ZHOOK_HANDLE)
!
!       3. Update snow grains parameters : GRAN1, GRAN2
!        PSNOWGRAN1(KL1)=ZSNOWGRAN1(KL1)
!        PSNOWGRAN2(KL1)=ZSNOWGRAN2(KL1)
!
 CONTAINS
!
SUBROUTINE GET_AGREG(KID1,KID2,PFIELD1,PFIELD2,PFIELD)
!
INTEGER, INTENT(IN) :: KID1, KID2
REAL, INTENT(IN) :: PFIELD1, PFIELD2
REAL, INTENT(OUT) :: PFIELD
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG:GET_AGREG',0,ZHOOK_HANDLE)
!
PFIELD = ( PFIELD1 * PSNOWRHO(KID1) * ( PSNOWDZN(KID1) - ABS(PSNOWDDZ(IL)) ) &
         + PFIELD2 * PSNOWRHO(KID2) * ABS(PSNOWDDZ(IL))                        ) / &
         (           PSNOWRHO (KL1) * ( PSNOWDZN (KL1) - ABS(PSNOWDDZ(IL)) ) + &
                     PSNOWRHO (KL2) * ABS(PSNOWDDZ(IL))                        ) 
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG:GET_AGREG',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_AGREG
!
END SUBROUTINE SNOW3LAGREG
!###############################################################################    
!###############################################################################
!
!
!ajout EB : ajout des arguments "N" pour faire idem variables d'origine
SUBROUTINE SNOW3LAVGRAIN(PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,                &
                         PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN,PNDENT,PNVIEU,&
                         HSNOWMETAMO)
!
USE MODD_SNOW_METAMO, ONLY : XVDIAM6, XUEPSI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!       0.1 declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST
! ajout EB
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN 
!
REAL, DIMENSION(:), INTENT(IN)    :: PNDENT, PNVIEU
!
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO
!       0.2 declaration of local variables
!
REAL, DIMENSION(SIZE(PSNOWGRAN1,1)) :: ZGRAN1, ZGRAN2, ZHIST
!
LOGICAL, DIMENSION(SIZE(PSNOWGRAN1,1),SIZE(PSNOWGRAN1,2)) :: GDENDRITIC
!
INTEGER :: JI, JL
INTEGER :: INLVLS, INI
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!       0.3 initialization
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAVGRAIN',0,ZHOOK_HANDLE)
!
INLVLS    = SIZE(PSNOWGRAN1,2)
INI       = SIZE(PSNOWGRAN1,1)
!
ZGRAN1(:) = 0.0
ZGRAN2(:) = 0.0
ZHIST (:) = 0.0
!
DO JI = 1,INI
  !
  IF ( PNDENT(JI)==0.0 .AND. PNVIEU(JI)==0.0 ) THEN
    !
    ZGRAN1(JI) = 1.0
    ZGRAN2(JI) = 1.0
    ZHIST (JI) = 1.0
    !
  ELSE
    !
    DO JL = 1,INLVLS
      IF ( HSNOWMETAMO=='B92' ) THEN
        GDENDRITIC(JI,JL) = ( PSNOWGRAN1(JI,JL) < 0.0 )
      ELSE
        GDENDRITIC(JI,JL) = ( PSNOWGRAN1(JI,JL) < XVDIAM6*(4.-PSNOWGRAN2(JI,JL)) - XUEPSI )
      ENDIF
    ENDDO
    !
    IF ( PNDENT(JI)>=PNVIEU(JI) ) THEN      ! more dendritic than non dendritic snow layer
      !
      DO JL = 1,INLVLS
        IF ( GDENDRITIC(JI,JL) ) THEN
          ZGRAN1(JI) = ZGRAN1(JI) + PSNOWGRAN1(JI,JL)
          ZGRAN2(JI) = ZGRAN2(JI) + PSNOWGRAN2(JI,JL)
        ENDIF
      ENDDO
      !
      PSNOWGRAN1N(JI,:) = ZGRAN1(JI) / PNDENT(JI)
      PSNOWGRAN2N(JI,:) = ZGRAN2(JI) / PNDENT(JI)
      PSNOWHISTN (JI,:) = 0.0
      !
    ELSE                              ! more non dendritic than dendritic snow layers  
      !
      DO JL = 1,INLVLS
        IF ( .NOT.GDENDRITIC(JI,JL) ) THEN
          ZGRAN1(JI) = ZGRAN1(JI) + PSNOWGRAN1(JI,JL)
          ZGRAN2(JI) = ZGRAN2(JI) + PSNOWGRAN2(JI,JL)
          ZHIST (JI) = ZHIST (JI) + PSNOWHIST (JI,JL)
        ENDIF
      ENDDO
      !
      PSNOWGRAN1N(JI,:) = ZGRAN1(JI) / PNVIEU(JI)
      PSNOWGRAN2N(JI,:) = ZGRAN2(JI) / PNVIEU(JI)
      PSNOWHISTN (JI,:) = ZHIST (JI) / PNVIEU(JI)
      !
    ENDIF
    !
  ENDIF
  !
ENDDO



!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAVGRAIN',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LAVGRAIN
!
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW3LDIFTYP(PGRAIN1,PGRAIN2,PGRAIN3,PGRAIN4,HSNOWMETAMO) RESULT(ZDIFTYPE)
!
! à remplacer sans doute par une routine equivalente du nouveau crocus
!*    CALCUL DE LA DIFFERENCE ENTRE DEUX TYPES DE GRAINS
!     VALEUR ENTRE 200 ET 0
!
USE MODD_SNOW_METAMO, ONLY : XGRAN, XVDIAM6, XUEPSI
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!*      0.1    declarations of arguments
REAL, INTENT(IN) :: PGRAIN1, PGRAIN2, PGRAIN3, PGRAIN4
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO
REAL :: ZDIFTYPE, ZCOEF3, ZCOEF4
!REAL(KIND=JPRB) :: ZHOOK_HANDLE

!*      0.2    calcul de la difference entre type de grains
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDIFTYP',0,ZHOOK_HANDLE)
!
IF ( HSNOWMETAMO=='B92' ) THEN
  !
  IF ( ( PGRAIN1<0.  .AND. PGRAIN2>=0.) .OR. ( PGRAIN1>=0. .AND. PGRAIN2<0. ) ) THEN
    ZDIFTYPE = 200.
  ELSEIF ( PGRAIN1<0. ) THEN
    ZDIFTYPE = ABS( PGRAIN1-PGRAIN2 ) * .5 + ABS( PGRAIN3-PGRAIN4 ) * .5
  ELSE
    ZDIFTYPE = ABS( PGRAIN1-PGRAIN2 )      + ABS( PGRAIN3-PGRAIN4 ) * 5. * 10000.
  ENDIF
  !
ELSE
  !
  ZCOEF3 = XVDIAM6 * (4.-PGRAIN3) - XUEPSI
  ZCOEF4 = XVDIAM6 * (4.-PGRAIN4) - XUEPSI
  IF ( ( PGRAIN1<ZCOEF3 .AND. PGRAIN2>=ZCOEF4 ) .OR. ( PGRAIN1>=ZCOEF3 .AND. PGRAIN2<ZCOEF4 ) ) THEN
    ZDIFTYPE = 200.
  ELSEIF ( PGRAIN1<ZCOEF3 ) THEN
    ZDIFTYPE = ABS( (PGRAIN3-PGRAIN4)*XGRAN ) * .5 + &
               ABS( ( (PGRAIN1/XVDIAM6 - 4. + PGRAIN3) / (PGRAIN3 - 3.) - &
                      (PGRAIN2/XVDIAM6 - 4. + PGRAIN4) / (PGRAIN4 - 3.) ) * XGRAN ) * .5

  ELSE
    ZDIFTYPE = ABS( (PGRAIN3-PGRAIN4)*XGRAN )      + ABS( ZCOEF3-ZCOEF4 ) * 5. * 10000.
  ENDIF
  !
ENDIF
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDIFTYP',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LDIFTYP

!####################################################################
SUBROUTINE GET_MASS_HEAT(KJ,KNLVLS_NEW,KNLVLS_OLD,                                &
                         PSNOWZTOP_OLD,PSNOWZTOP_NEW,PSNOWZBOT_OLD,PSNOWZBOT_NEW, &
                         PSNOWRHOO,PSNOWDZO,PSNOWGRAN1O,PSNOWGRAN2O,PSNOWHISTO,   &
                         PSNOWAGEO,PSNOWHEATO,                                    &
                         PSNOWRHON,PSNOWDZN,PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN,   &
                         PSNOWAGEN, PSNOWHEATN,HSNOWMETAMO                        )
!
USE MODD_SNOW_PAR, ONLY : XSNOWCRITD, XD1, XD2, XD3, XX, XVALB5, XVALB6
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENt(IN) :: KJ
INTEGER, INTENT(IN) :: KNLVLS_NEW, KNLVLS_OLD
! trude, changed from REAL, to REAL*8
REAL*8, DIMENSION(:), INTENT(IN) :: PSNOWZTOP_OLD, PSNOWZBOT_OLD
REAL*8, DIMENSION(:), INTENT(IN) :: PSNOWZTOP_NEW, PSNOWZBOT_NEW
REAL*8, DIMENSION(:), INTENT(IN) :: PSNOWRHOO, PSNOWDZO, PSNOWGRAN1O, PSNOWGRAN2O, &
                                  PSNOWHISTO, PSNOWAGEO, PSNOWHEATO
REAL,  DIMENSION(:), INTENT(IN) :: PSNOWDZN
 CHARACTER(3), INTENT(IN)       :: HSNOWMETAMO
REAL*8, DIMENSION(:), INTENT(OUT) :: PSNOWRHON, PSNOWGRAN1N, PSNOWGRAN2N, &
                                   PSNOWHISTN, PSNOWAGEN, PSNOWHEATN
!
REAL :: ZPROPOR, ZMASDZ_OLD, ZDIAM, ZMASTOT_T07
REAL :: ZSNOWHEAN, ZMASTOTN, ZDENTMOYN, ZSPHERMOYN, ZALBMOYN, ZHISTMOYN
REAL :: ZAGEMOYN
!
INTEGER :: JST_NEW, JST_OLD
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('GET_MASS_HEAT',0,ZHOOK_HANDLE)
!
!      write(6,*) "in get_mass_heat"   ! trude

PSNOWRHON  (:) = 0.
PSNOWGRAN1N(:) = 0.
PSNOWGRAN2N(:) = 0.
PSNOWHISTN (:) = 0.
PSNOWAGEN  (:) = 0.
PSNOWHEATN (:) = 0.
!
DO JST_NEW = 1,KNLVLS_NEW
  !
  ZSNOWHEAN   = 0.
  ZMASTOTN    = 0.
  ZMASTOT_T07 = 0.
  ZDENTMOYN   = 0.
  ZSPHERMOYN  = 0.
  ZALBMOYN    = 0.
  ZDIAM       = 0.
  ZHISTMOYN   = 0.
  ZAGEMOYN    = 0.
  !
  ! lopp over the ols snow layers
!   write(6,*) "-------"
  DO JST_OLD = 1,KNLVLS_OLD
!      write(6,*) "knlvls_old ", knlvls_old, jst_old, jst_new
!      write(6,*) PSNOWZTOP_OLD(JST_OLD), PSNOWZBOT_NEW(JST_NEW)
!      write(6,*) PSNOWZBOT_OLD(JST_OLD), PSNOWZTOP_NEW(JST_NEW)
    !
    IF( PSNOWZTOP_OLD(JST_OLD)<=PSNOWZBOT_NEW(JST_NEW) ) THEN
      ! JST_OLD lower than JJ_NEW ==> no contribution
    ELSEIF ( PSNOWZBOT_OLD(JST_OLD)>=PSNOWZTOP_NEW(JST_NEW) ) THEN
      ! JST_OLD higher than JJ_NEW ==> no contribution
    ELSE
      ! old layer contributes to the new one
      ! computation of its contributing ratio and mass/heat
      ZPROPOR = ( MIN( PSNOWZTOP_OLD(JST_OLD), PSNOWZTOP_NEW(JST_NEW) )   &
                - MAX( PSNOWZBOT_OLD(JST_OLD), PSNOWZBOT_NEW(JST_NEW) ) ) &
                 / PSNOWDZO(JST_OLD)
      ZMASDZ_OLD = ZPROPOR * PSNOWRHOO(JST_OLD) * PSNOWDZO(JST_OLD)
!      write(6,*) "zpropor ", zpropor
!      write(6,*) "psnowrhoo ", psnowrhoo(jst_old)
!      write(6,*) "psnowdzo ", psnowdzo(jst_old)
      !
      ZMASTOTN    = ZMASTOTN + ZMASDZ_OLD
!      write(6,*) "zmastotn ", zmastotn
!      write(6,*) "zmasdz_old ", zmasdz_old
      ZMASTOT_T07 = ZMASTOT_T07 + 1.
      !
      ZSNOWHEAN = ZSNOWHEAN + ZPROPOR * PSNOWHEATO(JST_OLD)
      !
      IF ( HSNOWMETAMO=='B92' ) THEN
        !
        ! contribution to the grain optical size and then to the albedo
        IF ( PSNOWGRAN1O(JST_OLD)<0. ) THEN
          ZDIAM = -PSNOWGRAN1O(JST_OLD)*XD1/XX + (1.+PSNOWGRAN1O(JST_OLD)/XX) * &
                 ( PSNOWGRAN2O(JST_OLD)*XD2/XX + (1.-PSNOWGRAN2O(JST_OLD)/XX)*XD3 ) 
          ZDIAM = ZDIAM/10000.
          ZDENTMOYN  = ZDENTMOYN  - ZMASDZ_OLD * PSNOWGRAN1O(JST_OLD) / XX
          ZSPHERMOYN = ZSPHERMOYN + ZMASDZ_OLD * PSNOWGRAN2O(JST_OLD) / XX
        ELSE
          ZDIAM = PSNOWGRAN2O(JST_OLD)
          ZDENTMOYN  = ZDENTMOYN  + ZMASDZ_OLD * 0.
          ZSPHERMOYN = ZSPHERMOYN + ZMASDZ_OLD * PSNOWGRAN1O(JST_OLD) / XX
        ENDIF
        !
      ELSE
        !
        ZDIAM = PSNOWGRAN1O(JST_OLD)
        ZSPHERMOYN = ZSPHERMOYN + ZMASDZ_OLD * PSNOWGRAN2O(JST_OLD)
        !
      ENDIF
      !
      ZALBMOYN  = ZALBMOYN  + MAX( 0., ZMASDZ_OLD * (XVALB5-XVALB6*SQRT(ZDIAM)) )
      ZHISTMOYN = ZHISTMOYN + ZMASDZ_OLD * PSNOWHISTO(JST_OLD)
      ZAGEMOYN  = ZAGEMOYN  + ZMASDZ_OLD * PSNOWAGEO (JST_OLD)
      !
    ENDIF
    !
  ENDDO
  !
  ! the new layer inherits from the weihted average properties of the old ones
  ! heat and mass
  PSNOWHEATN(JST_NEW) = ZSNOWHEAN
! trude test

!      write(6,*) "zmastotn ", zmastotn
!      write(6,*) "psnowdzn ", psnowdzn(jst_new)
  PSNOWRHON (JST_NEW) = ZMASTOTN / PSNOWDZN(JST_NEW)
 !     write(6,*) "psnowrhon ", psnowrhon(jst_new)
  ! grain type and size decuced from the average albedo
  ZALBMOYN   = ZALBMOYN / ZMASTOTN
  ZSPHERMOYN = MAX( 0., ZSPHERMOYN/ZMASTOTN )
  ZDENTMOYN  = MAX( 0., ZDENTMOYN /ZMASTOTN )
  ZDIAM = ( (XVALB5-ZALBMOYN)/XVALB6 )**2
  !
  IF ( HSNOWMETAMO=='B92' ) THEN
    !
    ! size between D2 and D3 and dendricity < 0
    ! sphericity is firts preserved, if possible. If not,
    ! denditricity =0
    PSNOWGRAN1N(JST_NEW) = -XX * ZDENTMOYN
    !
    IF ( ZDENTMOYN/=1.) THEN
      PSNOWGRAN2N(JST_NEW) = XX * ( ( ZDIAM*10000. + PSNOWGRAN1N(JST_NEW)*XD1/XX ) &
                                 / ( 1. + PSNOWGRAN1N(JST_NEW)/XX ) - XD3 )        &
                             / ( XD2-XD3 )
    ENDIF
    !
    ! dendricity is preserved if possible and sphericity is adjusted
    IF ( ZDIAM < XD2/10000. - 0.0000001 ) THEN
      !
      IF ( ABS( PSNOWGRAN1N(JST_NEW)+XX ) < 0.01 ) THEN
        !
        PSNOWGRAN2N(JST_NEW) = XX * ZSPHERMOYN
        !
      ELSEIF ( ABS( PSNOWGRAN1N(JST_NEW) ) < 0.0001 ) THEN ! dendritic snow
        !
        PSNOWGRAN1N(JST_NEW) = XX * ZSPHERMOYN
        PSNOWGRAN2N(JST_NEW) = ZDIAM
        !
      ELSEIF ( PSNOWGRAN2N(JST_NEW) < 0. ) THEN ! non dendritic
        !
        PSNOWGRAN2N(JST_NEW) = 0.
        !
      ELSEIF ( PSNOWGRAN2N(JST_NEW) > XX + 0.0000001 ) THEN ! non dendritic
        !
        PSNOWGRAN2N(JST_NEW) = XX
        !
      ENDIF
      !
    ELSEIF ( ZDIAM > XD3/10000. .OR. ZDENTMOYN <= 0. + 0.0000001 .OR. &
             PSNOWGRAN2N(JST_NEW) < 0. .OR. PSNOWGRAN2N(JST_NEW) > XX ) THEN
      !
      ! dendritic snow
      ! inconsistency with ZDIAM ==>  dendricity = 0
      ! size between D2 and D3 and dendricity == 0
      PSNOWGRAN1N(JST_NEW) = XX * ZSPHERMOYN
      PSNOWGRAN2N(JST_NEW) = ZDIAM
      !
    ENDIF
    !
  ELSE
    !
    PSNOWGRAN1N(JST_NEW) = ZDIAM
    PSNOWGRAN2N(JST_NEW) = MIN( 1., ZSPHERMOYN )
    !
  ENDIF
  !
  PSNOWHISTN(JST_NEW) = NINT( ZHISTMOYN/ZMASTOTN )
  PSNOWAGEN (JST_NEW) = ZAGEMOYN / ZMASTOTN
  !
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('GET_MASS_HEAT',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_MASS_HEAT
!####################################################################
SUBROUTINE GET_DIAM(PSNOWGRAN1,PSNOWGRAN2,PDIAM,HSNOWMETAMO)
!
USE MODD_SNOW_PAR, ONLY : XD1, XD2, XD3, XX
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PSNOWGRAN1
REAL, INTENT(IN) :: PSNOWGRAN2
REAL, INTENT(OUT) :: PDIAM
!
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('GET_DIAM',0,ZHOOK_HANDLE)
!
IF ( HSNOWMETAMO=='B92' ) THEN
  !
  IF( PSNOWGRAN1<0. ) THEN
    PDIAM = -PSNOWGRAN1*XD1/XX + (1.+PSNOWGRAN1/XX) * &
           ( PSNOWGRAN2*XD2/XX + (1.-PSNOWGRAN2/XX) * XD3 )
    PDIAM = PDIAM/10000.
  ELSE
    PDIAM = PSNOWGRAN2*PSNOWGRAN1/XX + &
            MAX( 0.0004, 0.5*PSNOWGRAN2 ) * ( 1.-PSNOWGRAN1/XX )
  ENDIF
  !
ELSE
  !
  PDIAM = PSNOWGRAN1
  !
ENDIF
!
!IF (LHOOK) CALL DR_HOOK('GET_DIAM',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_DIAM
!####################################################################
!####################################################################
!####################################################################
!####################################################################
!####################################################################
!####################################################################
      SUBROUTINE SNOW3LTHRM(PSNOWRHO,PSCOND,PSNOWTEMP,PPS)
!
!!    PURPOSE
!!    -------
!     Calculate snow thermal conductivity from
!     Sun et al. 1999, J. of Geophys. Res., 104, 19587-19579 (vapor)
!     and Yen, 1981, CRREL Rep 81-10 (snow)
!     or Anderson, 1976, NOAA Tech. Rep. NWS 19 (snow).
!
!
USE MODD_CSTS,     ONLY : XP00, XCONDI, XRHOLW
!
USE MODD_SNOW_PAR, ONLY : XVRKZ6, XSNOWTHRMCOND1, &
                          XSNOWTHRMCOND2,         &
                          XSNOWTHRMCOND_AVAP,     &
                          XSNOWTHRMCOND_BVAP,     &
                          XSNOWTHRMCOND_CVAP
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWTEMP, PSNOWRHO
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSCOND
!
!
!*      0.2    declarations of local variables
!
INTEGER                              :: JJ, JI
!
INTEGER                              :: INI
INTEGER                              :: INLVLS
!
 CHARACTER(LEN=5)                     :: YSNOWCOND !should be in namelist
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LTHRM',0,ZHOOK_HANDLE)
!
INI    = SIZE(PSNOWRHO(:,:),1)
INLVLS = SIZE(PSNOWRHO(:,:),2)
!
! 1. Snow thermal conductivity
! ----------------------------
!
YSNOWCOND='YEN81' !should be in namelist
!
IF(YSNOWCOND=='AND76')THEN
!  Thermal conductivity coefficients from Anderson (1976)
  PSCOND(:,:) = (XSNOWTHRMCOND1 + XSNOWTHRMCOND2*PSNOWRHO(:,:)*PSNOWRHO(:,:))
ELSE
! Thermal conductivity coefficients from Yen (1981)
  PSCOND(:,:) = XCONDI * EXP(XVRKZ6*LOG(PSNOWRHO(:,:)/XRHOLW))
ENDIF
!
! 2. Implicit vapor diffn effects
! -------------------------------
!
DO JJ=1,INLVLS
   DO JI=1,INI
    PSCOND(JI,JJ) = PSCOND(JI,JJ) + MAX(0.0,(XSNOWTHRMCOND_AVAP+(XSNOWTHRMCOND_BVAP/(PSNOWTEMP(JI,JJ) &
                                  + XSNOWTHRMCOND_CVAP)))*(XP00/PPS(JI)))
   ENDDO
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LTHRM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LTHRM
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW3LDOPT_2D(PSNOWRHO,PSNOWAGE) RESULT(PDOPT)
!
!!    PURPOSE
!!    -------
!     Calculate the optical grain diameter.
!
USE MODD_SNOW_PAR, ONLY : XDSGRAIN_MAX,XSNOW_AGRAIN, &
                          XSNOW_BGRAIN,XSNOW_CGRAIN
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWRHO,PSNOWAGE
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PDOPT
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZAGE
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSRHO4
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_2D',0,ZHOOK_HANDLE)
!
ZAGE(:,:) = MIN(15.,PSNOWAGE(:,:))
!
ZSRHO4(:,:) = PSNOWRHO(:,:)*PSNOWRHO(:,:)*PSNOWRHO(:,:)*PSNOWRHO(:,:)
!
PDOPT(:,:) = MIN(XDSGRAIN_MAX,XSNOW_AGRAIN+XSNOW_BGRAIN*ZSRHO4(:,:)+XSNOW_CGRAIN*ZAGE(:,:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LDOPT_2D
!####################################################################
FUNCTION SNOW3LDOPT_1D(PSNOWRHO,PSNOWAGE) RESULT(PDOPT)
!
!!    PURPOSE
!!    -------
!     Calculate the optical grain diameter.
!
USE MODD_SNOW_PAR, ONLY : XDSGRAIN_MAX,XSNOW_AGRAIN, &
                          XSNOW_BGRAIN,XSNOW_CGRAIN
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWRHO,PSNOWAGE
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: PDOPT
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZAGE
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZSRHO4
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_1D',0,ZHOOK_HANDLE)
!
ZAGE(:) = MIN(15.,PSNOWAGE(:))
!
ZSRHO4(:) = PSNOWRHO(:)*PSNOWRHO(:)*PSNOWRHO(:)*PSNOWRHO(:)
!
PDOPT(:) = MIN(XDSGRAIN_MAX,XSNOW_AGRAIN+XSNOW_BGRAIN*ZSRHO4(:)+XSNOW_CGRAIN*ZAGE(:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LDOPT_1D
!####################################################################
FUNCTION SNOW3LDOPT_0D(PSNOWRHO,PSNOWAGE) RESULT(PDOPT)
!
!!    PURPOSE
!!    -------
!     Calculate the optical grain diameter.
!
USE MODD_SNOW_PAR, ONLY : XDSGRAIN_MAX,XSNOW_AGRAIN, &
                          XSNOW_BGRAIN,XSNOW_CGRAIN
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)  :: PSNOWRHO,PSNOWAGE
!
REAL :: PDOPT
REAL :: ZAGE
REAL :: ZSRHO4
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_0D',0,ZHOOK_HANDLE)
!
ZAGE = MIN(15.,PSNOWAGE)
!
ZSRHO4 = PSNOWRHO*PSNOWRHO*PSNOWRHO*PSNOWRHO
!
PDOPT = MIN(XDSGRAIN_MAX,XSNOW_AGRAIN+XSNOW_BGRAIN*ZSRHO4+XSNOW_CGRAIN*ZAGE)
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDOPT_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LDOPT_0D
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOW3LALB(PALBEDOSC,PSPECTRALALBEDO,PSNOWRHO,PSNOWAGE,   &
                     PPERMSNOWFRAC,PPS)
!
!!    PURPOSE
!!    -------
!     Calculate the snow surface albedo. Use the method of
!     CROCUS with 3 spectral albedo depending on snow density
!     and age
!
!
USE MODD_SNOW_PAR, ONLY : XVAGING_GLACIER, XVAGING_NOGLACIER,     &
                          XVALB2,XVALB3,XVALB4,XVALB5,XVALB6,     &
                          XVALB7,XVALB8,XVALB9,XVALB10,XVALB11,   &
                          XVDIOP1,XVRPRE1,XVRPRE2,XVPRES1,        &
                          XVW1,XVW2,XVSPEC1,XVSPEC2,XVSPEC3
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWRHO
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWAGE
REAL, DIMENSION(:), INTENT(IN)      :: PPERMSNOWFRAC
REAL, DIMENSION(:), INTENT(IN)      :: PPS
!
REAL, DIMENSION(:),   INTENT(INOUT) :: PALBEDOSC
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSPECTRALALBEDO
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                 :: ZALBNIR1 = 0.3
REAL, PARAMETER                 :: ZALBNIR2 = 0.0
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZVAGING, ZDIAM, ZAGE,  &
                                   ZWORK, ZPRES_EFFECT
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZALB1, ZALB2, ZALB3
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LALB',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! ------------------
!
!Snow age effect parameter for Visible (small over glacier)
ZVAGING(:)=XVAGING_GLACIER*PPERMSNOWFRAC(:) + XVAGING_NOGLACIER*(1.0-PPERMSNOWFRAC(:))
!
!Atm pression effect parameter on albedo
ZPRES_EFFECT(:) = XVALB10*MIN(MAX(PPS(:)/XVPRES1,XVRPRE1),XVRPRE2)
!
! 1. Snow optical grain diameter :
! --------------------------------
!
!Snow optical diameter do not depend on snow age over glacier or polar regions
ZAGE(:) = (1.0-PPERMSNOWFRAC(:))*PSNOWAGE(:)
!
ZDIAM(:) = SNOW3LDOPT(PSNOWRHO(:),ZAGE(:))
!
! 2. spectral albedo over 3 bands :
! ---------------------------------
!
!Snow age effect limited to 1 year
ZAGE(:) = MIN(365.,PSNOWAGE(:))
!
ZWORK(:)=SQRT(ZDIAM(:))
!
! Visible
ZALB1(:)=MIN(XVALB4,XVALB2-XVALB3*ZWORK(:))
ZALB1(:)=MAX(XVALB11,ZALB1(:)-ZPRES_EFFECT(:)*ZAGE(:)/ZVAGING(:))
!
! near Infra-red 1
ZALB2(:)=XVALB5-XVALB6*ZWORK(:)
ZALB2(:)=MAX(ZALBNIR1,ZALB2(:))
!
! near Infra-red 2
ZDIAM(:)=MIN(XVDIOP1,ZDIAM(:))
ZWORK(:)=SQRT(ZDIAM(:))
ZALB3(:)=XVALB7*ZDIAM(:)-XVALB8*ZWORK(:)+XVALB9
ZALB3(:)=MAX(ZALBNIR2,ZALB3(:))
!
PSPECTRALALBEDO(:,1)=ZALB1(:)
PSPECTRALALBEDO(:,2)=ZALB2(:)
PSPECTRALALBEDO(:,3)=ZALB3(:)
!
! 3. total albedo :
! -----------------
!
PALBEDOSC(:)=XVSPEC1*ZALB1(:)+XVSPEC2*ZALB2(:)+XVSPEC3*ZALB3(:)
!
!IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LALB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOW3LALB
!####################################################################
!####################################################################
!####################################################################
END MODULE MODE_SNOW3L
