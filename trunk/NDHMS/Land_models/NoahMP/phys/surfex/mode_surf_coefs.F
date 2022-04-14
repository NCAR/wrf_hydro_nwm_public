MODULE MODE_SURF_COEFS

!++Trude. To reduce the numbers of module used, I merged module modi_surface_aero_cond, modi_surface_cd and modi_surface_ri into one module. 

!++ Trude: Also included wind_threshold function
CONTAINS

!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ######################################################################
    SUBROUTINE SURFACE_AERO_COND(PRI, PZREF, PUREF, PVMOD, PZ0,&
                                     PZ0H, PAC, PRA, PCH           ) 
!   ######################################################################
!
!!****  *SURFACE_AERO_COND*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the drag coefficients for heat and momentum near the ground
!         
!     
!!**  METHOD
!!    ------
!
!
!
!    1 and 2 : computation of relative humidity near the ground
!
!    3 : richardson number
!
!    4 : the aerodynamical resistance for heat transfers is deduced
!
!    5 : the drag coefficient for momentum ZCD is computed
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/01/98 
!!                  02/04/01 (P Jabouille) limitation of Z0 with 0.5 PUREF
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,ONLY : XKARMAN
!USE MODI_WIND_THRESHOLD
!
USE MODE_THERMOS
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD    ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the wind
                                              ! NOTE this is different from ZZREF
                                              ! ONLY in stand-alone/forced mode,
                                              ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:), INTENT(IN)    :: PZ0      ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H     ! roughness length for heat
!
REAL, DIMENSION(:), INTENT(OUT)   :: PAC      ! aerodynamical conductance
REAL, DIMENSION(:), INTENT(OUT)   :: PRA      ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)   :: PCH      ! drag coefficient for heat
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PRI)) :: ZZ0, ZZ0H, ZMU,          &
                               ZFH, ZCHSTAR, ZPH, ZCDN, &
                               ZSTA, ZDI, ZWORK1, ZWORK2, ZWORK3 
REAL, DIMENSION(SIZE(PRI)) :: ZVMOD
!
INTEGER                    :: JJ
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Functions:
REAL :: X, CHSTAR, PH
 CHSTAR(X) = 3.2165 + 4.3431*X + 0.5360*X*X - 0.0781*X*X*X
PH    (X) = 0.5802 - 0.1571*X + 0.0327*X*X - 0.0026*X*X*X
!
!-------------------------------------------------------------------------------
!
!*       4.     Surface aerodynamic resistance for heat transfers
!               -------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('SURFACE_AERO_COND',0,ZHOOK_HANDLE)
ZVMOD(:) = WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
DO JJ=1,SIZE(PRI)
!write(*,*) PZ0(jj), puref(jj), zz0(jj), pz0h(jj)
  ZZ0(JJ)  = MIN(PZ0(JJ),PUREF(JJ)*0.5)
  ZZ0H(JJ) = MIN(ZZ0(JJ),PZ0H(JJ))
  ZZ0H(JJ) = MIN(ZZ0H(JJ),PZREF(JJ)*0.5)
!
  ZWORK1(JJ)=LOG( PUREF(JJ)/ZZ0(JJ) )
  ZWORK2(JJ)=PZREF(JJ)/ZZ0H(JJ)
  ZWORK3(JJ)=ZVMOD(JJ)*ZVMOD(JJ)

  ZMU(JJ) = MAX( LOG( ZZ0(JJ)/ZZ0H(JJ) ), 0.0 )
  ZFH(JJ) = ZWORK1(JJ) / LOG(ZWORK2(JJ))
!
  ZCHSTAR(JJ) = CHSTAR(ZMU(JJ))
  ZPH(JJ)     = PH(ZMU(JJ))
!
! 
  ZCDN(JJ) = (XKARMAN/ZWORK1(JJ))**2.
!
!
  ZSTA(JJ) = PRI(JJ)*ZWORK3(JJ)
!
!
  IF ( PRI(JJ) < 0.0 ) THEN
    ZDI(JJ) = 1. / ( ZVMOD(JJ)                                  &
                   +ZCHSTAR(JJ)*ZCDN(JJ)*15.                         &
                                *ZWORK2(JJ)**ZPH(JJ)  &
                                *ZFH(JJ) * SQRT(-ZSTA(JJ))           &
                  ) 
    PAC(JJ) = ZCDN(JJ)*(ZVMOD(JJ)-15.* ZSTA(JJ)*ZDI(JJ))*ZFH(JJ)

  ELSE
    ZDI(JJ) = SQRT(ZWORK3(JJ) + 5. * ZSTA(JJ) )
    PAC(JJ) = ZCDN(JJ)*ZVMOD(JJ)/(1.+15.*ZSTA(JJ)*ZDI(JJ)  &
             / ZWORK3(JJ) /ZVMOD(JJ) )*ZFH(JJ)    
  ENDIF
!
  PRA(JJ) = 1. / PAC(JJ)
!
  PCH(JJ) = 1. / (PRA(JJ) * ZVMOD(JJ))
!
ENDDO
!IF (LHOOK) CALL DR_HOOK('SURFACE_AERO_COND',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURFACE_AERO_COND

!   #################################################################
    SUBROUTINE SURFACE_CD(PRI, PZREF, PUREF, PZ0EFF, PZ0H,   &
                              PCD, PCDN) 
!   #################################################################
!
!!****  *SURFACE_CD*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the drag coefficients for momentum near the ground
!         
!     
!!**  METHOD
!!    ------
!
!
!
!    1 and 2 : computation of relative humidity near the ground
!
!    3 : richardson number
!
!    4 : the aerodynamical resistance for heat transfers is deduced
!
!    5 : the drag coefficient for momentum ZCD is computed
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!    MODD_GROUND_PAR
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/01/98 
!!                  02/04/01 (P Jabouille) limitation of Z0 with 0.5 PUREF
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,ONLY : XKARMAN
!
USE MODE_THERMOS
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the wind
!                                             ! NOTE this is different from ZZREF
!                                             ! ONLY in stand-alone/forced mode,
!                                             ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:), INTENT(IN)    :: PZ0EFF   ! roughness length for momentum
                                              ! with subgrid-scale orography
REAL, DIMENSION(:), INTENT(IN)    :: PZ0H     ! roughness length for heat
!
REAL, DIMENSION(:), INTENT(OUT)   :: PCD      ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(OUT)   :: PCDN     ! neutral drag coefficient for momentum
!
!*      0.2    declarations of local variables
!
!
REAL                       :: ZZ0EFF, ZZ0H, ZMU,     &
                               ZCMSTAR, ZPM, ZCM, ZFM 
INTEGER                    :: JJ
!REAL(KIND=JPRB) :: ZHOOK_HANDLE

! Functions :
REAL :: X, CMSTAR, PM
 CMSTAR(X) = 6.8741 + 2.6933*X - 0.3601*X*X + 0.0154*X*X*X
PM    (X) = 0.5233 - 0.0815*X + 0.0135*X*X - 0.0010*X*X*X

!-------------------------------------------------------------------------------
!
!*       1.     Drag coefficient for momentum transfers
!               ---------------------------------------
!

!
!IF (LHOOK) CALL DR_HOOK('SURFACE_CD',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(PRI)
  ZZ0EFF = MIN(PZ0EFF(JJ),PUREF(JJ)*0.5)
  ZZ0H   = MIN(ZZ0EFF,PZ0H(JJ))
!
  ZMU = LOG( MIN(ZZ0EFF/ZZ0H,200.) )
!
  PCDN(JJ) = (XKARMAN/LOG(PUREF(JJ)/ZZ0EFF))**2

  ZCMSTAR = CMSTAR(ZMU)
  ZPM     = PM(ZMU)
!
  ZCM = 10.*ZCMSTAR*PCDN(JJ)*( PUREF(JJ)/ZZ0EFF )**ZPM
!
  IF ( PRI(JJ) > 0.0 ) THEN
    ZFM = 1. + 10.*PRI(JJ) / SQRT( 1.+5.*PRI(JJ) )
    ZFM = 1. / ZFM
  ELSE
    ZFM = 1. - 10.*PRI(JJ) / ( 1.+ZCM*SQRT(-PRI(JJ)) )
  ENDIF
!
  PCD(JJ) = PCDN(JJ)*ZFM
!
ENDDO
!IF (LHOOK) CALL DR_HOOK('SURFACE_CD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURFACE_CD

!     #########
    SUBROUTINE SURFACE_RI(PTG, PQS, PEXNS, PEXNA, PTA, PQA,   &
                               PZREF, PUREF, PDIRCOSZW, PVMOD, PRI )  
!   ######################################################################
!
!!****  *SURFACE_RI*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the richardson number near the ground
!         
!     
!!**  METHOD
!!    ------
!
!
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!    MODD_GROUND_PAR
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/09/98 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XRV, XRD, XG
USE MODD_SURF_ATM, ONLY : XRIMAX
!USE MODI_WIND_THRESHOLD
!
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PTG      ! surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PQS      ! surface specific humidity
REAL, DIMENSION(:), INTENT(IN)    :: PEXNS    ! surface exner function
REAL, DIMENSION(:), INTENT(IN)    :: PTA      ! temperature at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PQA      ! specific humidity
                                              ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PEXNA    ! exner function
                                              ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD    ! module of the horizontal wind
!
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the wind
!                                             ! NOTE this is different from ZZREF
!                                             ! ONLY in stand-alone/forced mode,
!                                             ! NOT when coupled to a model (MesoNH)
REAL, DIMENSION(:), INTENT(IN)    :: PDIRCOSZW! Cosine of the angle between
!                                             ! the normal to the surface and
!                                             ! the vertical
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRI      ! Richardson number
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTG))   :: ZTHVA, ZTHVS
REAL, DIMENSION(SIZE(PVMOD)) :: ZVMOD
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       1.     Richardson number
!              -----------------
!                
!                                                 virtual potential        
!                                                 temperature at the 
!                                                 first atmospheric level and
!                                                 at the surface
!
!IF (LHOOK) CALL DR_HOOK('SURFACE_RI',0,ZHOOK_HANDLE)
!
ZTHVA(:)=PTA(:)/PEXNA(:)*( 1.+(XRV/XRD-1.)*PQA(:) )   
ZTHVS(:)=PTG(:)/PEXNS(:)*( 1.+(XRV/XRD-1.)*PQS(:) )
!                                                 
ZVMOD(:) = WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
                                                ! Richardson's number
PRI(:) = XG * PDIRCOSZW(:) * PUREF(:) * PUREF(:)              &
          * (ZTHVA(:)-ZTHVS(:)) / (0.5 * (ZTHVA(:)+ZTHVS(:)) )  &
          / (ZVMOD(:)*ZVMOD(:)) /PZREF(:)  
!
PRI(:) = MIN(PRI(:),XRIMAX)
!
!IF (LHOOK) CALL DR_HOOK('SURFACE_RI',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SURFACE_RI


!     #########
    FUNCTION WIND_THRESHOLD(PWIND,PUREF) RESULT(PWIND_NEW)
!   ############################################################################
!
!!****  *WIND_THRESHOLD*  
!!
!!    PURPOSE
!!    -------
!
!     Set a minimum value to the wind for exchange coefficient computations.
!     This minimum value depends on the forcing height
!     
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007 
!-------------------------------------------------------------------------------
!
USE MODD_SURF_ATM, ONLY: XCISMIN, XVMODMIN, LALDTHRES
!
!*       0.     DECLARATIONS
!               ------------
!
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)   :: PWIND      ! wind
REAL, DIMENSION(:), INTENT(IN)   :: PUREF      ! forcing level
!
REAL, DIMENSION(SIZE(PWIND))     :: PWIND_NEW  ! modified wind
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------
!
!  wind gradient
!
!IF (LHOOK) CALL DR_HOOK('WIND_THRESHOLD',0,ZHOOK_HANDLE)
IF (.NOT.LALDTHRES) THEN
!        
!  minimum value for exchange coefficients computations : 1m/s / 10m
   PWIND_NEW = MAX(PWIND , 0.1 * MIN(10.,PUREF) )
ELSE
!  minimum value for exchange coefficients computations : 1m/s / 10m
   PWIND_NEW = MAX( XVMODMIN, SQRT( PWIND**2 + (XCISMIN*PUREF)**2 ) )
ENDIF
!IF (LHOOK) CALL DR_HOOK('WIND_THRESHOLD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION WIND_THRESHOLD




END MODULE MODE_SURF_COEFS
