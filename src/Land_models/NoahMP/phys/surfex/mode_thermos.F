!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      MODULE MODE_THERMOS
!     ####################
!
!!****  *MODE_THERMO* -
!!
!!    PURPOSE
!!    -------
!      
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!       NONE          
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    28/08/94 
!!      Modified    01/2006 : sea flux parameterization.
!!      B. Decharme 05/2013 : Qsat function of XTT
!!                            so, Qsat=Qsati if Tg <= XTT and inversely
!!      S. Belamari 03/2014 : new formula (QSAT_SEAWATER2) for sat. air pressure
!!                            over seawater (with explicit salinity dependency)
!!                            
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!-------------------------------------------------------------------------------
!
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
INTERFACE PSAT
  MODULE PROCEDURE PSAT_0D
  MODULE PROCEDURE PSAT_1D
  MODULE PROCEDURE PSAT_2D
END INTERFACE
INTERFACE DPSAT
  MODULE PROCEDURE DPSAT_1D
END INTERFACE

INTERFACE QSAT
  MODULE PROCEDURE QSATW_0D         
  MODULE PROCEDURE QSATW_1D
  MODULE PROCEDURE QSATW_2D
END INTERFACE
INTERFACE QSAT_SEAWATER
  MODULE PROCEDURE QSATSEAW_1D
END INTERFACE
INTERFACE QSAT_SEAWATER2
  MODULE PROCEDURE QSATSEAW2_1D
END INTERFACE
INTERFACE DQSAT
  MODULE PROCEDURE DQSATW_O_DT_1D
END INTERFACE
INTERFACE QSATI
  MODULE PROCEDURE QSATI_1D
  MODULE PROCEDURE QSATI_2D
END INTERFACE
INTERFACE DQSATI
  MODULE PROCEDURE DQSATI_O_DT_1D
END INTERFACE
 CONTAINS
!-------------------------------------------------------------------------------
!     ######################################
      FUNCTION PSAT_0D(PT) RESULT(PPSAT)
!     ######################################
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!++ Trude, moved CQSAT to MODD_SNOW_PAR
!USE MODD_REPROD_OPER, ONLY : CQSAT
USE MODD_SNOW_PAR
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, INTENT(IN)                :: PT     ! Temperature (Kelvin)
REAL                            :: PPSAT  ! saturation vapor 
                                          ! specific humidity
                                          ! with respect to
                                          ! water (kg/kg)
!
REAL            :: ZALP, ZBETA, ZGAM
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE                                          
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_0D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!

ZALP  = XALPW
ZBETA = XBETAW
ZGAM  = XGAMW
!
IF(CQSAT=='NEW'.AND.PT<=XTT)THEN
 ZALP  = XALPI
 ZBETA = XBETAI
 ZGAM  = XGAMI       
ENDIF
!
PPSAT = EXP( ZALP - ZBETA/PT - ZGAM*LOG(PT) )
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_0D',1,ZHOOK_HANDLE)
!
END FUNCTION PSAT_0D
!-------------------------------------------------------------------------------
!     ######################################
      FUNCTION PSAT_1D(PT) RESULT(PPSAT)
!     ######################################
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!++ Trude, moved CQSAT to MODD_SNOW_PAR
!USE MODD_REPROD_OPER, ONLY : CQSAT
USE MODD_SNOW_PAR
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN)                :: PT     ! Temperature (Kelvin)
REAL, DIMENSION(SIZE(PT))                     :: PPSAT  ! saturation vapor pressure (Pa)
!
REAL, DIMENSION(SIZE(PT))  :: ZALP, ZBETA, ZGAM
!
INTEGER                         :: JJ !loop index
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_1D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!

ZALP (:) = XALPW
ZBETA(:) = XBETAW
ZGAM (:) = XGAMW
!
IF(CQSAT=='NEW')THEN
 WHERE(PT<=XTT)
   ZALP  (:) = XALPI
   ZBETA (:) = XBETAI
   ZGAM  (:) = XGAMI 
 ENDWHERE
ENDIF
!
!cdir nodep
DO JJ=1,SIZE(PT)
  PPSAT(JJ) = EXP( ZALP(JJ) - ZBETA(JJ)/PT(JJ) - ZGAM(JJ)*LOG(PT(JJ)) )
ENDDO
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_1D',1,ZHOOK_HANDLE)
!
END FUNCTION PSAT_1D
!-------------------------------------------------------------------------------
!     ######################################
      FUNCTION PSAT_2D(PT,KMASK) RESULT(PPSAT)
!     ######################################
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!++ Trude, moved CQSAT to MODD_SNOW_PAR
!USE MODD_REPROD_OPER, ONLY : CQSAT
USE MODD_SNOW_PAR

!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:,:), INTENT(IN)              :: PT     ! Temperature (Kelvin)
INTEGER, DIMENSION(:), INTENT(IN)             :: KMASK
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2))        :: PPSAT  ! saturation vapor pressure (Pa)
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2))        :: ZALP, ZBETA, ZGAM
!
INTEGER         :: JJ, JL, INI, INL, IWORK !loop index
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_2D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!

INI=SIZE(PT,1)
INL=SIZE(PT,2)
!
PPSAT(:,:) = 0.0
!
ZALP (:,:) = XALPW
ZBETA(:,:) = XBETAW
ZGAM (:,:) = XGAMW
!
IF(CQSAT=='NEW')THEN
 WHERE(PT(:,:)<=XTT)
   ZALP  (:,:) = XALPI
   ZBETA (:,:) = XBETAI
   ZGAM  (:,:) = XGAMI 
 ENDWHERE
ENDIF
!
DO JL=1,INL
  DO JJ=1,INI
     IWORK=KMASK(JJ)
     IF(JL<=IWORK)THEN
       PPSAT(JJ,JL) = EXP( ZALP(JJ,JL) - ZBETA(JJ,JL)/PT(JJ,JL) - ZGAM(JJ,JL)*LOG(PT(JJ,JL)) )
     ENDIF
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:PSAT_2D',1,ZHOOK_HANDLE)
!
END FUNCTION PSAT_2D
!-------------------------------------------------------------------------------
!     ######################################
      FUNCTION DPSAT_1D(PT) RESULT(PDPSAT)
!     ######################################
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!++ Trude moved CQSAT to MODD_SNOW_PAR
!USE MODD_REPROD_OPER, ONLY : CQSAT
USE MODD_SNOW_PAR
!
IMPLICIT NONE

!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN) :: PT      ! Temperature (Kelvin)
!
REAL, DIMENSION(SIZE(PT))      :: PDPSAT  
!
REAL, DIMENSION(SIZE(PT))      :: ZBETA, ZGAM
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DPSAT_1D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!

ZBETA(:) = XBETAW
ZGAM (:) = XGAMW
!
IF(CQSAT=='NEW')THEN
 WHERE(PT<=XTT)
   ZBETA (:) = XBETAI
   ZGAM  (:) = XGAMI 
 ENDWHERE
ENDIF
!
PDPSAT(:) = ZBETA(:)/PT(:)**2 - ZGAM(:)/PT(:)
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DPSAT_1D',1,ZHOOK_HANDLE)
!
END FUNCTION DPSAT_1D
!-------------------------------------------------------------------------------
!     ######################################
      FUNCTION QSATW_0D(PT,PP) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, INTENT(IN)                :: PT     ! Temperature (Kelvin)
REAL, INTENT(IN)                :: PP     ! Pressure (Pa)
REAL                            :: PQSAT  ! saturation vapor 
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL                           :: ZFOES  ! saturation vapor 
                                                        ! pressure
                                                        ! (Pascal) 
!
REAL                           :: ZWORK1
REAL                           :: ZWORK2
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_0D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES  = PSAT(PT)
ZWORK1 = ZFOES/PP
ZWORK2 = XRD/XRV
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT = ZWORK2*ZWORK1 / (1.+(ZWORK2-1.)*ZWORK1)
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_0D',1,ZHOOK_HANDLE)
!
END FUNCTION QSATW_0D
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATW_1D(PT,PP) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN)                :: PT     ! Temperature
                                                        ! (Kelvin)
REAL, DIMENSION(:), INTENT(IN)                :: PP     ! Pressure
                                                        ! (Pa)
REAL, DIMENSION(SIZE(PT))                   :: PQSAT  ! saturation vapor 
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))                   :: ZFOES  ! saturation vapor 
                                                        ! pressure
                                                        ! (Pascal) 
!
REAL, DIMENSION(SIZE(PT))                   :: ZWORK1
REAL                                        :: ZWORK2 
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_1D',0,ZHOOK_HANDLE)
!
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES (:) = PSAT(PT(:))
ZWORK1(:) = ZFOES(:)/PP(:)
ZWORK2    = XRD/XRV
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT(:) = ZWORK2*ZWORK1(:) / (1.+(ZWORK2-1.)*ZWORK1(:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION QSATW_1D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATW_2D(PT,PP,KMASK,KL) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SNOW_PAR,  ONLY : XUNDEF
USE MODD_CSTS
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:,:), INTENT(IN)              :: PT     ! Temperature
                                                        ! (Kelvin)
REAL, DIMENSION(:,:), INTENT(IN)              :: PP     ! Pressure
                                                        ! (Pa)
!                                                        
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: KMASK
!                                                KMASK = Number of soil moisture layers (DIF option)
INTEGER,               INTENT(IN), OPTIONAL   :: KL
!                                                KL = Max number of soil moisture layers (DIF option)
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2))        :: PQSAT  ! saturation vapor 
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2)) :: ZFOES
!
INTEGER, DIMENSION(SIZE(PT,1))         :: IMASK
!
INTEGER         :: INL
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_2D',0,ZHOOK_HANDLE)
!
IF(PRESENT(KMASK).AND.PRESENT(KL))THEN
  IMASK(:)=KMASK(:)
  INL=KL
ELSE
  IMASK(:)=SIZE(PT,2)  
  INL=SIZE(PT,2)
ENDIF
!

PQSAT(:,:)=XUNDEF
ZFOES(:,:)=0.0
!
!  
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES(:,1:INL) = PSAT(PT(:,1:INL),IMASK(:))
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT(:,:) = XRD/XRV*ZFOES(:,:)/PP(:,:) / (1.+(XRD/XRV-1.)*ZFOES(:,:)/PP(:,:))  
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATW_2D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION QSATW_2D
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATSEAW_1D(PT,PP) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature over saline seawater
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!    The reduction due to salinity is compute with the factor 0.98 (reduction of 2%)
!!     
!!         es(T)= 0.98*EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!      Zeng, X., Zhao, M., and Dickinson, R. E., 1998 : Intercomparaison of bulk
!!      aerodynamic algorithm for the computation of sea surface fluxes using
!!      TOGA COARE and TAO data. Journal of Climate, vol 11, n°10, pp 2628--2644
!!
!!
!!    AUTHOR
!!    ------
!!      C. Lebeaupin    * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    6/04/2005 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN)                :: PT     ! Temperature
                                                        ! (Kelvin)
REAL, DIMENSION(:), INTENT(IN)                :: PP     ! Pressure
                                                        ! (Pa)
REAL, DIMENSION(SIZE(PT))                   :: PQSAT  ! saturation vapor 
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))                   :: ZFOES  ! saturation vapor 
                                                        ! pressure
                                                        ! (Pascal) 
!
REAL, DIMENSION(SIZE(PT))                   :: ZWORK1
REAL                                        :: ZWORK2
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATSEAW_1D',0,ZHOOK_HANDLE)
!
ZFOES (:) = PSAT(PT(:))
ZFOES (:) = 0.98*ZFOES(:)
! vapor pressure reduction of 2% over saline seawater could have a significant 
! impact on the computation of surface latent heat flux under strong wind 
! conditions (Zeng et al, 1998). 
!
ZWORK1(:) = ZFOES(:)/PP(:)
ZWORK2    = XRD/XRV
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT(:) = ZWORK2*ZWORK1(:) / (1.+(ZWORK2-1.)*ZWORK1(:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATSEAW_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION QSATSEAW_1D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATSEAW2_1D(PT,PP,PSSS) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature over saline seawater
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT) and salinity S (PSSS), the saturation vapor 
!!    pressure es(T,S) (FOES(PT,PSSS)) is computed following Weiss and Price
!!    (1980).
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : contains physical constants
!!      
!!    REFERENCE
!!    ---------
!!      Weiss, R.F., and Price, B.A., 1980 : Nitrous oxide solubility in water
!!      and seawater. Marine Chemistry, n°8, pp 347-359.
!!
!!
!!    AUTHOR
!!    ------
!!      S. Belamari     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    19/03/2014 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS, ONLY : XRD, XRV
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN)                :: PT     ! Temperature
                                                        ! (Kelvin)
REAL, DIMENSION(:), INTENT(IN)                :: PP     ! Pressure
                                                        ! (Pascal)
REAL, DIMENSION(:), INTENT(IN)                :: PSSS   ! Salinity
                                                        ! (g/kg)
REAL, DIMENSION(SIZE(PT))                   :: PQSAT  ! saturation vapor
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))                   :: ZFOES  ! saturation vapor
                                                        ! pressure
                                                        ! (Pascal)
!
REAL, DIMENSION(SIZE(PT))                   :: ZWORK1
REAL                                        :: ZWORK2
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATSEAW2_1D',0,ZHOOK_HANDLE)
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES(:) = EXP( 24.4543 -67.4509*(100.0/PT(:)) -4.8489*LOG(PT(:)/100.0)   &
                -5.44E-04*(PSSS(:)/1.00472) ) !see Sharqawy et al (2010) Eq32 p368
ZFOES(:) = ZFOES(:)*1013.25E+02             !convert from atm to Pa
!
ZWORK1(:) = ZFOES(:)/PP(:)
ZWORK2    = XRD/XRV
!
!*       2.    COMPUTE SATURATION SPECIFIC HUMIDITY
!              ------------------------------------
!
PQSAT(:) = ZWORK2*ZWORK1(:) / (1.0+(ZWORK2-1.0)*ZWORK1(:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATSEAW2_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION QSATSEAW2_1D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################################
      FUNCTION DQSATW_O_DT_1D(PT,PP,PQSAT) RESULT(PDQSAT)
!     ##############################################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!
!!      Finally, dqsat / dT  (T) is computed.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS       
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:),  INTENT(IN)             :: PT     ! Temperature
                                                          ! (Kelvin)
REAL, DIMENSION(:),  INTENT(IN)             :: PP     ! Pressure
                                                          ! (Pa)
REAL, DIMENSION(:),  INTENT(IN)             :: PQSAT  ! saturation vapor 
                                                          ! specific humidity
                                                          ! with respect to
                                                          ! water (kg/kg))
REAL, DIMENSION(SIZE(PT))                   :: PDQSAT ! derivative according
                                                          ! to temperature of
                                                          ! saturation vapor 
                                                          ! specific humidity
                                                          ! with respect to
                                                          ! water (kg/kg))
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))  :: ZFOES  ! saturation vapor 
                                                          ! pressure
                                                          ! (Pascal) 
!
REAL                       :: ZWORK1
REAL, DIMENSION(SIZE(PT))  :: ZWORK2
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DQSATW_O_DT_1D',0,ZHOOK_HANDLE)
!
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZWORK1    = XRD/XRV
ZFOES (:) = PP(:) / (1.+ZWORK1*(1./PQSAT(:)-1.))
ZWORK2(:) = DPSAT(PT(:))
!
!*       2.    DERIVATION ACCORDING TO TEMPERATURE
!              -----------------------------------
!
PDQSAT(:) = ZWORK2(:) * PQSAT(:) / (1.+(ZWORK1-1.)*ZFOES(:)/PP(:) )
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DQSATW_O_DT_1D',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION DQSATW_O_DT_1D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!     ##############################################################
      FUNCTION DQSATI_O_DT_1D(PT,PP,PQSAT) RESULT(PDQSAT)
!     ##############################################################
!
!!****  *QSATW * - function to compute saturation vapor humidity from
!!                 temperature (with respect to ice)
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPW) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAW) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMW) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!
!!      Finally, dqsat / dT  (T) is computed.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPW   : Constant for saturation vapor pressure function
!!        XBETAW  : Constant for saturation vapor pressure function
!!        XGAMW   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS       
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL,    DIMENSION(:), INTENT(IN)               :: PT     ! Temperature
                                                          ! (Kelvin)
REAL,    DIMENSION(:), INTENT(IN)               :: PP     ! Pressure
                                                          ! (Pa)
REAL,    DIMENSION(:), INTENT(IN)               :: PQSAT  ! saturation vapor 
                                                          ! specific humidity
                                                          ! with respect to
                                                          ! water (kg/kg))
REAL,    DIMENSION(SIZE(PT))                    :: PDQSAT ! derivative according
                                                          ! to temperature of
                                                          ! saturation vapor 
                                                          ! specific humidity
                                                          ! with respect to
                                                          ! water (kg/kg))
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))                       :: ZFOES  ! saturation vapor 
                                                          ! pressure
                                                          ! (Pascal) 
!
REAL                      :: ZWORK1
REAL, DIMENSION(SIZE(PT)) :: ZWORK2
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DQSATI_O_DT_1D',0,ZHOOK_HANDLE)
!
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZWORK1    = XRD/XRV
ZFOES (:) = PP(:) / (1.+ZWORK1*(1./PQSAT(:)-1.))
ZWORK2(:) = DPSAT(PT(:))
!
!*       2.    DERIVATION ACCORDING TO TEMPERATURE
!              -----------------------------------
!
PDQSAT(:) = ZWORK2(:) * PQSAT(:) / (1.+(ZWORK1-1.)*ZFOES(:)/PP(:) )
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:DQSATI_O_DT_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION DQSATI_O_DT_1D
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATI_1D(PT,PP) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATI * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPI) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAI) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMI) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPI   : Constant for saturation vapor pressure function
!!        XBETAI  : Constant for saturation vapor pressure function
!!        XGAMI   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS       
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:), INTENT(IN)                :: PT     ! Temperature
                                                        ! (Kelvin)
REAL, DIMENSION(:), INTENT(IN)                :: PP     ! Pressure
                                                        ! (Pa)
REAL, DIMENSION(SIZE(PT))                   :: PQSAT  ! saturation vapor 
                                                        ! specific humidity
                                                        ! with respect to
                                                        ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT))                   :: ZFOES  ! saturation vapor 
                                                        ! pressure
                                                        ! (Pascal) 
!
REAL, DIMENSION(SIZE(PT))                   :: ZWORK1
REAL                                        :: ZWORK2
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATI_1D',0,ZHOOK_HANDLE)
!
!
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES (:) = PSAT(PT(:))
ZWORK1(:) = ZFOES(:)/PP(:)
ZWORK2    = XRD/XRV
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT(:) = ZWORK2*ZWORK1(:) / (1.+(ZWORK2-1.)*ZWORK1(:))
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATI_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION QSATI_1D
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
!     ######################################
      FUNCTION QSATI_2D(PT,PP,KMASK,KL) RESULT(PQSAT)
!     ######################################
!
!!****  *QSATI * - function to compute saturation vapor humidity from
!!                 temperature
!!
!!    PURPOSE
!!    -------
!       The purpose of this function is to compute the saturation vapor 
!     pressure from temperature 
!      
!
!!**  METHOD
!!    ------
!!       Given temperature T (PT), the saturation vapor pressure es(T)
!!    (FOES(PT)) is computed by integration of the Clapeyron equation
!!    from the triple point temperature Tt (XTT) and the saturation vapor 
!!    pressure of the triple point es(Tt) (XESTT), i.e  
!!     
!!         es(T)= EXP( alphaw - betaw /T - gammaw Log(T) )
!!  
!!     with :
!!       alphaw (XALPI) = LOG(es(Tt))+ betaw/Tt + gammaw Log(Tt) 
!!       betaw (XBETAI) = Lv(Tt)/Rv + gammaw Tt
!!       gammaw (XGAMI) = (Cl -Cpv) /Rv
!!
!!      Then, the specific humidity at saturation is deduced.
!!  
!!
!!    EXTERNAL
!!    --------
!!      NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST : comtains physical constants
!!        XALPI   : Constant for saturation vapor pressure function
!!        XBETAI  : Constant for saturation vapor pressure function
!!        XGAMI   : Constant for saturation vapor pressure function  
!!      
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH 
!!
!!
!!    AUTHOR
!!    ------
!!      V. Masson       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/09/98 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SNOW_PAR,  ONLY : XUNDEF
USE MODD_CSTS       
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments and results
!
!
REAL, DIMENSION(:,:), INTENT(IN)            :: PT     ! Temperature
                                                      ! (Kelvin)
REAL, DIMENSION(:,:), INTENT(IN)            :: PP     ! Pressure
                                                      ! (Pa)
!                                                        
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL   :: KMASK
!                                                KMASK = Number of soil moisture layers (DIF option)
INTEGER,               INTENT(IN), OPTIONAL   :: KL
!                                                KL = Max number of soil moisture layers (DIF option)
!                                                      
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2))      :: PQSAT  ! saturation vapor 
                                                      ! specific humidity
                                                      ! with respect to
                                                      ! water (kg/kg)
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PT,1),SIZE(PT,2))      :: ZFOES  ! saturation vapor pressure (Pascal) 
!
INTEGER, DIMENSION(SIZE(PT,1)) :: IMASK
!
INTEGER         :: INL
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATI_2D',0,ZHOOK_HANDLE)
!
IF(PRESENT(KMASK))THEN
  IMASK(:)=KMASK(:)    
  INL=KL
ELSE
  IMASK(:)=SIZE(PT,2)  
  INL=SIZE(PT,2)
ENDIF
!
PQSAT(:,:)=XUNDEF
ZFOES(:,:)=0.0
!
!  
!*       1.    COMPUTE SATURATION VAPOR PRESSURE
!              ---------------------------------
!
ZFOES(:,1:INL) = PSAT(PT(:,1:INL),IMASK(:))
!
!*       2.    COMPUTE SATURATION HUMIDITY
!              ---------------------------
!
PQSAT(:,:) = XRD/XRV*ZFOES(:,:)/PP(:,:) / (1.+(XRD/XRV-1.)*ZFOES(:,:)/PP(:,:))  
!
!IF (LHOOK) CALL DR_HOOK('MODE_THERMOS:QSATI_2D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END FUNCTION QSATI_2D
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END MODULE MODE_THERMOS
