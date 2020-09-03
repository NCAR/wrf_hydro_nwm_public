!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############
      MODULE MODD_CSTS      
!     ###############
!
!!****  *MODD_CSTS* - declaration of Physic constants 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     Physics constants.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!      V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/05/94  
!!      J. Stein    02/01/95  add xrholw                    
!!      J.-P. Pinty 13/12/95  add XALPI,XBETAI,XGAMI
!!      J. Stein    25/07/97  add XTH00                    
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!      J. Escobar     06/13  add XSURF_TIMY XSURF_TIMY_12 XSURF_EPSILON for REAL*4
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
REAL,SAVE :: XPI                ! Pi
!
REAL,SAVE :: XDAY,XSIYEA,XSIDAY ! day duration, sideral year duration,
                                ! sideral day duration
!
REAL,SAVE :: XKARMAN            ! von karman constant
REAL,SAVE :: XLIGHTSPEED        ! light speed
REAL,SAVE :: XPLANCK            ! Planck constant
REAL,SAVE :: XBOLTZ             ! Boltzman constant 
REAL,SAVE :: XAVOGADRO          ! Avogadro number
!
REAL,SAVE :: XRADIUS,XOMEGA     ! Earth radius, earth rotation
REAL,SAVE :: XG                 ! Gravity constant
!
REAL,SAVE :: XP00               ! Reference pressure
!
REAL,SAVE :: XSTEFAN,XI0        ! Stefan-Boltzman constant, solar constant
!
REAL,SAVE :: XMD,XMV            ! Molar mass of dry air and molar mass of vapor
REAL,SAVE :: XRD,XRV            ! Gaz constant for dry air, gaz constant for vapor
REAL,SAVE :: XCPD,XCPV          ! Cpd (dry air), Cpv (vapor)
REAL,SAVE :: XRHOLW             ! Volumic mass of liquid water
REAL,SAVE :: XCL,XCI            ! Cl (liquid), Ci (ice)
REAL,SAVE :: XTT                ! Triple point temperature
REAL,SAVE :: XTTSI              ! Temperature of ice fusion over salty sea
REAL,SAVE :: XTTS               ! Equivalent temperature of ice fusion over a mixed of sea and sea-ice
REAL,SAVE :: XICEC              ! Threshold fraction over which the tile is considered as only covered with ice
REAL,SAVE :: XLVTT              ! Vaporization heat constant
REAL,SAVE :: XLSTT              ! Sublimation heat constant
REAL,SAVE :: XLMTT              ! Melting heat constant
REAL,SAVE :: XESTT              ! Saturation vapor pressure  at triple point
                                ! temperature  
REAL,SAVE :: XALPW,XBETAW,XGAMW ! Constants for saturation vapor 
                                !  pressure  function 
REAL,SAVE :: XALPI,XBETAI,XGAMI ! Constants for saturation vapor
                                !  pressure  function over solid ice
REAL, SAVE        :: XTH00      ! reference value  for the potential
                                ! temperature
REAL,SAVE :: XRHOLI             ! Volumic mass of ice
REAL,SAVE :: XCONDI             ! thermal conductivity of ice (W m-1 K-1)
!
INTEGER, SAVE :: NDAYSEC        ! Number of seconds in a day
!
REAL,SAVE     :: XSURF_TINY          ! minimum real on this machine
REAL,SAVE     :: XSURF_TINY_12       ! sqrt(minimum real on this machine)
REAL,SAVE     :: XSURF_EPSILON       ! minimum space with 1.0

REAL,SAVE     :: XZ0ICEZ0SNOW, XRHOTHRESHOLD_ICE


!
END MODULE MODD_CSTS

