!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
! ajoutEB
! correction de l'erreur interversion de XVTANG2 et XVTANG3
!######################
      MODULE MODD_SNOW_METAMO
!     ######################
!
!!****  *MODD_SNOW_METAMO* - declaration of parameters related
!!                          to snow metamorphism!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the metamorphism parameterization of snow.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!      V. Vionnet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2008                
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
! minimum snow layer thickness for thermal calculations.
! Used to prevent numerical problems as snow becomes vanishingly thin.
REAL, PARAMETER                 :: XSNOWDZMIN = 0.0001
!
! Optical diameter properties
REAL, PARAMETER                 :: XDIAET = 1.E-4
REAL, PARAMETER                 :: XDIAGF = 3.E-4
REAL, PARAMETER                 :: XDIAFP = 4.E-4
!
! Compaction/Settling Coefficients from Crocus v2.4
!
REAL, PARAMETER        :: XVVISC1 = 7.62237E6   ! pre-exponential viscosity factor (UNIT : N m-2 s)
REAL, PARAMETER        :: XVVISC3 = 0.023       ! density adjustement in the exponential correction for viscosity (UNIT : m3 kg-1)
REAL, PARAMETER        :: XVVISC4 = .1          ! temperature adjustement in the exponential correction for viscosity (UNIT : K-1)
REAL, PARAMETER        :: XVVISC5 = 1.          ! factor for viscosity adjustement to grain type - to be checked
REAL, PARAMETER        :: XVVISC6 = 60.         ! factor for viscosity adjustement to grain type - to be checked 
!                                                             (especially this one ; inconsistency with Crocus v2.4)
REAL, PARAMETER        :: XVVISC7 = 10.         ! factor for viscosity adjustement to grain type - to be checked
REAL, PARAMETER        :: XVRO11 = 250.  ! normalization term for density dependence of the viscosity calculation (UNIT : kg m-3)
! 
! Maximum value for TPSNOW%GRAN2
REAL, PARAMETER                 :: XVGRAN1 = 99.
REAL, PARAMETER                 :: XGRAN = 99.
!
INTEGER, PARAMETER            :: NVHIS1 = 1                
INTEGER, PARAMETER            :: NVHIS2 = 2                
INTEGER, PARAMETER            :: NVHIS3 = 3                
INTEGER, PARAMETER            :: NVHIS4 = 4                
INTEGER, PARAMETER            :: NVHIS5 = 5 
!
! Properties of fresh snow
REAL, PARAMETER                 :: XNDEN1 = 17.12
REAL, PARAMETER                 :: XNDEN2 = 128.
REAL, PARAMETER                 :: XNDEN3 = -20.
REAL, PARAMETER                 :: XNSPH1 = 7.87
REAL, PARAMETER                 :: XNSPH2 = 38.
REAL, PARAMETER                 :: XNSPH3 = 50.
REAL, PARAMETER                 :: XNSPH4 = 90.
!
REAL, PARAMETER                 :: XUEPSI = 1.E-8
REAL, PARAMETER                 :: XEPSI = 1.E-8
REAL, PARAMETER                 :: XUPOURC = 100.
!
! Parameters for Marbouty's function
!
REAL, PARAMETER                 :: XVTANG1 = 40.              
REAL, PARAMETER                 :: XVTANG2 = 6.              
REAL, PARAMETER                 :: XVTANG3 = 22.              
REAL, PARAMETER                 :: XVTANG4 = .7               
REAL, PARAMETER                 :: XVTANG5 = .3               
REAL, PARAMETER                 :: XVTANG6 = 6.               
REAL, PARAMETER                 :: XVTANG7 = 1.               
REAL, PARAMETER                 :: XVTANG8 = .8               
REAL, PARAMETER                 :: XVTANG9 = 16.              
REAL, PARAMETER                 :: XVTANGA = .2               
REAL, PARAMETER                 :: XVTANGB = .2               
REAL, PARAMETER                 :: XVTANGC = 18.             
REAL, PARAMETER                 :: XVRANG1 = 400.               
REAL, PARAMETER                 :: XVRANG2 = 150.              
REAL, PARAMETER                 :: XVGANG1 = 70.               
REAL, PARAMETER                 :: XVGANG2 = 25.              
REAL, PARAMETER                 :: XVGANG3 = 40.               
REAL, PARAMETER                 :: XVGANG4 = 50.               
REAL, PARAMETER                 :: XVGANG5 = .1               
REAL, PARAMETER                 :: XVGANG6 = 15.              
REAL, PARAMETER                 :: XVGANG7 = .1               
REAL, PARAMETER                 :: XVGANG8 = .55              
REAL, PARAMETER                 :: XVGANG9 = .65              
REAL, PARAMETER                 :: XVGANGA = .2               
REAL, PARAMETER                 :: XVGANGB = .85              
REAL, PARAMETER                 :: XVGANGC = .15      
!
! Parameters for snow metamorphism
!
REAL, PARAMETER                 :: XVDENT1 = 2314.81481
REAL, PARAMETER                 :: XVDENT2 = 7.2338E-7 
REAL, PARAMETER                 :: XVGRAN6 = 51.              
REAL, PARAMETER                 :: XVVAP1 = -6000.            
REAL, PARAMETER                 :: XVVAP2 = .4                
REAL, PARAMETER                 :: XVDIAM1 = 4.E-4               
REAL, PARAMETER                 :: XVDIAM2 = 5.E-4              
REAL, PARAMETER                 :: XVDIAM3 = 3.E-4               
REAL, PARAMETER                 :: XVDIAM4 = 2.E-4
REAL, PARAMETER                 :: XVDIAM5 = 1.E-4
REAL, PARAMETER                 :: XVDIAM6 = 1.E-4               
REAL, PARAMETER                 :: XVSPHE1 = 1.               
REAL, PARAMETER                 :: XVSPHE2 = 11574.074             
REAL, PARAMETER                 :: XVSPHE3 = .5               
REAL, PARAMETER                 :: XVSPHE4 = .1               
REAL, PARAMETER                 :: XVTAIL1 = 1.28E-17          
REAL, PARAMETER                 :: XVTAIL2 = 4.22E-19         
REAL, PARAMETER                 :: XVGRAT1 = 5.              
REAL, PARAMETER                 :: XVGRAT2 = 15.             
REAL, PARAMETER                 :: XVFI = 1.0417E-9                 
REAL, PARAMETER                 :: XVTELV1 = 0.005 
!
INTEGER,PARAMETER               :: NVDENT1 = 3
!
INTEGER :: NVARDIMS !number of dimensions of netcdf input variable
INTEGER :: NLENDIM1,NLENDIM2,NLENDIM3
INTEGER :: NID_VAR ! Netcdf IDs for  variable
!
INTEGER :: NID_FILE
REAL, DIMENSION(:,:,:), POINTER :: XDRDT0,XTAU,XKAPPA   ! field read
!
END MODULE MODD_SNOW_METAMO



