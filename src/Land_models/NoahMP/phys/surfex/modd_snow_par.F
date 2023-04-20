!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_SNOW_PAR
!     ######################
!
!!****  *MODD_SNOW_PAR* - declaration of parameters related
!!                          to the snow parameterization
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the
!     parameters related to the surface parameterization of snow.
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
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!! P. Samuelsson  10/2014   MEB complements
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!--------------------------------------------------------------------------------
! Snow on the ground: Given in ini_surf_csts and/or in NAM_SURF_CSTS
!--------------------------------------------------------------------------------
!
! Snow emissivity:
!
!REAL, SAVE       :: XEMISSN
REAL, PARAMETER       :: XEMISSN = 0.99   ! trude. Check  how this stands up to ini_sfc_csts. 
!
! Minimum and maximum values of the albedo of snow:
!
!REAL, SAVE       :: XANSMIN
!REAL, SAVE       :: XANSMAX
REAL, PARAMETER     :: XANSMIN=0.50    ! (orig 0.50, trude test with 0.55 (0.6))   ! trude
REAL, PARAMETER     :: XANSMAX =0.85  !trude


!
! Minimum and maximum values of the albedo of permanet snow/ice:
!
!REAL, SAVE       :: XAGLAMIN
!REAL, SAVE       :: XAGLAMAX
REAL, PARAMETER       :: XAGLAMIN = 0.8   ! trude
REAL, PARAMETER       :: XAGLAMAX = 0.85  ! trude

!
! Use recommended settings for snow albedo (FALSE = ISBA default)
!
LOGICAL,SAVE     :: LMEBREC
!
! Fraction of maximum value of the albedo of snow that is reached for melting
! snow
!
REAL, SAVE       :: XANSFRACMEL
!
! Threeshold temperature above which the snow albedo starts to decrease
!
REAL, SAVE       :: XTEMPANS
!
! Minimum value of the albedo of snow reached under canopy vegetation:
!
REAL, SAVE       :: XANSMINMEB
!
! Prescribed ice albedo in 3 spectral bands for glacier simulation with CROCUS scheme.
!REAL, SAVE       :: XALBICE1,XALBICE2,XALBICE3
!REAL, PARAMETER       :: XALBICE1 = 0.23   ! (0.23 from Gerbaux,  0.38 orig crocus)     ! trude 
!REAL, PARAMETER       :: XALBICE2 = 0.15   ! (0.15 from Gerbaux,  0.23  orig crocus)  ! trude
!REAL, PARAMETER       :: XALBICE3 = 0.06  ! 0.06 from Gerbaux ,   0.08 orig crocus     ! trude
REAL, PARAMETER       :: XALBICE1 = 0.38   ! (0.23 from Gerbaux,  0.38 orig crocus)     ! trude 
REAL, PARAMETER       :: XALBICE2 = 0.23   ! (0.15 from Gerbaux,  0.23  orig crocus)  ! trude
REAL, PARAMETER       :: XALBICE3 = 0.08  ! 0.06 from Gerbaux ,   0.08 orig crocus     ! trude

!REAL, PARAMETER       :: XALBICE1 = 0.67   ! (0.23 from Gerbaux,  0.38 orig crocus)     ! trude test snowalbedo instead
!REAL, PARAMETER       :: XALBICE2 = 0.67   ! (0.15 from Gerbaux,  0.23  orig crocus)  ! trude
!REAL, PARAMETER       :: XALBICE3 = 0.67  ! 0.06 from Gerbaux ,   0.08 orig crocus     ! trude

!

! Density threshold for ice detection in CROCUS scheme.
!REAL, SAVE       :: XRHOTHRESHOLD_ICE   ! trude comment out in modd_csts

!for ageing effects
!REAL, SAVE      :: XVAGING_NOGLACIER, XVAGING_GLACIER
REAL, PARAMETER      :: XVAGING_NOGLACIER = 60.
REAL, PARAMETER      ::  XVAGING_GLACIER  = 900.

! percentage of the total pore volume to compute the max liquid water holding capacity
!REAL, SAVE      :: XPERCENTAGEPORE
REAL, PARAMETER      :: XPERCENTAGEPORE = 0.03   !   5   ! trude try 3 instead of 5.
!REAL, PARAMETER      :: XPERCENTAGEPORE = 0.05   !   5   ! trude try 3 instead of 5.
! Height (m) of aged snow in glacier case (allows Pn=1)
!
REAL, SAVE       :: XHGLA
!
! Coefficient for calculation of snow fraction over vegetation
!
REAL, SAVE       :: XWSNV
!
! Roughness length of pure snow surface (m)
!
!REAL, SAVE       :: XZ0SN
!REAL, PARAMETER       :: XZ0SN =0.00005  ! trude,  (0.05 mm)
!REAL, PARAMETER        :: XZ0SN =0.00013  ! trude,  (0.13 mm)
REAL, PARAMETER        :: XZ0SN =0.001  ! trude,  (1.0 mm)
!REAL, PARAMETER        :: XZ0SN =0.0002  ! trude,  (0.2 mm)
! Roughness length for heat of pure snow surface (m)
!
!REAL, SAVE       :: XZ0HSN
REAL, PARAMETER       :: XZ0HSN   =0.00015   ! trude (test 5 mm instead of 0.5 mm)
!REAL, PARAMETER       :: XZ0HSN   =0.0001   ! orignal in surfex is 0.0001


! Roughness length ratio between ice and snow
!REAL, SAVE       :: XZ0ICEZ0SNOW     ! trude comment out. declared in modd_csts
!
! Snow Melt timescale with D95 (s): needed to prevent time step
! dependence of melt when snow fraction < unity.
!
REAL, SAVE       :: XTAU_SMELT
!
!--------------------------------------------------------------------------------
! Snow on the ground: PARAMETER
!--------------------------------------------------------------------------------
!
! Critical value of the equivalent water content
! of the snow reservoir for snow fractional coverage and albedo computations
!
REAL, PARAMETER       :: XWCRN      = 10.0   ! (kg m-2) Veg (default value)
REAL, PARAMETER       :: XWCRN_EXPL =  1.0   ! (kg m-2) Veg explicit
REAL, PARAMETER       :: XWCRN_ROOF =  1.0   ! (kg m-2)  Roofs
REAL, PARAMETER       :: XWCRN_ROAD =  1.0   ! (kg m-2)  Roads
REAL, PARAMETER       :: XWCRN_VEG  =  1.0   ! (kg m-2)  Urban veg
!
! Critical value of the total snow depth for ground snow fractional coverage
!
REAL, PARAMETER       :: XDCRN_EXPL = 0.01  ! (m) Veg explicit
!
! Critical value of snow emissivity
!
REAL, PARAMETER       :: XEMCRIN = 0.98
!
! Minimum and maximum values of the albedo of snow:
!
REAL, PARAMETER       :: XANSMIN_ROOF = 0.30 ! (-)   Roofs
REAL, PARAMETER       :: XANSMIN_ROAD = 0.15 ! (-)   Roads
!
REAL, PARAMETER       :: XANSMAX_ROOF = 0.85 ! (-)   Roofs
REAL, PARAMETER       :: XANSMAX_ROAD = 0.85 ! (-)   Roads
!
! Snow aging coefficients (albedo and Force-Restore density):
!
REAL, PARAMETER       :: XANS_TODRY    = 0.008     ! (-) Veg (default value)
REAL, PARAMETER       :: XANS_TODRY_ROOF = 0.008   ! (-)  Roofs
REAL, PARAMETER       :: XANS_TODRY_ROAD = 0.008   ! (-)  Roads
REAL, PARAMETER       :: XANS_TODRY_MEB  = 0.016   ! (-) Surface under canopy vegetation
!
REAL, PARAMETER       :: XANS_T        = 0.240     ! (-) Veg (default value)
REAL, PARAMETER       :: XANS_T_ROOF     = 0.174   ! (-)  Roofs
REAL, PARAMETER       :: XANS_T_ROAD     = 0.174   ! (-)  Roads (alley simul)
REAL, PARAMETER       :: XANS_T_MEB    = 0.480     ! (-) Surface under canopy vegetation
!
! Minimum and maximum values of the density of snow
! for Force-Restore snow option
!
REAL, PARAMETER       :: XRHOSMIN = 100.       ! (kg m-3)   Veg (Default value)
REAL, PARAMETER       :: XRHOSMIN_ROOF = 100.  ! (kg m-3)   Roofs
REAL, PARAMETER       :: XRHOSMIN_ROAD = 100.  ! (kg m-3)   Roads
!
REAL, PARAMETER       :: XRHOSMAX = 300.       ! (kg m-3)   Veg (Default value)
REAL, PARAMETER       :: XRHOSMAX_ROOF = 300.  ! (kg m-3)   Roofs
REAL, PARAMETER       :: XRHOSMAX_ROAD = 350.  ! (kg m-3)   Roads
!
! Minimum and maximum values of the density of snow
! for ISBA-ES snow option
!
REAL, PARAMETER       :: XRHOSMIN_ES =  50.  ! (kg m-3)
REAL, PARAMETER       :: XRHOSMAX_ES = 750.  ! (kg m-3)
!
! ISBA-ES Critical snow depth at which snow grid thicknesses constant
!
REAL, PARAMETER       :: XSNOWCRITD = 0.03  ! (m)
!
! ISBA-ES Minimum total snow depth for thermal calculations.
! Used to prevent numerical problems as snow becomes vanishingly thin.
!
REAL, PARAMETER      :: XSNOWDMIN = 0.000001  ! (m)
!
! Maximum Richardson number limit for very stable conditions using the ISBA-ES 'RIL' option
!
REAL, PARAMETER       :: X_RI_MAX = 0.20
!REAL, PARAMETER       :: X_RI_MAX = 0.026  !  trude test lower limit
!
! ISBA-ES Maximum snow liquid water holding capacity (fraction by mass) parameters:
!
REAL, PARAMETER       :: XWSNOWHOLDMAX2   = 0.10  ! (-)
REAL, PARAMETER       :: XWSNOWHOLDMAX1   = 0.03  ! (-)
REAL, PARAMETER       :: XSNOWRHOHOLD     = 200.0 ! (kg/m3)
!
! ISBA-ES arameters for grain size computation :
!
REAL, PARAMETER       :: XSNOW_AGRAIN = 1.6e-4   ! (m)
REAL, PARAMETER       :: XSNOW_BGRAIN = 1.1e-13  ! (m13/kg4)
REAL, PARAMETER       :: XSNOW_CGRAIN = 0.5e-4   ! (m)
REAL, PARAMETER       :: XDSGRAIN_MAX = 2.796e-3 ! m
!
!--------------------------------------------------------------------------------
! Calibration coefficients for CROCUS and ES albedo computation
!--------------------------------------------------------------------------------
!
REAL, PARAMETER :: XD1 = 1., XD2 = 3., XD3 = 4., XX = 99., &
                   XVALB2 = .96, XVALB3 = 1.58, XVALB4 = .92, XVALB5 = .90, &
                   XVALB6 = 15.4, XVALB7 = 346.3, XVALB8 = 32.31, XVALB9 = .88, &
                   XVALB10 = .200, XVALB11 = .65, XVDIOP1 = 2.3E-3, XVRPRE1 = .5, &
                   XVRPRE2=1.5

! trude , test XVALB11 = 0.7 instead of 0.6
! trude , test XVALB11 = 0.65 instead of 0.6

! for ageing effects:
REAL, PARAMETER :: XVPRES1 = 87000.
!
! spectral bands
!
INTEGER, PARAMETER :: NSPEC_BAND_SNOW = 3
!
! for spectral distribution and thickness effects
REAL, PARAMETER :: XVSPEC1 = .71, XVSPEC2 = .21, XVSPEC3 = .08
!
! for thickness effects
REAL, PARAMETER :: XVW1 = .80, XVW2 = .20 , XVD1 = .02, XVD2 = .01
!
!--------------------------------------------------------------------------------
! calibration coefficients for exctinction computation
REAL, PARAMETER :: XVBETA1 = 1.92E-3, XVBETA2 = 40., XVBETA3 = 1.098E-2, &
                   XVBETA4 = 100.,  XVBETA5 = 2000.
!
! ISBA-ES minimum cosinus of zenithal angle
REAL, PARAMETER :: XMINCOSZEN = 0.01
!
!--------------------------------------------------------------------------------
! ISBA-ES Thermal conductivity coefficients from Anderson (1976):
! see Boone, Meteo-France/CNRM Note de Centre No. 70 (2002)
!
REAL, PARAMETER :: XSNOWTHRMCOND1 = 0.02    ! [W/(m K)]
REAL, PARAMETER :: XSNOWTHRMCOND2 = 2.5E-6  ! [W m5/(kg2 K)]
!
! ISBA-ES Thermal conductivity: Implicit vapor diffn effects
! (sig only for new snow OR high altitudes)
! from Sun et al. (1999): based on data from Jordan (1991)
! see Boone, Meteo-France/CNRM Note de Centre No. 70 (2002)
!
REAL, PARAMETER :: XSNOWTHRMCOND_AVAP = -0.06023 ! [W/(m K)]
REAL, PARAMETER :: XSNOWTHRMCOND_BVAP = -2.5425  ! (W/m)
REAL, PARAMETER :: XSNOWTHRMCOND_CVAP = -289.99  ! (K)
!
! Crocus thermal conducitivity coefficient from Yen (1981)
REAL, PARAMETER :: XVRKZ6 = 1.88
!
!--------------------------------------------------------------------------------
! ISBA-ES CROCUS (Pahaut 1976): snowfall density coefficients:
!
REAL, PARAMETER :: XSNOWFALL_A_SN = 109.0  ! kg/m3
REAL, PARAMETER :: XSNOWFALL_B_SN =   6.0  ! kg/(m3 K)
REAL, PARAMETER :: XSNOWFALL_C_SN =  26.0  ! kg/(m7/2 s1/2)
!
! Coefficients for the optimal vertical grid calculation
REAL, PARAMETER :: XDZ1 = 0.01
REAL, PARAMETER :: XDZ2 = 0.0125
REAL, PARAMETER :: XDZ3 = 0.015
REAL, PARAMETER :: XDZ3_BIS = 0.03
REAL, PARAMETER :: XDZ4 = 0.04
REAL, PARAMETER :: XDZ5 = 0.05
REAL, PARAMETER :: XDZ_BASE = 0.02
REAL, PARAMETER :: XDZ_INTERNAL = 0.07
REAL, PARAMETER :: XSCALE_CM = 100.
REAL,DIMENSION(5), PARAMETER :: XDZMAX_INTERNAL = (/0.5,1.,2.,4.,10./)
REAL, PARAMETER :: XDZMIN_TOP_EXTREM = 0.0001
!
! Below this threshold of snowfall, new snowfall are aggregated with surface layer to avoid numerical problems
! (0.03 mm/h)
REAL,PARAMETER :: XSNOWFALL_THRESHOLD = 0.0333/3600.

! The ratio between a new surface layer thickness and the second layer surface thickness is limited to 1/10
REAL,PARAMETER :: XRATIO_NEWLAYER = 0.1

! Coefficients for cases with very thick snowpacks
REAL, PARAMETER :: XDEPTH_THRESHOLD1 = 3.
REAL, PARAMETER :: XDEPTH_THRESHOLD2 = 20.
REAL, PARAMETER :: XDEPTH_SURFACE = 3.
!
! Coefficients for computing the difference in 2 snow layer characteristics
REAL, PARAMETER :: XDIFF_1 = 20.
REAL, PARAMETER :: XDIFF_MAX = 200.
REAL, PARAMETER :: XSCALE_DIFF = 25.
!
! Coeefficients for snow layer splitting
REAL, PARAMETER :: XDZMIN_TOP = 0.01
REAL, PARAMETER :: XDZMIN_TOP_BIS = 0.005
REAL, PARAMETER :: XDZMIN_BOT = 0.02
REAL, PARAMETER :: XSPLIT_COEF = 8.
!
! Coeefficients for snow layer agregation
REAL, PARAMETER :: XAGREG_COEF_1 = 5.
REAL, PARAMETER :: XAGREG_COEF_2 = 4.5
!
!--------------------------------------------------------------------------------
!
! Calibration coefficients
REAL, PARAMETER :: XVTIME = 48*3600. ! characteristic time for
!compaction and metamorphism by wind drift
!
REAL, PARAMETER :: XVROMAX = 350. !  maximum density for
! drift compaction     UNIT : kg m-3
REAL, PARAMETER :: XVROMIN = 50.  !  minimum density for
! mobility computation UNIT : kg m-3
REAL, PARAMETER :: XVMOB1 = 0.295  !  coefficient for computing
! the mobility index
REAL, PARAMETER :: XVMOB2 = 0.833  !  coefficient for computing
! the mobility index
REAL, PARAMETER :: XVMOB3 = 0.583  !  coefficient for computing
! the mobility index
REAL, PARAMETER :: XVMOB4 = -0.0583 !  coefficient for computing
! the mobility index
REAL, PARAMETER :: XVDRIFT1 = 2.868 !  coefficient for computing
! the drift index
REAL, PARAMETER :: XVDRIFT2 = 0.085 !  coefficient for computing
! the drift index
REAL, PARAMETER :: XVDRIFT3 = 3.25  !  coefficient for computing
! the drift index
REAL, PARAMETER :: XVSIZEMIN = 3.E-4 !  minimum size decrease
! by drift  UNIT = m
!
! modif_EB pour sublim
! a pour but de tenir compte du fait que le vent moyen est > rafales
! on en tient compte egalement pour diminuer la duree de l'effet
REAL, PARAMETER :: XCOEF_FF = 1.25 ! coefficient for gust diagnosis from average wind 
REAL, PARAMETER :: XCOEF_EFFECT = 1.0 ! coefficient for impact on density du drift
REAL, PARAMETER :: XQS_REF = 2.E-5 ! valeur de reference de ZQS pour effet neige
!
!--------------------------------------------------------------------------------
!
! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: XSGCOEF1  = (/0.25, 0.50, 0.25/)
REAL, PARAMETER, DIMENSION(2)     :: XSGCOEF2  = (/0.05, 0.34/)
REAL, PARAMETER, DIMENSION(10)    :: XSGCOEF3  = (/0.025, 0.033, 0.043, &
                                     0.055, 0.071, 0.091, 0.117, 0.150, &
                                     0.193, 0.247/)
!
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: XSNOWTRANS = 0.20                ! (m)
REAL, PARAMETER                   :: XSNOWTRANS1 = 0.40                ! (m)
REAL, PARAMETER                   :: XSNOWTRANS2 = 0.6061                ! (m)
REAL, PARAMETER                   :: XSNOWTRANS3 = 0.7143               ! (m)
REAL, PARAMETER                   :: XSNOWTRANS4 = 0.9259                ! (m)
REAL, PARAMETER                   :: XSNOWTRANS5 = 1.4493                ! (m)

! * qsat computation
!
 CHARACTER(LEN=3), PARAMETER :: CQSAT = 'NEW' ! qsat computation option
!                         ! 'OLD' = do not depend on temperature
!                         ! 'NEW' = qsat and qsati merged (recommended)
!
REAL,    PARAMETER :: XUNDEF = 1.E+20


!
!------------------------------------------------------------------------------
!
END MODULE MODD_SNOW_PAR
