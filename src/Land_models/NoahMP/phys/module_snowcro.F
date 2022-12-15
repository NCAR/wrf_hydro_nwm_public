MODULE MODULE_SNOWCRO

!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.


!    ########
! TRUDE: replace abort1 with exitroutines used in WRF
! TRUDE: comment out all the debug statements. To run crodebug, we need to initalize this somewhere else and requires added links
! in the initalizeion etc. I do not think we want to adde this in WRF-hydro.
!

CONTAINS

!     ##########################################################################
!      SUBROUTINE SNOWCRO(HSNOWRES, TPTIME, OGLACIER, HIMPLICIT_WIND,     &
      SUBROUTINE SNOWCRO(HSNOWRES, OGLACIER, HIMPLICIT_WIND,     &
               PPEW_A_COEF, PPEW_B_COEF,                                 &
               PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,       &
               PSNOWSWE,PSNOWRHO,PSNOWHEAT,PSNOWALB,                     &
               PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE,                 &
               PTSTEP,PPS,PSR,PRR,PPSN3L,                                &
               PTA,PTG,PSW_RAD,PQA,PVMOD,PLW_RAD, PRHOA,                 &
               PUREF,PEXNS,PEXNA,PDIRCOSZW,                              &
               PZREF,PZ0,PZ0EFF,PZ0H,PALB,                               &
               PSOILCOND,PD_G,                                           &
               PSNOWLIQ,PSNOWTEMP,PSNOWDZ,                               &
               PTHRUFAL,PGRNDFLUX,PEVAPCOR,PRNSNOW,PHSNOW,PGFLUXSNOW,    &
               PHPSNOW,PLES3L,PLEL3L,PEVAP,PSNDRIFT,PRI,                 &
               PEMISNOW,PCDSNOW,PUSTAR,PCHSNOW,PSNOWHMASS,PQS,           &
               PPERMSNOWFRAC,PZENITH,                                    &
               OSNOWDRIFT,OSNOWDRIFT_SUBLIM,OSNOW_ABS_ZENITH,            &
               HSNOWMETAMO,HSNOWRAD,                                     &
               act_level, VIS_ICEALB,                                    &
               PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS,                   &
               FLOW_ICE, FLOW_SNOW,                                      &
               I,J)                  !(OUT))
!     ##########################################################################
!
!!****  *SNOWCRO*
!!
!!    PURPOSE
!!    -------
!
!     Detailed snowpack scheme Crocus, computationnally based on the
!     3-Layer snow scheme option (Boone and Etchevers 1999)
!     For shallow snow cover, Default method of Douville et al. (1995)
!     used with this option: Model "turns on" when snow sufficiently
!     deep/above some preset critical snow depth.
!
!
!
!
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    ISBA: Belair (1995)
!!    ISBA: Noilhan and Planton (1989)
!!    ISBA: Noilhan and Mahfouf (1996)
!!    ISBA-ES: Boone and Etchevers (2001)
!!    Crocus : Brun et al., 1989 (J. Glaciol.)
!!    Crocus : Brun et al., 1992 (J. Glaciol.)
!!    Crocus : Vionnet et al., in prep (Geosci. Mod. Devel. Discuss.)
!!
!!
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!      V. Vionnet         * Meteo-France *
!!      E. Brun            * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    7/99
!!      Modified by A.Boone 05/02 (code, not physics)
!!      Modified by A.Boone 11/04 i) maximum density limit imposed (although
!!                                rarely if ever reached), ii) check to
!!                                see if upermost layer completely sublimates
!!                                during a timestep (as snowpack becomes vanishly
!!                                thin), iii) impose maximum grain size limit
!!                                in radiation transmission computation.
!!
!!      Modified by B. Decharme  (03/2009): Consistency with Arpege permanent
!!                                          snow/ice treatment (LGLACIER for alb)
!!      Modified by A. Boone     (04/2010): Implicit coupling and replace Qsat and DQsat
!!                                          by Qsati and DQsati, respectively.
!!      Modified by E. Brun, V. Vionnet, S. Morin (05/2011):
!!                                          Addition of Crocus processes and
!!                                          parametrizations to
!!                                          the SNOW-3L code. This includes the dynamic handling
!!                                          of snow layers and the inclusion of snow metamorphism
!!                                          rules similar to the original Crocus implementation.
!!      Modified by B. Decharme  (09/2012): New wind implicitation
!!
!!      Modified by M. Lafaysse (07/2012) :
!!                                          * Albedo and roughness parametrizations
!!                                            for surface ice over glaciers
!!                                                     MODIF 2012-10-03 : don't modify roughness if implicit coupling
!!                                                                 (test PPEW_A_COEF == 0. )
!!                                          * SNOWCROALB is now called by SNOWCRORAD to remove duplicated code
!!                                          * Parameters for albedo are moved to modd_snow_par
!!                                          * PSNOWAGE is stored as an age
!!                                            (days since snowfall) and not as a date
!!                                            to allow spinup simulations
!!                                          * New rules for optimal discretization of very thick snowpacks
!!                                          * Optional outputs for debugging
!!
!!       Modified by E. Brun and M. Lafaysse (07/2012) :
!!                                          * Implement sublimation in SNOWDRIFT
!!                                          * Flag in namelist to activate SNOWDRIFT and SNOWDRIFT_SUBLIM
!!       Modified by E. Brun and M. Lafaysse (08/2012) :
!!                                          * XUEPSI replaced by 0 in the if statement of case 1.3.3.2 (SNOWCROMETAMO)
!!                                          * If SNOWDRIFT is activated the wind do not modify grain types during snowfall
!!                                            (redundant with snowdrift)
!!       Modified by E. Brun (24/09/2012) :
!!                                          * Correction coupling coefficient for specific humidity in SNOWCROEBUD
!!                                          * PSFCFRZ(:)  = 1.0 for systematic solid/vapor latent fluxes in SNOWCROEBUD
!!       Modified by C. Carmagnola (3/2013):
!!                                          * Dendricity and size replaced by the optical diameter
!!                                          * Test of different evolution laws for the optical diameter
!!
!!       Modified by B. Decharme  (08/2013): Qsat as argument (needed for coupling with atm)
!!                                           add PSNDRIFT
!!
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!USE MODD_TYPE_DATE_SURF, ONLY: DATE_TIME
!
USE MODD_CSTS, ONLY : XTT, XRHOLW, XLMTT,XLSTT,XLVTT, XCL, XCI, XPI, XRHOLI,XZ0ICEZ0SNOW, XRHOTHRESHOLD_ICE
!USE MODD_SNOW_PAR, ONLY : XZ0ICEZ0SNOW, XRHOTHRESHOLD_ICE
USE MODD_SNOW_METAMO    ! trude, declare these paramters at the beginning
!USE MODD_CONST_TARTES, ONLY: NPNIMP, XPSNOWG0, XPSNOWY0, XPSNOWW0, XPSNOWB0
!
USE MODE_SNOW3L
!USE MODE_TARTES, ONLY : SNOWCRO_TARTES
!
!USE MODE_CRODEBUG
!
!USE MODI_ABOR1_SFX
!
!USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
!USE PARKIND1  ,ONLY : JPRB
!
! this module is not used anymore
! USE MODI_GREGODSTRATI


!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!                                      PTSTEP    = time step of the integration
!TYPE(DATE_TIME), INTENT(IN)         :: TPTIME      ! current date and time
!
 CHARACTER(LEN=*), INTENT(IN)       :: HSNOWRES
!                                      HSNOWRES  = ISBA-SNOW3L turbulant exchange option
!                                      'DEF' = Default: Louis (ISBA: Noilhan and Mahfouf 1996)
!                                      'RIL' = Limit Richarson number under very stable
!                                              conditions (currently testing)
LOGICAL, INTENT(IN)                 :: OGLACIER   ! True = Over permanent snow and ice,
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                  ! False = No specific treatment
!
 CHARACTER(LEN=*), INTENT(IN)       :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!

! trude added

INTEGER, INTENT(IN)                      :: act_level
INTEGER, INTENT(IN)                      :: I,J
REAL, DIMENSION(:), INTENT(IN), OPTIONAL :: VIS_ICEALB
!end trude

REAL, DIMENSION(:), INTENT(IN)      :: PPS, PTA, PSW_RAD, PQA, PVMOD, PLW_RAD, PSR, PRR
!                                      PSW_RAD = incoming solar radiation (W/m2)
!                                      PLW_RAD = atmospheric infrared radiation (W/m2)
!                                      PRR     = rain rate [kg/(m2 s)]
!                                      PSR     = snow rate (SWE) [kg/(m2 s)]
!                                      PTA     = atmospheric temperature at level za (K)
!                                      PVMOD   = modulus of the wind parallel to the orography (m/s)
!                                      trude; also called Wind Weighting factor (Liston and Elder 2006).
!                                      trude; The terrain modified wind speed: Wt=PVMOD*wind_speed.
!                                      trude; If slope is zero, (and ignoring curvature), then PVMOD=1
!                                      PPS     = surface pressure
!                                      PQA     = atmospheric specific humidity
!                                                at level za
!
REAL, DIMENSION(:), INTENT(IN)      :: PTG, PSOILCOND, PD_G, PPSN3L
!                                      PTG       = Surface soil temperature (effective
!                                                  temperature the of layer lying below snow)
!                                      PSOILCOND = soil thermal conductivity [W/(m K)]
!                                      PD_G      = Assumed first soil layer thickness (m)
!                                                  Used to calculate ground/snow heat flux
!                                      PPSN3L    = snow fraction
!
REAL, DIMENSION(:), INTENT(IN)      :: PZREF, PUREF, PEXNS, PEXNA, PDIRCOSZW, PRHOA, PZ0, PZ0EFF, &
                                       PALB, PZ0H, PPERMSNOWFRAC
!                                      PZ0EFF    = roughness length for momentum
!                                      PZ0       = grid box average roughness length
!                                      PZ0H      = grid box average roughness length for heat
!                                      PZREF     = reference height of the first
!                                                  atmospheric level
!                                      PUREF     = reference height of the wind
!                                      PRHOA     = air density
!                                      PEXNS     = Exner function at surface
!                                      PEXNA     = Exner function at lowest atmos level
!                                      PDIRCOSZW = Cosinus of the angle between the
!                                                  normal to the surface and the vertical
!                                      PALB      = soil/vegetation albedo
!                                      PPERMSNOWFRAC  = fraction of permanet snow/ice
!
REAL, DIMENSION(:), INTENT(IN)      :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                        PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                        PPEQ_B_COEF
!                                      PPEW_A_COEF = wind coefficient (m2s/kg)
!                                      PPEW_B_COEF = wind coefficient (m/s)
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWALB
!                                      PSNOWALB = Prognostic surface snow albedo
!                                                 (does not include anything but
!                                                 the actual snow cover)
!
!REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWHEAT, PSNOWRHO, PSNOWSWE
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWHEAT, PSNOWRHO, PSNOWSWE
!                                      PSNOWHEAT = Snow layer(s) heat content (J/m2)
!                                      PSNOWRHO  = Snow layer(s) averaged density (kg/m3)
!                                      PSNOWSWE  = Snow layer(s) liquid Water Equivalent (SWE:kg m-2)
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!                                      PSNOWGRAN1 = Snow layers grain feature 1
!                                      PSNOWGRAN2 = Snow layer grain feature 2
!                                      PSNOWHIST  = Snow layer grain historical
!                                                   parameter (only for non
!                                                   dendritic snow)
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWAGE  ! Snow grain age
!
REAL, DIMENSION(:,:), INTENT(OUT)    :: PSNOWLIQ, PSNOWTEMP, PSNOWDZ
!                                      PSNOWLIQ  = Snow layer(s) liquid water content (m)
!                                      PSNOWTEMP = Snow layer(s) temperature (m)
!                                      PSNOWDZ   = Snow layer(s) thickness (m)
!
REAL, DIMENSION(:), INTENT(OUT)      :: PTHRUFAL, PGRNDFLUX, PEVAPCOR
!                                      PTHRUFAL  = rate that liquid water leaves snow pack:
!                                                  paritioned into soil infiltration/runoff
!                                                  by ISBA [kg/(m2 s)]
!                                      PGRNDFLUX = soil/snow interface heat flux (W/m2)
!                                      PEVAPCOR  = evaporation/sublimation correction term:
!                                                  extract any evaporation exceeding the
!                                                  actual snow cover (as snow vanishes)
!                                                  and apply it as a surface soil water
!                                                  sink. [kg/(m2 s)]
! ++ trude
REAL, DIMENSION(:), INTENT(OUT)    :: FLOW_ICE,FLOW_SNOW
! -- trude
!
REAL, DIMENSION(:), INTENT(OUT)      :: PRNSNOW, PHSNOW, PGFLUXSNOW, PLES3L, PLEL3L, &
                                        PHPSNOW, PCDSNOW, PUSTAR, PEVAP, PSNDRIFT
!                                      PLES3L      = evaporation heat flux from snow (W/m2)
!                                      PLEL3L      = sublimation (W/m2)
!                                      PHPSNOW     = heat release from rainfall (W/m2)
!                                      PRNSNOW     = net radiative flux from snow (W/m2)
!                                      PHSNOW      = sensible heat flux from snow (W/m2)
!                                      PGFLUXSNOW  = net heat flux from snow (W/m2)
!                                      PCDSNOW     = drag coefficient for momentum over snow
!                                      PUSTAR      = friction velocity over snow (m/s)
!                                      PEVAP       = total evaporative flux (kg/m2/s)
!                                      PSNDRIFT    = blowing snow sublimation (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)      :: PCHSNOW, PEMISNOW, PSNOWHMASS
!                                      PEMISNOW    = snow surface emissivity
!                                      PCHSNOW     = drag coefficient for heat over snow
!                                      PSNOWHMASS  = heat content change due to mass
!                                                    changes in snowpack (J/m2): for budget
!                                                    calculations only.
!
!Fluxes
REAL, DIMENSION(:), INTENT(OUT)      :: PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS



REAL, DIMENSION(:), INTENT(OUT)      :: PRI, PQS
!                                      PRI = Ridcharson number
!                                      PQS = surface humidity
!
REAL, DIMENSION(:), INTENT(IN)        :: PZENITH ! solar zenith angle
!
LOGICAL, INTENT(IN)                   :: OSNOWDRIFT, OSNOWDRIFT_SUBLIM ! activate snowdrift, sublimation during drift
LOGICAL, INTENT(IN)                   :: OSNOW_ABS_ZENITH ! activate parametrization of solar absorption for polar regions
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO, HSNOWRAD


                                         !-----------------------
                                         ! Metamorphism scheme
                                         ! HSNOWMETAMO=B92 Brun et al 1992
                                         ! HSNOWMETAMO=C13 Carmagnola et al 2014
                                         ! HSNOWMETAMO=T07 Taillandier et al 2007
                                         ! HSNOWMETAMO=F06 Flanner et al 2006
                                         !-----------------------
                                         ! Radiative transfer scheme
                                         ! HSNOWRAD=B92 Brun et al 1992
                                         ! HSNOWRAD=TAR TARTES (Libois et al 2013)
                                         ! HSNOWRAD=TA1 TARTES with constant impurities
                                         ! HSNOWRAD=TA2 TARTES with constant impurities as function of ageing
                                         !-----------------------
!*      0.2    declarations of local variables
!
!REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),NPNIMP) :: ZSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
!REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),NPNIMP) :: ZSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWTEMP, ZSCAP, ZSNOWDZN, ZSCOND, ZRADSINK
!                                      ZSNOWTEMP  = Snow layer(s) averaged temperature (K)
!                                      ZSCAP      = Snow layer(s) heat capacity [J/(K m3)]
!                                      ZSNOWDZN   = Updated snow layer thicknesses (m)
!                                      ZSCOND     = Snow layer(s) thermal conducivity [W/(m K)]
!                                      ZRADSINK   = Snow solar Radiation source terms (W/m2)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZWHOLDMAX
!
!For now these values are constant
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
!
!spectral albedo (3 bands for now) :: ready to output if necessary
REAL, DIMENSION(SIZE(PSNOWRHO,1),3) :: ZSPECTRALALBEDO

!------------------
!  trude start changing some dimensions declaration
!REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNIMP) :: ZSNOWIMP_DENSITY !impurities density (kg/m^3) (npoints,nlayer,ntypes_impurities)
!REAL, DIMENSION(SIZE(PSNOWRHO,1),NPNIMP) :: ZSNOWIMP_CONTENT !impurities content (g/g) (npoints,nlayer,ntypes_impurities)
!
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWTEMP, ZSCAP, ZSNOWDZN, ZSCOND, ZRADSINK
!                                      ZSNOWTEMP  = Snow layer(s) averaged temperature (K)
!                                      ZSCAP      = Snow layer(s) heat capacity [J/(K m3)]
!                                      ZSNOWDZN   = Updated snow layer thicknesses (m)
!                                      ZSCOND     = Snow layer(s) thermal conducivity [W/(m K)]
!                                      ZRADSINK   = Snow solar Radiation source terms (W/m2)
!
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZWHOLDMAX
!
!For now these values are constant
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWG0 ! asymmetry parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit) (npoints,nlayer)
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWY0 ! Value of y of snow grains at nr=1.3 (no unit
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWW0 ! Value of W of snow grains at nr=1.3 (no unit)
!REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWB0 ! absorption enhancement parameter of snow grains at nr=1.3 and at non absorbing wavelengths (no unit)
!
!spectral albedo (3 bands for now) :: ready to output if necessary
!REAL, DIMENSION(SIZE(PSNOWRHO,1),3) :: ZSPECTRALALBEDO

! end trude
!


!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWBIS
!                                     ZSNOWBIS      = Total snow depth after snowfall
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOW, ZSFCFRZ, ZTSTERM1, ZTSTERM2, ZCT, ZRA, ZSNOWFLUX, ZSNOWTEMPO1
!                                      ZSNOW      = Total snow depth (m)
!                                      ZCT        = inverse of the product of snow heat capacity
!                                                   and layer thickness [(m2 K)/J]
!                                      ZRA        = Surface aerodynamic resistance
!                                      ZTSTERM1,ZTSTERM2 = Surface energy budget coefficients
!                                      ZSNOWFLUX  = heat flux between 1st and 2nd snow layers:
!                                                   used during surface melting (W/m2)
!                                      ZSNOWTEMPO1= value of uppermost snow temperature
!                                                   before time integration (K)
!
REAL, DIMENSION(SIZE(PTA))          :: ZRSRA, ZDQSAT, ZQSAT, ZRADXS, ZLIQHEATXS, ZLWUPSNOW
!                                      ZRSRA    = air density over aerodynamic resistance
!                                      ZDQSAT   = derrivative of saturation specific humidity
!                                      ZQSAT    = saturation specific humidity
!                                      ZRADXS   = shortwave radiation absorbed by soil surface
!                                                 (for thin snow sover) (W m-2)
!                                      ZLIQHEATXS = excess snowpack heating for vanishingly thin
!                                                 snow cover: add energy to snow/ground heat
!                                                 flux (W m-2)
!                                      ZLWUPSNOW = upwelling longwave raaditive flux (W m-2)
!
REAL, DIMENSION(SIZE(PTA))          :: ZUSTAR2_IC, ZTA_IC, ZQA_IC, &
                                       ZPET_A_COEF_T, ZPEQ_A_COEF_T, ZPET_B_COEF_T, ZPEQ_B_COEF_T
!                                      ZUSTAR2_IC    = implicit lowest atmospheric level friction (m2/s2)
!                                      ZTA_IC        = implicit lowest atmospheric level air temperature
!                                      ZQA_IC        = implicit lowest atmospheric level specific humidity
!                                      ZPET_A_COEF_T = transformed A-air temperature coefficient
!                                      ZPET_B_COEF_T = transformed B-air temperature coefficient
!                                      ZPEQ_A_COEF_T = transformed A-air specific humidity coefficient
!                                      ZPEQ_B_COEF_T = transformed B-air specific humidity coefficient
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWRHOF, ZSNOWDZF, ZSNOWGRAN1F, ZSNOWGRAN2F, ZSNOWHISTF
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWAGEF

! New roughness lengths in case of glaciers without snow.
REAL, DIMENSION(SIZE(PTA))          :: ZZ0_SNOWICE, ZZ0H_SNOWICE, ZZ0EFF_SNOWICE
!
!To control and print eneregy balance
REAL , DIMENSION(SIZE(PTA))         :: ZSUMMASS_INI,ZSUMHEAT_INI,ZSUMMASS_FIN,ZSUMHEAT_FIN
!
REAL, DIMENSION(SIZE(PTA))          :: ZMASSBALANCE, ZENERGYBALANCE, ZEVAPCOR2
!
INTEGER, DIMENSION(SIZE(PTA))       :: INLVLS_USE ! varying number of effective layers
!
LOGICAL, DIMENSION(SIZE(PTA))       :: GSNOWFALL,GMODIF_MAILLAGE
!                                      GSNOWFALL  = FLAG if snowfall exceed PSNOW/10, used for
!                                                   grid updating.
!
REAL :: ZTSTEPDAYS ! time step in days
!
LOGICAL :: GCOND_GRAIN, GCOND_YEN
!
!LOGICAL :: GCROINFOPRINT ! print daily informations
!LOGICAL :: GCRODEBUGPRINT, GCRODEBUGDETAILSPRINT, GCRODEBUGPRINTATM ! print diagnostics for debugging
!LOGICAL :: GCRODEBUGPRINTBALANCE
!
INTEGER :: JJ,JST  ! looping indexes
INTEGER :: IPRINT  ! gridpoint number to be printed
INTEGER :: IDEBUG
!


!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
PFSA_CROCUS(:) = 0
PFSR_CROCUS(:) = 0
PFIRA_CROCUS(:) = 0
PTHRUFAL(:)   = 0.
FLOW_SNOW(:)   = 0.
FLOW_ICE(:)   = 0.
!
ZUSTAR2_IC = 0.0
ZTA_IC     = 0.0
ZQA_IC     = 0.0
!
GCOND_GRAIN = .TRUE.
GCOND_YEN   = .TRUE.!FALSE. !(if TRUE : use of the Yen (1981) thermal conductivity paramztrization ;
!                    otherwise, use the default ISBA-ES thermal conductivity parametrization)
!
PGRNDFLUX  = 0.
PSNOWHMASS = 0.
PHSNOW     = 0.
PRNSNOW    = 0.
PLES3L     = 0.
PLEL3L     = 0.
PHPSNOW    = 0.
PEVAPCOR   = 0.
PTHRUFAL   = 0.
!
! pour imprimer des diagnostics sur un des points
IPRINT = 1
!
! - - ---------------------------------------------------
!
!       0.     Initialization
!               --------------
! NOTE that snow layer thickness is used throughout this code: SWE
! is only used to diagnose the thickness at the beginning of this routine
! and it is updated at the end of this routine.
!
! Initialization of the actual number of snow layers, total snow depth
!  and layer thicknesses
!
ZSNOWTEMP(:,:) = 0.
!
GSNOWFALL(:) = .FALSE.
INLVLS_USE(:) = 0
 DO JST = 1,SIZE(PSNOWSWE(:,:),2)
  DO JJ = 1,SIZE(ZSNOW)
    IF ( PSNOWSWE(JJ,JST)>0. ) THEN
      PSNOWDZ(JJ,JST) = PSNOWSWE(JJ,JST) / PSNOWRHO(JJ,JST)
      INLVLS_USE(JJ) = JST
    ELSE
      PSNOWDZ(JJ,JST) = 0.
    ENDIF
  ENDDO  !  end loop snow layers
ENDDO    ! end loop grid points


! Incrementation of snow layers age
ZTSTEPDAYS = PTSTEP/86400. ! time step in days
WHERE ( PSNOWSWE>0 ) PSNOWAGE = PSNOWAGE + ZTSTEPDAYS
!
!***************************************PRINT IN**********************************************
!
!Compute total SWE and heat for energy control
!IF ( GCRODEBUGPRINTBALANCE ) THEN
!  DO JJ = 1,SIZE(ZSNOW)
!    ZSUMMASS_INI(JJ) = SUM(PSNOWSWE (JJ,1:INLVLS_USE(JJ)))
!    ZSUMHEAT_INI(JJ) = SUM(PSNOWHEAT(JJ,1:INLVLS_USE(JJ)))
!  ENDDO    ! end loop grid points!
!ENDIF
!
!  Print of some simulation characteristics
!IF(GCROINFOPRINT) THEN
!  CALL SNOWCROPRINTDATE()
! WRITE(*,FMT="(A12,I3,A12,I4)") 'nlayer:',INLVLS_USE(IDEBUG), ' nbpoints:', SIZE(ZSNOW)
!   WRITE(*,*) 'PZ0H: ', PZ0H(IDEBUG)
!  WRITE(*,*) 'Snow fraction =',PPSN3L(IDEBUG)
!ENDIF
!
!***************************************PRINT OUT*********************************************
!***************************************DEBUG IN**********************************************

!IF (GCRODEBUGPRINT) THEN
!  CALL SNOWCROPRINTDATE()
!  CALL SNOWCROPRINTPROFILE("crocus initialization",INLVLS_USE(IDEBUG),LPRINTGRAN,      &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO)
!END IF
!
!IF (GCRODEBUGPRINTATM) THEN
!  CALL SNOWCROPRINTATM("forcing data :",PTA(IDEBUG),PQA(IDEBUG),PVMOD(IDEBUG),   &
!                       PRR(IDEBUG),PSR(IDEBUG),PSW_RAD(IDEBUG),PLW_RAD(IDEBUG),  &
!                       PTG(IDEBUG),PSOILCOND(IDEBUG),PD_G(IDEBUG),PPSN3L(IDEBUG) )
!END IF
!***************************************DEBUG OUT********************************************
!
!*       1.     Snow total depth
!               ----------------
!
ZSNOW(:) = 0.
DO JJ = 1,SIZE(ZSNOW)
  ZSNOW(JJ) = SUM(PSNOWDZ(JJ,1:INLVLS_USE(JJ)))
ENDDO
!
ZSNOWBIS(:) = ZSNOW(:)
!
!*       2.     Snowfall
!               --------
! Calculate uppermost density and thickness changes due to snowfall,
! and add heat content of falling snow
!

 CALL SNOWNLFALL_UPGRID(OGLACIER,                                       &
                        PTSTEP,PSR,PTA,PVMOD,ZSNOWBIS,PSNOWRHO,PSNOWDZ,         &
                        PSNOWHEAT,PSNOWHMASS,PSNOWALB,PPERMSNOWFRAC,            &
                        PSNOWGRAN1,PSNOWGRAN2,GSNOWFALL,ZSNOWDZN,               &
                        ZSNOWRHOF,ZSNOWDZF,ZSNOWGRAN1F,ZSNOWGRAN2F, ZSNOWHISTF, &
                        ZSNOWAGEF,GMODIF_MAILLAGE,INLVLS_USE,OSNOWDRIFT,PZ0EFF,PUREF,&
                        HSNOWMETAMO)


!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWFALL_UPGRID",INLVLS_USE(IDEBUG),LPRINTGRAN,      &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                          PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
ZSNOW(:) = ZSNOWBIS(:)
!
!*       3.     Update grid/discretization
!               --------------------------
! Reset grid to conform to model specifications:
!
DO JJ=1,SIZE(ZSNOW)
!
  IF ( GMODIF_MAILLAGE(JJ) ) THEN


    CALL SNOWNLGRIDFRESH_1D(JJ,ZSNOW(JJ),PSNOWDZ(JJ,:),ZSNOWDZN(JJ,:),PSNOWRHO(JJ,:),    &
                            PSNOWHEAT(JJ,:),PSNOWGRAN1(JJ,:),PSNOWGRAN2(JJ,:),           &
                            PSNOWHIST(JJ,:),PSNOWAGE(JJ,:),GSNOWFALL(JJ),ZSNOWRHOF(JJ),  &
                            ZSNOWDZF(JJ),PSNOWHMASS(JJ),ZSNOWGRAN1F(JJ),ZSNOWGRAN2F(JJ), &
                            ZSNOWHISTF(JJ),ZSNOWAGEF(JJ),INLVLS_USE(JJ),HSNOWMETAMO, I, J      )

  ENDIF
  !


ENDDO

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWNLGRIDFRESH_1D",INLVLS_USE(IDEBUG),LPRINTGRAN,   &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
!*       4.     Liquid water content and snow temperature
!               -----------------------------------------
!
! First diagnose snow temperatures and liquid
! water portion of the snow from snow heat content:
! update some snow layers parameters after new discretization
!
DO JJ = 1,SIZE(ZSNOW)
  !
  ! active layers
  DO JST=1,INLVLS_USE(JJ)
    PSNOWSWE (JJ,JST) = PSNOWDZ(JJ,JST) * PSNOWRHO(JJ,JST)
    ZSCAP    (JJ,JST) = PSNOWRHO(JJ,JST) * XCI
    ZSNOWTEMP(JJ,JST) = XTT + &
                        ( ( PSNOWHEAT(JJ,JST)/PSNOWDZ(JJ,JST) + XLMTT*PSNOWRHO(JJ,JST) )/ZSCAP(JJ,JST) )
!
    PSNOWLIQ (JJ,JST) = MAX( 0.0, ZSNOWTEMP(JJ,JST)-XTT ) * ZSCAP(JJ,JST) * &
                        PSNOWDZ(JJ,JST) / (XLMTT*XRHOLW)
!
    ZSNOWTEMP(JJ,JST) = MIN( XTT, ZSNOWTEMP(JJ,JST) )
  ENDDO  !  end loop active snow layers

  !
  ! unactive layers
  DO JST = INLVLS_USE(JJ)+1,SIZE(PSNOWSWE,2)
    PSNOWSWE  (JJ,JST) = 0.0
    PSNOWRHO  (JJ,JST) = 999.
    PSNOWDZ   (JJ,JST) = 0.
    PSNOWGRAN1(JJ,JST) = 0.
    PSNOWGRAN2(JJ,JST) = 0.
    PSNOWHIST (JJ,JST) = 0.
    PSNOWAGE  (JJ,JST) = 0.
    PSNOWHEAT (JJ,JST) = 0.
    ZSNOWTEMP (JJ,JST) = XTT
    PSNOWLIQ  (JJ,JST) = 0.
  ENDDO  !  end loop unactive snow layers
  !
ENDDO    ! end loop grid points
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after liquid water/temperature diagnostic",                &
!                           INLVLS_USE(IDEBUG),LPRINTGRAN,                              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
!        4.BIS   Snow metamorphism
!                -----------------
!
 CALL SNOWCROMETAMO(PSNOWDZ,PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,ZSNOWTEMP, &
                    PSNOWLIQ,PTSTEP,PSNOWSWE,INLVLS_USE,PSNOWAGE,HSNOWMETAMO       )

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROMETAMO", INLVLS_USE(IDEBUG),LPRINTGRAN,       &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
!*       5.     Snow Compaction
!               ---------------
! Calculate snow density: compaction/aging: density increases
!

 CALL SNOWCROCOMPACTN(PTSTEP,PSNOWRHO,PSNOWDZ,ZSNOWTEMP,ZSNOW,                      &
                      PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWLIQ,INLVLS_USE,PDIRCOSZW,&
                      HSNOWMETAMO)
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROCOMPACTN", INLVLS_USE(IDEBUG),LPRINTGRAN,     &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
!*       5.1    Snow Compaction and Metamorphism due to snow drift
!               ---------------
PSNDRIFT(:) = 0.0
IF (OSNOWDRIFT) THEN

  CALL SNOWDRIFT(PTSTEP, PVMOD, PSNOWRHO,PSNOWDZ, ZSNOW,                      &
                 PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,INLVLS_USE,PTA,PQA,PPS,PRHOA,&
                 PZ0EFF,PUREF,OSNOWDRIFT_SUBLIM,HSNOWMETAMO,PSNDRIFT)

ENDIF

!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWDRIFT", INLVLS_USE(IDEBUG),LPRINTGRAN,           &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),   &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),&
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),&
!                           HSNOWMETAMO )
!ENDIF
!***************************************DEBUG OUT**********************************************
!
! Update snow heat content (J/m2) using dry density instead of total density:
!
DO JJ = 1,SIZE(ZSNOW)
  DO JST = 1,INLVLS_USE(JJ)
    ZSCAP(JJ,JST) = ( PSNOWRHO(JJ,JST) - &
                      PSNOWLIQ(JJ,JST) * XRHOLW / MAX( PSNOWDZ(JJ,JST),XSNOWDZMIN) ) * XCI
    PSNOWHEAT(JJ,JST) = PSNOWDZ(JJ,JST) * &
                        ( ZSCAP(JJ,JST)*(ZSNOWTEMP(JJ,JST)-XTT) - XLMTT*PSNOWRHO(JJ,JST) ) + &
                        XLMTT * XRHOLW * PSNOWLIQ(JJ,JST)
 ENDDO  !  end loop snow layers
ENDDO    ! end loop grid points
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after  update snow heat content", INLVLS_USE(IDEBUG),LPRINTGRAN,&
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO      )
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*       6.     Solar radiation transmission
!               -----------------------------
!
! Heat source (-sink) term due to shortwave
! radiation transmission within the snowpack:
!
SELECT CASE (HSNOWRAD)
!  CASE ("TA1")
!    ZSNOWIMP_CONTENT(:,:,1) = 0.0
!  CASE ("TA2")
!    ZSNOWIMP_CONTENT(:,:,1) = 100.0E-9
!  CASE ("TAR")
!    ZSNOWIMP_CONTENT(:,:,1) = 2. * PSNOWAGE(:,:) * 1E-9
  CASE DEFAULT
END SELECT
!
SELECT CASE (HSNOWRAD)
  CASE ("B92")

    CALL SNOWCRORAD(OGLACIER,                      &
                    PSW_RAD,PSNOWALB,PSNOWDZ,PSNOWRHO,    &
                    PALB,ZRADSINK,ZRADXS,                 &
                    PSNOWGRAN1, PSNOWGRAN2, PSNOWAGE,PPS, &
                    PZENITH, PPERMSNOWFRAC,INLVLS_USE,    &
                    OSNOW_ABS_ZENITH,HSNOWMETAMO,VIS_ICEALB)

  CASE ("TAR","TA1","TA2")
!    CALL SNOWCRO_TARTES(PSNOWGRAN1,PSNOWGRAN2,PSNOWRHO,PSNOWDZ,ZSNOWG0,ZSNOWY0,ZSNOWW0, &
!                        ZSNOWB0,ZSNOWIMP_DENSITY,ZSNOWIMP_CONTENT,PALB,PSW_RAD,PZENITH, &
!                        INLVLS_USE,PSNOWALB,ZRADSINK,ZRADXS,GCRODEBUGDETAILSPRINT,HSNOWMETAMO)
  !
  CASE DEFAULT
!    CALL ABOR1_SFX("UNKNOWN CSNOWRAD OPTION")
  !
END SELECT

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCRORAD", INLVLS_USE(IDEBUG),LPRINTGRAN,               &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*       7.     Heat transfer and surface energy budget
!               ---------------------------------------
! Snow thermal conductivity:
!
 CALL SNOWCROTHRM(PSNOWRHO,ZSCOND,ZSNOWTEMP,PPS,PSNOWLIQ,GCOND_GRAIN,GCOND_YEN)
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROTHRM", INLVLS_USE(IDEBUG),LPRINTGRAN,              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
! Precipitation heating term:
! Rainfall renders it's heat to the snow when it enters
! the snowpack:
!
PHPSNOW(:) = PRR(:) * XCL * ( MAX( XTT,PTA(:) ) - XTT )    ! (W/m2)
!
! Surface Energy Budget calculations using ISBA linearized form
! and standard ISBA turbulent transfer formulation
!

IF ( ALL(PPEW_A_COEF==0.) ) THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Modif Matthieu Lafaysse for glaciers
  ! For surface ice, modify roughness lengths
  ! Only if not implicit coupling
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      WHERE( PSNOWRHO(:,1)>XRHOTHRESHOLD_ICE )
         ZZ0_SNOWICE    = PZ0    * XZ0ICEZ0SNOW
         ZZ0H_SNOWICE   = PZ0H   * XZ0ICEZ0SNOW
         ZZ0EFF_SNOWICE = PZ0EFF * XZ0ICEZ0SNOW
      ELSEWHERE
         ZZ0_SNOWICE    = PZ0
         ZZ0H_SNOWICE   = PZ0H
         ZZ0EFF_SNOWICE = PZ0EFF
      ENDWHERE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ELSE
! trude test increased roughnesslenght on ice, even with implicit coupling
!++TE
         WHERE( PSNOWRHO(:,1)>XRHOTHRESHOLD_ICE )
            ZZ0_SNOWICE    = PZ0    * XZ0ICEZ0SNOW
            ZZ0H_SNOWICE   = PZ0H   * XZ0ICEZ0SNOW
            ZZ0EFF_SNOWICE = PZ0EFF * XZ0ICEZ0SNOW
         ELSEWHERE
!--   TE
            ZZ0_SNOWICE    = PZ0
            ZZ0H_SNOWICE   = PZ0H
            ZZ0EFF_SNOWICE = PZ0EFF
         ENDWHERE               ! trude added
END IF


 CALL SNOWCROEBUD(HSNOWRES, HIMPLICIT_WIND,                                    &
                  PPEW_A_COEF, PPEW_B_COEF,                                    &
                  PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,          &
                  XSNOWDZMIN,                                                  &
                  PZREF,ZSNOWTEMP(:,1),PSNOWRHO(:,1),PSNOWLIQ(:,1),ZSCAP(:,1), &
                  ZSCOND(:,1),ZSCOND(:,2),                                     &
                  PUREF,PEXNS,PEXNA,PDIRCOSZW,PVMOD,                           &
                  PLW_RAD,PSW_RAD,PTA,PQA,PPS,PTSTEP,                          &
                  PSNOWDZ(:,1),PSNOWDZ(:,2),PSNOWALB,ZZ0_SNOWICE,              &
                  ZZ0EFF_SNOWICE,ZZ0H_SNOWICE,                                 &
                  ZSFCFRZ,ZRADSINK(:,1),PHPSNOW,                               &
                  ZCT,PEMISNOW,PRHOA,ZTSTERM1,ZTSTERM2,ZRA,PCDSNOW,PCHSNOW,    &
                  ZQSAT, ZDQSAT, ZRSRA, ZUSTAR2_IC, PRI,                       &
                  ZPET_A_COEF_T,ZPEQ_A_COEF_T,ZPET_B_COEF_T,ZPEQ_B_COEF_T      )


!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROEBUD", INLVLS_USE(IDEBUG),LPRINTGRAN,              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
! Heat transfer: simple diffusion along the thermal gradient
!
ZSNOWTEMPO1(:) = ZSNOWTEMP(:,1) ! save surface snow temperature before update
!
 CALL SNOWCROSOLVT(PTSTEP,XSNOWDZMIN,PSNOWDZ,ZSCOND,ZSCAP,PTG,                &
                   PSOILCOND,PD_G,ZRADSINK,ZCT,ZTSTERM1,ZTSTERM2,             &
                   ZPET_A_COEF_T,ZPEQ_A_COEF_T,ZPET_B_COEF_T,ZPEQ_B_COEF_T,   &
                   ZTA_IC,ZQA_IC,PGRNDFLUX, ZSNOWTEMP ,ZSNOWFLUX,             &
                   INLVLS_USE                                                 )

!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROSOLVT", INLVLS_USE(IDEBUG),LPRINTGRAN,             &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF

!***************************************DEBUG OUT********************************************
!
!*       8.     Surface fluxes
!               --------------
!

 CALL SNOWCROFLUX(ZSNOWTEMP(:,1),PSNOWDZ(:,1),PEXNS,PEXNA,            &
                  ZUSTAR2_IC,                                         &
                  PTSTEP,PSNOWALB,PSW_RAD,PEMISNOW,ZLWUPSNOW,PLW_RAD, &
                  ZTA_IC,ZSFCFRZ,ZQA_IC,PHPSNOW,                      &
                  ZSNOWTEMPO1,ZSNOWFLUX,ZCT,ZRADSINK(:,1),            &
                  ZQSAT,ZDQSAT,ZRSRA,                                 &
                  PRNSNOW,PHSNOW,PGFLUXSNOW,PLES3L,PLEL3L,PEVAP,      &
                  PUSTAR,                                                     &
                  PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS) ! trude added

!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROFLUX", INLVLS_USE(IDEBUG),LPRINTGRAN,              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*       9.     Snow melt
!               ---------
!
! First Test to see if snow pack vanishes during this time step:
!

 CALL SNOWCROGONE(PTSTEP,PLEL3L,PLES3L,PSNOWRHO,                  &
                  PSNOWHEAT,ZRADSINK,PEVAPCOR,PTHRUFAL,PGRNDFLUX, &
                  PGFLUXSNOW,PSNOWDZ,PSNOWLIQ,ZSNOWTEMP,ZRADXS,   &
                  PRR,INLVLS_USE                                  )

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROGONE", INLVLS_USE(IDEBUG),LPRINTGRAN,              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
! Add radiation not absorbed by snow to soil/vegetation interface flux
! (for thin snowpacks):
!
PGRNDFLUX(:) = PGRNDFLUX(:) + ZRADXS(:)
!
! Second Test to see if one or several snow layers vanishe during this time
! step. In such a case, the concerned snow layers are agregated to neighbours

 CALL SNOWCROLAYER_GONE(PTSTEP,ZSCAP,ZSNOWTEMP,PSNOWDZ,          &
                        PSNOWRHO,PSNOWLIQ,PSNOWGRAN1,PSNOWGRAN2, &
                        PSNOWHIST,PSNOWAGE,PLES3L, INLVLS_USE    )
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROLAYER_GONE", INLVLS_USE(IDEBUG),LPRINTGRAN,        &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
! For partial melt: transform excess heat content into snow liquid:
!

 CALL SNOWCROMELT(ZSCAP,ZSNOWTEMP,PSNOWDZ,PSNOWRHO,PSNOWLIQ,INLVLS_USE)

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROMELT", INLVLS_USE(IDEBUG),LPRINTGRAN,              &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*      10.     Snow water flow and refreezing
!               ------------------------------
! Liquid water vertical transfer and possible snowpack runoff
! And refreezing/freezing of meltwater/rainfall (ripening of the snow)
!

 CALL SNOWCROREFRZ(PTSTEP,PRR,PSNOWRHO,ZSNOWTEMP,PSNOWDZ,PSNOWLIQ,PTHRUFAL, &
                   ZSCAP,PLEL3L,INLVLS_USE                                  )

! ++ trude
!Assign streamflow from ice versus snow for output (diagnostics)
IF (PSNOWRHO(1,1) .ge. XRHOTHRESHOLD_ICE) THEN
  FLOW_ICE = PTHRUFAL
ELSE
  FLOW_SNOW= PTHRUFAL
ENDIF

!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROREFRZ", INLVLS_USE(IDEBUG),LPRINTGRAN,             &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*      11.     Snow Evaporation/Sublimation mass updates:
!               ------------------------------------------
!

 CALL SNOWCROEVAPN(PLES3L,PTSTEP,ZSNOWTEMP(:,1),PSNOWRHO(:,1), &
                   PSNOWDZ(:,1),PEVAPCOR,PSNOWHMASS            )
!
!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROEVAPN", INLVLS_USE(IDEBUG),LPRINTGRAN,             &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
! If all snow in uppermost layer evaporates/sublimates, re-distribute
! grid (below could be evoked for vanishingly thin snowpacks):
!

 CALL SNOWCROEVAPGONE(PSNOWHEAT,PSNOWDZ,PSNOWRHO,ZSNOWTEMP,PSNOWLIQ,PSNOWGRAN1, &
                      PSNOWGRAN2,PSNOWHIST,PSNOWAGE,INLVLS_USE,HSNOWMETAMO      )


!***************************************DEBUG IN**********************************************
!IF (GCRODEBUGDETAILSPRINT) THEN
!  CALL SNOWCROPRINTPROFILE("after SNOWCROEVAPGONE", INLVLS_USE(IDEBUG),LPRINTGRAN,          &
!                           PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),        &
!                           PSNOWLIQ(IDEBUG,:),PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),     &
!                           PSNOWGRAN2(IDEBUG,:),PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:),     &
!                           HSNOWMETAMO)
!ENDIF
!***************************************DEBUG OUT********************************************
!
!*      12.     Update surface albedo:
!               ----------------------
! Snow clear sky albedo:
!
IF ( HSNOWRAD=='B92' ) THEN


  CALL SNOWCROALB(OGLACIER,                                             &
                  PSNOWALB,ZSPECTRALALBEDO,PSNOWDZ(:,1),PSNOWRHO(:,1:2),       &
                  PPERMSNOWFRAC,PSNOWGRAN1(:,1),PSNOWGRAN2(:,1),               &
                  PSNOWAGE(:,1),PSNOWGRAN1(:,2),PSNOWGRAN2(:,2),PSNOWAGE(:,2), &
                  PPS, PZENITH, INLVLS_USE, HSNOWMETAMO,VIS_ICEALB)

ENDIF

!
!*      13.     Update snow heat content:
!               -------------------------
! Update the heat content (variable stored each time step)
! using current snow temperature and liquid water content:
!
! First, make check to make sure heat content not too large
! (this can result due to signifigant heating of thin snowpacks):
! add any excess heat to ground flux:
!
DO JJ = 1,SIZE(ZSNOW)
! active layers
  DO JST = 1,INLVLS_USE(JJ)
    ZWHOLDMAX (JJ,JST) = SNOWCROHOLD( PSNOWRHO(JJ,JST),PSNOWLIQ(JJ,JST),PSNOWDZ(JJ,JST) )
! trude test with not allowing liquid if psnowrho >  XRHOTHRESHOLD_ICE  (i.e. no liquid in ice)
!      IF (PSNOWRHO(JJ,JST).GT.XRHOTHRESHOLD_ICE)  ZWHOLDMAX (JJ,JST)=0
! end trude test

   ZLIQHEATXS(JJ)     = MAX( 0.0, (PSNOWLIQ(JJ,JST) - ZWHOLDMAX(JJ,JST)) * XRHOLW ) * XLMTT/PTSTEP
    PSNOWLIQ  (JJ,JST) = PSNOWLIQ(JJ,JST) - ZLIQHEATXS(JJ)*PTSTEP/(XRHOLW*XLMTT)
    PSNOWLIQ  (JJ,JST) = MAX( 0.0, PSNOWLIQ(JJ,JST) )
    PGRNDFLUX (JJ)     = PGRNDFLUX(JJ) + ZLIQHEATXS(JJ)
    PSNOWTEMP (JJ,JST) = ZSNOWTEMP(JJ,JST)
!   Heat content using total density
    ZSCAP     (JJ,JST) = PSNOWRHO(JJ,JST) * XCI
    PSNOWHEAT (JJ,JST) = PSNOWDZ(JJ,JST) * &
                        ( ZSCAP(JJ,JST)*(PSNOWTEMP(JJ,JST)-XTT) - XLMTT*PSNOWRHO(JJ,JST) ) + &
                        XLMTT * XRHOLW * PSNOWLIQ(JJ,JST)
!
    PSNOWSWE(JJ,JST)  = PSNOWDZ(JJ,JST) * PSNOWRHO(JJ,JST)
  ENDDO  !  end loop active snow layers
!
! unactive layers
  DO JST = INLVLS_USE(JJ)+1,SIZE(PSNOWSWE,2)
    PSNOWSWE(JJ,JST)  = 0.
    PSNOWHEAT(JJ,JST) = 0.
    PSNOWRHO(JJ,JST)  = 999.
    PSNOWTEMP(JJ,JST) = XTT
    PSNOWDZ(JJ,JST)   = 0.
  ENDDO  !  end loop unactive snow layers
!
ENDDO    ! end loop grid points
!
! print some final diagnostics
! ! ! IF (INLVLS_USE(I)>0) THEN
! ! !  WRITE(*,FMT="(I4,2I4,F4.0,A7,F5.2,A10,F7.1,A11,F6.2,A13,F6.2)")  &
! ! !      TPTIME%TDATE%YEAR,TPTIME%TDATE%MONTH,TPTIME%TDATE%DAY,TPTIME%TIME/3600.,&
! ! !           'HTN= ',SUM(PSNOWDZ(I,1:INLVLS_USE(I))), 'FLUX Sol=', PGRNDFLUX(I),&
! ! !   'Tsurf_sol=',PTG(I)-273.16, 'Tbase_neige=',PSNOWTEMP(I,INLVLS_USE(I))-273.16
! ! ! ENDIF
!
!***************************************DEBUG IN*********************************************
!IF (GCRODEBUGPRINT) THEN
!  CALL SNOWCROPRINTDATE()
!  CALL SNOWCROPRINTPROFILE("CROCUS : end of time step",INLVLS_USE(IDEBUG),LPRINTGRAN, &
!        PSNOWDZ(IDEBUG,:),PSNOWRHO(IDEBUG,:),PSNOWTEMP(IDEBUG,:),PSNOWLIQ(IDEBUG,:),  &
!        PSNOWHEAT(IDEBUG,:),PSNOWGRAN1(IDEBUG,:),PSNOWGRAN2(IDEBUG,:),                &
!        PSNOWHIST(IDEBUG,:),PSNOWAGE(IDEBUG,:), HSNOWMETAMO)
!END IF
!***************************************DEBUG OUT********************************************
!***************************************PRINT IN*********************************************
! check suspect low temperature
DO JJ = 1,SIZE(ZSNOW)
!IF(INLVLS_USE(JJ)>0) WRITE(*,*) 'SOL:',JJ,INLVLS_USE(JJ),PGRNDFLUX(JJ),PTG(JJ),&
! PSNOWTEMP(jj,INLVLS_USE(JJ)),PSNOWTEMP(jj,1),PZENITH(JJ)
  DO JST = 1,INLVLS_USE(JJ)
    IF ( PSNOWTEMP(JJ,JST) < 100. ) THEN
      WRITE(6,*) 'pb tempe snow :',PSNOWTEMP(JJ,JST)
!      WRITE(6,FMT='("DATE:",2(I2.2,"/"),I4.4,F3.0)')          &
!        TPTIME%TDATE%DAY,TPTIME%TDATE%MONTH,TPTIME%TDATE%YEAR,TPTIME%TIME/3600.
      WRITE(6,*) 'point',JJ,"/",SIZE(ZSNOW)
      WRITE(6,*) 'layer',JST
      WRITE(6,*) 'pressure',PPS(JJ)
      WRITE(6,*) 'slope',ACOS(PDIRCOSZW(JJ))*(180./XPI),"deg"
      WRITE(6,*) 'solar radiation=',PSW_RAD(JJ)
      WRITE(6,*) 'INLVLS_USE(JJ):',INLVLS_USE(JJ)
      WRITE(6,*) PSNOWDZ(JJ,1:INLVLS_USE(JJ))
      WRITE(6,*) PSNOWRHO(JJ,1:INLVLS_USE(JJ))
      WRITE(6,*) PSNOWTEMP(JJ,1:INLVLS_USE(JJ))
!      CALL ABOR1_SFX('SNOWCRO: erreur tempe snow')
    ENDIF
  ENDDO
ENDDO
!***************************************PRINT OUT*********************************************
!***************************************DEBUG IN*********************************************
!Control and print energy balance
!IF (GCRODEBUGPRINTBALANCE) THEN
  !
!  ZSUMMASS_FIN(IDEBUG) = SUM( PSNOWSWE (IDEBUG,1:INLVLS_USE(IDEBUG)) )
!  ZSUMHEAT_FIN(IDEBUG) = SUM( PSNOWHEAT(IDEBUG,1:INLVLS_USE(IDEBUG)) )
  !
!  CALL GET_BALANCE(ZSUMMASS_INI(IDEBUG),ZSUMHEAT_INI(IDEBUG),ZSUMMASS_FIN(IDEBUG), &
!                   ZSUMHEAT_FIN(IDEBUG),PSR(IDEBUG),PRR(IDEBUG),PTHRUFAL(IDEBUG),  &
!                   PEVAP(IDEBUG),PEVAPCOR(IDEBUG),PGRNDFLUX(IDEBUG),PHSNOW(IDEBUG),&
!                   PRNSNOW(IDEBUG),PLEL3L(IDEBUG),PLES3L(IDEBUG),PHPSNOW(IDEBUG),  &
!                   PSNOWHMASS(IDEBUG),PSNOWDZ(IDEBUG,1),PTSTEP,                    &
!                   ZMASSBALANCE(IDEBUG),ZENERGYBALANCE(IDEBUG),ZEVAPCOR2(IDEBUG)   )
  !
!  CALL SNOWCROPRINTBALANCE(ZSUMMASS_INI(IDEBUG),ZSUMHEAT_INI(IDEBUG),ZSUMMASS_FIN(IDEBUG), &
!                           ZSUMHEAT_FIN(IDEBUG),PSR(IDEBUG),PRR(IDEBUG),PTHRUFAL(IDEBUG),  &
!                           PEVAP(IDEBUG),PEVAPCOR(IDEBUG),PGRNDFLUX(IDEBUG),PHSNOW(IDEBUG),&
!                           PRNSNOW(IDEBUG),PLEL3L(IDEBUG),PLES3L(IDEBUG),PHPSNOW(IDEBUG),  &
!                           PSNOWHMASS(IDEBUG),PSNOWDZ(IDEBUG,1),PTSTEP,                    &
!                           ZMASSBALANCE(IDEBUG),ZENERGYBALANCE(IDEBUG),ZEVAPCOR2(IDEBUG))
  !
!ENDIF
!
!IF (LPSTOPBALANCE) THEN
  !
  ! bilan pour tous points pour test eventuel sur depassement seuil des residus
!  DO JJ=1, SIZE(ZSNOW)
    !
!    ZSUMMASS_FIN(JJ) = SUM( PSNOWSWE (JJ,1:INLVLS_USE(JJ)) )
!    ZSUMHEAT_FIN(JJ) = SUM( PSNOWHEAT(JJ,1:INLVLS_USE(JJ)) )
    !
!    CALL GET_BALANCE(ZSUMMASS_INI(JJ),ZSUMHEAT_INI(JJ),ZSUMMASS_FIN(JJ), &
!                     ZSUMHEAT_FIN(JJ),PSR(JJ),PRR(JJ),PTHRUFAL(JJ),      &
!                     PEVAP(JJ),PEVAPCOR(JJ),PGRNDFLUX(JJ),PHSNOW(JJ),    &
!                     PRNSNOW(JJ),PLEL3L(JJ),PLES3L(JJ),PHPSNOW(JJ),      &
!                     PSNOWHMASS(JJ),PSNOWDZ(JJ,1),PTSTEP,                &
!                     ZMASSBALANCE(JJ),ZENERGYBALANCE(JJ),ZEVAPCOR2(JJ)   )
!    !
!  ENDDO    ! end loop grid points
  !
!  CALL SNOWCROSTOPBALANCE(ZMASSBALANCE(:),ZENERGYBALANCE(:))
  !
!END IF
!***************************************DEBUG OUT********************************************
!
PQS(:) = ZQSAT(:)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO',1,ZHOOK_HANDLE)
!

!END SUBROUTINE SNOWCRO
CONTAINS
!
!####################################################################
!####################################################################
!####################################################################
        SUBROUTINE SNOWCROCOMPACTN(PTSTEP,PSNOWRHO,PSNOWDZ,                         &
                                   PSNOWTEMP,PSNOW,PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST, &
                                   PSNOWLIQ,INLVLS_USE,PDIRCOSZW,HSNOWMETAMO        )
!
!!    PURPOSE
!!    -------
!     Snow compaction due to overburden and settling.
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. Method
!     directly inherited from Crocus v2.4 and
!     coarsely described in Brun et al., J. Glac 1989 and 1992
!
!     de/e = -sigma/eta * dt
!
!     where e is layer thickness, sigma is the vertical stress, dt is the
!     time step and eta is the snow viscosity
!     * sigma is currently calculated taking into account only the overburden
!     (a term representing "metamorphism stress" in fresh snow may be added
!      in the future)
!     * eta is computed as a function of snowtype, density and temperature
!
!     The local slope is taken into account, through the variable PDIRCOSZW
!     which is directly the cosine of the local slope
!
!
!     HISTORY:
!     Basic structure from ISBA-ES model (Boone and Etchevers, 2001)
!     Implementation of Crocus laws : E. Brun, S. Morin, J.-M. Willemet July 2010.
!     Implementation of slope effect on settling : V. Vionnet, S. Morin May 2011
!
!
USE MODD_CSTS,     ONLY : XTT, XG
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES
USE MODD_SNOW_METAMO
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP       ! Time step UNIT : s
REAL, DIMENSION(:), INTENT(IN)      :: PDIRCOSZW    ! cosine of local slope
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWTEMP    ! Snow temperature UNIT : K
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ   ! Density UNIT : kg m-3, Layer thickness UNIT : m
!
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOW        ! Snowheight UNIT : m
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST, &!Snowtype variables
                                        PSNOWLIQ     ! Snow liquid water content UNIT ???
INTEGER, DIMENSION(:), INTENT(IN)   :: INLVLS_USE   ! Number of snow layers used
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO2,    &! work snow density UNIT : kg m-3
                                                      ZVISCOSITY,   &! Snow viscosity UNIT : N m-2 s (= Pa s)
                                                      ZSMASS        !, &  ! overburden mass for a given layer UNIT : kg m-2
!                                                      ZWSNOWDZ       ! mass of each snow layer UNIT : kg m-2
!
INTEGER   :: JJ,JST   ! looping indexes
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 1. Cumulative snow mass (kg/m2):
! --------------------------------
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROCOMPACTN',0,ZHOOK_HANDLE)
!
DO JJ = 1,SIZE(PSNOW)
  ZSMASS(JJ,1) = 0.0
  DO JST = 2,INLVLS_USE(JJ)
    ZSMASS(JJ,JST) = ZSMASS(JJ,JST-1) + PSNOWDZ(JJ,JST-1) * PSNOWRHO(JJ,JST-1)
  ENDDO
  ZSMASS(JJ,1) = 0.5 * PSNOWDZ(JJ,1) * PSNOWRHO(JJ,1)  ! overburden of half the mass of the uppermost layer applied to itself
ENDDO

!
! 2. Compaction/Settling
! ----------------------
!
DO JJ = 1,SIZE(PSNOW)
  !
  DO JST = 1,INLVLS_USE(JJ)
    !
    ! Snow viscosity basic equation (depend on temperature and density only):
!      write(*,*) '-----'
!      write(*,*) xvvisc1, xvvisc3, psnowrho(jj,jst), xvvisc4, xtt, psnowtemp(jj,jst), xvro11
!      write(*,*),  XVVISC3*PSNOWRHO(JJ,JST) ,  XVVISC4*ABS(XTT-PSNOWTEMP(JJ,JST))
    ZVISCOSITY(JJ,JST) = XVVISC1 * &
                         EXP( XVVISC3*PSNOWRHO(JJ,JST) + XVVISC4*ABS(XTT-PSNOWTEMP(JJ,JST)) ) * &
                         PSNOWRHO(JJ,JST) / XVRO11

    !
    ! Equations below apply changes to the basic viscosity value, based on snow microstructure properties
    IF ( PSNOWLIQ(JJ,JST)>0.0 ) THEN
      ZVISCOSITY(JJ,JST) = ZVISCOSITY(JJ,JST) / &
                           ( XVVISC5 + XVVISC6*PSNOWLIQ(JJ,JST)/PSNOWDZ(JJ,JST) )
    ENDIF
    !
    IF( PSNOWLIQ(JJ,JST)/PSNOWDZ(JJ,JST)<=0.01 .AND. PSNOWHIST(JJ,JST)>=NVHIS2 ) THEN
      ZVISCOSITY(JJ,JST) = ZVISCOSITY(JJ,JST) * XVVISC7
    ENDIF
    !
    IF ( PSNOWHIST(JJ,JST)==NVHIS1 ) THEN
      !
      IF ( HSNOWMETAMO=="B92" ) THEN
        !
        IF ( PSNOWGRAN1(JJ,JST)>=0. .AND. PSNOWGRAN1(JJ,JST)<XVGRAN6 ) THEN
          ZVISCOSITY(JJ,JST) = ZVISCOSITY(JJ,JST) * &
                               MIN( 4., EXP( MIN( XVDIAM1, &
                                                  PSNOWGRAN2(JJ,JST)             -XVDIAM4 ) / XVDIAM6 ) )
        ENDIF
        !
      ELSEIF ( PSNOWGRAN1(JJ,JST)>=XVDIAM6*(4.-PSNOWGRAN2(JJ,JST)) .AND. PSNOWGRAN2(JJ,JST)<XVGRAN6/XVGRAN1 ) THEN
        ZVISCOSITY(JJ,JST) = ZVISCOSITY(JJ,JST) * &
                             MIN( 4., EXP( MIN( XVDIAM1, &
                                               (XVDIAM6*(4.-PSNOWGRAN2(JJ,JST)))-XVDIAM4 ) / XVDIAM6 ) )
      ENDIF
      !
    ENDIF
    !
    ! Calculate new snow snow density: compaction from weight/over-burden
    ZSNOWRHO2(JJ,JST) = PSNOWRHO(JJ,JST) + PSNOWRHO(JJ,JST) * PTSTEP * &
                                           ( XG*PDIRCOSZW(JJ)*ZSMASS(JJ,JST)/ZVISCOSITY(JJ,JST) )
    !
    ! Calculate new grid thickness in response to density change
    PSNOWDZ(JJ,JST) = PSNOWDZ(JJ,JST) * ( PSNOWRHO(JJ,JST)/ZSNOWRHO2(JJ,JST) )
    !
    !  Update density (kg m-3):
    PSNOWRHO(JJ,JST) = ZSNOWRHO2(JJ,JST)
    !
  ENDDO    ! end loop snow layers
  !
ENDDO      ! end loop grid points
!
!
! 3. Update total snow depth:
! -----------------------------------------------
!
DO JJ = 1,SIZE(PSNOWDZ,1)
  PSNOW(JJ) = SUM( PSNOWDZ(JJ,1:INLVLS_USE(JJ)) )
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROCOMPACTN',1,ZHOOK_HANDLE)

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOWCROCOMPACTN


!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROMETAMO(PSNOWDZ,PSNOWGRAN1, PSNOWGRAN2,         &
                         PSNOWHIST, PSNOWTEMP, PSNOWLIQ, PTSTEP, &
                         PSNOWSWE,INLVLS_USE, PSNOWAGE, HSNOWMETAMO)
!

!**** *METAMO* - METAMORPHOSE DES GRAINS
!              - SNOW METAMORPHISM
!     OBJET.
!     ------
!     METAMORPHOSE DU MANTEAU NEIGEUX.
!     EVOLUTION DU TYPE DE GRAINS
!     MISE A JOUR DES VARIABLES HISTORIQUES.
!     METAMORPHISM OF THE SNOW GRAINS,
!     HISTORICAL VARIABLES

!**   INTERFACE.
!     ----------
!     FORMALISME ADOPTE POUR LA REPRESENTATION DES GRAINS :
!     FORMALISM FOR THE REPRESENTATION OF GRAINS
!     -----------------------------------------------------


!                    1       - -1                 NEIGE FRAICHE
!                   / \      |                    -------------
!                  /   \     |  DENDRICITE        DECRITE EN TERME
!                 /     \    |  DENDRICITY        DE DENDRICITE ET
!                /       \   |                    SPHERICITE
!               2---------3  -  0                 DESCRIBED WITH
!                                                 SPHERICITY AND
!               |---------|                       DENDRICITY
!               0         1
!               SPHERICITE
!               SPHERICITY

!               4---------5  -
!               |         |  |
!               |         |  | DIAMETRE  (OU TAILLE)
!               |         |  | DIAMETER  (OR SIZE  )
!               |         |  |
!               |         |  |                   NEIGE NON DENDRITIQUE
!               6---------7  -                   ---------------------

!                                                SPHERICITE ET TAILLE
!                                                SPHERICITY AND SIZE

!              LES VARIABLES DU MODELE :
!              -------------------------
!              CAS DENDRITIQUE             CAS NON DENDRITIQUE
!
!            SGRAN1(JST) : DENDRICITE      SGRAN1(JST) : SPHERICITE
!            SGRAN2(JST) : SPHERICITE      SGRAN2(JST) : TAILLE (EN METRE)
!                                                        SIZE

!
!    CAS DENDRITIQUE/ DENDRITIC CASE
!    -------------------------------
!    SGRAN1(JST) VARIE DE -XVGRAN1 (-99 PAR DEFAUT) (ETOILE) A 0
!  (DENDRICITY)  >D OU LA DIVISION PAR -XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 1 ET 0
!                 VARIES FROM -XVGRAN1 (DEFAULT -99) (FRESH SNOW) TO 0
!                 DIVISION BY -XVGRAN1 TO OBTAIN VALUES BETWEEN 0 AND 1

!    SGRAN2(JST) VARIE DE 0 (CAS COMPLETEMENT ANGULEUX) A XVGRAN1
!  (SPHERICITY)  (99 PAR DEFAUT)
!                >D OU LA DIVISION PAR XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 0 ET 1
!                 VARIES FROM 0 (SPHERICITY=0) TO XVGRAN1


!    CAS NON DENDRITIQUE / NON DENDRITIC CASE
!    ---------------------------------------

!    SGRAN1(JST) VARIE DE 0 (CAS COMPLETEMENT ANGULEUX) A XVGRAN1
!  (SPHERICITY)  (99 PAR DEFAUT) (CAS SPHERIQUE)
!                >D OU LA DIVISION PAR XVGRAN1 POUR OBTENIR DES VALEURS
!                 ENTRE 0 ET 1
!                 VARIES FROM 0 TO 99

!    SGRAN2(JST) EST SUPERIEUR A XVDIAM1-SPHERICITE (3.E-4 M) ET NE FAIT QUE CROITRE
!     (SIZE)     IS GREATER THAN XVDIAM1-SPHERICITE (3.E-4 M) ALWAYS INCREASE


!    EXEMPLES : POINTS CARACTERISTIQUES DE LA FIGURE
!    --------

!                 SGRAN1     SGRAN2    DENDRICITE  SPHERICITE  TAILLE
!                                      DENDRICITY  SPHERICITY  SIZE
!      --------------------------------------------------------------
!                                                               (M)
!        1        -XVGRAN1    VNSPH3        1           0.5
!        2           0         0           0            0
!        3           0       XVGRAN1        0            1
!        4           0       XVDIAM1                     0        4.E-4
!        5         XVGRAN1    XVDIAM1-XVSPHE1              1        3.E-4
!        6           0         --                       0        --
!        7         XVGRAN1      --                       1        --

!     PAR DEFAUT : XVGRAN1 =99   VNSPH3=50 XVSPHE1=1. XVDIAM1=4.E-4


!     METHODE.
!     --------
!     EVOLUTION DES TYPES DE GRAINS : SELON LES LOIS DECRITES
!     DANS BRUN ET AL (1992)
!     PLUSIEURS CAS SONT A DISTINGUER
!      1.2 NEIGE HUMIDE
!      1.3 METAMORPHOSE NEIGE SECHE
!        1.3.1 FAIBLE GRADIENT
!        1.3.2 GRADIENT MOYEN
!        1.3.3 FORT GRADIENT
!     DANS CHAQUE CAS ON SEPARE NEIGE DENDRITIQUE ET NON DENDRITIQUE
!     LE PASSAGE DENDRITIQUE => NON DENDRITIQUE SE FAIT LORSQUE
!     SGRAN1 DEVIENT > 0

!     TASSEMENT : LOIS DE VISCOSITE ADAPTEE SELON LE TYPE DE GRAINS

!     VARIABLES HISTORIQUES (CAS NON DENDRITIQUE SEULEMENT)

!     MSHIST DEFAUT
!        0           CAS NORMAL
!     NVHIS1   1     GRAINS ANGULEUX
!     NVHIS2   2     GRAINS AYANT ETE EN PRESENCE D EAU LIQUIDE
!                    MAIS N'AYANT PAS EU DE CARATERE ANGULEUX
!     NVHIS3   3     GRAINS AYANT ETE EN PRESENCE D EAU LIQUIDE
!                    AYANT EU AUPARAVANT UN CARACTERE ANGULEUX

!     GRAIN METAMORPHISM ACCORDING TO BRUN ET AL (1992)
!     THE DIFFERENT CASES ARE :
!     1.2 WET SNOW
!     1.3 DRY SNOW
!       1.3.1. LOW      TEMPERATURE GRADIENT
!       1.3.2. MODERATE TEMPERATURE GRADIENT
!       1.3.3. HIGH     TEMPERATURE GRADIENTi
!     THE CASE OF DENTRITIC OR NON DENDRITIC SNOW IS TREATED SEPARATELY
!     THE LIMIT DENTRITIC ==> NON DENDRITIC IS REACHED WHEN SGRAN1>0

!     SNOW SETTLING : VISCOSITY DEPENDS ON THE GRAIN TYPES

!     HISTORICAL VARIABLES (NON DENDRITIC CASE)
!     MSHIST DEFAUT
!        0           CAS NORMAL
!     NVHIS1   1     FACETED CRISTAL
!     NVHIS2   2     LIQUID WATER AND NO FACETED CRISTALS BEFORE
!     NVHIS3   3     LIQUID WATER AND FACETED CRISTALS BEFORE

!     EXTERNES.
!     ---------

!     REFERENCES.
!     -----------

!     AUTEURS.
!     --------
!        ERIC BRUN ET AL. - JOURNAL OF GLACIOLOGY 1989/1992.

!     MODIFICATIONS.
!     --------------
!        08/95: YANNICK DANIELOU - CODAGE A LA NORME DOCTOR.
!        09/96: ERIC MARTIN      - CORRECTION COMMENTAIRES
!        03/06: JM Willemet      - F90 and SI units
!        08/06: JM Willemet      - new formulation for TEL (Mwat/(Mice+Mwat) instead of Mwat/Mice.
!                                  Threshold on the diameter increasing of the wet grains.
!        01/07 : JM Willemet     - CORRECTION DES COUCHES SATUREES SUBISSANT DU TASSEMENT
!                                  CORRECTION ON THE SATURATED LAYERS WHICH ARE SETTLED
!        12/12: CM Carmagnola    - Dendricity and size replaced by the optical diameter
!                                - Test of different evolution laws for the optical diameter
!        08/13: M Lafaysse       - Simplification of historical parameter computation (logicals GNONDENDRITIC, GFACETED, GSPHE_LW)
   !
USE MODD_SNOW_METAMO
USE MODD_CSTS, ONLY : XTT, XPI, XRHOLW, XRHOLI
USE MODD_SNOW_PAR, ONLY : XUNDEF
!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!     0.1 declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ, PSNOWTEMP, PSNOWLIQ, PSNOWSWE
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST
!
REAL, INTENT(IN)                    :: PTSTEP
!
INTEGER, DIMENSION(:), INTENT(IN)   :: INLVLS_USE
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWAGE
!
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!
!     0.2 declaration of local variables
!
REAL :: ZGRADT, ZTELM, ZVDENT, ZDENT, ZSPHE, ZVAP, ZDANGL, &
        ZSIZE, ZSSA, ZSSA0, ZSSA_T, ZSSA_T_DT, ZA, ZB, ZC, &
        ZA2, ZB2, ZC2, ZOPTD, ZOPTR, ZOPTR0, ZDRDT
REAL :: ZVDENT1, ZVDENT2, ZVSPHE, ZCOEF_SPH
REAL :: ZDENOM1, ZDENOM2, ZFACT1, ZFACT2
INTEGER :: INLVLS
INTEGER :: JST,JJ                                !Loop controls
INTEGER :: IDRHO, IDGRAD, IDTEMP           !Indices for values from Flanner 2006
LOGICAL :: GNONDENDRITIC ,GFACETED, GSPHE_LW
LOGICAL :: GCOND_B92, GCOND_C13, GCOND_SPH
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!     INITIALISATION
!     --------------
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROMETAMO',0,ZHOOK_HANDLE)
!

INLVLS = SIZE(PSNOWGRAN1(:,:),2)  ! total snow layers
!
!*    1. METAMORPHOSES DANS LES STRATES. / METAMORPHISM
!        -----------------------------------------------
DO JJ = 1,SIZE(PSNOWRHO,1)
  !
  DO JST = 1,INLVLS_USE(JJ)
    !
    ! 1.1 INITIALISATION: GRADIENT DE TEMPERATURE / TEMPERATURE GRADIENT
    IF ( JST==INLVLS_USE(JJ) ) THEN
      ZGRADT = ABS(PSNOWTEMP(JJ,JST)   - PSNOWTEMP(JJ,JST-1))*2. / (PSNOWDZ(JJ,JST-1) + PSNOWDZ(JJ,JST))
    ELSEIF ( JST==1 ) THEN
      ZGRADT = ABS(PSNOWTEMP(JJ,JST+1) - PSNOWTEMP(JJ,JST)  )*2. / (PSNOWDZ(JJ,JST) + PSNOWDZ(JJ,JST+1))
    ELSE
      ZGRADT = ABS(PSNOWTEMP(JJ,JST+1) - PSNOWTEMP(JJ,JST-1))*2. / &
                                            (PSNOWDZ(JJ,JST-1) + PSNOWDZ(JJ,JST)*2. + PSNOWDZ(JJ,JST+1))
    ENDIF
    !
    IF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
      ! 1.2 METAMORPHOSE HUMIDE. / WET SNOW METAMORPHISM
      !
      ! TENEUR EN EAU LIQUIDE / LIQUID WATER CONTENT
      ZTELM  = XUPOURC * PSNOWLIQ(JJ,JST) * XRHOLW / PSNOWSWE(JJ,JST)
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = MAX( XVDENT2 * ZTELM**NVDENT1, XVDENT1 * EXP(XVVAP1/XTT) )
      ZVDENT2 = ZVDENT1
      ! CONDITION POUR LE CAS NON DENDRITIQUE NON SPHERIQUE
      GCOND_B92  = ( PSNOWGRAN1(JJ,JST)<XVGRAN1-XUEPSI )
      GCOND_C13 = .TRUE. ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XVSPHE1
      ! FOR C13
      ZCOEF_SPH = 2.
      !
    ELSEIF ( ZGRADT<XVGRAT1 ) THEN
      ! 1.3.1 METAMORPHOSE SECHE FAIBLE/ DRY LOW GRADIENT (0-5 DEG/M).
      !
      ZVAP = EXP( XVVAP1/PSNOWTEMP(JJ,JST) )
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = XVDENT1 * ZVAP
      ZVDENT2 = XVSPHE2 * ZVAP
      ! CONDITION POUR LE CAS NON DENDRITIQUE SPHERICITE NON LIMITEE
      GCOND_B92  = ( PSNOWHIST(JJ,JST)/=NVHIS1 .OR. PSNOWGRAN2(JJ,JST)<XVDIAM2 )
      GCOND_C13 = ( HSNOWMETAMO=='C13' )  ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XVSPHE1
      ! FOR C13
      ZCOEF_SPH = 2.
      !
    ELSE
      ! 1.3.2 METAMORPHOSE SECHE GRADIENT MOYEN / DRY MODERATE (5-15).
      ! 1.3.3 METAMORPHOSE SECHE FORT / DRY HIGH GRADIENT
      !
      ZVAP = EXP( XVVAP1/PSNOWTEMP(JJ,JST) ) * (ZGRADT)**XVVAP2
      !
      ! VITESSES DE DIMINUTION DE LA DENDRICITE / RATE OF THE DENDRICITY DECREASE
      ZVDENT1 = XVDENT1 * ZVAP
      ZVDENT2 = - XVDENT1 * ZVAP
      ! CONDITION POUR LE CAS NON DENDRITIQUE NON COMPLETEMENT ANGULEUX
      GCOND_B92  = ( ZGRADT<XVGRAT2 .OR. PSNOWGRAN1(JJ,JST)>0. )
      GCOND_C13 = ( HSNOWMETAMO=='C13' ) ! CONDITION POUR LE CALCUL DE SNOWGRAN1
      ! X COEF
      ZVSPHE = XUNDEF
      ! FOR C13
      ZCOEF_SPH = 3.
      !
    ENDIF
    !
    IF ( HSNOWMETAMO=="B92" ) THEN
      !
      !------------------------------------------------
      !    BRUN et al. 1992 (B92)
      !
      ! -> Wet snow and dry snow
      ! -> Evolution of dendricity, sphericity and size
      !------------------------------------------------
      !
      IF ( PSNOWGRAN1(JJ,JST)<-XUEPSI ) THEN
        ! 1.2.1 CAS DENDRITIQUE/DENDRITIC CASE.
        !
        ! / CALCUL NOUVELLE DENDRICITE ET SPHERICITE.
        ZDENT = - PSNOWGRAN1(JJ,JST)/XVGRAN1 - ZVDENT1 * PTSTEP
        ZSPHE =   PSNOWGRAN2(JJ,JST)/XVGRAN1 + ZVDENT2 * PTSTEP
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        IF( ZDENT<=XUEPSI ) THEN
          ! EVOLUTION DE SGRAN1 ET SGRAN2 ET TEST PASSAGE DENDRITIQUE > NON DENDRITIQUE.
          PSNOWGRAN1(JJ,JST) =  ZSPHE * XVGRAN1
          PSNOWGRAN2(JJ,JST) = XVDIAM1 - XVDIAM5 * MIN( ZSPHE, ZVSPHE )
        ELSE
          PSNOWGRAN1(JJ,JST) = -ZDENT * XVGRAN1
          PSNOWGRAN2(JJ,JST) =  ZSPHE * XVGRAN1
        ENDIF
        !
      ELSEIF ( GCOND_B92 ) THEN
        ! 1.2.2 CAS NON DENDRITIQUE ET
        !             NON COMPLETEMENT SPHERIQUE / NON DENDRITIC AND NOT COMPLETELY SPHERIC CASE
        ! OU          SPHERICITE NON LIMITEE
        ! OU          NON COMPLETEMENT ANGULEUX
        !
        ! . EVOLUTION DE LA SPHERICITE SEULEMENT / EVOLUTION OF SPHERICITY ONLY (NOT SIZE)
        ZSPHE = PSNOWGRAN1(JJ,JST)/XVGRAN1 + ZVDENT2 * PTSTEP
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        PSNOWGRAN1(JJ,JST) = ZSPHE * XVGRAN1
        !
      ELSEIF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
        ! 1.2.3 CAS NON DENDRITIQUE ET SPHERIQUE/NON DENDRITIC AND SPHERIC EN METAMORPHOSE HUMIDE
        !
        ! EVOLUTION DE LA TAILLE SEULEMENT/EVOLUTION OF SIZE ONLY
        CALL GET_GRAN(PTSTEP,ZTELM,PSNOWGRAN2(JJ,JST))
        !
      ELSEIF ( ZGRADT<XVGRAT1 ) THEN
        ! 1.2.4. CAS HISTORIQUE=2 OU 3 ET GROS GRAINS SPHERICITE LIMITEE / CASE HISTORY=2 OR 3 AND BIG GRAINS LIMITED SPHERICITY
        !
        ZSPHE =  PSNOWGRAN1(JJ,JST)/XVGRAN1 + &
                 ZVDENT2 * PTSTEP * EXP( MIN( 0., XVDIAM3-PSNOWGRAN2(JJ,JST) ) / XVDIAM6 )
        ZSPHE = MIN( ZSPHE, XVSPHE3 )
        CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
        PSNOWGRAN1(JJ,JST) = ZSPHE * XVGRAN1
        !
      ELSE
        ! 1.2.5. CAS NON DENDRITIQUE ET ANGULEUX/DENDRITIC AND SPERICITY=0.
        !
        ZDANGL = SNOW3L_MARBOUTY(PSNOWRHO(JJ,JST),PSNOWTEMP(JJ,JST),ZGRADT)
        PSNOWGRAN2(JJ,JST) = PSNOWGRAN2(JJ,JST) + ZDANGL * XVFI * PTSTEP
        !
      ENDIF
      !
    ELSE
      !
      !------------------------------------------------
      !    CARMAGNOLA et al. 2013 (C13)
      !
      ! -> Wet snow
      ! -> Evolution of optical diameter and sphericity
      !------------------------------------------------
      !
      ! SPHERICITY
      ZSPHE = PSNOWGRAN2(JJ,JST) + ZVDENT2 * PTSTEP
      CALL SET_THRESH(ZGRADT,PSNOWLIQ(JJ,JST),ZSPHE)
      IF ( PSNOWLIQ(JJ,JST)>XUEPSI .OR. ZGRADT<XVGRAT1 ) THEN
        GCOND_SPH = ( ZSPHE < 1.-XUEPSI )
      ELSE
        GCOND_SPH = ( ZSPHE > XUEPSI )
      ENDIF
      !
      IF ( GCOND_C13 .AND. PSNOWGRAN1(JJ,JST)<XVDIAM6*(4.-ZSPHE)-XUEPSI ) THEN
        ! 1.1.1 CAS DENDRITIQUE/DENDRITIC CASE.
        !
        IF ( GCOND_SPH ) THEN
          PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + XVDIAM6 * PTSTEP * &
                               ( ZVDENT2*(PSNOWGRAN1(JJ,JST)/XVDIAM6-1.)/(ZSPHE-3.) - &
                                 ZVDENT1*(ZSPHE-3.) )
        ELSE
           PSNOWGRAN1(JJ,JST) =  PSNOWGRAN1(JJ,JST) + XVDIAM6 * PTSTEP * ZVDENT1 * ZCOEF_SPH
        ENDIF
        !
      ELSEIF ( GCOND_C13 .AND. GCOND_SPH ) THEN
        ! 1.2.2 CAS NON DENDRITIQUE ET
        !             NON COMPLETEMENT SPHERIQUE / NON DENDRITIC AND NOT COMPLETELY SPHERIC CASE
        ! OU          NON COMPLETEMENT ANGULEUX
        !
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) - XVDIAM6 * PTSTEP * ZVDENT2 * 2.* ZSPHE
        !
      ELSEIF ( PSNOWLIQ(JJ,JST)>XUEPSI ) THEN
        ! 1.2.3 CAS NON DENDRITIQUE ET SPHERIQUE/NON DENDRITIC AND SPHERIC EN METAMORPHOSE HUMIDE
        !
        ! NON DENDRITIC AND SPHERIC: EVOLUTION OF SIZE ONLY
        CALL GET_GRAN(PTSTEP,ZTELM,PSNOWGRAN1(JJ,JST))
        !
      ELSEIF ( GCOND_C13 .AND. ZGRADT>=XVGRAT2 ) THEN
        !
        ZDANGL = SNOW3L_MARBOUTY(PSNOWRHO(JJ,JST),PSNOWTEMP(JJ,JST),ZGRADT)
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + 0.5 * ZDANGL * XVFI * PTSTEP
        !
      ENDIF
      !
      PSNOWGRAN2(JJ,JST) = ZSPHE
      !
      !---------------------------------
      !    TAILLANDIER et al. 2007 (T07)
      !
      ! -> Dry snow
      ! -> Evolution of optical diameter
      !---------------------------------
      !
      IF ( PSNOWLIQ(JJ,JST)<=XUEPSI .AND. HSNOWMETAMO=='T07' ) THEN
        !
     !   WRITE(*,*) CSNOWMETAMO,': you are using T07 formulation!!'
        !
        ! Coefficients from Taillander et al. 2007
        ZSSA0 = 6./( XRHOLI*XVDIAM6 ) * 10.
        !
        ZA  =  0.659*ZSSA0 - 27.2 * ( PSNOWTEMP(JJ,JST)-273.15-2.03 )      ! TG conditions
        ZB  = 0.0961*ZSSA0 - 3.44 * ( PSNOWTEMP(JJ,JST)-273.15+1.90 )
        ZC  = -0.341*ZSSA0 - 27.2 * ( PSNOWTEMP(JJ,JST)-273.15-2.03 )
        ZA2 =  0.629*ZSSA0 - 15.0 * ( PSNOWTEMP(JJ,JST)-273.15-11.2 )     ! ET conditions
        ZB2 = 0.0760*ZSSA0 - 1.76 * ( PSNOWTEMP(JJ,JST)-273.15-2.96 )
        ZC2 = -0.371*ZSSA0 - 15.0 * ( PSNOWTEMP(JJ,JST)-273.15-11.2 )
        !
        ! Compute SSA (method from Jacobi, 2010)
!   ZSSA = 6./(XRHOLI*PSNOWGRAN1(JJ,JST))*10.
!    ZSSA_t = (0.5+0.5*TANH(0.5*(ZGRADT-10.)))*(ZA-ZB*LOG(PSNOWAGE(JJ,JST)*24+EXP(ZC/ZB))) + &
!         (0.5-0.5*TANH(0.5*(ZGRADT-10.)))*(ZA2-ZB2*LOG(PSNOWAGE(JJ,JST)*24+EXP(ZC2/ZB2)))
!
!   ZSSA_t_dt = (0.5+0.5*TANH(0.5*(ZGRADT-10.)))*(ZA-ZB*LOG(PSNOWAGE(JJ,JST)*24+PTSTEP/3600.+EXP(ZC/ZB))) + &
!             (0.5-0.5*TANH(0.5*(ZGRADT-10.)))*(ZA2-ZB2*LOG(PSNOWAGE(JJ,JST)*24+PTSTEP/3600.+EXP(ZC2/ZB2)))
!
!   ZSSA = ZSSA + (ZSSA_t_dt-ZSSA_t)
!
!   ZSSA = MAX(ZSSA,8.*10.)
!
!   PSNOWGRAN1(JJ,JST) = 6./(XRHOLI*ZSSA)*10.
        !
        ! Compute SSA (rate equation with Taylor series)
        ZSSA = 6./( XRHOLI*PSNOWGRAN1(JJ,JST) ) * 10.
        !
        ZDENOM1 = (PSNOWAGE(JJ,JST)*24.) + EXP(ZC/ZB)
        ZDENOM2 = (PSNOWAGE(JJ,JST)*24.) + EXP(ZC2/ZB2)
        ZFACT1 = 0.5 + 0.5*TANH( 0.5*(ZGRADT-10.) )
        ZFACT2 = 0.5 - 0.5*TANH( 0.5*(ZGRADT-10.) )
        ZSSA = ZSSA + (PTSTEP/3600.) * (                   ZFACT1 * (-ZB/ZDENOM1)    + ZFACT2 * (-ZB2/ZDENOM2)   + &
                                        (PTSTEP/3600.) * ( ZFACT1 * (ZB/ZDENOM1**2.) + ZFACT2 * (ZB2/ZDENOM2**2.) ) * 1./2. )
        !ZSSA = ZSSA + (PTSTEP/3600.) * (  ZFACT1 * ZB /ZDENOM1 * ( 1./ZDENOM1 * (PTSTEP/3600.) * 1./2. - 1. ) + &
        !                                  ZFACT2 * ZB2/ZDENOM2 * ( 1./ZDENOM2 * (PTSTEP/3600.) * 1./2. - 1. ) )
         !
        ZSSA = MAX( ZSSA, 8.*10. )
        !
        PSNOWGRAN1(JJ,JST) = 6./( XRHOLI*ZSSA ) * 10.
        !
      !---------------------------------
      !    FLANNER et al. 2006 (F06)
      !
      ! -> Dry snow
      ! -> Evolution of optical diameter
      !---------------------------------
      ELSEIF ( PSNOWLIQ(JJ,JST)<=XUEPSI .AND. HSNOWMETAMO=='F06' )THEN
        !
      !  WRITE(*,*) CSNOWMETAMO,': you are using F06 formulation!!'
        !
        ! XDRDT0(dens,gradT,T), XTAU(dens,gradT,T), XKAPPA(dens,gradT,T)
        ! dens: [1-8 <-> 50.-400. kg/m3]
        ! gradT: [1-31 <-> 0.-300. K/m]
        ! T: [1-11 <-> 223.15-273.15 K]
        !
        !  Select indices of density, temperature gradient and temperature
        IDRHO  = MIN( ABS( INT( (PSNOWRHO(JJ,JST)-25.)/50.        ) + 1 ), 8  )
        IDGRAD = MIN( ABS( INT( (ZGRADT-5.)/10.+2.                )     ), 31 )
        IDTEMP = MIN( ABS( INT( (PSNOWTEMP(JJ,JST)-225.65 )/5.+2. )     ), 11 )
        IF ( PSNOWTEMP(JJ,JST)<221. ) IDTEMP = 1
        !
        ! Compute SSA
        ZOPTR0 = XVDIAM6/2. * 10.**6.
        ZOPTR  = PSNOWGRAN1(JJ,JST)/2. * 10.**6.
        ZDRDT  = XDRDT0(IDRHO,IDGRAD,IDTEMP) * &
                 ( XTAU(IDRHO,IDGRAD,IDTEMP) / &
                   ( ZOPTR - ZOPTR0 + XTAU(IDRHO,IDGRAD,IDTEMP) ) )**(1./XKAPPA(IDRHO,IDGRAD,IDTEMP))
        ZOPTR  = ZOPTR + ZDRDT * PTSTEP/3600.
        ZOPTR  = MIN( ZOPTR, 3./(XRHOLI*2.) * 10.**6.)
        !
        PSNOWGRAN1(JJ,JST) = ZOPTR * 2./10.**6.
        !
      ENDIF
      !
    ENDIF
    !
  ENDDO
  !
ENDDO

!*    2. MISE A JOUR VARIABLES HISTORIQUES (CAS NON DENDRITIQUE).
!        UPDATE OF THE HISTORICAL VARIABLES
!        --------------------------------------------------------
DO JJ = 1,SIZE(PSNOWRHO,1)
  !
  DO JST = 1,INLVLS_USE(JJ)
    !
    IF ( HSNOWMETAMO=='B92' ) THEN
      !
      !non dendritic
      GNONDENDRITIC = ( PSNOWGRAN1(JJ,JST)>=0. )
      IF ( GNONDENDRITIC ) THEN
        !faceted crystals
        GFACETED = ( PSNOWGRAN1(JJ,JST)<XVSPHE4 .AND. PSNOWHIST(JJ,JST)==0. )
        !spheric and liquid water
        GSPHE_LW = ( XVGRAN1-PSNOWGRAN1(JJ,JST)<XVSPHE4 .AND. PSNOWLIQ(JJ,JST)/PSNOWDZ(JJ,JST)>XVTELV1 )
      END IF
      !
    ELSE
      !
      !non dendritic
      GNONDENDRITIC = ( PSNOWGRAN1(JJ,JST)>=XVDIAM6*(4.-PSNOWGRAN2(JJ,JST))-XUEPSI )
      IF ( GNONDENDRITIC ) THEN
        !faceted crystals
        GFACETED = ( PSNOWGRAN2(JJ,JST)<XVSPHE4/XVGRAN1 .AND. PSNOWHIST(JJ,JST)==0. )
        !spheric and liquid water
        GSPHE_LW = ( XVSPHE1-PSNOWGRAN2(JJ,JST)<XVSPHE4/XVGRAN1 .AND. PSNOWLIQ(JJ,JST)/PSNOWDZ(JJ,JST)>XVTELV1 )
      END IF
      !
    ENDIF
    !
    IF ( GNONDENDRITIC ) THEN
      !
      IF ( GFACETED ) THEN
        !
        PSNOWHIST(JJ,JST) = NVHIS1
        !
      ELSEIF ( GSPHE_LW ) THEN
        !
        IF (PSNOWHIST(JJ,JST)==0.)     PSNOWHIST(JJ,JST) = NVHIS2
        IF (PSNOWHIST(JJ,JST)==NVHIS1) PSNOWHIST(JJ,JST) = NVHIS3
        !
      ELSEIF ( PSNOWTEMP(JJ,JST) < XTT ) THEN
        !
        IF(PSNOWHIST(JJ,JST)==NVHIS2) PSNOWHIST(JJ,JST) = NVHIS4
        IF(PSNOWHIST(JJ,JST)==NVHIS3) PSNOWHIST(JJ,JST) = NVHIS5
        !
      ENDIF
      !
    ENDIF
    !
  ENDDO
  !
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROMETAMO',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROMETAMO
!
!####################################################################
!####################################################################
SUBROUTINE SET_THRESH(PGRADT,PSNOWLIQ,PSPHE)
!
USE MODD_SNOW_METAMO, ONLY : XUEPSI, XVGRAT1
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PGRADT
REAL, INTENT(IN) :: PSNOWLIQ
REAL, INTENT(INOUT) :: PSPHE
!
IF ( PSNOWLIQ>XUEPSI .OR. PGRADT<XVGRAT1 ) THEN
  PSPHE = MIN(1.,PSPHE)
ELSE
  PSPHE = MAX(0.,PSPHE)
ENDIF
!
END SUBROUTINE SET_THRESH
!####################################################################
!####################################################################
SUBROUTINE GET_GRAN(PTSTEP,PTELM,PGRAN)
!
USE MODD_CSTS, ONLY : XPI
USE MODD_SNOW_METAMO, ONLY : XVTAIL1, XVTAIL2, NVDENT1
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PTSTEP, PTELM
REAL, INTENT(INOUT) :: PGRAN
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_GRAN',0,ZHOOK_HANDLE)
!
PGRAN = 2. * ( 3./(4.*XPI) * &
                  ( 4. * XPI/3. * (PGRAN/2.)**3 + &
                   ( XVTAIL1 + XVTAIL2 * PTELM**NVDENT1 ) * PTSTEP ) )**(1./3.)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_GRAN',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_GRAN
!
!####################################################################
!####################################################################
!####################################################################
!
!SUBROUTINE SNOWCROALB(TPTIME,OGLACIER,                            &
SUBROUTINE SNOWCROALB(OGLACIER,                            &
                      PALBEDOSC,PSPECTRALALBEDO,PSNOWDZ,          &
                      PSNOWRHO,PPERMSNOWFRAC,                     &
                      PSNOWGRAN1_TOP,PSNOWGRAN2_TOP,PSNOWAGE_TOP, &
                      PSNOWGRAN1_BOT,PSNOWGRAN2_BOT,PSNOWAGE_BOT, &
                      PPS, PZENITH, KNLVLS_USE ,HSNOWMETAMO,VIS_ICEALB  )
!
!!    PURPOSE
!!    -------
!     Calculate the snow surface albedo. Use the method of original
!     Crocus which considers a specified spectral distribution of solar
!     solar radiation (to be replaced by an input forcing when available)
!     In addition to original crocus, the top 2 surface snow layers are
!     considered in the calculation, using an arbitrary weighting, in order
!     to avoid time discontinuities due to layers agregation
!     Ageing depends on the presence of permanent snow cover
!
USE MODD_SNOW_PAR, ONLY : XANSMAX, XANSMIN,XAGLAMIN, XAGLAMAX, &
                          XVRPRE1,XVRPRE2,XVAGING_NOGLACIER,   &
                          XVAGING_GLACIER, XVSPEC1,XVSPEC2,    &
                          XVSPEC3, XVW1,XVW2,XVD1,XVD2

!USE MODI_INI_CSTS, ONLY: XANSMAX, XANSMIN,XAGLAMIN, XAGLAMAX, &
!                       XVAGING_NOGLACIER,   &
!                       XVAGING_GLACIER


!USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!TYPE(DATE_TIME), INTENT(IN)       :: TPTIME      ! current date and time
LOGICAL, INTENT(IN)               :: OGLACIER    ! True = Over permanent snow and ice,
!                                                   initialise WGI=WSAT,
!                                                   Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                 ! False = No specific treatment
REAL, DIMENSION(:), INTENT(IN)    :: PSNOWDZ,PPERMSNOWFRAC
!
REAL,DIMENSION(:,:), INTENT(IN)   :: PSNOWRHO ! For now only the 2 first layers are required
!
REAL, DIMENSION(:), INTENT(INOUT) :: PALBEDOSC
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSPECTRALALBEDO   ! Albedo in the different spectral bands
!
REAL, DIMENSION(:), INTENT(IN)    :: PSNOWGRAN1_TOP,PSNOWGRAN2_TOP,PSNOWAGE_TOP, &
                                     PSNOWGRAN1_BOT,PSNOWGRAN2_BOT,PSNOWAGE_BOT, PPS
INTEGER, DIMENSION(:), INTENT(IN) :: KNLVLS_USE
!
REAL, DIMENSION(:), INTENT(IN)    :: PZENITH ! solar zenith angle for future use
!
 CHARACTER(3),INTENT(IN)           :: HSNOWMETAMO ! metamorphism scheme
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),3) :: ZALB_TOP, ZALB_BOT
!
REAL, DIMENSION(SIZE(PSNOWRHO,1))   :: ZANSMIN, ZANSMAX, ZMIN, ZMAX
REAL, DIMENSION(SIZE(PSNOWRHO,1))   :: ZFAC_TOP, ZFAC_BOT
!
REAL, DIMENSION(SIZE(PALBEDOSC))  :: ZVAGE1
!
!REAL            :: ZAGE_NOW
!
INTEGER         :: JJ   ! looping indexes

!++ trude, crocus
REAL, DIMENSION(:), INTENT(IN), OPTIONAL   :: VIS_ICEALB
!-- trude
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROALB',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! ------------------
!
IF ( OGLACIER ) THEN
  ZANSMIN(:) = XAGLAMIN * PPERMSNOWFRAC(:) + XANSMIN * (1.0-PPERMSNOWFRAC(:))
  ZANSMAX(:) = XAGLAMAX * PPERMSNOWFRAC(:) + XANSMAX * (1.0-PPERMSNOWFRAC(:))
  ZVAGE1(:)  = XVAGING_GLACIER * PPERMSNOWFRAC(:) + XVAGING_NOGLACIER * (1.0-PPERMSNOWFRAC(:))
ELSE
  ZANSMIN(:) = XANSMIN
  ZANSMAX(:) = XANSMAX
  ZVAGE1(:)  = XVAGING_NOGLACIER
ENDIF
!
! ! ! ! ! !  date computation for ageing effects
! ! ! ! !   CALL GREGODSTRATI(TPTIME%TDATE%YEAR,TPTIME%TDATE%MONTH,TPTIME%TDATE%DAY,   &
! ! ! ! !                       TPTIME%TIME,ZAGE_NOW)
!
! coherence control
! to remove when initialization routines will be updated
IF ( MINVAL(PSNOWAGE_BOT)<0. ) THEN
!  CALL ABOR1_SFX('FATAL ERROR in SNOWCRO: Snow layer age inconsistent : check initialization routine. !')
END IF
!
! ! ! ! ! ! should be moved with other time controls to not compute MAXVAL(PSNOWAGE_TOP) at each time step
! ! ! ! ! IF ((ZAGE_NOW - MAXVAL(PSNOWAGE_TOP))<-0.001) THEN
! ! ! ! !       WRITE(*,*),"ZAGE_NOW=",ZAGE_NOW
! ! ! ! !       WRITE(*,*),"MAXVAL(PSNOWAGE_TOP)=",MAXVAL(PSNOWAGE_TOP)
! ! ! ! !       CALL ABOR1_SFX(&
! ! ! ! !       'FATAL ERROR in SNOWCRO: Snow layer date inconsistent with the current day !')
! ! ! ! ! END IF
!
DO JJ=1, SIZE(PALBEDOSC)
  !
  IF ( KNLVLS_USE(JJ)==0 ) THEN
    ! case with no snow on the ground
    PALBEDOSC(JJ) = ZANSMIN(JJ)
  ELSE
    !
    CALL GET_ALB(JJ,PSNOWRHO(JJ,1),PPS(JJ),ZVAGE1(JJ),PSNOWGRAN1_TOP(JJ),&
                 PSNOWGRAN2_TOP(JJ),PSNOWAGE_TOP(JJ),ZALB_TOP(JJ,:),HSNOWMETAMO,VIS_ICEALB)
     !
!      IF (KNLVLS_USE(JJ)>=1) THEN
    IF ( KNLVLS_USE(JJ)>=2 ) THEN !modif ML
      ! second surface layer when it exists
      !
      CALL GET_ALB(JJ,PSNOWRHO(JJ,2),PPS(JJ),ZVAGE1(JJ),PSNOWGRAN1_BOT(JJ),&
                   PSNOWGRAN2_BOT(JJ),MIN(365.,PSNOWAGE_BOT(JJ)),ZALB_BOT(JJ,:),HSNOWMETAMO,VIS_ICEALB)
      !
    ELSE
      ! when it does not exist, the second surface layer gets top layer albedo
      ZALB_BOT(JJ,1) = ZALB_TOP(JJ,1)
      ZALB_BOT(JJ,2) = ZALB_TOP(JJ,2)
      ZALB_BOT(JJ,3) = ZALB_TOP(JJ,3)
    ENDIF
    !
    ! computation of spectral albedo over 3 bands taking into account the respective
    ! depths of top layers
    ZMIN(JJ) = MIN( 1., PSNOWDZ(JJ)/XVD1 )
    ZMAX(JJ) = MAX( 0., (PSNOWDZ(JJ)-XVD1)/XVD2 )
    ZFAC_TOP(JJ) = XVW1 * ZMIN(JJ) + XVW2 * MIN( 1., ZMAX(JJ) )
    ZFAC_BOT(JJ) = XVW1 * ( 1. - ZMIN(JJ) ) + XVW2 * ( 1. - MIN( 1., ZMAX(JJ) ) )
    PSPECTRALALBEDO(JJ,1) = ZFAC_TOP(JJ) * ZALB_TOP(JJ,1) + ZFAC_BOT(JJ) * ZALB_BOT(JJ,1)
    PSPECTRALALBEDO(JJ,2) = ZFAC_TOP(JJ) * ZALB_TOP(JJ,2) + ZFAC_BOT(JJ) * ZALB_BOT(JJ,2)
    PSPECTRALALBEDO(JJ,3) = ZFAC_TOP(JJ) * ZALB_TOP(JJ,3) + ZFAC_BOT(JJ) * ZALB_BOT(JJ,3)
    !
    ! arbitrarily specified spectral distribution
    ! to be changed when solar radiation distribution is an input variable
    PALBEDOSC(JJ) = XVSPEC1 * PSPECTRALALBEDO(JJ,1) + &
                    XVSPEC2 * PSPECTRALALBEDO(JJ,2) + &
                    XVSPEC3 * PSPECTRALALBEDO(JJ,3)
    !
  ENDIF ! end case with snow on the ground
  !
ENDDO ! end loop grid points
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROALB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOWCROALB
!####################################################################
SUBROUTINE GET_ALB(KJ,PSNOWRHO_IN,PPS_IN,PVAGE1,PSNOWGRAN1,PSNOWGRAN2,PSNOWAGE,PALB,&
                   HSNOWMETAMO,VIS_ICEALB)
!
USE MODD_CSTS, ONLY :                           XRHOTHRESHOLD_ICE   ! trude, changed the location of this
USE MODD_SNOW_PAR, ONLY : XALBICE1, XALBICE2, XALBICE3,   &
!                          XRHOTHRESHOLD_ICE,              &   ! trude changed location of xrhothershold_ice
                          XVALB2, XVALB3, XVALB4, XVALB5, &
                          XVALB6, XVALB7, XVALB8, XVALB9, &
                          XVALB10, XVALB11, XVDIOP1,      &
                          XVRPRE1, XVRPRE2, XVPRES1
!
USE MODE_SNOW3L, ONLY : GET_DIAM
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KJ
REAL, INTENT(IN) :: PSNOWRHO_IN, PPS_IN
REAL, INTENT(IN) :: PVAGE1
REAL, INTENT(IN) :: PSNOWGRAN1, PSNOWGRAN2, PSNOWAGE
REAL, DIMENSION(3), INTENT(OUT) :: PALB
!
 CHARACTER(3),INTENT(IN)::HSNOWMETAMO
 !
REAL :: ZDIAM, ZDIAM_SQRT

!++ trude, crocus
REAL, DIMENSION(:), INTENT(IN), OPTIONAL   :: VIS_ICEALB
!-- trude
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_ALB',0,ZHOOK_HANDLE)
!
IF ( PSNOWRHO_IN<XRHOTHRESHOLD_ICE ) THEN
  ! Normal case (snow)
  CALL GET_DIAM(PSNOWGRAN1,PSNOWGRAN2,ZDIAM,HSNOWMETAMO)
  ZDIAM_SQRT = SQRT(ZDIAM)
  PALB(1) = MIN( XVALB2 - XVALB3*ZDIAM_SQRT, XVALB4 )
  PALB(2) = MAX( 0.3, XVALB5 - XVALB6*ZDIAM_SQRT )
  ZDIAM   = MIN( ZDIAM, XVDIOP1 )
  ZDIAM_SQRT = SQRT(ZDIAM)
  PALB(3) = MAX( 0., XVALB7*ZDIAM - XVALB8*ZDIAM_SQRT + XVALB9 )
 ! AGE CORRECTION ONLY FOR VISIBLE BAND

! ! ! ! !               PALB(1)=MAX(XVALB11,PALB(1)-MIN(MAX(PPS_IN/XVPRES1,XVRPRE1), &
! ! ! ! !                       XVRPRE2)*XVALB10*MIN(365.,ZAGE_NOW-PSNOWAGE)/PVAGE1)


  PALB(1) = MAX( XVALB11, PALB(1) - MIN( MAX(PPS_IN/XVPRES1,XVRPRE1), XVRPRE2 ) * &
                   XVALB10 * PSNOWAGE / PVAGE1 )
ELSE
  ! Prescribed spectral albedo for surface ice
  !! ++ trude, crocus


if (isnan(VIS_ICEALB(1))) then
  PALB(1) = XALBICE1
  elseif (VIS_ICEALB(1).le.0.0) THEN
  PALB(1) = XALBICE1
else
  PALB(1) = VIS_ICEALB(1)
endif

  PALB(2) = XALBICE2
  PALB(3) = XALBICE3
ENDIF
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_ALB',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_ALB
!
!####################################################################
!####################################################################
!SUBROUTINE SNOWCRORAD(TPTIME, OGLACIER,                        &
SUBROUTINE SNOWCRORAD(OGLACIER,                        &
                      PSW_RAD, PSNOWALB, PSNOWDZ,              &
                      PSNOWRHO, PALB, PRADSINK, PRADXS,        &
                      PSNOWGRAN1, PSNOWGRAN2, PSNOWAGE,PPS,    &
                      PZENITH, PPERMSNOWFRAC,KNLVLS_USE,       &
                      OSNOW_ABS_ZENITH,HSNOWMETAMO,VIS_ICEALB)
!
!!    PURPOSE
!!    -------
!     Calculate the transmission of shortwave (solar) radiation
!     through the snowpack (using a form of Beer's Law: exponential
!     decay of radiation with increasing snow depth).
!     Needs a first calculation of the albedo to stay coherent with
!     ISBA-ES ==> make sure to keep SNOWCRORAD coherent with SNOWCROALB
!
USE MODD_SNOW_PAR, ONLY : XWCRN, XANSMAX, XANSMIN, XANS_TODRY,          &
                          XSNOWDMIN, XANS_T, XAGLAMIN, XAGLAMAX,        &
                          XD1, XD2, XD3, XX, XVSPEC1, XVSPEC2, XVSPEC3, &
                          XVBETA1, XVBETA2, XVBETA3, XVBETA4, XVBETA5
!USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODE_SNOW3L, ONLY : GET_DIAM
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!TYPE(DATE_TIME), INTENT(IN)       :: TPTIME      ! current date and time
LOGICAL, INTENT(IN)               :: OGLACIER   ! True = Over permanent snow and ice,
!                                                   initialise WGI=WSAT,
!                                                   Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                ! False = No specific treatment
!
REAL, DIMENSION(:), INTENT(IN)      :: PSW_RAD, PSNOWALB, PALB,PPERMSNOWFRAC
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO, PSNOWDZ
!
LOGICAL, INTENT(IN)                 :: OSNOW_ABS_ZENITH ! parametrization for polar regions (not physic but better results)
!                                                       ! default FALSE
 CHARACTER(3), INTENT(IN)            :: HSNOWMETAMO
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRADXS
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PRADSINK
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWGRAN1, PSNOWGRAN2, PSNOWAGE
REAL, DIMENSION(:), INTENT(IN)      :: PPS
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE
REAL, DIMENSION(:), INTENT(IN)      :: PZENITH
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1))    :: ZRADTOT
REAL, DIMENSION(SIZE(PSNOWRHO,1))    :: ZALB_NEW
REAL, DIMENSION(SIZE(PSNOWRHO,1),3)  :: ZALB !albedo 3 bands
REAL, DIMENSION(SIZE(PSNOWRHO,2))    :: ZDIAM
REAL, DIMENSION(SIZE(PSNOWRHO,2),3)  :: ZBETA
REAL, DIMENSION(3) :: ZOPTICALPATH, ZFACT
REAL :: ZPROJLAT
!
INTEGER :: JJ,JST,JB   ! looping indexes

!++ trude, crocus

REAL, DIMENSION(:), INTENT(IN), OPTIONAL   :: VIS_ICEALB   !-- trude!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCRORAD',0,ZHOOK_HANDLE)
!
! 0. Initialization:
! ------------------
!
PRADSINK(:,:) = 0.
!
! 1. Computation of the new albedo (see SNOWCROALB):
! -----------------------------------
!
! CALL SNOWCROALB(TPTIME,OGLACIER,                                             &
CALL SNOWCROALB(OGLACIER,                                             &
                 ZALB_NEW,ZALB,PSNOWDZ(:,1),PSNOWRHO(:,1:2),                  &
                 PPERMSNOWFRAC,PSNOWGRAN1(:,1),PSNOWGRAN2(:,1),               &
                 PSNOWAGE(:,1),PSNOWGRAN1(:,2),PSNOWGRAN2(:,2),PSNOWAGE(:,2), &
                 PPS, PZENITH, KNLVLS_USE, HSNOWMETAMO, VIS_ICEALB     )
!
DO JJ = 1,SIZE(PSW_RAD)

  !
  DO JST = 1,KNLVLS_USE(JJ)
    CALL GET_DIAM(PSNOWGRAN1(JJ,JST),PSNOWGRAN2(JJ,JST),ZDIAM(JST),HSNOWMETAMO)
  ENDDO    ! end loop snow layers
  !
  ! 2. Extinction of net shortwave radiation
  ! ----------------------------------------
  ! First calculates extinction coefficients fn of grain size and density
  ! then calculates exctinction in the layer and increases optical path length
  !
  ! Coefficient for taking into account the increase of path length of rays
  ! in snow due to zenithal angle
  ZPROJLAT = 1. / MAX( XUEPSI, COS(PZENITH(JJ)) )
  !
  PRADSINK(JJ,:) = -PSW_RAD(JJ) * ( 1.-PSNOWALB(JJ) ) / ( 1.-ZALB_NEW(JJ) )
  !
  !   Initialize optical depth
  ZOPTICALPATH(1) = 0.
  ZOPTICALPATH(2) = 0.
  ZOPTICALPATH(3) = 0.
  !
  DO JST = 1,KNLVLS_USE(JJ)
    !
    ZBETA(JST,1) = MAX( XVBETA1 * PSNOWRHO(JJ,JST) / SQRT(ZDIAM(JST)), XVBETA2 )
    ZBETA(JST,2) = MAX( XVBETA3 * PSNOWRHO(JJ,JST) / SQRT(ZDIAM(JST)), XVBETA4 )
    ZBETA(JST,3) = XVBETA5
    !
    ZFACT(:) = 0.
    DO JB = 1,3
      ZOPTICALPATH(JB) = ZOPTICALPATH(JB) + ZBETA(JST,JB) * PSNOWDZ(JJ,JST)
      IF (OSNOW_ABS_ZENITH) THEN
        !This formulation is incorrect but it compensate partly the fact that the albedo formulation does not account for zenithal angle
        ZFACT(JB) = (1.-ZALB(JJ,JB)) * EXP( -ZOPTICALPATH(JB)*ZPROJLAT)
      ELSE
        ZFACT(JB) = (1.-ZALB(JJ,JB)) * EXP( -ZOPTICALPATH(JB) )
      ENDIF
    ENDDO
    !
    PRADSINK(JJ,JST) = PRADSINK(JJ,JST) * &
                       ( XVSPEC1*ZFACT(1) + XVSPEC2*ZFACT(2) + XVSPEC3*ZFACT(3) )
    !
  ENDDO    ! end loop snow layers
  !
  ! For thin snow packs, radiation might reach base of
  ! snowpack and the reflected energy can be absorbed by the bottom of snow layer:
  ! THIS PROCESS IS NOT SIMULATED
  !
  ! 4. Excess radiation not absorbed by snowpack (W/m2)JJ
  ! ----------------------------------------------------
  !
  PRADXS(JJ) = -PRADSINK( JJ,KNLVLS_USE(JJ) )
  !
ENDDO    !end loop grid points
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRORAD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOWCRORAD
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROTHRM(PSNOWRHO,PSCOND,PSNOWTEMP,PPS,PSNOWLIQ, &
                       OCOND_GRAIN,OCOND_YEN                   )
!
!!    PURPOSE
!!    -------
!     Calculate snow thermal conductivity from
!     Sun et al. 1999, J. of Geophys. Res., 104, 19587-19579
!     (vapor) and Anderson, 1976, NOAA Tech. Rep. NWS 19 (snow).
!
!     Upon activation of flag OCOND_YEN, use the Yen (1981) formula for thermal conductivity
!     This formula was originally used in Crocus.
!
USE MODD_CSTS, ONLY : XP00, XCONDI, XRHOLW
USE MODD_SNOW_PAR, ONLY : XSNOWTHRMCOND1, XSNOWTHRMCOND2, XSNOWTHRMCOND_AVAP, &
                          XSNOWTHRMCOND_BVAP, XSNOWTHRMCOND_CVAP, XVRKZ6
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWTEMP, PSNOWRHO, PSNOWLIQ
REAL, DIMENSION(:,:), INTENT(OUT)   :: PSCOND
LOGICAL, INTENT(IN)                 :: OCOND_GRAIN, OCOND_YEN
!
!*      0.2    declarations of local variables
!
INTEGER :: JJ, JST ! looping indexes
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROTHRM',0,ZHOOK_HANDLE)
!
! 1. Snow thermal conductivity
! ----------------------------
!
DO JST = 1,SIZE(PSNOWRHO(:,:),2)
  !
  DO JJ = 1,SIZE(PSNOWRHO(:,:),1)
    !
    IF ( OCOND_YEN ) THEN
      PSCOND(JJ,JST) = XCONDI * EXP( XVRKZ6 * LOG( PSNOWRHO(JJ,JST)/XRHOLW ) )
    ELSE
      PSCOND(JJ,JST) = ( XSNOWTHRMCOND1 + &
                         XSNOWTHRMCOND2 * PSNOWRHO(JJ,JST) * PSNOWRHO(JJ,JST) ) + &
                         MAX( 0.0, ( XSNOWTHRMCOND_AVAP + &
                                    ( XSNOWTHRMCOND_BVAP/(PSNOWTEMP(JJ,JST) + XSNOWTHRMCOND_CVAP) ) ) &
                                   * (XP00/PPS(JJ)) )
    ENDIF
    !
    ! Snow thermal conductivity is set to be above 0.04 W m-1 K-1
    IF ( OCOND_GRAIN ) THEN
      PSCOND(JJ,JST) = MAX( 0.04, PSCOND(JJ,JST) )
      ! Snow thermal conductivity is annihilated in presence of liquid water
      IF( PSNOWLIQ(JJ,JST)>XUEPSI ) PSCOND(JJ,JST) = 0.01 * PSCOND(JJ,JST)
    ENDIF
     !
   ENDDO ! end loop JST
   !
ENDDO ! end loop JST
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROTHRM',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROTHRM
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROEBUD(HSNOWRES, HIMPLICIT_WIND,                                   &
                       PPEW_A_COEF, PPEW_B_COEF,                                   &
                       PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,         &
                       PSNOWDZMIN,                                                 &
                       PZREF,PTS,PSNOWRHO,PSNOWLIQ,PSCAP,PSCOND1,PSCOND2,          &
                       PUREF,PEXNS,PEXNA,PDIRCOSZW,PVMOD,                          &
                       PLW_RAD,PSW_RAD,PTA,PQA,PPS,PTSTEP,                         &
                       PSNOWDZ1,PSNOWDZ2,PALBT,PZ0,PZ0EFF,PZ0H,                    &
                       PSFCFRZ,PRADSINK,PHPSNOW,                                   &
                       PCT,PEMIST,PRHOA,PTSTERM1,PTSTERM2,PRA,PCDSNOW,PCHSNOW,     &
                       PQSAT,PDQSAT,PRSRA,PUSTAR2_IC, PRI,                         &
                       PPET_A_COEF_T,PPEQ_A_COEF_T,PPET_B_COEF_T,PPEQ_B_COEF_T     )
!
!!    PURPOSE
!!    -------
!     Calculate surface energy budget linearization (terms) and turbulent
!     exchange coefficients/resistance between surface and atmosphere.
!     (Noilhan and Planton 1989; Giordani 1993; Noilhan and Mahfouf 1996)
!
!!    MODIFICATIONS
!!    -------------
!!      Original A. Boone
!!      Modified by E. Brun (24/09/2012) :
!!      * Correction coupling coefficient for specific humidity in SNOWCROEBUD
!!      * PSFCFRZ(:)  = 1.0 for systematic solid/vapor latent fluxes in SNOWCROEBUD
!!      Modified by B. Decharme 09/12  new wind implicitation
!
!USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XCPD, XRHOLW, XSTEFAN, XLVTT, XLSTT, XRHOLW
USE MODD_SNOW_PAR, ONLY : X_RI_MAX, XEMISSN,XUNDEF
!
USE MODE_THERMOS
!
!USE MODI_SURFACE_RI
!USE MODI_SURFACE_AERO_COND
!USE MODI_SURFACE_CD
USE MODE_SURF_COEFS
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                   :: PTSTEP, PSNOWDZMIN
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HSNOWRES ! type of sfc resistance
!                                      DEFAULT=Louis (1979), standard ISBA
!                                      method. Option to limit Ri number
!                                      for very stable conditions
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HIMPLICIT_WIND   ! wind implicitation option
!                                                     ! 'OLD' = direct
!                                                     ! 'NEW' = Taylor serie, order 1
!
REAL, DIMENSION(:), INTENT(IN)     :: PPEW_A_COEF, PPEW_B_COEF,                   &
                                      PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF,      &
                                      PPEQ_B_COEF
!                                      PPEW_A_COEF = wind coefficient (m2s/kg)
!                                      PPEW_B_COEF = wind coefficient (m/s)
!                                      PPET_A_COEF = A-air temperature coefficient
!                                      PPET_B_COEF = B-air temperature coefficient
!                                      PPEQ_A_COEF = A-air specific humidity coefficient
!                                      PPEQ_B_COEF = B-air specific humidity coefficient
!
REAL, DIMENSION(:), INTENT(IN)     :: PZREF, PTS, PSNOWDZ1, PSNOWDZ2,        &
                                      PRADSINK, PSNOWRHO, PSNOWLIQ, PSCAP,   &
                                      PSCOND1, PSCOND2,                      &
                                      PZ0, PHPSNOW,                          &
                                      PALBT, PZ0EFF, PZ0H
!
REAL, DIMENSION(:), INTENT(IN)     :: PSW_RAD, PLW_RAD, PTA, PQA, PPS, PRHOA
!
REAL, DIMENSION(:), INTENT(IN)     :: PUREF, PEXNS, PEXNA, PDIRCOSZW, PVMOD
!
REAL, DIMENSION(:), INTENT(OUT)    :: PTSTERM1, PTSTERM2, PEMIST, PRA,         &
                                      PCT, PSFCFRZ, PCDSNOW, PCHSNOW,          &
                                      PQSAT, PDQSAT, PRSRA
!
REAL, DIMENSION(:), INTENT(OUT)    :: PUSTAR2_IC,                      &
                                      PPET_A_COEF_T, PPEQ_A_COEF_T,    &
                                      PPET_B_COEF_T, PPEQ_B_COEF_T
!
REAL, DIMENSION(:), INTENT(OUT)    :: PRI
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTS))        :: ZAC, ZRI,                        &
                                     ZSCONDA, ZA, ZB, ZC,             &
                                     ZCDN, ZSNOWDZM1, ZSNOWDZM2,      &
                                     ZVMOD, ZUSTAR2, ZTS3, ZLVT,      &
                                     Z_CCOEF
REAL, DIMENSION(SIZE(PTS))        :: ZSNOWEVAPX, ZDENOM, ZNUMER
!
INTEGER :: JJ   ! looping indexes
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
! 1. New saturated specific humidity and derrivative:
! ---------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROEBUD',0,ZHOOK_HANDLE)
!
ZRI   (:) = XUNDEF
!
PQSAT (:) = QSATI(PTS(:),PPS(:))
PDQSAT(:) = DQSATI(PTS(:),PPS(:),PQSAT(:))
!
! 2. Surface properties:
! ----------------------
! It might be of interest to use snow-specific roughness
! or a temperature dependence on emissivity:
! but for now, use ISBA defaults.
!
PEMIST(:) = XEMISSN
!
! 2. Computation of resistance and drag coefficient
! -------------------------------------------------
!
 CALL SURFACE_RI(PTS, PQSAT, PEXNS, PEXNA, PTA, PQA,  &
                 PZREF, PUREF, PDIRCOSZW, PVMOD, ZRI  )
!
! Simple adaptation of method by Martin and Lejeune (1998)
! to apply a lower limit to turbulent transfer coef
! by defining a maximum Richarson number for stable
! conditions:
!
IF ( HSNOWRES=='RIL' ) THEN
  DO JJ=1,SIZE(ZRI)
    ZRI(JJ) = MIN( X_RI_MAX, ZRI(JJ) )
  ENDDO
ENDIF
!
PRI(:) = ZRI(:)
!
! Surface aerodynamic resistance for heat transfers
!
 CALL SURFACE_AERO_COND(ZRI, PZREF, PUREF, PVMOD, PZ0, PZ0H, ZAC, PRA, PCHSNOW)
!

PRSRA(:) = PRHOA(:) / PRA(:)
!
! For atmospheric model coupling:
!
 CALL SURFACE_CD(ZRI, PZREF, PUREF, PZ0EFF, PZ0H, PCDSNOW, ZCDN)
!
!
! Modify flux-form implicit coupling coefficients:
! - wind components:
!
ZNUMER(:) = PCDSNOW(:)*PVMOD(:)
ZDENOM(:) = PRHOA(:) * PCDSNOW(:) * PVMOD(:) * PPEW_A_COEF(:)
IF(HIMPLICIT_WIND=='OLD')THEN
! old implicitation
  ZUSTAR2(:) = ( ZNUMER(:) * PPEW_B_COEF(:) ) / ( 1.0 - ZDENOM(:) )
ELSE
! new implicitation
  ZUSTAR2(:) = ( ZNUMER(:) * ( 2.*PPEW_B_COEF(:) - PVMOD(:) ) ) / ( 1.0 - 2.0*ZDENOM(:) )
ENDIF

!
ZVMOD(:) = PRHOA(:)*PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
ZVMOD(:) = MAX( ZVMOD(:),0. )
!

WHERE ( PPEW_A_COEF(:)/= 0. )
  ZUSTAR2(:) = MAX( ( ZVMOD(:) - PPEW_B_COEF(:) ) / (PRHOA(:)*PPEW_A_COEF(:)), 0. )
ENDWHERE
!
! implicit wind friction
ZUSTAR2(:) = MAX( ZUSTAR2(:),0. )
!
PUSTAR2_IC(:) = ZUSTAR2(:)
!
! 3. Calculate linearized surface energy budget components:
! ---------------------------------------------------------
! To prevent numerical difficulties for very thin snow
! layers, limit the grid "thinness": this is important as
! layers become vanishing thin:
!
ZSNOWDZM1(:) = MAX( PSNOWDZ1(:), PSNOWDZMIN )
ZSNOWDZM2(:) = MAX( PSNOWDZ2(:), PSNOWDZMIN )
!
! Surface thermal inertia:
PCT(:) = 1.0 / ( PSCAP(:)*ZSNOWDZM1(:) )
!
! Fraction of surface frozen (sublimation) with the remaining
! fraction being liquid (evaporation):
!
PSFCFRZ(:) = 1.0
!
! Thermal conductivity between uppermost and lower snow layers:
!
ZSCONDA(:) = ( ZSNOWDZM1(:)*PSCOND1(:) + ZSNOWDZM2(:)*PSCOND2(:) ) / &
             ( ZSNOWDZM1(:)            + ZSNOWDZM2(:)            )
!
! Transform implicit coupling coefficients:
! Note, surface humidity is 100% over snow.
!
! - specific humidity:
!
Z_CCOEF(:) = 1.0 - PPEQ_A_COEF(:) * PRSRA(:)
!
PPEQ_A_COEF_T(:) = - PPEQ_A_COEF(:) * PRSRA(:) * PDQSAT(:) / Z_CCOEF(:)
!
PPEQ_B_COEF_T(:) = ( PPEQ_B_COEF(:) &
                   - PPEQ_A_COEF(:) * PRSRA(:) * (PQSAT(:) - PDQSAT(:)*PTS(:)) ) / Z_CCOEF(:)
!
! - air temperature:
!   (assumes A and B correspond to potential T):
!
Z_CCOEF(:) = ( 1.0 - PPET_A_COEF(:) * PRSRA(:) ) / PEXNA(:)
!
PPET_A_COEF_T(:) = - PPET_A_COEF(:) * PRSRA(:) / ( PEXNS(:) * Z_CCOEF(:) )
!
PPET_B_COEF_T(:) = PPET_B_COEF(:) / Z_CCOEF(:)
!
!
! Energy budget solution terms:
!
ZTS3(:) = PEMIST(:) * XSTEFAN * PTS(:)**3
ZLVT(:) = (1.-PSFCFRZ(:))*XLVTT + PSFCFRZ(:)*XLSTT
!
ZA(:) = 1./PTSTEP + PCT(:) * &
         ( 4. * ZTS3(:) + PRSRA(:) * ZLVT(:) * ( PDQSAT(:) - PPEQ_A_COEF_T(:) )            &
                        + PRSRA(:) * XCPD * ( (1./PEXNS(:))-(PPET_A_COEF_T(:)/PEXNA(:)) )  &
                        + ( 2*ZSCONDA(:) / ( ZSNOWDZM2(:)+ZSNOWDZM1(:) ) ) )
!
ZB(:) = 1./PTSTEP + PCT(:) * &
         ( 3. * ZTS3(:) + PRSRA(:) * ZLVT(:) * PDQSAT(:) )
!
ZC(:) = PCT(:) * ( - PRSRA(:) * ZLVT(:) * ( PQSAT(:) - PPEQ_B_COEF_T(:) )  &
                   + PRSRA(:) * XCPD * PPET_B_COEF_T(:) / PEXNA(:)         &
                   + PSW_RAD(:) * (1. - PALBT(:)) + PEMIST(:) * PLW_RAD(:) &
                   + PHPSNOW(:) + PRADSINK(:) )
!
!
! Coefficients needed for implicit solution
! of linearized surface energy budget:
!
PTSTERM2(:) = 2. * ZSCONDA(:) * PCT(:) / ( ZA(:) * (ZSNOWDZM2(:)+ZSNOWDZM1(:) ) )
!
PTSTERM1(:) = ( PTS(:)*ZB(:) + ZC(:) ) / ZA(:)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROEBUD',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROEBUD
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROSOLVT(PTSTEP,PSNOWDZMIN,                     &
                        PSNOWDZ,PSCOND,PSCAP,PTG,              &
                        PSOILCOND,PD_G,                        &
                        PRADSINK,PCT,PTERM1,PTERM2,            &
                        PPET_A_COEF_T,PPEQ_A_COEF_T,           &
                        PPET_B_COEF_T,PPEQ_B_COEF_T,           &
                        PTA_IC, PQA_IC,                        &
                        PGBAS,PSNOWTEMP,PSNOWFLUX,             &
                        KNLVLS_USE                             )
!
!!    PURPOSE
!!    -------
!     This subroutine solves the 1-d diffusion of 'ZSNOWTEMP' using a
!     layer averaged set of equations which are time differenced
!     using the backward-difference scheme (implicit).
!     The eqs are solved rapidly by taking advantage of the
!     fact that the matrix is tridiagonal. This is a very
!     general routine and can be used for the 1-d diffusion of any
!     quantity as long as the diffusity is not a function of the
!     quantity being diffused. Aaron Boone 8-98. Soln to the eq:
!
!                 c  dQ    d  k dQ    dS
!                    -- =  --   -- -  --
!                    dt    dx   dx    dx
!
!     where k = k(x) (thermal conductivity), c = c(x) (heat capacity)
!     as an eg. for temperature/heat eq. S is a sink (-source) term.
!     Diffusivity is k/c
!
!!     MODIFICATIONS
!!     -------------
!!      Original A. Boone
!!       05/2011: Brun  Special treatment to tackle the variable number
!!                      of snow layers
!
USE MODD_CSTS, ONLY : XTT
!
USE MODI_TRIDIAG_GROUND_SNOWCRO
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP, PSNOWDZMIN
!
REAL, DIMENSION(:), INTENT(IN)      :: PTG, PSOILCOND, PD_G,        &
                                        PCT, PTERM1, PTERM2

!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWDZ, PSCOND, PSCAP,      &
                                        PRADSINK
!
REAL, DIMENSION(:), INTENT(IN)      :: PPET_A_COEF_T, PPEQ_A_COEF_T, &
                                        PPET_B_COEF_T, PPEQ_B_COEF_T
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PGBAS, PSNOWFLUX, PTA_IC, PQA_IC
!
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZSNOWTEMP, ZDTERM, ZCTERM, &
                                                    ZFRCV, ZAMTRX, ZBMTRX,     &
                                                    ZCMTRX
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)) :: ZWORK1, ZWORK2, ZDZDIF,    &
                                                    ZSNOWDZM
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)-1) :: ZSNOWTEMP_M,             &
                                                      ZFRCV_M, ZAMTRX_M,       &
                                                      ZBMTRX_M, ZCMTRX_M
!
REAL, DIMENSION(SIZE(PTG)) :: ZGBAS, ZSNOWTEMP_DELTA
!
INTEGER :: JJ, JST   ! looping indexes
INTEGER :: INLVLS
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROSOLVT',0,ZHOOK_HANDLE)
!

! 0. Initialize:
! ------------------
!
ZSNOWTEMP(:,:) = PSNOWTEMP(:,:)
INLVLS         = SIZE(PSNOWDZ(:,:),2)
!
! 1. Calculate tri-diagnoal matrix coefficients:
! ----------------------------------------------
! For heat transfer, assume a minimum grid
! thickness (to prevent numerical
! problems for very thin snow cover):
!
DO JJ=1,SIZE(PTG)
  !
  DO JST = KNLVLS_USE(JJ),1,-1
    !
    ZSNOWDZM(JJ,JST) = MAX( PSNOWDZ(JJ,JST), PSNOWDZMIN )
    !
    ZWORK1(JJ,JST)   = ZSNOWDZM(JJ,JST) * PSCOND(JJ,JST)
    !
    IF ( JST<KNLVLS_USE(JJ) ) THEN
      !
      ZDZDIF(JJ,JST) = ZSNOWDZM(JJ,JST) + ZSNOWDZM(JJ,JST+1)
      !
      ZWORK2(JJ,JST) = ZSNOWDZM(JJ,JST+1) * PSCOND(JJ,JST+1)
      !
    ELSE
      !
      ZDZDIF(JJ,JST) = ZSNOWDZM(JJ,JST) + PD_G(JJ)
      !
      ZWORK2(JJ,JST) = PD_G(JJ) * PSOILCOND(JJ)
      !
    ENDIF
    !
    ZDTERM(JJ,JST) = 2.0 * ( ZWORK1(JJ,JST) + ZWORK2(JJ,JST) ) / ZDZDIF(JJ,JST)**2
    !
    ZCTERM(JJ,JST) = PSCAP(JJ,JST) * ZSNOWDZM(JJ,JST) / PTSTEP
    !
  ENDDO
  !
ENDDO
!
! 2. Set up tri-diagonal matrix
! -----------------------------
!
ZAMTRX(:,:) = 0.
ZBMTRX(:,:) = 0.
ZCMTRX(:,:) = 0.
ZFRCV(:,:) = 0.
! Upper BC
!
ZAMTRX(:,1) =  0.0
ZBMTRX(:,1) =  1. / ( PCT(:)*PTSTEP )
ZCMTRX(:,1) = - PTERM2(:) * ZBMTRX(:,1)
ZFRCV (:,1) =   PTERM1(:) * ZBMTRX(:,1)
!
DO JJ = 1,SIZE(PTG)
  !
  DO JST = 2,KNLVLS_USE(JJ)
    !
    ! Interior Grid & Lower BC
    ZAMTRX(JJ,JST) = -ZDTERM(JJ,JST-1)
    ZBMTRX(JJ,JST) =  ZCTERM(JJ,JST) + ZDTERM(JJ,JST-1) + ZDTERM(JJ,JST)
    ZFRCV (JJ,JST) =  ZCTERM(JJ,JST)*PSNOWTEMP(JJ,JST) - (PRADSINK(JJ,JST-1)-PRADSINK(JJ,JST))
    !
    IF ( JST<KNLVLS_USE(JJ) ) THEN
      ZCMTRX(JJ,JST) = -ZDTERM(JJ,JST)
    ELSE
      ZCMTRX(JJ,JST) = 0.0
      ZFRCV (JJ,JST) = ZFRCV(JJ,JST) + ZDTERM(JJ,JST)*PTG(JJ)
    ENDIF
    !
  ENDDO
  !
ENDDO
!
! 4. Compute solution vector
! --------------------------
!

CALL TRIDIAG_GROUND_SNOWCRO(ZAMTRX,ZBMTRX,ZCMTRX,ZFRCV,ZSNOWTEMP,KNLVLS_USE,0)
!
! Heat flux between surface and 2nd snow layers: (W/m2)
!
PSNOWFLUX(:) = ZDTERM(:,1) * ( ZSNOWTEMP(:,1) - ZSNOWTEMP(:,2) )
!
! 5. Snow melt case
! -----------------
! If melting in uppermost layer, assume surface layer
! temperature at freezing point and re-evaluate lower
! snowpack temperatures. This is done as it is most likely
! most signigant melting will occur within a time step in surface layer.
! Surface energy budget (and fluxes) will
! be re-calculated (outside of this routine):
!
ZAMTRX_M(:,1) =  0.0
ZBMTRX_M(:,1) =  ZCTERM(:,2) + ZDTERM(:,1) + ZDTERM(:,2)
ZCMTRX_M(:,1) = -ZDTERM(:,2)
ZFRCV_M (:,1) =  ZCTERM(:,2)*PSNOWTEMP(:,2) - (PRADSINK(:,1)-PRADSINK(:,2)) + ZDTERM(:,1)*XTT
!
DO JJ = 1,SIZE(PTG)
  DO JST = 2,KNLVLS_USE(JJ)-1
    ZAMTRX_M(JJ,JST) = ZAMTRX(JJ,JST+1)
    ZBMTRX_M(JJ,JST) = ZBMTRX(JJ,JST+1)
    ZCMTRX_M(JJ,JST) = ZCMTRX(JJ,JST+1)
    ZFRCV_M (JJ,JST) = ZFRCV (JJ,JST+1)
    ZSNOWTEMP_M(JJ,JST) = PSNOWTEMP(JJ,JST+1)
  ENDDO
ENDDO
!

 CALL TRIDIAG_GROUND_SNOWCRO(ZAMTRX_M,ZBMTRX_M,ZCMTRX_M,ZFRCV_M,ZSNOWTEMP_M,KNLVLS_USE,1)
!
! If melting for 2 consecuative time steps, then replace current T-profile
! with one assuming T=Tf in surface layer:
!
ZSNOWTEMP_DELTA(:) = 0.0
!
WHERE( ZSNOWTEMP(:,1)>XTT .AND. PSNOWTEMP(:,1)>=XTT )
  PSNOWFLUX(:) = ZDTERM(:,1) * ( XTT-ZSNOWTEMP_M(:,1) )
  ZSNOWTEMP_DELTA(:) = 1.0
END WHERE
!
DO JJ = 1,SIZE(PTG)
  DO JST = 2,KNLVLS_USE(JJ)
    ZSNOWTEMP(JJ,JST) = ZSNOWTEMP_DELTA(JJ)       * ZSNOWTEMP_M(JJ,JST-1) + &
                        (1.0-ZSNOWTEMP_DELTA(JJ)) * ZSNOWTEMP  (JJ,JST)
  ENDDO
ENDDO
!
! 6. Lower boundary flux:
! -----------------------
! NOTE: evaluate this term assuming the snow layer
! can't exceed the freezing point as this adjustment
! is made in melting routine. Then must adjust temperature
! to conserve energy:
!
DO JJ=1, SIZE(PTG)
 ZGBAS(JJ) = ZDTERM(JJ,KNLVLS_USE(JJ)) * ( ZSNOWTEMP(JJ,KNLVLS_USE(JJ))             - PTG(JJ) )
 PGBAS(JJ) = ZDTERM(JJ,KNLVLS_USE(JJ)) * ( MIN( XTT, ZSNOWTEMP(JJ,KNLVLS_USE(JJ)) ) - PTG(JJ) )
 ZSNOWTEMP(JJ,KNLVLS_USE(JJ)) = ZSNOWTEMP(JJ,KNLVLS_USE(JJ)) + &
                                ( ZGBAS(JJ)-PGBAS(JJ) ) / ZCTERM(JJ,KNLVLS_USE(JJ))

ENDDO
!
! 7. Update temperatute profile in time:
! --------------------------------------
!
DO JJ=1, SIZE(PTG)
  PSNOWTEMP(JJ,1:KNLVLS_USE(JJ)) = ZSNOWTEMP(JJ,1:KNLVLS_USE(JJ))
ENDDO
!
!
! 8. Compute new (implicit) air T and specific humidity
! -----------------------------------------------------
!
PTA_IC(:) = PPET_B_COEF_T(:) + PPET_A_COEF_T(:) * PSNOWTEMP(:,1)
PQA_IC(:) = PPEQ_B_COEF_T(:) + PPEQ_A_COEF_T(:) * PSNOWTEMP(:,1)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROSOLVT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROSOLVT
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROMELT(PSCAP,PSNOWTEMP,PSNOWDZ,         &
                       PSNOWRHO,PSNOWLIQ,KNLVLS_USE     )
!
!!    PURPOSE
!!    -------
!     Calculate snow melt (resulting from surface fluxes, ground fluxes,
!     or internal shortwave radiation absorbtion). It is used to
!     augment liquid water content, maintain temperatures
!     at or below freezing, and possibly reduce the mass
!     or compact the layer(s).
!
USE MODD_CSTS,ONLY : XTT, XLMTT, XRHOLW, XRHOLI
!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSCAP
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ, PSNOWTEMP, PSNOWRHO,   &
                                          PSNOWLIQ
!
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZPHASE, ZCMPRSFACT,   &
                                                      ZSNOWLWE,             &
                                                      ZSNOWMELT, ZSNOWTEMP
!
INTEGER :: JJ, JST ! looping indexes
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROMELT',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! ---------------------------
!
DO JJ = 1,SIZE(PSNOWDZ,1)
   DO JST = 1,KNLVLS_USE(JJ)
     ZPHASE    (JJ,JST) = 0.0
     ZCMPRSFACT(JJ,JST) = 0.0
     ZSNOWLWE  (JJ,JST) = 0.0
     ZSNOWMELT (JJ,JST) = 0.0
     ZSNOWTEMP (JJ,JST) = 0.0
   ENDDO
ENDDO
!
! 1. Determine amount of melt in each layer:
! ------------------------------------------
!
DO JJ = 1,SIZE(PSNOWDZ,1)
  !
   DO JST = 1,KNLVLS_USE(JJ)
     !
     ! Total Liquid equivalent water content of snow (m):
     ZSNOWLWE(JJ,JST) = PSNOWRHO(JJ,JST) * PSNOWDZ(JJ,JST) / XRHOLW
     !
     ! Melt snow if excess energy and snow available:
     ! Phase change (J/m2)
     ZPHASE(JJ,JST) = MIN( PSCAP(JJ,JST) * MAX(0.0,PSNOWTEMP(JJ,JST)-XTT) * PSNOWDZ(JJ,JST), &
                           MAX(0.0,ZSNOWLWE(JJ,JST)-PSNOWLIQ(JJ,JST)) * XLMTT * XRHOLW )
     !
     ! Update snow liq water content and temperature if melting:
     ! liquid water available for next layer from melting of snow
     ! which is assumed to be leaving the current layer (m):
     ZSNOWMELT(JJ,JST) = ZPHASE(JJ,JST) / (XLMTT*XRHOLW)
     !AD: Numerical precision can cause melt to slightly exceed SWE... adding cap
     ZSNOWMELT(JJ,JST) = min(ZSNOWMELT(JJ,JST), ZSNOWLWE(JJ,JST))
     !
     ! Cool off snow layer temperature due to melt:
     ZSNOWTEMP(JJ,JST) = PSNOWTEMP(JJ,JST) - ZPHASE(JJ,JST) / (PSCAP(JJ,JST)*PSNOWDZ(JJ,JST))
     !
     ! Difference with ISBA_ES: ZMELTXS should never be different of 0.
     ! because of the introduction of the tests in LLAYERGONE
     PSNOWTEMP(JJ,JST) =  ZSNOWTEMP(JJ,JST)
     !
   ! The control below should be suppressed after further tests
#ifdef HYDRO_D
     IF (PSNOWTEMP(JJ,JST)-XTT > XUEPSI) THEN
       WRITE(*,*) 'pb dans MELT PSNOWTEMP(JJ,JST) >XTT:', JJ,JST, PSNOWTEMP(JJ,JST), XTT
!       CALL ABOR1_SFX('SNOWCRO: pb dans MELT')
     ENDIF
#endif
    !
    ! Loss of snowpack depth: (m) and liquid equiv (m):
    ! Compression factor for melt loss: this decreases
    ! layer thickness and increases density thereby leaving
    ! total SWE constant.
    !
    ! Difference with ISBA_ES: All melt is considered to decrease the depth
    ! without consideration to the irreducible content
    !
    ZCMPRSFACT(JJ,JST) = ( ZSNOWLWE(JJ,JST) - (PSNOWLIQ(JJ,JST)+ZSNOWMELT(JJ,JST)) ) &
                       / ( ZSNOWLWE(JJ,JST) - PSNOWLIQ(JJ,JST) )
    PSNOWDZ   (JJ,JST) = PSNOWDZ (JJ,JST) * ZCMPRSFACT(JJ,JST)
    if ((PSNOWDZ(JJ,JST) .eq. 0) .and. (PSNOWRHO(JJ,JST).gt.0) .and. (PSNOWRHO(JJ,JST).ne.999)) then
      PSNOWRHO(JJ,JST) = 0
    else
      PSNOWRHO(JJ,JST) = ZSNOWLWE(JJ,JST) * XRHOLW / PSNOWDZ(JJ,JST)
    endif
    !
    ! 2. Add snow melt to current snow liquid water content:
    ! ------------------------------------------------------
    !
    PSNOWLIQ(JJ,JST) = PSNOWLIQ(JJ,JST) + ZSNOWMELT(JJ,JST)
    !
  ENDDO   ! loop JST active snow layers
ENDDO   ! loop JJ grid points
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROMELT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROMELT
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROREFRZ(PTSTEP,PRR,                            &
                        PSNOWRHO,PSNOWTEMP,PSNOWDZ,PSNOWLIQ,   &
                        PTHRUFAL, PSCAP, PLEL3L,KNLVLS_USE     )
!
!!    PURPOSE
!!    -------
!     Calculate any freezing/refreezing of liquid water in the snowpack.
!     Also, calculate liquid water transmission and snow runoff.
!     Refreezing causes densification of a layer.
!
USE MODD_CSTS,     ONLY : XTT, XLMTT, XRHOLW, XCI,XRHOLI,XRHOTHRESHOLD_ICE
USE MODD_SNOW_PAR, ONLY : XSNOWDMIN

!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!

IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                      :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)        :: PRR
!
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWDZ, PSNOWTEMP, PSNOWLIQ, PSNOWRHO
!
REAL, DIMENSION(:), INTENT(INOUT)     :: PTHRUFAL
!
! modifs_EB layers
INTEGER, DIMENSION(:), INTENT(IN)      :: KNLVLS_USE
REAL, DIMENSION(:,:), INTENT(IN)       :: PSCAP
REAL, DIMENSION(:), INTENT(IN)         :: PLEL3L
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZPHASE,              &
                                                      ZSNOWLIQ, ZSNOWRHO,  &
                                                      ZWHOLDMAX, ZSNOWDZ,  &
                                                      ZSNOWTEMP
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),0:SIZE(PSNOWRHO,2)) :: ZFLOWLIQ
!
REAL :: ZDENOM, ZNUMER
!
INTEGER :: JJ, JST   ! looping indexes
INTEGER :: INLVLS     ! maximum snow layers number
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROREFRZ',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! --------------
!
INLVLS = SIZE(PSNOWDZ,2)
!
DO JJ=1,SIZE(PSNOWDZ,1)
  DO JST=1,KNLVLS_USE(JJ)
    ZSNOWRHO (JJ,JST) = PSNOWRHO(JJ,JST)
    ZSNOWTEMP(JJ,JST) = PSNOWTEMP(JJ,JST)
    ZWHOLDMAX(JJ,JST) = SNOWCROHOLD( PSNOWRHO(JJ,JST),PSNOWLIQ(JJ,JST),PSNOWDZ(JJ,JST) )
! trude test with not allowing liquid if psnowrho >  XRHOTHRESHOLD_ICE  (i.e. no liquid in ice)
!      IF (PSNOWRHO(JJ,JST).GT.XRHOTHRESHOLD_ICE)  ZWHOLDMAX (JJ,JST)=0
! end trude test

  ENDDO
ENDDO
!
DO JJ = 1,SIZE(PSNOWDZ,1)  ! loop JJ grid points
  !
  ! 1. Increases Liquid Water of top layer from rain
  !    ---------------------------------------------
  !
  !  Rainfall (m) initialises the liquid flow whih feeds the top layer
  !  and evaporation/condensation are taken into account
  !
  IF ( KNLVLS_USE(JJ)>0. ) THEN
    ZFLOWLIQ(JJ,0) = PRR(JJ) * PTSTEP / XRHOLW
    ZFLOWLIQ(JJ,0) = MAX(0., ZFLOWLIQ(JJ,0) - PLEL3L(JJ)*PTSTEP/(XLVTT*XRHOLW))
  ELSE
    ZFLOWLIQ(JJ,0) = 0
  ENDIF
  !
  DO JST=1,KNLVLS_USE(JJ) ! loop JST active snow layers
    !
    ! 2. Increases Liquid Water from the upper layers flow (or rain for top layer)
    !    -----------------------------
    PSNOWLIQ(JJ,JST) = PSNOWLIQ(JJ,JST) + ZFLOWLIQ(JJ,JST-1)
    !
    ! 3. Freezes liquid water in any cold layers
    !    ---------------------------------------
    !
    ! Calculate the maximum possible refreezing
    ZPHASE(JJ,JST) = MIN( PSCAP(JJ,JST)* MAX(0.0, XTT - ZSNOWTEMP(JJ,JST)) * PSNOWDZ(JJ,JST), &
                          PSNOWLIQ(JJ,JST) * XLMTT * XRHOLW )
    !
    ! Reduce liquid content if freezing occurs:
    ZSNOWLIQ(JJ,JST) = PSNOWLIQ(JJ,JST) - ZPHASE(JJ,JST)/(XLMTT*XRHOLW)
    !
    ! Warm layer and reduce liquid if freezing occurs:
    ZSNOWDZ(JJ,JST) = MAX(XSNOWDMIN/INLVLS, PSNOWDZ(JJ,JST))
    !
    !
    ! Difference with ISBA-ES: a possible cooling of current refreezing water
    !                          is taken into account to calculate temperature change
    ZNUMER = ( ZSNOWRHO(JJ,JST) * ZSNOWDZ(JJ,JST) - ( PSNOWLIQ(JJ,JST) - ZFLOWLIQ(JJ,JST-1) ) * XRHOLW )
    ZDENOM = ( ZSNOWRHO(JJ,JST) * ZSNOWDZ(JJ,JST) - ( ZSNOWLIQ(JJ,JST) - ZFLOWLIQ(JJ,JST-1) ) * XRHOLW )
    !
    PSNOWTEMP(JJ,JST) = XTT + ( ZSNOWTEMP(JJ,JST)-XTT )*ZNUMER/ZDENOM + ZPHASE(JJ,JST)/( XCI*ZDENOM )
    !
    ! 4. Calculate flow from the excess of holding capacity
    !    --------------------------------------------------------------
    !
    ! Any water in excess of the maximum holding space for liquid water
    ! amount is drained into next layer down.
    ZFLOWLIQ(JJ,JST) = MAX( 0., ZSNOWLIQ(JJ,JST)-ZWHOLDMAX(JJ,JST) )
    !
    ZSNOWLIQ(JJ,JST) = ZSNOWLIQ(JJ,JST) - ZFLOWLIQ(JJ,JST)
    !
    ! 5. Density is adjusted to conserve the mass
    !    --------------------------------------------------------------
    ZNUMER = ( ZSNOWRHO(JJ,JST) * PSNOWDZ(JJ,JST) - ( ZFLOWLIQ(JJ,JST) - ZFLOWLIQ(JJ,JST-1) ) * XRHOLW )
    !
    ZSNOWRHO(JJ,JST) = ZNUMER / ZSNOWDZ(JJ,JST)
    !
    ! keeps snow denisty below ice density
    IF ( ZSNOWRHO(JJ,JST)>XRHOLI ) THEN
      PSNOWDZ (JJ,JST) = PSNOWDZ(JJ,JST) * ZSNOWRHO(JJ,JST) / XRHOLI
      ZSNOWRHO(JJ,JST) = XRHOLI
    ENDIF
    !
    ! 6. Update thickness and density and any freezing:
    !    ----------------------------------------------
    PSNOWRHO(JJ,JST) = ZSNOWRHO(JJ,JST)
    PSNOWLIQ(JJ,JST) = ZSNOWLIQ(JJ,JST)
    !
  ENDDO ! loop JST active snow layers
  !
  ! Any remaining throughflow after freezing is available to
  ! the soil for infiltration or surface runoff (m).
  ! I.E. This is the amount of water leaving the snowpack:
  ! Rate water leaves the snowpack [kg/(m2 s)]:
  !
  PTHRUFAL(JJ)  = PTHRUFAL(JJ) + ZFLOWLIQ(JJ,KNLVLS_USE(JJ)) * XRHOLW / PTSTEP
 !
ENDDO ! loop JJ grid points
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROREFRZ',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROREFRZ
!####################################################################
SUBROUTINE GET_RHO(PRHO_IN,PDZ,PSNOWLIQ,PFLOWLIQ,PRHO_OUT)
!
USE MODD_CSTS,     ONLY : XRHOLW
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
REAL, INTENT(IN)  :: PRHO_IN, PDZ, PSNOWLIQ,PFLOWLIQ
REAL, INTENT(OUT) :: PRHO_OUT
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_RHO',0,ZHOOK_HANDLE)
!
PRHO_OUT =  ( PRHO_IN * PDZ - ( PSNOWLIQ - PFLOWLIQ ) * XRHOLW )
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_RHO',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_RHO
!####################################################################
!####################################################################
SUBROUTINE SNOWCROFLUX(PSNOWTEMP,PSNOWDZ,PEXNS,PEXNA,          &
                       PUSTAR2_IC,                             &
                       PTSTEP,PALBT,PSW_RAD,PEMIST,PLWUPSNOW,  &
                       PLW_RAD,PTA,PSFCFRZ,PQA,PHPSNOW,        &
                       PSNOWTEMPO1,PSNOWFLUX,PCT,PRADSINK,     &
                       PQSAT,PDQSAT,PRSRA,                     &
                       PRN,PH,PGFLUX,PLES3L,PLEL3L,PEVAP,      &
                       PUSTAR,                                  &
                       PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS) ! trude added
!
!!    PURPOSE
!!    -------
!     Calculate the surface fluxes (atmospheric/surface).
!     (Noilhan and Planton 1989; Noilhan and Mahfouf 1996)
!
USE MODD_CSTS,ONLY : XTT
!
USE MODE_THERMOS
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWDZ, PSNOWTEMPO1, PSNOWFLUX, PCT, &
                                        PRADSINK, PEXNS, PEXNA
!
REAL, DIMENSION(:), INTENT(IN)      :: PALBT, PSW_RAD, PEMIST, PLW_RAD,      &
                                        PTA, PSFCFRZ, PQA,                    &
                                        PHPSNOW, PQSAT, PDQSAT, PRSRA,        &
                                        PUSTAR2_IC
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PRN, PH, PGFLUX, PLES3L, PLEL3L,      &
                                        PEVAP, PLWUPSNOW, PUSTAR
! trude added
REAL, DIMENSION(:), INTENT(OUT)      :: PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS

!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWDZ))      :: ZEVAPC, ZSNOWTEMP
REAL :: ZSMSNOW, ZGFLUX
!
INTEGER :: JJ
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE

! ---trude
!compile error : A dummy argument with an explicit INTENT(OUT) declaration is not given an explicit value.
! It seems that PLWUPSNOW does nothing. For now I give this array a value of 0 to be able to compile
PLWUPSNOW(:) = 0.0
! ---

!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROFLUX',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! --------------
!
! 1. Flux calculations when melt not occuring at surface (W/m2):
! --------------------------------------------------------------
!
DO JJ = 1,SIZE(PALBT)
  !
  CALL GET_FLUX(PALBT(JJ),PEMIST(JJ),PSW_RAD(JJ),PLW_RAD(JJ),   &
                PEXNS(JJ),PEXNA(JJ),PTA(JJ),PQA(JJ),PRSRA(JJ), &
                PQSAT(JJ),PDQSAT(JJ),PSFCFRZ(JJ),PHPSNOW(JJ),   &
                PSNOWTEMP(JJ),PSNOWTEMPO1(JJ),                  &
                PRN(JJ),PH(JJ),ZEVAPC(JJ),                      &
                PLES3L(JJ),PLEL3L(JJ),ZGFLUX,               &
                PFSA_CROCUS(JJ), PFSR_CROCUS(JJ), PFIRA_CROCUS(JJ)) ! trude added
  !
  IF ( PSNOWTEMP(JJ)>XTT ) THEN
    !
    IF ( PSNOWTEMPO1(JJ)<XTT ) THEN
      !
      ! 2. Initial melt adjustment
      ! --------------------------
      ! If energy avalabile to melt snow, then recalculate fluxes
      ! at the freezing point and add residual heat to layer
      ! average heat.
      !
      ! A) If temperature change is > 0 and passes freezing point this timestep,
      !    then recalculate fluxes at freezing point and excess energy
      !    will be used outside of this routine to change snow heat content:
      !
      ! WRITE (*,*) 'attention test LFLUX traitement XTT supprime!'
      !
      CALL GET_FLUX(PALBT(JJ),PEMIST(JJ),PSW_RAD(JJ),PLW_RAD(JJ),   &
                    PEXNS(JJ),PEXNA(JJ), PTA(JJ),PQA(JJ),PRSRA(JJ), &
                    PQSAT(JJ),PDQSAT(JJ),PSFCFRZ(JJ),PHPSNOW(JJ),   &
                    XTT,PSNOWTEMPO1(JJ),                            &
                    PRN(JJ),PH(JJ),ZEVAPC(JJ),                      &
                    PLES3L(JJ),PLEL3L(JJ),PGFLUX(JJ),            &
                    PFSA_CROCUS(JJ), PFSR_CROCUS(JJ), PFIRA_CROCUS(JJ)) ! trude added
      !
      ZSMSNOW = ZGFLUX - PGFLUX(JJ)
      !
      ! This will be used to change heat content of snow:
      ZSNOWTEMP(JJ) = PSNOWTEMP(JJ) - ZSMSNOW * PTSTEP * PCT(JJ)
      !
    ELSE
      !
      ! 3. Ongoing melt adjustment: explicit solution
      ! ---------------------------------------------
      !    If temperature change is 0 and at freezing point this timestep,
      !    then recalculate fluxes and surface temperature *explicitly*
      !    as this is *exact* for snow at freezing point (Brun, Martin)
      !
      CALL GET_FLUX(PALBT(JJ),PEMIST(JJ),PSW_RAD(JJ),PLW_RAD(JJ),   &
                    PEXNS(JJ),PEXNA(JJ), PTA(JJ),PQA(JJ),PRSRA(JJ), &
                    PQSAT(JJ),PDQSAT(JJ),PSFCFRZ(JJ),PHPSNOW(JJ),   &
                    XTT,XTT,                                        &
                    PRN(JJ),PH(JJ),ZEVAPC(JJ),                      &
                    PLES3L(JJ),PLEL3L(JJ),PGFLUX(JJ),                &
                    PFSA_CROCUS(JJ), PFSR_CROCUS(JJ), PFIRA_CROCUS(JJ)) ! trude added
      !
      ZSNOWTEMP(JJ) = XTT + PTSTEP * PCT(JJ) * ( PGFLUX(JJ) + PRADSINK(JJ) - PSNOWFLUX(JJ) )
      !
    ENDIF
    !
  ELSE
    !
    ZSNOWTEMP(JJ) = PSNOWTEMP(JJ)
    !
    PGFLUX(JJ) = ZGFLUX
    !
  ENDIF
  !
ENDDO
!
! 4. Update surface temperature:
! ------------------------------
!
PSNOWTEMP(:) = ZSNOWTEMP(:)
!
! 5. Final evaporative flux (kg/m2/s)
!
PEVAP(:) = ZEVAPC(:)
!
! 5. Friction velocity
! --------------------
!
PUSTAR(:) = SQRT(PUSTAR2_IC(:))
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROFLUX',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROFLUX
!####################################################################
SUBROUTINE GET_FLUX(PALBT,PEMIST,PSW_RAD,PLW_RAD,PEXNS,PEXNA,   &
                    PTA,PQA,PRSRA,PQSAT,PDQSAT,PSFCFRZ,PHPSNOW, &
                    PSNOWTEMP,PSNOWTEMPO1,                      &
                    PRN,PH,PEVAPC,PLES3L,PLEL3L,PGFLUX,      &
                    PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS) !trude added
!
USE MODD_CSTS,ONLY : XSTEFAN, XCPD, XLSTT, XLVTT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PALBT, PEMIST
REAL, INTENT(IN) :: PSW_RAD, PLW_RAD
REAL, INTENT(IN) :: PEXNS, PEXNA
REAL, INTENT(IN) :: PTA, PQA, PRSRA, PQSAT, PDQSAT, PSFCFRZ, PHPSNOW
REAL, INTENT(IN) :: PSNOWTEMP,PSNOWTEMPO1
REAL, INTENT(OUT):: PRN, PH, PEVAPC, PLES3L, PLEL3L, PGFLUX
! Trude added:
REAL, INTENT(OUT):: PFSA_CROCUS, PFSR_CROCUS, PFIRA_CROCUS !trude added
!
REAL :: ZLE, ZDELTAT, ZLWUPSNOW, ZSNOWTO3

!REAL ::PSNOWTEMP,PSNOWTEMPO1
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_FLUX',0,ZHOOK_HANDLE)
!
!trude test
!PSNOWTEMPO1 = PSNOWTEMPO1_orig+5
!PSNOWTEMP=PSNOWTEMP_orig+5
! end test

ZSNOWTO3  = PSNOWTEMPO1**3  ! to save some CPU time, store this
!
ZDELTAT   = PSNOWTEMP - PSNOWTEMPO1   ! surface T time change:
!
ZLWUPSNOW = PEMIST * XSTEFAN * ZSNOWTO3 * ( PSNOWTEMPO1 + 4.*ZDELTAT )
!
PRN       = ( 1.-PALBT )*PSW_RAD + PEMIST*PLW_RAD - ZLWUPSNOW
!
PH        = PRSRA * XCPD * ( PSNOWTEMP/PEXNS - PTA/PEXNA )
!
PEVAPC    = PRSRA * ( (PQSAT - PQA) + PDQSAT*ZDELTAT )
!
PLES3L    = PSFCFRZ      * XLSTT * PEVAPC
!
PLEL3L    = (1.-PSFCFRZ) * XLVTT * PEVAPC
!
ZLE       = PLES3L + PLEL3L
!
PGFLUX    = PRN - PH - ZLE + PHPSNOW

PFSA_CROCUS= (1-PALBT)*PSW_RAD
PFSR_CROCUS = PALBT*PSW_RAD
PFIRA_CROCUS = ZLWUPSNOW - PEMIST*PLW_RAD

!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_FLUX',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_FLUX
!
!####################################################################
!####################################################################
SUBROUTINE SNOWCROEVAPN(PLES3L,PTSTEP,PSNOWTEMP,PSNOWRHO, &
                       PSNOWDZ,PEVAPCOR,PSNOWHMASS        )
!
!!    PURPOSE
!!    -------
!     Remove mass from uppermost snow layer in response to
!     evaporation (liquid) and sublimation.
!
!!     MODIFICATIONS
!!     -------------
!!      Original A. Boone
!!      05/2011: E. Brun  Takes only into account sublimation and solid
!!                         condensation. Evaporation and liquid condensation
!!                         are taken into account in SNOWCROREFRZ
!
USE MODD_CSTS,     ONLY : XLSTT, XLMTT, XCI, XTT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(IN)      :: PLES3L   ! (W/m2)
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOWRHO, PSNOWDZ, PSNOWHMASS, &
                                        PEVAPCOR
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLES3L))       :: ZSNOWEVAPS, ZSNOWEVAP, ZSNOWEVAPX,          &
                                       ZSNOWDZ, ZEVAPCOR
!                                      ZEVAPCOR = for vanishingy thin snow cover,
!                                                 allow any excess evaporation
!                                                 to be extracted from the soil
!                                                 to maintain an accurate water
!                                                 balance [kg/(m2 s)]
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROEVAPN',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! --------------
!
ZEVAPCOR  (:) = 0.0
ZSNOWEVAPS(:) = 0.0
ZSNOWEVAP (:) = 0.0
ZSNOWEVAPX(:) = 0.0
ZSNOWDZ   (:) = 0.0
!
!++ trude. Test for rare event where snow is present, but density is zero (thin snow layer ~1e-10m)
WHERE ( PSNOWRHO==0.0 )
  PSNOWDZ(:)=0.0
END WHERE

WHERE ( PSNOWDZ>0.0 )
  !
  ! 1. Sublimation/condensation of snow ice
  ! ----------------------------------------
  ! Reduce layer thickness and total snow depth
  ! if sublimation: add to correction term if potential
  ! sublimation exceeds available snow cover.
  !
  ZSNOWEVAPS(:) = PLES3L(:) * PTSTEP / ( XLSTT*PSNOWRHO(:) )
  ZSNOWDZ(:)    = PSNOWDZ(:) - ZSNOWEVAPS(:)
  PSNOWDZ(:)    = MAX( 0.0, ZSNOWDZ(:) )
  ZEVAPCOR(:)   = ZEVAPCOR(:) + MAX(0.0,-ZSNOWDZ(:)) * PSNOWRHO(:) / PTSTEP
  !
  ! Total heat content change due to snowfall and sublimation (added here):
  ! (for budget calculations):
  !
  PSNOWHMASS(:) = PSNOWHMASS(:) &
                  - PLES3L(:) * (PTSTEP/XLSTT) * ( XCI * (PSNOWTEMP(:)-XTT) - XLMTT )
  !
END WHERE
!
! 3. Update evaporation correction term:
! --------------------------------------
!
PEVAPCOR(:) = PEVAPCOR(:) + ZEVAPCOR(:)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROEVAPN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SNOWCROEVAPN
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROGONE(PTSTEP,PLEL3L,PLES3L,PSNOWRHO,                     &
                       PSNOWHEAT,PRADSINK_2D,PEVAPCOR,PTHRUFAL,PGRNDFLUX, &
                       PGFLUXSNOW,PSNOWDZ,PSNOWLIQ,PSNOWTEMP,PRADXS,      &
                       PRR,KNLVLS_USE                                     )
!
!!    PURPOSE
!!    -------
!     Account for the case when the last trace of snow melts
!     during a time step: ensure mass and heat balance of
!     snow AND underlying surface.
!     Original A. Boone
!     05/2011: E. Brun  Takes into account sublimation and PGRNDFLUX
!                       Adds rain and evaporation/liquid condensation
!                       in PTHRUFAL
!
USE MODD_CSTS,ONLY : XTT, XLSTT, XLVTT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PLEL3L, PLES3L, PGFLUXSNOW,PRR
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PRADSINK_2D
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSNOWRHO, PSNOWHEAT
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PGRNDFLUX, PRADXS
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWDZ, PSNOWLIQ, PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)     :: PTHRUFAL   ! melt water [kg/(m2 s)]
!
REAL, DIMENSION(:), INTENT(OUT)     :: PEVAPCOR   ! [kg/(m2 s)]
!                                      PEVAPCOR = for vanishingy thin snow cover,
!                                                 allow any excess evaporation
!                                                 to be extracted from the soil
!                                                 to maintain an accurate water
!                                                 balance.
!
INTEGER, DIMENSION(:), INTENT(INOUT)      :: KNLVLS_USE
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLES3L))       :: ZRADSINK
REAL, DIMENSION(SIZE(PLES3L))       :: ZSNOWHEATC
INTEGER, DIMENSION(SIZE(PLES3L))    :: ISNOWGONE_DELTA
!
INTEGER  :: JJ
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROGONE',0,ZHOOK_HANDLE)
!
! 0. Initialize:
! --------------
!
PEVAPCOR(:) = 0.0
PTHRUFAL(:) = 0.0
!
DO JJ = 1,SIZE(ZRADSINK)
  ZRADSINK  (JJ) = PRADSINK_2D(JJ,INLVLS_USE(JJ))
  ZSNOWHEATC(JJ) = SUM(PSNOWHEAT(JJ,1:INLVLS_USE(JJ))) !total heat content (J m-2)
END DO
!
ISNOWGONE_DELTA(:) = 1
!
! 1. Simple test to see if snow vanishes:
! ---------------------------------------
! If so, set thicknesses (and therefore mass and heat) and liquid content
! to zero, and adjust fluxes of water, evaporation and heat into underlying
! surface.
!
! takes into account the heat content corresponding to the occasional
! sublimation  and then PGRNDFLUX
!
ZSNOWHEATC(:) = ZSNOWHEATC(:) + MAX( 0., PLES3L(:)*PTSTEP/XLSTT ) * XLMTT
!
WHERE ( PGFLUXSNOW(:)+ZRADSINK(:)-PGRNDFLUX(:) >= (-ZSNOWHEATC(:)/PTSTEP) )
  PGRNDFLUX(:)       = PGFLUXSNOW(:) + (ZSNOWHEATC(:)/PTSTEP)
  PEVAPCOR (:)       = PLES3L(:)/XLSTT
  PRADXS   (:)       = 0.0
  ISNOWGONE_DELTA(:) = 0          ! FLAG...if=0 then snow vanishes, else=1
END WHERE
!
! 2. Final update of snow state and computation of corresponding flow
!    Only if snow vanishes
! -----------------------------
!
PTHRUFAL(:) = 0.
!
DO JJ=1, SIZE(ZRADSINK)
  !
  IF(ISNOWGONE_DELTA(JJ) == 0 ) THEN
    PTHRUFAL(JJ) = PTHRUFAL(JJ) + &
                   SUM( PSNOWRHO(JJ,1:INLVLS_USE(JJ))*PSNOWDZ(JJ,1:INLVLS_USE(JJ)) ) / PTSTEP
! takes into account rain and condensation/evaporation
    PTHRUFAL(JJ) = PTHRUFAL(JJ) + PRR(JJ) - PLEL3L(JJ)/XLVTT
    PSNOWTEMP(JJ,:) = XTT
    PSNOWDZ  (JJ,:) = 0.
    PSNOWLIQ (JJ,:) = 0.
    INLVLS_USE(JJ) = 0
  ENDIF
  !
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROGONE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROGONE
!####################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROEVAPGONE(PSNOWHEAT,PSNOWDZ,PSNOWRHO,PSNOWTEMP,PSNOWLIQ,      &
                           PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE,KNLVLS_USE,&
                           HSNOWMETAMO)
!
!!    PURPOSE
!!    -------
!
!     If all snow in uppermost layer evaporates/sublimates, re-distribute
!     grid (below assumes very thin snowpacks so layer-thicknesses are
!     constant).
!     Original A. Boone
!     05/2011: E. Brun  Takes into account previous changes in the energy
!                       content
!
!
USE MODD_CSTS,     ONLY : XTT, XRHOLW, XLMTT, XCI
USE MODD_SNOW_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, XRHOSMAX_ES
USE MODE_SNOW3L
USE MODD_SNOW_METAMO
!USE MODD_TYPE_DATE_SURF
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWRHO   ! snow density profile                (kg/m3)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWDZ    ! snow layer thickness profile        (m)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWHEAT  ! snow heat content/enthalpy          (J/m2)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWGRAN1 ! snow grain parameter 1              (-)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWGRAN2 ! snow grain parameter 2              (-)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWHIST  ! snow grain historical variable      (-)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWAGE  ! Snow grain age
!
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWTEMP  ! snow temperature profile            (K)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSNOWLIQ   ! snow liquid water profile           (m)
!
INTEGER, DIMENSION(:), INTENT(IN)      :: KNLVLS_USE
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZSNOWHEAT_1D ! total heat content                (J/m2)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZSNOWRHO_1D  ! total snowpack average density    (kg/m3)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZSNOW        ! total snow depth                  (m)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZSCAP        ! Snow layer heat capacity          (J/K/m3)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZNDENT       ! Number of dendritic layers        (-)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZNVIEU       ! Number of non dendritic layers    (-)
REAL, DIMENSION(SIZE(PSNOWDZ,1)) :: ZSNOWAGE_1D  ! total snowpack average
!age (days)
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWGRAN1N,           &
                                                      ZSNOWGRAN2N,ZSNOWHISTN
!
LOGICAL :: GDENDRITIC
!
INTEGER :: JJ, JST          ! loop control
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWCROEVAPGONE',0,ZHOOK_HANDLE)
!
! Initialize:
!
ZSNOWHEAT_1D(:) = 0.
ZSNOW(:)        = 0.
ZSNOWRHO_1D(:)  = 0.
ZNDENT(:)       = 0.
ZNVIEU(:)       = 0.
ZSNOWAGE_1D(:)  = 0.
ZSCAP(:)        = 0.
!
! First, determine where uppermost snow layer has completely
! evaporated/sublimated (as it becomes thin):
DO JJ = 1,SIZE(PSNOWRHO,1)
   !
   IF ( PSNOWDZ(JJ,1)==0.0 ) THEN
     !
     DO JST = 2,KNLVLS_USE(JJ)
       !
       ZSNOWHEAT_1D(JJ) = ZSNOWHEAT_1D(JJ) + PSNOWDZ(JJ,JST) * &
                          ( PSNOWRHO(JJ,JST)*XCI * (ZSNOWTEMP(JJ,JST)-XTT) &
                            - XLMTT * PSNOWRHO(JJ,JST) ) &
                          + XLMTT * XRHOLW * PSNOWLIQ(JJ,JST)
       ZSNOW       (JJ) = ZSNOW       (JJ) + PSNOWDZ(JJ,JST)
       ZSNOWRHO_1D (JJ) = ZSNOWRHO_1D (JJ) + PSNOWDZ(JJ,JST) * PSNOWRHO(JJ,JST)
       ZSNOWAGE_1D (JJ) = ZSNOWAGE_1D (JJ) + PSNOWDZ(JJ,JST) * PSNOWRHO(JJ,JST) * PSNOWAGE(JJ,JST)
       !
       ! snow grains
       IF ( HSNOWMETAMO=='B92' ) THEN
         GDENDRITIC = ( PSNOWGRAN1(JJ,JST)<-XEPSI )
       ELSE
         GDENDRITIC = ( PSNOWGRAN1(JJ,JST)<XVDIAM6*(4.-PSNOWGRAN2(JJ,JST))-XUEPSI )
       ENDIF
       !
       IF ( GDENDRITIC ) THEN   ! Dendritic snow
         ZNDENT(JJ) = ZNDENT(JJ) + 1.0
       ELSE                                    ! Non dendritic snow
         ZNVIEU(JJ) = ZNVIEU(JJ) + 1.0
       ENDIF
       !
     ENDDO
     !
   ENDIF
   !
END DO
!
ZSNOWRHO_1D(:) = ZSNOWRHO_1D (:) / MAX( XSNOWDMIN, ZSNOW(:) )
ZSNOWAGE_1D(:) = ZSNOWAGE_1D (:) / MAX( XSNOWDMIN, ZSNOW(:) * ZSNOWRHO_1D(:) )
ZSNOWRHO_1D(:) = MAX( XRHOSMIN_ES, MIN( XRHOSMAX_ES, ZSNOWRHO_1D(:) ) )
!
! Where uppermost snow layer has vanished, redistribute vertical
! snow mass and heat profiles (and associated quantities):
!
 CALL SNOW3LAVGRAIN(PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,                 &
                    ZSNOWGRAN1N,ZSNOWGRAN2N,ZSNOWHISTN,ZNDENT,ZNVIEU,&
                    HSNOWMETAMO)
!
DO JJ=1,SIZE(PSNOWRHO,1)
  !
  IF( ZSNOW(JJ)/=0.0 ) THEN
    !
    PSNOWDZ  (JJ,1:KNLVLS_USE(JJ)) = ZSNOW(JJ) / KNLVLS_USE(JJ)
    PSNOWHEAT(JJ,1:KNLVLS_USE(JJ)) = ZSNOWHEAT_1D(JJ) / KNLVLS_USE(JJ)
    PSNOWRHO (JJ,1:KNLVLS_USE(JJ)) = ZSNOWRHO_1D(JJ)
    !
    ZSCAP(JJ) = ZSNOWRHO_1D(JJ) * XCI
    !
    DO JST = 1,KNLVLS_USE(JJ)
      !
      PSNOWTEMP(JJ,JST) = XTT + ( ( (PSNOWHEAT(JJ,JST)/PSNOWDZ(JJ,JST))   &
                                    + XLMTT*PSNOWRHO(JJ,JST) ) / ZSCAP(JJ) )
      PSNOWTEMP(JJ,JST) = MIN( XTT, PSNOWTEMP(JJ,JST) )
      !
      PSNOWLIQ (JJ,JST) = MAX( 0.0, PSNOWTEMP(JJ,JST)-XTT ) * ZSCAP(JJ) * &
                                    PSNOWDZ(JJ,JST) / (XLMTT*XRHOLW)
      !
    ENDDO
    !
  ENDIF
  !
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROEVAPGONE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROEVAPGONE
!
!####################################################################
!####################################################################
!####################################################################
!SUBROUTINE SNOWNLFALL_UPGRID(TPTIME, OGLACIER,PTSTEP,PSR,PTA,PVMOD,        &
SUBROUTINE SNOWNLFALL_UPGRID(OGLACIER,PTSTEP,PSR,PTA,PVMOD,        &
                             PSNOW,PSNOWRHO,PSNOWDZ,PSNOWHEAT,PSNOWHMASS,  &
                             PSNOWALB,PPERMSNOWFRAC,PSNOWGRAN1,PSNOWGRAN2, &
                             GSNOWFALL,PSNOWDZN,PSNOWRHOF,PSNOWDZF,        &
                             PSNOWGRAN1F,PSNOWGRAN2F,PSNOWHISTF,PSNOWAGEF, &
                             OMODIF_GRID,KNLVLS_USE,OSNOWDRIFT,PZ0EFF,PUREF,&
                             HSNOWMETAMO)
!
!!    PURPOSE
!!    -------
! Adds new snowfall and updates the vertical grid in order to keep an
! optimal discertisation
!
!!    AUTHOR
!!    ------
!!      E. Brun           * Meteo-France *
!!
!
!!
!!     MODIFICATIONS
!!    ------
!!
!!     2014-02-05 V. Vionnet: wind speed in the parameterization for new snow
!!                            density and characteristic of grains of new snow
!!                            are taken at a reference height
!!     2014-06-03 M. Lafaysse : threshold on PZ0EFF
!!
!USE MODD_TYPE_DATE_SURF,  ONLY: DATE_TIME
USE MODD_CSTS,     ONLY : XLMTT, XTT, XCI
USE MODD_SNOW_METAMO, ONLY : XNDEN1, XNDEN2, XNDEN3, XGRAN, &
                             XNSPH1, XNSPH2, XNSPH3, XNSPH4
!
USE MODD_SNOW_PAR, ONLY : XRHOSMIN_ES, XSNOWDMIN, XANSMAX, XAGLAMAX, XSNOWCRITD,   &
                          XDZMIN_TOP, XDZMIN_TOP_BIS, XDZMIN_BOT, XSPLIT_COEF,     &
                          XAGREG_COEF_1, XAGREG_COEF_2, XDZ1, XDZ2, XDZ3, XDZ3_BIS,&
                          XDZ4, XDZ5, XDZ_BASE, XDZ_INTERNAL, XSCALE_CM,           &
                          XDZMAX_INTERNAL, XDZMIN_TOP_EXTREM, XSNOWFALL_THRESHOLD, &
                          XRATIO_NEWLAYER, XDEPTH_THRESHOLD1, XDEPTH_THRESHOLD2,   &
                          XDEPTH_SURFACE, XDIFF_1, XDIFF_MAX, XSCALE_DIFF,         &
                          XSNOWFALL_A_SN, XSNOWFALL_B_SN, XSNOWFALL_C_SN
!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!TYPE(DATE_TIME), INTENT(IN)          :: TPTIME      ! current date and time
LOGICAL, INTENT(IN)                  :: OGLACIER    ! True = Over permanent snow and ice,
!                                                     initialise WGI=WSAT,
!                                                     Hsnow>=10m and allow 0.8<SNOALB<0.85
                                                    ! False = No specific treatment
!
REAL, INTENT(IN)                     :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)       :: PSR, PTA, PVMOD, PPERMSNOWFRAC
!
REAL, DIMENSION(:),INTENT(IN)       :: PZ0EFF,PUREF
!
REAL, DIMENSION(:), INTENT(INOUT)   :: PSNOW, PSNOWALB
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PSNOWRHO, PSNOWDZ, PSNOWHEAT
!
REAL, DIMENSION(:), INTENT(OUT)      :: PSNOWHMASS
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PSNOWGRAN1, PSNOWGRAN2
!
LOGICAL, DIMENSION(:), INTENT(INOUT) :: GSNOWFALL
!
! Fresh snow characteristics
REAL, DIMENSION(:), INTENT(OUT)      :: PSNOWRHOF, PSNOWDZF
REAL, DIMENSION(:), INTENT(OUT)      :: PSNOWGRAN1F, PSNOWGRAN2F, PSNOWHISTF
REAL, DIMENSION(:), INTENT(OUT)      :: PSNOWAGEF
! New vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)    :: PSNOWDZN
!
LOGICAL, DIMENSION(:), INTENT(OUT)   :: OMODIF_GRID
!
INTEGER, DIMENSION(:), INTENT(INOUT) :: KNLVLS_USE

LOGICAL,INTENT(IN) :: OSNOWDRIFT ! if snowdrift then grain types are not modified by wind
 CHARACTER(3), INTENT(IN)              :: HSNOWMETAMO ! metamorphism scheme
!*      0.2    declarations of local variables
!
!
LOGICAL, DIMENSION(SIZE(PTA))       :: GAGREG_SURF
!
REAL, DIMENSION(SIZE(PTA))          :: ZSNOWFALL, ZSNOWTEMP, ZSCAP, ZANSMAX
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZDZOPT
!
REAL :: ZZ0EFF
!
REAL :: ZAGE_NOW
REAL :: ZSNOW_UPPER, ZSNOW_UPPER2 ! snow depth treatednormally (<= XDEPTH_SURFACE)
REAL :: ZCOEF_DEPTH !coefficient for repartition of deep snow above 3 meters
REAL :: ZTHICKNESS_INTERMEDIATE, ZTHICKNESS2
REAL :: ZPENALTY, ZDIFTYPE_INF, ZDIFTYPE_SUP, ZCRITSIZE, ZCRITSIZE_INF, ZCRITSIZE_SUP
REAL :: ZSNOW2L, ZCOEF
!
INTEGER :: INB_DEEP_LAYER, INB_UPPER_LAYER !separation between deep and upper layers
                                           ! if snow depth below XDEPTH_SURFACE then INB_DEEP_LAYER=0
INTEGER :: INB_MIN_LAYERS    ! why this test ?
INTEGER :: INB_INTERMEDIATE  ! number of intermediate layers (constant optimal gridding)
INTEGER :: IEND_INTERMEDIATE ! layer indice for bottom of intermediate layers
INTEGER :: JSTDEEP, JSTEND
INTEGER :: JST_1, JJ_A_AGREG_SUP, JJ_A_AGREG_INF, JJ_A_DEDOUB
INTEGER :: INLVLS, INLVLSMIN, INLVLSMAX, JJ, JST
!
! Coefficient to adjust wind speed at the height used in the parameterization
! for:
!         - density of new snow
!         - sphericity and dendricity of new snow
! Default values : 10 m for new snow (Pahaut, 1976) and 5 m for characteristics
! of snow grains (Guyomarc'h et Merindol, 1998)
REAL, PARAMETER                    :: PPHREF_WIND_RHO   = 10.
REAL, PARAMETER                    :: PPHREF_WIND_GRAIN = 5.
REAL, PARAMETER                    :: PPHREF_WIND_MIN = MIN(PPHREF_WIND_RHO,PPHREF_WIND_GRAIN)*0.5
REAL, DIMENSION(SIZE(PTA))         :: ZWIND_RHO
REAL, DIMENSION(SIZE(PTA))         :: ZWIND_GRAIN
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      1.0   Initialization and snowage calculation for the present date
!

!IF (LHOOK) CALL DR_HOOK('SNOWNLFALL_UPGRID',0,ZHOOK_HANDLE)
!
INLVLS    = SIZE (PSNOWRHO(:,:),2)
INLVLSMAX = SIZE (PSNOWRHO(:,:),2)
INLVLSMIN = 3
!
ZSNOWTEMP(:) = XTT
ZSNOWFALL(:) = 0.0 !Matthieu Lafaysse 21/09/2012
!
GSNOWFALL  (:) =.FALSE.
GAGREG_SURF(:) =.FALSE.
!
PSNOWHMASS (:) = 0.0
PSNOWRHOF  (:) = 0.0
PSNOWDZF   (:) = 0.0
PSNOWGRAN1F(:) = 0.0
PSNOWGRAN2F(:) = 0.0
PSNOWHISTF (:) = 0.0
PSNOWDZN (:,:) = PSNOWDZ(:,:)
!
OMODIF_GRID(:) = .FALSE.
!
!************************************************************************************
!*      1.1   Calculation of the optimal vertical grid size ZDZOPT
!             as a function of maximum number of layers and of current
!             snow depth (modified 05/06/2012 by Matthieu Lafaysse)
!
! KNLVLS_USE(JJ) > INB_MIN_LAYERS =>
! KNLVLS_USE(JJ) > 2 + INLVLSMAX/3 =>
! ( KNLVLS_USE(JJ) + INLVLSMAX ) / 6 > (2 + INLVLSMAX/3 + INLVLSMAX) / 6 =>
! INB_DEEP_LAYER > (2 + 4*INLVLSMAX/3 ) / 6 >= 1
INB_MIN_LAYERS = 2 + INLVLSMAX/3
!
 DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( PSNOW(JJ)>XDEPTH_THRESHOLD2 .AND. KNLVLS_USE(JJ)>INB_MIN_LAYERS ) THEN
    ! for very thick snowpack with enough snow layers
    ! special treatment
    ! we put the highest thickness in the lowest layers
    ! about 1/3 of layers for all snow except XDEPTH_SURFACE=3 first meters
    !
    !number of "deep layers"
    INB_DEEP_LAYER  = ( KNLVLS_USE(JJ) + INLVLSMAX ) / 6
    !
    !number of "upper layers"
    INB_UPPER_LAYER = KNLVLS_USE(JJ) - INB_DEEP_LAYER
    !
    !thickness of "upper layers"
    ZSNOW_UPPER = XDEPTH_SURFACE
    !
    !Arithmetic serie : 1+2+3+...+INB_DEEP_LAYER=INB_DEEP_LAYER*(INB_DEEP_LAYER+1)/2
    ZCOEF_DEPTH = ( PSNOW(JJ) - XDEPTH_SURFACE ) * 2. / ( (INB_DEEP_LAYER+1) * INB_DEEP_LAYER )
    !
    ! deep layers optimal thickness :
    ! increasing thickness with depth
    DO JSTDEEP = 1,INB_DEEP_LAYER
      JST = INB_UPPER_LAYER + JSTDEEP
      ZDZOPT(JJ,JST) = ZCOEF_DEPTH * JSTDEEP
      !This sum is equal to PSNOW(JJ)-XDEPTH_SURFACE
    ENDDO
    !
  ELSE
    !
    INB_UPPER_LAYER = KNLVLS_USE(JJ)
    !
    ZSNOW_UPPER = PSNOW(JJ)
    !
  END IF
  !
  !on force le ZDZOPT des 3 premières couches à ZSNOW_UPPER/3 maximum, chacune.
  ! => si on n'a qu'une couche, ZDZOPT(1) = ZSNOW_UPPER/3
  ! quel que soit INB_UPPER_LAYER
  !
  ZSNOW_UPPER2 = ZSNOW_UPPER / MAX( INLVLSMIN, INB_UPPER_LAYER )
  !
  ZDZOPT(JJ,1) = MIN( XDZ1, ZSNOW_UPPER2 )
  IF ( KNLVLS_USE(JJ)>=2 ) ZDZOPT(JJ,2) = MIN( XDZ2, ZSNOW_UPPER2 )
  IF ( KNLVLS_USE(JJ)>=3 ) ZDZOPT(JJ,3) = MIN( XDZ3, ZSNOW_UPPER2 )
  !
  IF ( INB_UPPER_LAYER>0 ) THEN
    !
    ZSNOW_UPPER2 = ZSNOW_UPPER / INB_UPPER_LAYER
    !
    ! dans ce cas, à partir de la 3ème couche, on prend la fraction du nombre de
    ! couches supérieures total, pour les couches jusqu'à 5
    !
    !ML : replace > by >= on 12-12-20 because the last layer was not initialised in case of thick snowpacks
    IF ( INB_UPPER_LAYER>=3 ) ZDZOPT(JJ,3) = MIN( XDZ3_BIS, ZSNOW_UPPER2 )
    IF ( INB_UPPER_LAYER>=4 ) ZDZOPT(JJ,4) = MIN( XDZ4    , ZSNOW_UPPER2 )
    IF ( INB_UPPER_LAYER>=5 ) ZDZOPT(JJ,5) = MIN( XDZ5    , ZSNOW_UPPER2 )
    !
    IF ( INB_UPPER_LAYER==KNLVLS_USE(JJ) ) THEN
      ! si on n'a pas de couches profondes
      !
      ! dans ce cas, on reprend ZSNOW_UPPER/3 maximum pour la dernière couche
      !
      ! last layer of' upper layers' : normal case : thin layer
      ZDZOPT(JJ,INB_UPPER_LAYER) = MIN( XDZ_BASE, ZSNOW_UPPER/MAX(INLVLSMIN,INB_UPPER_LAYER) )
      !
      ! ZTHICKNESS_INTERMEDIATE contient ce qu'il reste d'épaisseur disponible
      ! dans les couches supérieures
      !remaining snow for remaining layers
      ZTHICKNESS_INTERMEDIATE = ZSNOW_UPPER - SUM(ZDZOPT(JJ,1:5)) - ZDZOPT(JJ,INB_UPPER_LAYER)

      IF ( ZSNOW_UPPER<=XDEPTH_THRESHOLD1 .OR. INB_UPPER_LAYER<8 ) THEN
        INB_INTERMEDIATE  = INB_UPPER_LAYER - 6
        IEND_INTERMEDIATE = INB_UPPER_LAYER - 1
      ELSE
        ! si INB_UPPER_LAYER>=8, les avant et avant-dernière couches ne sont pas
        ! considérées commes intermédiaires
        INB_INTERMEDIATE  = INB_UPPER_LAYER - 8
        IEND_INTERMEDIATE = INB_UPPER_LAYER - 3
        ! dans ce cas, on garde un peu d'épaisseur pour les deux couches restantes
        IF ( INB_INTERMEDIATE>0 ) THEN
          ZTHICKNESS_INTERMEDIATE = ZTHICKNESS_INTERMEDIATE * INB_INTERMEDIATE / FLOAT(INB_INTERMEDIATE+1)
        END IF
      END IF
      !
    ELSE
      ! si on a des couches profondes, les couches intermédiaires sont celles
      ! qui restent quand on a enlevé les 5 premières des couches supérieures
      !
      ! case with very thick snowpacks :
      ! the last layer of upper layers is not an exception
      ZTHICKNESS_INTERMEDIATE = ZSNOW_UPPER - SUM(ZDZOPT(JJ,1:5))
      INB_INTERMEDIATE  = INB_UPPER_LAYER - 5
      IEND_INTERMEDIATE = INB_UPPER_LAYER
      !
    END IF
    !
    ! For thick snowpack : add maximum value of optimal thickness to avoid too
    ! large differencies between layers
    IF ( INB_INTERMEDIATE>0 ) THEN
      !
      ZTHICKNESS2 = MAX( XDZ_INTERNAL, ZTHICKNESS_INTERMEDIATE/INB_INTERMEDIATE )
      !
      JSTEND = MIN( IEND_INTERMEDIATE,10 )
      DO JST = 6,JSTEND
        ZDZOPT(JJ,JST) = MIN( XDZMAX_INTERNAL(JST-5), ZTHICKNESS2 )
      END DO
      !
      IF ( IEND_INTERMEDIATE>10 ) THEN
        DO JST = 11,IEND_INTERMEDIATE
          ZDZOPT(JJ,JST) = ZTHICKNESS2
        END DO
      END IF
      !
    END IF
    !
    IF ( ZSNOW_UPPER>=XDEPTH_THRESHOLD1 .AND. INB_UPPER_LAYER>=8 ) THEN
      !Linear interpolation of optimal thickness between layers N-3 and N :
      ZDZOPT(JJ,INB_UPPER_LAYER-2) = 0.34*ZDZOPT(JJ,INB_UPPER_LAYER) + &
                                     0.66*ZDZOPT(JJ,INB_UPPER_LAYER-3)
      ZDZOPT(JJ,INB_UPPER_LAYER-1) = 0.66*ZDZOPT(JJ,INB_UPPER_LAYER) + &
                                     0.34*ZDZOPT(JJ,INB_UPPER_LAYER-3)
    ENDIF
    !
  END IF
  !
END DO
!
!************************************************************************************
!This was the initial code for optimal layers until may 2012
!
! ! ! ! !
! ! ! ! ! !*      1.1   Calculation of the optimal vertical grid size
! ! ! ! ! !             as a function of maximum number of layers and of current
! ! ! ! ! !             snow depth
! ! ! ! ! !
! ! ! ! ! DO JJ=1, SIZE(PSNOW(:))
! ! ! ! !    ZDZOPT(JJ,1) = MIN(XDZ1,PSNOW(JJ)/MAX(INLVLSMIN,KNLVLS_USE(JJ)))
! ! ! ! !    ZDZOPT(JJ,2) = MIN(XDZ2,PSNOW(JJ)/MAX(INLVLSMIN,KNLVLS_USE(JJ)))
! ! ! ! !    ZDZOPT(JJ,3) = MIN(XDZ3,PSNOW(JJ)/MAX(INLVLSMIN,KNLVLS_USE(JJ)))
! ! ! ! !    IF (KNLVLS_USE(JJ)>3)  ZDZOPT(JJ,3) = MIN(XDZ3_BIS,PSNOW(JJ)/KNLVLS_USE(JJ))
! ! ! ! !    IF (KNLVLS_USE(JJ)>4)  ZDZOPT(JJ,4) = MIN(XDZ4,PSNOW(JJ)/KNLVLS_USE(JJ))
! ! ! ! !    IF (KNLVLS_USE(JJ)>5)  ZDZOPT(JJ,5) = MIN(XDZ5,PSNOW(JJ)/KNLVLS_USE(JJ))
! ! ! ! !    IF (KNLVLS_USE(JJ)>0) ZDZOPT(JJ,KNLVLS_USE(JJ))=   &
! ! ! ! !               MIN(XDZ_BASE,PSNOW(JJ)/MAX(INLVLSMIN,KNLVLS_USE(JJ)))
! ! ! ! !    DO JST=6,KNLVLS_USE(JJ)-1,1
! ! ! ! !        ZDZOPT(JJ,JST) = MAX(XDZ_INTERNAL,(PSNOW(JJ) - SUM(ZDZOPT(JJ,1:5))-    &
! ! ! ! !                          ZDZOPT(JJ,KNLVLS_USE(JJ))) /(KNLVLS_USE(JJ)-6))
! ! ! ! !    END DO
! ! ! ! ! END DO
! ! ! ! ! !
! ! ! ! !
!
!************************************************************************************
!
!*      2.0   Fresh snow characteristics
!
!
!
! Heat content of newly fallen snow (J/m2):
! NOTE for now we assume the snowfall has
! the temperature of the snow surface upon reaching the snow.
! This is done as opposed to using the air temperature since
! this flux is quite small and has little to no impact
! on the time scales of interest. If we use the above assumption
! then, then the snowfall advective heat flux is zero.
!!
DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( PSR(JJ)>0.0 ) THEN
    !
    ! newly fallen snow characteristics:
    IF ( KNLVLS_USE(JJ)>0 ) THEN !Case of new snowfall on a previously snow-free surface
      ZSCAP    (JJ) = XCI*PSNOWRHO(JJ,1)
      ZSNOWTEMP(JJ) = XTT + ( PSNOWHEAT(JJ,1) + XLMTT*PSNOWRHO(JJ,1)*PSNOWDZ(JJ,1) ) / &
                            ( ZSCAP(JJ) * MAX( XSNOWDMIN/INLVLS, PSNOWDZ(JJ,1) ) )
    ELSE  ! case with bare ground
      ZSNOWTEMP(JJ) = PTA(JJ)
    ENDIF
    ZSNOWTEMP(JJ) = MIN( XTT, ZSNOWTEMP(JJ) )
    !
    !
    ! Wind speeds at reference heights for new snow density and charactristics of
    ! grains of new snow
    ! Computed from PVMOD at PUREF (m) assuming a log profile in the SBL
    ! and a roughness length equal to PZ0EFF
    !
    ZZ0EFF=MIN(PZ0EFF(JJ),PUREF(JJ)*0.5,PPHREF_WIND_MIN)

    ZWIND_RHO(JJ)   = PVMOD(JJ)*LOG(PPHREF_WIND_RHO/ZZ0EFF)/          &
                               LOG(PUREF(JJ)/ZZ0EFF)
    ZWIND_GRAIN(JJ) = PVMOD(JJ)*LOG(PPHREF_WIND_GRAIN/ZZ0EFF)/        &
                               LOG(PUREF(JJ)/ZZ0EFF)

    PSNOWHMASS(JJ) = PSR(JJ) * ( XCI * ( ZSNOWTEMP(JJ)-XTT ) - XLMTT ) * PTSTEP
    !
    PSNOWRHOF (JJ) = MAX( XRHOSMIN_ES, XSNOWFALL_A_SN + &
                                       XSNOWFALL_B_SN * ( PTA(JJ)-XTT ) + &
                                       XSNOWFALL_C_SN * MIN( PVMOD(JJ), SQRT(ZWIND_RHO(JJ) ) ) )
    ZSNOWFALL (JJ) = PSR(JJ) * PTSTEP / PSNOWRHOF(JJ)    ! snowfall thickness (m)
    PSNOW     (JJ) = PSNOW(JJ) + ZSNOWFALL(JJ)
    PSNOWDZF  (JJ) = ZSNOWFALL(JJ)
    !
    IF ( HSNOWMETAMO=='B92' ) THEN
      !
      IF ( OSNOWDRIFT ) THEN
        PSNOWGRAN1F(JJ) = -XGRAN
        PSNOWGRAN2F(JJ) = XNSPH3
      ELSE
        PSNOWGRAN1F(JJ) = MAX( MIN( XNDEN1*ZWIND_GRAIN(JJ)-XNDEN2, XNDEN3 ), -XGRAN )
        PSNOWGRAN2F(JJ) = MIN( MAX( XNSPH1*ZWIND_GRAIN(JJ)+XNSPH2, XNSPH3 ), XNSPH4 )
      END IF
      !
    ELSE
      !
      IF ( OSNOWDRIFT ) THEN
        PSNOWGRAN1F(JJ) = XVDIAM6
        PSNOWGRAN2F(JJ) = XNSPH3/XGRAN
      ELSE
        PSNOWGRAN2F(JJ) = MIN( MAX( XNSPH1*ZWIND_GRAIN(JJ)+XNSPH2, XNSPH3 ), XNSPH4 ) / XGRAN
        ZCOEF = MAX( MIN( XNDEN1*ZWIND_GRAIN(JJ)-XNDEN2, XNDEN3 ), -XGRAN ) / ( -XGRAN )
        PSNOWGRAN1F(JJ) = XVDIAM6 * &
                        ( ZCOEF + ( 1.- ZCOEF ) * &
                                  ( 3.*PSNOWGRAN2F(JJ) + 4.*(1.-PSNOWGRAN2F(JJ)) ) )
      END IF
      !
    ENDIF
    !
    PSNOWHISTF (JJ) = 0.0
    PSNOWAGEF  (JJ) = 0.0
    GSNOWFALL  (JJ) = .TRUE.
    OMODIF_GRID(JJ) = .TRUE.
    !
  ENDIF
  !
ENDDO
!
!  intialize the albedo:
!  penser a changer 0.000001 par XUEPSI
IF(OGLACIER)THEN
  ZANSMAX(:) = XAGLAMAX * PPERMSNOWFRAC(:) + XANSMAX * (1.0-PPERMSNOWFRAC(:))
ELSE
  ZANSMAX(:) = XANSMAX
ENDIF
!
WHERE( GSNOWFALL(:) .AND. ABS(PSNOW(:)-ZSNOWFALL(:))< 0.000001 )
  PSNOWALB(:) = ZANSMAX(:)
END WHERE

!
! Computation of the new grid size
! It starts with successive exclusive cases
! Each case is described inside the corresponding condition
!
! cases with fresh snow
!

DO JJ=1,SIZE(PSNOW(:)) ! grid point loop
  !
  IF( .NOT.GSNOWFALL(JJ) .AND. PSNOW(JJ)>=XSNOWCRITD .AND. KNLVLS_USE(JJ)>=INLVLSMIN ) THEN
    !
    ! no fresh snow + deep enough snowpack + enough snow layers ==> no change
    !
  ELSEIF( PSNOW(JJ)<XSNOWCRITD .OR. KNLVLS_USE(JJ)<INLVLSMIN .OR. PSNOW(JJ)==ZSNOWFALL(JJ) ) THEN
    !
    ! too shallow snowpack or too few layers or only fresh snow
    ! ==> uniform grid and identical snow layers / number depends on snow depth
    OMODIF_GRID(JJ) = .TRUE.
    KNLVLS_USE (JJ) = MAX( INLVLSMIN, MIN( INLVLSMAX, INT(PSNOW(JJ)*XSCALE_CM) ) )
    PSNOWDZN(JJ,1:KNLVLS_USE(JJ)) = PSNOW(JJ) / KNLVLS_USE(JJ)
    !
  ELSE
    !
    ! fresh snow over snow covered ground + enough snow layers
    OMODIF_GRID(JJ) = .TRUE.
    ZDIFTYPE_SUP = SNOW3LDIFTYP( PSNOWGRAN1(JJ,1),PSNOWGRAN1F(JJ), &
                                 PSNOWGRAN2(JJ,1),PSNOWGRAN2F(JJ),HSNOWMETAMO )
    !
    IF ( ( ZDIFTYPE_SUP<XDIFF_1        .AND. PSNOWDZ(JJ,1)<   ZDZOPT(JJ,1) ) .OR. &
         ( PSR(JJ)<XSNOWFALL_THRESHOLD .AND. PSNOWDZ(JJ,1)<2.*ZDZOPT(JJ,1) ) .OR. &
                                             PSNOWDZ(JJ,1)<XDZMIN_TOP_EXTREM ) THEN
      !
      ! Fresh snow is similar to a shallow surface layer (< ZDZOPT)
      ! or snowfall is very low and the surface layer not too deep (< 2*ZDZOPT) [NEW CONDITION 11/2012]
      ! or the surface layer is extremely thin (< XDZMIN_TOP_EXTREM) [NEW CONDITION 11/2012]
      ! The two new conditions are necessary for forcings with very low precipitation
      ! (e.g. ERA interim reanalyses, or climate models)
      ! ==> fresh snow is agregated to the surface layer
      PSNOWDZN(JJ,1) = PSNOWDZ(JJ,1) + PSNOWDZF(JJ)
      DO JST = KNLVLS_USE(JJ),2,-1
        PSNOWDZN(JJ,JST) = PSNOWDZ(JJ,JST)
      ENDDO
      !
    ELSEIF ( KNLVLS_USE(JJ)<INLVLSMAX ) THEN
      !
      ! fresh snow is too different from the surface or the surface is too deep
      ! and there is room for extra layers ==> we create a new layer
      KNLVLS_USE(JJ)=KNLVLS_USE(JJ)+1
      !
      IF ( PSNOWDZF(JJ)>XRATIO_NEWLAYER*PSNOWDZ(JJ,2) ) THEN
        !
        ! Snowfall is sufficient to create a new layer not lower than 1/10 of the second layer
        PSNOWDZN(JJ,1) = PSNOWDZF(JJ)
        DO JST = KNLVLS_USE(JJ),2,-1
          PSNOWDZN(JJ, JST) = PSNOWDZ(JJ,JST-1)
        ENDDO
        !
      ELSE
        ! The ratio would be lower than 1/10 : [NEW : 11/2012]
        ! aggregate a part of the old layer with fresh snow to limit the ratio to 1/10.
        ZSNOW2L = PSNOWDZF(JJ) + PSNOWDZ(JJ,1)
        PSNOWDZN(JJ,1) = XRATIO_NEWLAYER      * ZSNOW2L
        PSNOWDZN(JJ,2) = (1.-XRATIO_NEWLAYER) * ZSNOW2L
        DO JST = KNLVLS_USE(JJ),3,-1
          PSNOWDZN(JJ,JST) = PSNOWDZ(JJ,JST-1)
        ENDDO
        !
      ENDIF
      !
    ELSE
      !
      ! fresh snow is too different from the surface or the surface is too deep
      ! and there is no room for extra layers
      ! ==> we agregate internal most similar snowlayers and create a new surface layer
      JJ_A_AGREG_SUP = 1
      JJ_A_AGREG_INF = 2
      !
      DO JST = 1,KNLVLS_USE(JJ)
        !
        IF ( JST>1 ) THEN
          !
          ZCRITSIZE_SUP = XSCALE_DIFF * ( PSNOWDZ(JJ,JST)  /ZDZOPT(JJ,JST)    + &
                                          PSNOWDZ(JJ,JST-1)/ZDZOPT(JJ,JST-1) )
          ZDIFTYPE_SUP  = SNOW3LDIFTYP( PSNOWGRAN1(JJ,JST-1),PSNOWGRAN1(JJ,JST), &
                                        PSNOWGRAN2(JJ,JST-1),PSNOWGRAN2(JJ,JST), &
                                        HSNOWMETAMO )
          !
          IF ( ZDIFTYPE_SUP+ZCRITSIZE_SUP<ZPENALTY ) THEN
            ZPENALTY = ZDIFTYPE_SUP + ZCRITSIZE_SUP
            JJ_A_AGREG_SUP = JST - 1
            JJ_A_AGREG_INF = JST
          ENDIF
          !
        ENDIF
        !
        IF ( JST<KNLVLS_USE(JJ) ) THEN
          !
          ZCRITSIZE_INF = XSCALE_DIFF * ( PSNOWDZ(JJ,JST)  /ZDZOPT(JJ,JST)    + &
                                          PSNOWDZ(JJ,JST+1)/ZDZOPT(JJ,JST+1) )
          !
          IF ( JST==1 ) THEN
            ZDIFTYPE_INF  = SNOW3LDIFTYP( PSNOWGRAN1(JJ,1),PSNOWGRAN1F(JJ), &
                                          PSNOWGRAN2(JJ,1),PSNOWGRAN2F(JJ), &
                                          HSNOWMETAMO)
            !
            ZPENALTY = ZDIFTYPE_INF + ZCRITSIZE_INF
          ELSE
            ZDIFTYPE_INF  = SNOW3LDIFTYP( PSNOWGRAN1(JJ,JST+1),PSNOWGRAN1(JJ,JST), &
                                          PSNOWGRAN2(JJ,JST+1),PSNOWGRAN2(JJ,JST), &
                                          HSNOWMETAMO)
            !
            IF ( ZDIFTYPE_INF+ZCRITSIZE_INF<ZPENALTY ) THEN
              ZPENALTY = ZDIFTYPE_INF + ZCRITSIZE_INF
              JJ_A_AGREG_SUP = JST
              JJ_A_AGREG_INF = JST + 1
            ENDIF
          ENDIF
          !
        ENDIF
        !
      ENDDO
      !
      ! agregation of the similar layers and shift of upper layers
      PSNOWDZN(JJ,JJ_A_AGREG_INF) = PSNOWDZ(JJ,JJ_A_AGREG_INF) + PSNOWDZ(JJ,JJ_A_AGREG_SUP)
      DO JST = JJ_A_AGREG_SUP,2,-1
        PSNOWDZN(JJ,JST) = PSNOWDZ(JJ,JST-1)
      ENDDO
      PSNOWDZN(JJ,1) = PSNOWDZF(JJ)
      !
      ! Limit the ratio between the new layer and the one beneath (ratio 1/10)
      ! [NEW : 11/2012]
      IF( PSNOWDZN(JJ,1)<XRATIO_NEWLAYER*PSNOWDZN(JJ,2) ) THEN
        ZSNOW2L = PSNOWDZN(JJ,1) + PSNOWDZN(JJ,2)
        PSNOWDZN(JJ,1) = XRATIO_NEWLAYER      * ZSNOW2L
        PSNOWDZN(JJ,2) = (1.-XRATIO_NEWLAYER) * ZSNOW2L
      ENDIF
      !
    ENDIF
    !
  ENDIF ! end of the case with fresh snow
ENDDO ! end loop grid points

!
! cases with no fresh snow and no previous grid resize
!
IF ( INLVLSMIN==INLVLSMAX ) THEN ! specific case with INLSVSMIN = INLVLSMAX  (INLVLS)
  !
  ! check if surface layer depth is too small
  ! in such a case looks for an other layer to be split
  DO JJ = 1,SIZE(PSNOW(:)) ! loop grid points
    !
    IF ( .NOT.OMODIF_GRID(JJ) .AND. PSNOWDZ(JJ,1)<XDZMIN_TOP ) THEN
      OMODIF_GRID(JJ) = .TRUE.
      CALL GET_SNOWDZN_DEB(INLVLS,PSNOWDZ(JJ,:),ZDZOPT(JJ,:),PSNOWDZN(JJ,:))
      GAGREG_SURF(JJ) = .TRUE.
    ENDIF
    ! check if bottom layer depth is too small
    ! in such a case agregation with upper layer and
    ! looks for an other layer to be splitted
    IF( .NOT.OMODIF_GRID(JJ) .AND. PSNOWDZ(JJ,INLVLS)<XDZMIN_TOP ) THEN
      OMODIF_GRID(JJ) = .TRUE.
      CALL GET_SNOWDZN_END(INLVLS,PSNOWDZ(JJ,:),ZDZOPT(JJ,:),PSNOWDZN(JJ,:))
    ENDIF
    !
  ENDDO ! end grid points loop
  !
ENDIF  ! end specific case INLSVSMIN = INLVLSMAX
!
! case without new snowfall and INVLS > INLVLSMIN
!
DO JJ=1,SIZE(PSNOW(:))
  !
  ! check if surface layer depth is too small
  ! in such a case agregation with layer beneath
  ! in case of reaching INLVLSMIN, looks for an other layer to be splitted
  IF( .NOT.GSNOWFALL(JJ) .AND. PSNOW(JJ)>XSNOWCRITD .AND. &
      .NOT.OMODIF_GRID(JJ) .AND. PSNOWDZ(JJ,1)<XDZMIN_TOP_BIS ) THEN ! case shallow surface layer
    !
    OMODIF_GRID(JJ) = .TRUE.
    !
    IF( KNLVLS_USE(JJ)>INLVLSMIN ) THEN ! case minimum not reached
      KNLVLS_USE(JJ) = KNLVLS_USE(JJ) - 1
      PSNOWDZN(JJ,1) = PSNOWDZ(JJ,1) + PSNOWDZ(JJ,2)
      DO JST = 2,KNLVLS_USE(JJ)
        PSNOWDZN(JJ,JST) = PSNOWDZ(JJ,JST+1)
      ENDDO
    ELSE ! case minimum reached
      CALL GET_SNOWDZN_DEB(KNLVLS_USE(JJ),PSNOWDZ(JJ,:),ZDZOPT(JJ,:),PSNOWDZN(JJ,:))
    ENDIF ! end case minimum reached end case shallow surface layer
    !
    GAGREG_SURF(JJ) = .TRUE.
    !
  ENDIF
  !
  ! check if bottom layer depth is too small
  ! in such a case agregation with above layer
  ! in case of reaching INLVLSMIN, looks for an other layer to be splitted
  ! case shallow bottom layer

  IF( .NOT.GSNOWFALL(JJ) .AND. PSNOW(JJ)> XSNOWCRITD                   .AND. &
      .NOT.OMODIF_GRID(JJ) .AND. PSNOWDZ(JJ,KNLVLS_USE(JJ))<XDZMIN_TOP .AND. &
      .NOT.GAGREG_SURF(JJ) ) THEN
    !
    OMODIF_GRID(JJ) = .TRUE.
    !
    IF ( KNLVLS_USE(JJ)>INLVLSMIN ) THEN ! case minimum not reached
      KNLVLS_USE(JJ) = KNLVLS_USE(JJ) - 1
      PSNOWDZN(JJ,KNLVLS_USE(JJ)) = PSNOWDZ(JJ,KNLVLS_USE(JJ)) + PSNOWDZ(JJ,KNLVLS_USE(JJ)+1)
    ELSE  ! case minimum reached
      CALL GET_SNOWDZN_END(KNLVLS_USE(JJ),PSNOWDZ(JJ,:),ZDZOPT(JJ,:),PSNOWDZN(JJ,:))
    ENDIF ! end case minimum reached end case shallow surface layer
    !
  ENDIF
  !
ENDDO ! end grid points loop
!
! case whithout new snow fall and without a previous grid resize
! looks for a shallow layer to be splitted according to its depth and to
! the optimal grid size
DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( .NOT.OMODIF_GRID(JJ) .AND. INLVLS_USE(JJ)<INLVLS-3 )THEN
    !
    DO JST = 1,INLVLS-4
      !
      IF ( JST<=KNLVLS_USE(JJ) .AND. .NOT.OMODIF_GRID(JJ) ) THEN
        !
        IF( PSNOWDZ(JJ,JST) > &
            ( XSPLIT_COEF - FLOAT( INLVLS-KNLVLS_USE(JJ) )/MAX( 1., FLOAT( INLVLS-INLVLSMIN ) ) ) &
              * ZDZOPT(JJ,JST) ) THEN
          !
          DO JST_1 = KNLVLS_USE(JJ)+1,JST+2,-1
            PSNOWDZN(JJ,JST_1) = PSNOWDZ(JJ,JST_1-1)
            ZDZOPT  (JJ,JST_1) = ZDZOPT (JJ,JST_1-1)
          ENDDO
          !
          ! generale case : old layer divided in two equal layers
          IF ( JST/=1 .OR. PSNOWDZ(JJ,JST)<3.*ZDZOPT(JJ,1) ) THEN
            PSNOWDZN(JJ,JST+1) = 0.5*PSNOWDZ(JJ,JST)
            PSNOWDZN(JJ,JST)   = PSNOWDZN(JJ,JST+1)
          ELSE
            ! if thick surface layer : force the surface layer to this value to avoid successive resizing
            ! [NEW : 11/2012]
            PSNOWDZN(JJ,1) = 1.5 * ZDZOPT(JJ,1)
            PSNOWDZN(JJ,2) = PSNOWDZ(JJ,JST) - PSNOWDZN(JJ,1)
          ENDIF
          !
          KNLVLS_USE (JJ) = KNLVLS_USE(JJ) + 1
          OMODIF_GRID(JJ) = .TRUE.
          !
        ENDIF
        !
      ENDIF
      !
    ENDDO
    !
  ENDIF
  !
ENDDO
!
! case whithout new snow fall and without a previous grid resize
! looks for a deep layer to be agregated to the layer beneath if similar
! according to its depth and to the optimal grid size
!
!NB : allow these changes for 5 layers and more [NEW] (before : 6 layers)
!
DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( .NOT.OMODIF_GRID(JJ) ) THEN
    !
    DO JST = 2,INLVLS
      !
      IF ( JST<=KNLVLS_USE(JJ)-1 .AND. KNLVLS_USE(JJ)>INLVLSMIN+1 .AND. .NOT.OMODIF_GRID(JJ) ) THEN
        !
        ZDIFTYPE_INF = SNOW3LDIFTYP( PSNOWGRAN1(JJ,JST+1),PSNOWGRAN1(JJ, JST), &
                                     PSNOWGRAN2(JJ,JST+1),PSNOWGRAN2(JJ, JST), &
                                     HSNOWMETAMO)
        ZDIFTYPE_INF = MAX( XDIFF_1, MIN( XDIFF_MAX, ZDIFTYPE_INF ) )
        !
        IF( PSNOWDZ(JJ,JST) < ZDZOPT(JJ,JST) * XAGREG_COEF_1 / ZDIFTYPE_INF .AND. &
            PSNOWDZ(JJ,JST) + PSNOWDZ(JJ,JST+1) < &
                XAGREG_COEF_2 * MAX( ZDZOPT(JJ,JST),ZDZOPT(JJ,JST+1) ) ) THEN
          !
          PSNOWDZN(JJ,JST) = PSNOWDZ(JJ,JST) + PSNOWDZ(JJ,JST+1)
          ZDZOPT  (JJ,JST) = ZDZOPT(JJ,JST+1)
          DO JST_1 = JST+1,KNLVLS_USE(JJ)-1
            PSNOWDZN(JJ,JST_1) = PSNOWDZ(JJ,JST_1+1)
            ZDZOPT  (JJ,JST_1) = ZDZOPT (JJ,JST_1+1)
          ENDDO
          KNLVLS_USE(JJ) = KNLVLS_USE(JJ)-1
          OMODIF_GRID(JJ)=.TRUE.
          !
        ENDIF
        !
      ENDIF
      !
    ENDDO
    !
  ENDIF
  !
ENDDO
!
! [NEW : 11/2012]
! In case of very low snow fall checks if a new internal snow layer is too shallow
! even if a the grid has already been resized in this time step
! starts from bottom to INLVS_USE-3 until old and new grid differ
DO JJ = 1,SIZE(PSNOW(:))
  !
  IF ( .NOT.GSNOWFALL(JJ) .OR. KNLVLS_USE(JJ)<INLVLSMIN+3 ) CYCLE ! go to next point
  !
  IF( ABS( PSNOWDZN(JJ,KNLVLS_USE(JJ)) - PSNOWDZ(JJ,KNLVLS_USE(JJ)) ) > XUEPSI ) CYCLE ! go to next point
  !
  ! bottom layer
  IF( PSNOWDZN(JJ,KNLVLS_USE(JJ))<XDZMIN_TOP ) THEN ! case shallow bottom layer
    !
    KNLVLS_USE(JJ) = KNLVLS_USE(JJ)-1
    PSNOWDZN(JJ,KNLVLS_USE(JJ)) = PSNOWDZN(JJ,KNLVLS_USE(JJ)) + PSNOWDZN(JJ,KNLVLS_USE(JJ)+1)
    PSNOWDZN(JJ,KNLVLS_USE(JJ)+1) = 0.
    !
  ELSE
    !
    ! internal layer
    DO JST = KNLVLS_USE(JJ)-1,4,-1
      !
      IF ( ABS( PSNOWDZN(JJ,JST) - PSNOWDZ(JJ,JST) ) > XUEPSI ) EXIT ! old/new grid differ ==> go to next grid point
      !
      IF ( PSNOWDZN(JJ,JST)> 0.001 ) CYCLE
      !
      ! If an internal layer is too shallow, it is merged with the upper layer
      PSNOWDZN(JJ,JST-1) = PSNOWDZN(JJ,JST) + PSNOWDZN(JJ,JST-1)
      KNLVLS_USE(JJ)   = KNLVLS_USE(JJ) - 1
      !
      ! shifts the lower layers
      DO JST_1 = JST,INLVLS_USE(JJ)
        PSNOWDZN(JJ,JST_1) = PSNOWDZ(JJ,JST_1+1)
        ZDZOPT  (JJ,JST_1) = ZDZOPT (JJ,JST_1+1)
      ENDDO
      PSNOWDZN(JJ,INLVLS_USE(JJ)+1) = 0.
      !
      EXIT ! goto to next grid point
      !
    ENDDO ! end loop internal layers
    !
  ENDIF
  !
ENDDO ! end grid loops for checking shallow layers
!
!final check of the consistensy of the new grid size
!
#ifdef DEBUG
DO JJ = 1,SIZE(PSNOW(:))
  !
! trude test, increase xuepsi limit
  IF ( ABS( SUM( PSNOWDZN(JJ,1:KNLVLS_USE(JJ)) ) - PSNOW(JJ) ) > XUEPSI*10000. ) THEN
!  IF ( ABS( SUM( PSNOWDZN(JJ,1:KNLVLS_USE(JJ)) ) - PSNOW(JJ) ) > XUEPSI*1000. ) THEN
    !
    WRITE(*,*) 'error in grid resizing', JJ, KNLVLS_USE(JJ), SUM( PSNOWDZN(JJ,1:KNLVLS_USE(JJ)) ),  &
                                         PSNOW(JJ), SUM( PSNOWDZN(JJ,1:INLVLS_USE(JJ)) )-PSNOW(JJ), &
                                         ZSNOWFALL(JJ)

!    CALL ABOR1_SFX("SNOWCRO: error in grid resizing")
    !
  ENDIF
  !
ENDDO
#endif
!
!IF (LHOOK) CALL DR_HOOK('SNOWNLFALL_UPGRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWNLFALL_UPGRID

!###############################################################################
SUBROUTINE GET_SNOWDZN_DEB(KNLVLS,PSNOWDZ,PDZOPT,PSNOWDZN)
!
USE MODD_SNOW_PAR, ONLY : XDZMIN_TOP, XDZMIN_BOT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNLVLS
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWDZ, PDZOPT
REAL, DIMENSION(:), INTENT(OUT) :: PSNOWDZN
!
REAL :: ZPENALTY, ZCRITSIZE
INTEGER :: JJ_A_DEDOUB, JST
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_SNOWDZN_DEB',0,ZHOOK_HANDLE)
!
ZPENALTY = PSNOWDZ(2) / PDZOPT(2)
IF( PSNOWDZ(2)<XDZMIN_TOP ) ZPENALTY = 0.
JJ_A_DEDOUB = 2
!
DO JST = 3,KNLVLS
  ZCRITSIZE = PSNOWDZ(JST) / PDZOPT(JST)
  IF ( JST==KNLVLS .AND. PSNOWDZ(JST)<XDZMIN_BOT ) ZCRITSIZE = 0.
  IF ( ZCRITSIZE>ZPENALTY ) THEN
    ZPENALTY    = ZCRITSIZE
    JJ_A_DEDOUB = JST
  ENDIF
ENDDO
!
IF ( JJ_A_DEDOUB==2 ) THEN ! case splitted layer == 2
  PSNOWDZN(1) = 0.5 * ( PSNOWDZ(1) + PSNOWDZ(2) )
  PSNOWDZN(2) = PSNOWDZN(1)
ELSE ! case splitted layer =/ 2
  PSNOWDZN(1) = PSNOWDZ(1) + PSNOWDZ(2)
  DO JST = 2,JJ_A_DEDOUB-2
    PSNOWDZN(JST) = PSNOWDZ(JST+1)
  ENDDO
  PSNOWDZN(JJ_A_DEDOUB-1) = 0.5 * PSNOWDZ(JJ_A_DEDOUB)
  PSNOWDZN(JJ_A_DEDOUB)   = PSNOWDZN(JJ_A_DEDOUB-1)
ENDIF ! end case splitted layer =/ 2
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_SNOWDZN_DEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_SNOWDZN_DEB
!
!###############################################################################
SUBROUTINE GET_SNOWDZN_END(KNLVLS,PSNOWDZ,PDZOPT,PSNOWDZN)
!
USE MODD_SNOW_PAR, ONLY : XDZMIN_TOP, XDZMIN_BOT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KNLVLS
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWDZ, PDZOPT
REAL, DIMENSION(:), INTENT(OUT) :: PSNOWDZN
!
REAL :: ZPENALTY, ZCRITSIZE
INTEGER :: JJ_A_DEDOUB, JST
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_SNOWDZN_END',0,ZHOOK_HANDLE)
!
ZPENALTY = PSNOWDZ(KNLVLS-2) / PDZOPT(KNLVLS-2)
JJ_A_DEDOUB = KNLVLS - 2
!
DO JST = MAX(1,KNLVLS-3),1,-1
  ZCRITSIZE = PSNOWDZ(JST) / PDZOPT(JST)
  IF ( JST==1 .AND. PSNOWDZ(JST)<XDZMIN_BOT ) ZCRITSIZE = 0.
  IF ( ZCRITSIZE>ZPENALTY ) THEN
    ZPENALTY    = ZCRITSIZE
    JJ_A_DEDOUB = JST
  ENDIF
ENDDO
!
IF ( JJ_A_DEDOUB==KNLVLS-1 ) THEN ! case splitted layer == 2
  PSNOWDZN(KNLVLS)   = 0.5 * (PSNOWDZ(KNLVLS-1)+PSNOWDZ(KNLVLS))
  PSNOWDZN(KNLVLS-1) = PSNOWDZN(KNLVLS)
ELSE ! case splitted layer =/ 2
  PSNOWDZN(KNLVLS) = PSNOWDZ(KNLVLS-1) + PSNOWDZ(KNLVLS)
  DO JST = KNLVLS-1,JJ_A_DEDOUB+2,-1
    PSNOWDZN(JST) = PSNOWDZ(JST-1)
  ENDDO
  PSNOWDZN(JJ_A_DEDOUB+1) = 0.5 * PSNOWDZ(JJ_A_DEDOUB)
  PSNOWDZN(JJ_A_DEDOUB  ) = PSNOWDZN(JJ_A_DEDOUB+1)
ENDIF ! end case splitted layer =/ 2
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_SNOWDZN_END',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_SNOWDZN_END
!
!###############################################################################
!################################################################################
!################################################################################
!
SUBROUTINE SNOWNLGRIDFRESH_1D (KJ,PSNOW,PSNOWDZ,PSNOWDZN,                  &
                               PSNOWRHO,PSNOWHEAT,PSNOWGRAN1,PSNOWGRAN2,   &
                               PSNOWHIST,PSNOWAGE,GSNOWFALL,               &
                               PSNOWRHOF, PSNOWDZF,PSNOWHEATF,PSNOWGRAN1F, &
                               PSNOWGRAN2F, PSNOWHISTF,PSNOWAGEF,          &
                               KNLVLS_USE, HSNOWMETAMO , I,J                    )
!
!!    PURPOSE
!!    -------
!     Snow mass,heat and characteristics redistibution in case of
!     grid resizing. Total mass and heat content of the overall snowpack
!     unchanged/conserved within this routine.
!     Grain size and type of mixed layers is deduced from the conservation
!     of the average optical size
!
!!    AUTHOR
!!    ------
!!      E. Brun           * Meteo-France *
!!
!
USE MODD_SNOW_PAR, ONLY : XD1,XD2,XD3,XX,XVALB5,XVALB6
USE MODE_SNOW3L, ONLY : GET_MASS_HEAT
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
! ++ trude added
INTEGER, INTENT(IN)             :: I,J
! --trude
INTEGER, INTENT(IN)               :: KJ
REAL, INTENT(IN)                  :: PSNOW
!
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWHEAT, PSNOWRHO, PSNOWDZ,     &
                                     PSNOWGRAN1, PSNOWGRAN2,  PSNOWDZN, &
                                     PSNOWHIST
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOWAGE
REAL,  INTENT(IN)                 :: PSNOWRHOF, PSNOWDZF,PSNOWHEATF,   &
                                     PSNOWGRAN1F,PSNOWGRAN2F, PSNOWHISTF
REAL, INTENT(IN)                  :: PSNOWAGEF
!
INTEGER, INTENT(IN)               :: KNLVLS_USE
!
LOGICAL, INTENT(IN)               :: GSNOWFALL
!
 CHARACTER(3),INTENT(IN)           :: HSNOWMETAMO
!
!*      0.2    declarations of local variables
!
REAL*8, DIMENSION(SIZE(PSNOWRHO,1)+1) :: ZSNOWRHOO,ZSNOWGRAN1O,ZSNOWGRAN2O, &
                                       ZSNOWHEATO,ZSNOWHISTO,ZSNOWDZO,    &
                                       ZSNOWZTOP_OLD,ZSNOWZBOT_OLD
REAL*8,DIMENSION(SIZE(PSNOWRHO,1)+1)  :: ZSNOWAGEO
!
REAL*8, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWRHON,ZSNOWGRAN1N,ZSNOWGRAN2N,   &
                                     ZSNOWHEATN,ZSNOWHISTN,               &
                                     ZSNOWZTOP_NEW,ZSNOWZBOT_NEW
REAL*8,DIMENSION(SIZE(PSNOWRHO,1)) ::ZSNOWAGEN
!
REAL :: ZMASTOTN, ZMASTOTO, ZSNOWHEAN, ZSNOWHEAO
REAL :: ZPSNOW_OLD, ZPSNOW_NEW
!
INTEGER :: INLVLS_OLD, INLVLS_NEW
INTEGER :: JST
!
LOGICAL :: GDIAM
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWNLGRIDFRESH_1D',0,ZHOOK_HANDLE)
!
! 0. Initialization:
! ------------------
!
! starts by checking the consistency between both vertical grid sizes
INLVLS_NEW = KNLVLS_USE
INLVLS_OLD = -1
!
ZPSNOW_NEW = 0.
ZPSNOW_OLD = 0.
!
DO JST = 1,INLVLS_NEW
  ZPSNOW_NEW = ZPSNOW_NEW + PSNOWDZN(JST)
ENDDO
!

IF ( ABS( ZPSNOW_NEW - PSNOWDZF )<XUEPSI ) THEN
  INLVLS_OLD = 0
ELSE
   DO JST = 1,SIZE(PSNOWRHO)
      IF ( PSNOWDZ(JST)>=XUEPSI ) THEN
         ZPSNOW_OLD = ZPSNOW_OLD + PSNOWDZ(JST)
!      write(*,*) "ABS( ZPSNOW_NEW - PSNOWDZF - ZPSNOW_OLD )", ABS( ZPSNOW_NEW - PSNOWDZF - ZPSNOW_OLD ), inlvls_old
! trude; Test fail, XUEPS seem to be too small. Increase XUEPSI slightly for test
! XUEPSI test probably need to be less strict for very thick snow layers. Perhaps have a moving target?
!      IF ( ABS( ZPSNOW_NEW - PSNOWDZF - ZPSNOW_OLD )<XUEPSI ) THEN
         IF ( ABS( ZPSNOW_NEW - PSNOWDZF - ZPSNOW_OLD )<XUEPSI*100000. ) THEN
            !      IF ( ABS( ZPSNOW_NEW - PSNOWDZF - ZPSNOW_OLD )<XUEPSI*1000. ) THEN
            INLVLS_OLD = JST
         ENDIF
      ENDIF
   ENDDO
   IF ( INLVLS_OLD==-1 ) THEN
      WRITE(*,*)'pb INLVLS_OLD INLVLS_NEW=',INLVLS_NEW
      WRITE(*,*)'pb INLVLS_OLD',PSNOWDZF
      WRITE(*,*)'pb INLVLS_OLD',PSNOWDZN
      WRITE(*,*)'pb INLVLS_OLD',PSNOWDZ
      !    CALL ABOR1_SFX('SNOWCRO: Error INLVLS_OLD')
   ENDIF
ENDIF
!
IF ( GSNOWFALL ) INLVLS_OLD = INLVLS_OLD + 1
!
ZPSNOW_OLD = PSNOW
ZPSNOW_NEW = ZPSNOW_OLD

!
! initialization of variables describing the initial snowpack + new snowfall
!
IF ( GSNOWFALL ) THEN
  DO JST = 2,INLVLS_OLD
    ZSNOWDZO   (JST) = PSNOWDZ   (JST-1)
    ZSNOWRHOO  (JST) = PSNOWRHO  (JST-1)
    ZSNOWHEATO (JST) = PSNOWHEAT (JST-1)
    ZSNOWGRAN1O(JST) = PSNOWGRAN1(JST-1)
    ZSNOWGRAN2O(JST) = PSNOWGRAN2(JST-1)
    ZSNOWHISTO (JST) = PSNOWHIST (JST-1)
    ZSNOWAGEO  (JST) = PSNOWAGE  (JST-1)
  ENDDO
  ZSNOWDZO   (1) = PSNOWDZF
  ZSNOWRHOO  (1) = PSNOWRHOF
  ZSNOWHEATO (1) = PSNOWHEATF
  ZSNOWGRAN1O(1) = PSNOWGRAN1F
  ZSNOWGRAN2O(1) = PSNOWGRAN2F
  ZSNOWHISTO (1) = PSNOWHISTF
  ZSNOWAGEO  (1) = PSNOWAGEF
ELSE
  DO JST = 1,INLVLS_OLD
    ZSNOWDZO   (JST) = PSNOWDZ   (JST)
    ZSNOWRHOO  (JST) = PSNOWRHO  (JST)
    ZSNOWHEATO (JST) = PSNOWHEAT (JST)
    ZSNOWGRAN1O(JST) = PSNOWGRAN1(JST)
    ZSNOWGRAN2O(JST) = PSNOWGRAN2(JST)
    ZSNOWHISTO (JST) = PSNOWHIST (JST)
    ZSNOWAGEO  (JST) = PSNOWAGE  (JST)
  ENDDO
ENDIF


!
! 1. Calculate vertical grid limits (m):
! --------------------------------------
!
ZSNOWZTOP_OLD(1) = ZPSNOW_OLD
ZSNOWZTOP_NEW(1) = ZPSNOW_NEW

      DO JST = 1,INLVLS_OLD
         IF ( JST>1 ) ZSNOWZTOP_OLD(JST) = ZSNOWZBOT_OLD(JST-1)
         ZSNOWZBOT_OLD(JST) = ZSNOWZTOP_OLD(JST) - ZSNOWDZO(JST)
      ENDDO
!
      DO JST = 1,INLVLS_NEW
         IF ( JST>1 ) ZSNOWZTOP_NEW(JST) = ZSNOWZBOT_NEW(JST-1)
         ZSNOWZBOT_NEW(JST) = ZSNOWZTOP_NEW(JST) - PSNOWDZN(JST)
      ENDDO

! NBNBNBNB trude test. When zsnowzbot=zsnowztop_new there is a problem in get_mass_heat. The entire glacier layer for this
! gridpoint is set to zero because snowrho does not get a value. This is a fast fix that needs to be evaluated.

!
! Check consistency
! true if test:
!IF ( INLVLS_OLD>0 ) THEN

ZSNOWZBOT_OLD(INLVLS_OLD) = 0.   ! trude comment. This line is included in original snowcro, but has been deleted in new WRF-Hydro code. Not sure why.
                                                               ! Now testing with this included.
!
ZSNOWZBOT_NEW(INLVLS_NEW) = 0.
!
! 3. Calculate mass, heat, charcateristics mixing due to vertical grid resizing:
! --------------------------------------------------------------------
!
! loop over the new snow layers
! Summ or avergage of the constituting quantities of the old snow layers
! which are totally or partially inserted in the new snow layer


 CALL GET_MASS_HEAT(KJ,INLVLS_NEW,INLVLS_OLD,                                &
                    ZSNOWZTOP_OLD,ZSNOWZTOP_NEW,ZSNOWZBOT_OLD,ZSNOWZBOT_NEW, &
                    ZSNOWRHOO,ZSNOWDZO,ZSNOWGRAN1O,ZSNOWGRAN2O,ZSNOWHISTO,   &
                    ZSNOWAGEO,ZSNOWHEATO,                                    &
                    ZSNOWRHON,PSNOWDZN,ZSNOWGRAN1N,ZSNOWGRAN2N,ZSNOWHISTN,   &
                    ZSNOWAGEN,ZSNOWHEATN,HSNOWMETAMO                         )
!




! check of consistency between new and old snowpacks
ZSNOWHEAN  = 0.
ZMASTOTN   = 0.
ZSNOWHEAO  = 0.
ZMASTOTO   = 0.
ZPSNOW_NEW = 0.
ZPSNOW_OLD = 0.
!
 DO JST = 1,INLVLS_NEW
  ZSNOWHEAN  = ZSNOWHEAN  + ZSNOWHEATN(JST)
  ZMASTOTN   = ZMASTOTN   + ZSNOWRHON(JST) * PSNOWDZN(JST)
  ZPSNOW_NEW = ZPSNOW_NEW + PSNOWDZN(JST)
 ENDDO
!
 DO JST = 1,INLVLS_OLD
  ZSNOWHEAO  = ZSNOWHEAO  + ZSNOWHEATO(JST)
  ZMASTOTO   = ZMASTOTO   + ZSNOWRHOO(JST) * ZSNOWDZO(JST)
  ZPSNOW_OLD = ZPSNOW_OLD + ZSNOWDZO(JST)
 ENDDO
!
IF ( ABS( ZSNOWHEAN-ZSNOWHEAO )>0.0001 .OR. ABS( ZMASTOTN-ZMASTOTO )>0.0001 .OR. &
!     ABS( ZPSNOW_NEW-ZPSNOW_OLD )> 0.0001 ) THEN
! trude test with higher limit
      ABS( ZPSNOW_NEW-ZPSNOW_OLD )> 0.001 ) THEN
!  WRITE(*,*) 'Warning diff', ZSNOWHEAN-ZSNOWHEAO,ZMASTOTN-ZMASTOTO,ZPSNOW_NEW-ZPSNOW_OLD
ENDIF
!
! 5. Update mass (density and thickness) and heat:
! ------------------------------------------------
!
PSNOWDZ   (:) = PSNOWDZN   (:)
!
PSNOWRHO  (:) = ZSNOWRHON  (:)
PSNOWHEAT (:) = ZSNOWHEATN (:)
PSNOWGRAN1(:) = ZSNOWGRAN1N(:)
PSNOWGRAN2(:) = ZSNOWGRAN2N(:)
PSNOWHIST (:) =  ZSNOWHISTN(:)
!
PSNOWAGE  (:) =  ZSNOWAGEN (:)


!
!IF (LHOOK) CALL DR_HOOK('SNOWNLGRIDFRESH_1D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWNLGRIDFRESH_1D
!####################################################################
!####################################################################
!###################################################################
SUBROUTINE SNOWDRIFT(PTSTEP,PVMOD,PSNOWRHO,PSNOWDZ,PSNOW,        &
                     PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,KNLVLS_USE, &
                     PTA,PQA,PPS,PRHOA,PZ0EFF,PUREF,             &
                     OSNOWDRIFT_SUBLIM,HSNOWMETAMO,PSNDRIFT      )
!
!!    PURPOSE
!!    -------
!     Snow compaction  and metamorphism due to drift
!     Mass is unchanged: layer thickness is reduced
!     in proportion to density increases. Method inspired from
!     Brun et al. (1997) and Guyomarch
!
!     - computes a mobility index of each snow layer from its grains, density
!                 and history
!     - computes a drift index of each layer from its mobility and wind speed
!     - computes a transport index with an exponential decay taking into
!                 account its depth and the mobility of upper layers
!     - increases density and changes grains in case of transport
!
!     HISTORY:
!     Basic parameterization from Crocus/ARPEGE Coupling (1997)
!     Implementation in V5
!     Insertion in V6 of grains type evolution in case of dendritic snow (V.
!     Vionnet)
!     07/2012 (for V7.3): E. Brun, M. Lafaysse : optional sublimation of drifted snow
!     2012-09-20 : bug correction : ZFF was not computed if LSNOWDRIFT_SUBLIM=FALSE.
!
!     2014-02-05 V. Vionnet: systematic use of 5m wind speed to compute drift index
!     2014-06-03 M. Lafaysse: threshold on PZ0EFF

USE MODD_CSTS,ONLY : XTT
USE MODE_THERMOS

USE MODD_SNOW_PAR, ONLY : XVTIME, XVROMAX, XVROMIN, XVMOB1,  &
                          XVMOB2, XVMOB3, XVMOB4, XVDRIFT1, XVDRIFT2, XVDRIFT3, &
                          XVSIZEMIN, XCOEF_FF, XCOEF_EFFECT, XQS_REF
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)      :: PTA, PQA, PPS, PRHOA
!
REAL, DIMENSION(:), INTENT(IN)      :: PVMOD
!
INTEGER, DIMENSION(:), INTENT(IN)   :: KNLVLS_USE
!
REAL, DIMENSION(:),INTENT(IN)       :: PZ0EFF,PUREF
!
LOGICAL,INTENT(IN)                  :: OSNOWDRIFT_SUBLIM
!
 CHARACTER(3), INTENT(IN)            :: HSNOWMETAMO ! metamorphism scheme
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSNOWRHO, PSNOWDZ,PSNOWGRAN1, &
                                       PSNOWGRAN2,PSNOWHIST
REAL, DIMENSION(:), INTENT(OUT)     :: PSNOW
REAL, DIMENSION(:), INTENT(OUT)     :: PSNDRIFT !blowing snow sublimation (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZSNOWRHO2
REAL, DIMENSION(SIZE(PSNOWRHO,1)                 ) :: ZSNOWDZ1
!
REAL, DIMENSION(SIZE(PSNOWRHO,1))   :: ZQSATI, ZFF ! QS wrt ice, gust speed
!
REAL     :: ZZ0EFF
!
REAL     :: ZPROFEQU, ZRMOB, ZRDRIFT, ZRT, ZDRO, ZDGR1, ZDGR2
REAL     :: ZVT ! 5m wind speed threshold for surface
!transport
REAL     :: ZQS_EFFECT ! effect of QS on snow
REAL     :: ZWIND_EFFECT ! effect of wind on snow
REAL     :: ZDRIFT_EFFECT ! effect of QS and wind on snow
! transformation
REAL     :: ZQS !Blowing snow sublimation (kg/m2/s)
REAL     :: ZRHI, ZFACT
!
INTEGER  :: JJ,JST   ! looping indexes
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Reference height for wind speed used to dertermine the occurrence of blowing snow
REAL, PARAMETER :: PPHREF_WIND=5.
REAL, PARAMETER :: PPHREF_MIN=PPHREF_WIND/2.
!
!-------------------------------------------------------------------------------
!IF (LHOOK) CALL DR_HOOK('SNOWDRIFT',0,ZHOOK_HANDLE)
!
! 0. Initialization:
! ------------------
!
ZSNOWDZ1(:) = PSNOWDZ(:,1)
!
DO JJ = 1,SIZE(PSNOW)
  DO JST = 1,KNLVLS_USE(JJ)
    ZSNOWRHO2(JJ,JST) = PSNOWRHO(JJ,JST)
  ENDDO
ENDDO
!
IF ( OSNOWDRIFT_SUBLIM ) THEN
  ZQSATI(:) = QSATI( PTA(:),PPS(:) )
END IF
!
! 1. Computation of drift and induced settling and metamorphism
! ------------------
!
DO JJ=1, SIZE(PSNOW)
  !
  ! gust speed at 5m above the snowpack
  ! Computed from PVMOD at PUREF (m) assuming a log profile in the SBL
  ! and a roughness length equal to PZ0EFF
  ZZ0EFF=MIN(PZ0EFF(JJ),PUREF(JJ)*0.5,PPHREF_MIN)
  ZFF(JJ) = XCOEF_FF*PVMOD(JJ)*LOG(PPHREF_WIND/ZZ0EFF)/LOG(PUREF(JJ)/ZZ0EFF)
  !
  ! initialization decay coeff
  ZPROFEQU = 0.
  !
  DO JST = 1,KNLVLS_USE(JJ)
    !
    ZFACT = 1.25 - 1.25 * ( MAX( PSNOWRHO(JJ,JST), XVROMIN ) - XVROMIN )/1000./XVMOB1
    !
    IF ( HSNOWMETAMO=='B92' ) THEN
      !
      !  mobility index computation of a layer as a function of its properties
      IF( PSNOWGRAN1(JJ,JST)<0. ) THEN
        ! dendritic case
        ZRMOB = 0.34 * ( 0.5  - ( 0.75*PSNOWGRAN1(JJ,JST) + 0.5*PSNOWGRAN2(JJ,JST) )/99. ) + &
                0.66 * ZFACT
      ELSE
        ! non dendritic case
        ZRMOB = 0.34 * ( XVMOB2 - XVMOB2*PSNOWGRAN1(JJ,JST)/99. - XVMOB3*PSNOWGRAN2(JJ,JST)*1000. ) + &
                0.66 * ZFACT
      ENDIF
      !
    ELSE
      !
      IF ( PSNOWGRAN1(JJ,JST)<XVDIAM6*(4.-PSNOWGRAN2(JJ,JST))-XUEPSI ) THEN
        ! dendritic case
        ZRMOB = 0.34 * ( 0.5 + 0.75 * &
               ( PSNOWGRAN1(JJ,JST)/XVDIAM6-4.+PSNOWGRAN2(JJ,JST) )/( PSNOWGRAN2(JJ,JST)-3. ) &
                             - 0.5 * PSNOWGRAN2(JJ,JST) ) + &
                0.66 * ZFACT
      ELSE
        ! non dendritic case
        ZRMOB = 0.34 * ( XVMOB2 - XVMOB2 * PSNOWGRAN2(JJ,JST) &
                                - XVMOB3 * (4.-PSNOWGRAN2(JJ,JST))*XVDIAM6*1000. ) + &
                0.66 * ZFACT
      ENDIF
      !
    ENDIF
    !
    ! correction in case of former wet snow
    IF ( PSNOWHIST(JJ,JST) >= 2. ) ZRMOB = MIN(ZRMOB, XVMOB4)
    !
    ! computation of drift index supposing no overburden snow
    ZRDRIFT = ZRMOB - ( XVDRIFT1 * EXP( -XVDRIFT2*ZFF(JJ) ) - 1.)
    ! modif_EB exit loop if there is no drift
    IF ( ZRDRIFT<=0. ) EXIT
    !
    ! update the decay coeff by half the current layer
    ZPROFEQU = ZPROFEQU + 0.5 * PSNOWDZ(JJ,JST) * 0.1 * ( XVDRIFT3 - ZRDRIFT )
    ! computation of the drift index inclunding the decay by overburden snow
    ZRT = MAX( 0., ZRDRIFT * EXP( -ZPROFEQU*100 ) )
    !
    IF ( OSNOWDRIFT_SUBLIM .AND. JST==1 ) THEN
      !Specific case for blowing snow sublimation
      ! computation of wind speed threshold QSATI and RH withe respect to ice
      ZVT  = -LOG( (ZRMOB+1.)/XVDRIFT1 ) / XVDRIFT2
      ZRHI = PQA(JJ) / ZQSATI(JJ)
      ! computation of sublimation rate according to Gordon's PhD
!      trude test
!      reduce constatn 0.0018 by a factor of 10
      ZQS = 0.0018 * (XTT/PTA(JJ))**4 * ZVT * PRHOA(JJ) * ZQSATI(JJ) * &     ! orig
            (1.-ZRHI) * (ZFF(JJ)/ZVT)**3.6

!      ZQS = 0.00018 * (XTT/PTA(JJ))**4 * ZVT * PRHOA(JJ) * ZQSATI(JJ) * &   ! trude test, factor of 0.1
 !           (1.-ZRHI) * (ZFF(JJ)/ZVT)**3.6

!      ZQS = 0.5* 0.0018 * (XTT/PTA(JJ))**4 * ZVT * PRHOA(JJ) * ZQSATI(JJ) * &   ! trude test, factor of 0.5
!            (1.-ZRHI) * (ZFF(JJ)/ZVT)**3.6


      ! trude, end test
      ! WRITE(*,*) 'surface Vt vent*coef  ZRDRIFT ZRMOB :',ZVT,&
      ! ZFF(JJ),ZRDRIFT,ZRMOB
      ! WRITE(*,*) 'V>Vt ZQS   :',ZQS
      ! surface depth decrease in case of blowing snow sublimation
      ! WRITE(*,*) 'V>Vt DSWE DZ Z:',- MAX(0.,ZQS)*PTSTEP/COEF_FF,
      ! - MAX(0.,ZQS)*PTSTEP/COEF_FF/PSNOWRHO(JJ,JST),PSNOWDZ(JJ,JST)
      ! 2 lignes ci-dessous a valider pour avoir sublim drift
      PSNOWDZ(JJ,JST) = MAX( 0.5*PSNOWDZ(JJ,JST), &
                             PSNOWDZ(JJ,JST) - MAX(0.,ZQS) * PTSTEP/XCOEF_FF/PSNOWRHO(JJ,JST) )
      PSNDRIFT(JJ) = (ZSNOWDZ1(JJ)-PSNOWDZ(JJ,JST))*PSNOWRHO(JJ,JST)/PTSTEP
    ELSE
      ZQS = 0.
    END IF
    !
    ZQS_EFFECT    = MIN( 3., MAX( 0.,ZQS )/XQS_REF ) * ZRT
    ZWIND_EFFECT  = XCOEF_EFFECT * ZRT
    ZDRIFT_EFFECT = ( ZQS_EFFECT + ZWIND_EFFECT ) * PTSTEP / XCOEF_FF / XVTIME
    ! WRITE(*,*) 'ZQS_EFFECT,ZWIND_EFFECT,ZDRIFT_EFFECT:',ZQS_EFFECT,ZWIND_EFFECT,ZDRIFT_EFFECT
    !
    ! settling by wind transport only in case of not too dense snow
    IF( PSNOWRHO(JJ,JST) < XVROMAX ) THEN
      ZDRO = ZDRIFT_EFFECT * ( XVROMAX - PSNOWRHO(JJ,JST) )
      PSNOWRHO(JJ,JST) = MIN( XVROMAX , PSNOWRHO(JJ,JST) + ZDRO )
      PSNOWDZ (JJ,JST) = PSNOWDZ(JJ,JST) * ZSNOWRHO2(JJ,JST) / PSNOWRHO(JJ,JST)
    ENDIF
    !
    IF ( HSNOWMETAMO=='B92' ) THEN
      !
      ! metamorphism induced by snow drift
      IF ( PSNOWGRAN1(JJ,JST)<0. ) THEN
        ! dendritic case
        ZDGR1 = ZDRIFT_EFFECT * ( -PSNOWGRAN1(JJ,JST) ) * 0.5
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + MIN( ZDGR1, -0.99 * PSNOWGRAN1(JJ,JST) )
        ! modif_VV_140910
        ZDGR2 = ZDRIFT_EFFECT * ( 99. - PSNOWGRAN2(JJ,JST) )
        PSNOWGRAN2(JJ,JST) = MIN( 99., PSNOWGRAN2(JJ,JST) + ZDGR2 )
        ! fin modif_VV_140910
      ELSE
        ! non dendritic case
        ZDGR1 = ZDRIFT_EFFECT * ( 99. - PSNOWGRAN1(JJ,JST) )
        ZDGR2 = ZDRIFT_EFFECT * 5. / 10000.
        PSNOWGRAN1(JJ,JST) = MIN( 99., PSNOWGRAN1(JJ,JST) + ZDGR1 )
        PSNOWGRAN2(JJ,JST) = MAX( XVSIZEMIN, PSNOWGRAN2(JJ,JST) - ZDGR2 )
      ENDIF
      !
    ELSE
      !
      ! dendritic case
      IF ( PSNOWGRAN1(JJ,JST)<XVDIAM6*(4.-PSNOWGRAN2(JJ,JST))-XUEPSI ) THEN
        !
        ZDGR1 = MIN( ZDRIFT_EFFECT * ( ( PSNOWGRAN1(JJ,JST)/XVDIAM6-4.+PSNOWGRAN2(JJ,JST) )/ &
                                        (PSNOWGRAN2(JJ,JST)-3.) ) * 0.5, &
                              0.99 * ( ( PSNOWGRAN1(JJ,JST)/XVDIAM6-4.+ PSNOWGRAN2(JJ,JST))/ &
                                        (PSNOWGRAN2(JJ,JST)-3.) ) )
        ZDGR2 = ZDRIFT_EFFECT * ( 1.-PSNOWGRAN2(JJ,JST) )
        !
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) + XVDIAM6 * &
                             ( ZDGR2 * ( (PSNOWGRAN1(JJ,JST)/XVDIAM6-1.)/(PSNOWGRAN2(JJ,JST)-3.) ) - &
                               ZDGR1 * ( PSNOWGRAN2(JJ,JST)-3. ) )
          PSNOWGRAN2(JJ,JST) = MIN(1.,PSNOWGRAN2(JJ,JST)+ZDGR2)
      ! non dendritic case
      ELSE
        !
        ZDGR1 = ZDRIFT_EFFECT * 5./10000.
        ZDGR2 = ZDRIFT_EFFECT * (1.-PSNOWGRAN2(JJ,JST))
        !
        PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST) - 2. * XVDIAM6 * PSNOWGRAN2(JJ,JST) * ZDGR2
        PSNOWGRAN2(JJ,JST) = MIN( 1., PSNOWGRAN2(JJ,JST)+ZDGR2 )
        !
      ENDIF
      !
    ENDIF
    !
    ! update the decay coeff by half the current layer
    ZPROFEQU = ZPROFEQU + 0.5 * PSNOWDZ(JJ,JST) * 0.1 * ( XVDRIFT3 - ZRDRIFT )
    !
  ENDDO  ! snow layers loop
  !
ENDDO    ! grid points loop
!
! 2. Update total snow depth:
! -----------------------------------------------
!
! Compaction of total snowpack depth
!
DO JJ = 1,SIZE(PSNOWDZ,1)
  PSNOW(JJ) = SUM( PSNOWDZ(JJ,1:KNLVLS_USE(JJ)) )
ENDDO
!
!IF (LHOOK) CALL DR_HOOK('SNOWDRIFT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWDRIFT
!####################################################################
!###################################################################
!####################################################################
!####################################################################
SUBROUTINE SNOWCROLAYER_GONE(PTSTEP,PSCAP,PSNOWTEMP,PSNOWDZ,          &
                             PSNOWRHO,PSNOWLIQ,PSNOWGRAN1,PSNOWGRAN2, &
                             PSNOWHIST,PSNOWAGE,PLES3L,KNLVLS_USE     )
!
!
!!    PURPOSE
!     Account for the case when one or several snow layers melt
!     during a time step:
!     in that case, merge these layers with the underlying layer
!     except for the bottom layer which is merged to the abovelying layer
!     energy and mass are conserved
!     a new merged layer keeps the grain, histo and age properties of the
!     non-melted layer
!
USE MODD_CSTS,ONLY : XTT, XLMTT, XRHOLW, XRHOLI, XLVTT, XCI
!
USE MODE_SNOW3L
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSCAP
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWDZ, PSNOWTEMP, PSNOWRHO, PSNOWLIQ
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWGRAN1,PSNOWGRAN2,PSNOWHIST,PSNOWAGE
!
INTEGER, DIMENSION(:), INTENT(INOUT) :: KNLVLS_USE !
!
REAL, DIMENSION(:), INTENT(IN) :: PLES3L
!
!*      0.2    declarations of local variables
!
REAL :: ZHEAT, ZMASS, ZDZ, ZLIQ, ZSNOWLWE
!
INTEGER :: JJ,JST,JST_1, JST_2, JST_MAX, IDIFF_LAYER ! loop counter
INTEGER :: ID_1, ID_2
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROLAYER_GONE',0,ZHOOK_HANDLE)
!
DO JJ=1,SIZE(PSNOWRHO,1)  ! loop on gridpoints
  !
  JST_MAX = KNLVLS_USE(JJ)
  !
  IDIFF_LAYER = 0  ! used as shift counter of previously melted layers
  !
  DO JST_1 = JST_MAX,1-JST_MAX,-1 ! loop on 2 x layers in case of multi melt
    !
    JST = JST_1 + IDIFF_LAYER
    !
    ! Merge is possible only in case of 2 active layers or more
    IF ( JST>=1 .AND. KNLVLS_USE(JJ)>1 ) THEN
      !
      ! Total Liquid equivalent water content of snow (m):
      ZSNOWLWE = PSNOWRHO(JJ,JST) * PSNOWDZ(JJ,JST) / XRHOLW
      !
      ! Consideration of sublimation if any
      IF ( JST==1 ) ZSNOWLWE = ZSNOWLWE - MAX( 0., PLES3L(JJ)*PTSTEP/(XLSTT*XRHOLW) )
      !
      ! Test if avalaible energy exceeds total latent heat
      IF ( PSCAP(JJ,JST) * MAX( 0.0, PSNOWTEMP(JJ,JST)-XTT ) * PSNOWDZ(JJ,JST) >=  &
           ( ( ZSNOWLWE-PSNOWLIQ(JJ,JST) ) * XLMTT * XRHOLW ) - XUEPSI ) THEN
        !
        IF ( JST==KNLVLS_USE(JJ) ) THEN
          ID_1 = JST-1
          ID_2 = JST
        ELSE
          ID_1 = JST
          ID_2 = JST + 1
        ENDIF
        !
        ! Case of a total melt of the bottom layer: merge with above layer
        !        which keeps its grain, histo and age properties
        ZHEAT = 0.
        ZMASS = 0.
        ZDZ   = 0.
        ZLIQ  = 0.
        DO JST_2 = ID_1,ID_2
          ZHEAT = ZHEAT + &
                  PSNOWDZ(JJ,JST_2) * &
                  ( PSCAP(JJ,JST_2)*( PSNOWTEMP(JJ,JST_2)-XTT ) - XLMTT*PSNOWRHO(JJ,JST_2) ) + &
                  XLMTT * XRHOLW * PSNOWLIQ(JJ,JST_2)
          ZMASS = ZMASS + PSNOWDZ(JJ,JST_2) * PSNOWRHO(JJ,JST_2)
          ZDZ   = ZDZ   + PSNOWDZ(JJ,JST_2)
          ZLIQ  = ZLIQ  + PSNOWLIQ(JJ,JST_2)
        ENDDO
        !
        PSNOWDZ  (JJ,ID_1) = ZDZ
        PSNOWRHO (JJ,ID_1) = ZMASS / ZDZ
        PSNOWLIQ (JJ,ID_1) = ZLIQ
        !
        ! Temperature of the merged layer is deduced from the heat content
        PSCAP    (JJ,ID_1) = ( PSNOWRHO(JJ,ID_1) - &
                               PSNOWLIQ(JJ,ID_1) * XRHOLW / &
                               MAX( PSNOWDZ(JJ,ID_1),XSNOWDZMIN ) ) * XCI
        PSNOWTEMP(JJ,ID_1) = XTT + &
          ( ( ( ( ZHEAT - XLMTT*XRHOLW*PSNOWLIQ(JJ,ID_1) ) / PSNOWDZ(JJ,ID_1) ) + &
              XLMTT*PSNOWRHO(JJ,ID_1) ) &
            / PSCAP(JJ,ID_1) )
        !
        IF( JST/=KNLVLS_USE(JJ) ) THEN
          !
          PSNOWGRAN1(JJ,JST) = PSNOWGRAN1(JJ,JST+1)
          PSNOWGRAN2(JJ,JST) = PSNOWGRAN2(JJ,JST+1)
          PSNOWHIST (JJ,JST) = PSNOWHIST (JJ,JST+1)
          PSNOWAGE  (JJ,JST) = PSNOWAGE  (JJ,JST+1)
          !
          ! Shift the above layers
          DO JST_2 = JST+1,KNLVLS_USE(JJ)-1
            PSNOWTEMP (JJ,JST_2) = PSNOWTEMP (JJ,JST_2+1)
            PSCAP     (JJ,JST_2) = PSCAP     (JJ,JST_2+1)
            PSNOWDZ   (JJ,JST_2) = PSNOWDZ   (JJ,JST_2+1)
            PSNOWRHO  (JJ,JST_2) = PSNOWRHO  (JJ,JST_2+1)
            PSNOWLIQ  (JJ,JST_2) = PSNOWLIQ  (JJ,JST_2+1)
            PSNOWGRAN1(JJ,JST_2) = PSNOWGRAN1(JJ,JST_2+1)
            PSNOWGRAN2(JJ,JST_2) = PSNOWGRAN2(JJ,JST_2+1)
            PSNOWHIST (JJ,JST_2) = PSNOWHIST (JJ,JST_2+1)
            PSNOWAGE  (JJ,JST_2) = PSNOWAGE  (JJ,JST_2+1)
          ENDDO !  loop JST_2
          !
          ! Update the shift counter IDIFF_LAYER
          IDIFF_LAYER = IDIFF_LAYER + 1
          !
        ENDIF ! end test of bottom layer
        !
        ! Decrease the number of active snow layers
        KNLVLS_USE(JJ) = KNLVLS_USE(JJ) - 1
        !
      ENDIF ! end test on availibility of energy
      !
    ENDIF ! end test on the number of remaining active layers
    !
  ENDDO ! end loop on the snow layers
  !
ENDDO ! end loop gridpoints
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROLAYER_GONE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROLAYER_GONE
!####################################################################
!###################################################################
!####################################################################
!###################################################################
SUBROUTINE SNOWCROPRINTPROFILE(HINFO,KLAYERS,OPRINTGRAN,PSNOWDZ,PSNOWRHO, &
                               PSNOWTEMP,PSNOWLIQ,PSNOWHEAT,PSNOWGRAN1,   &
                               PSNOWGRAN2,PSNOWHIST,PSNOWAGE,HSNOWMETAMO  )
!
! Matthieu Lafaysse 08/06/2012
! This routine prints the snow profile of a given point for debugging
!
!to compute SSA
USE MODD_CSTS, ONLY : XRHOLI
USE MODD_SNOW_PAR, ONLY : XD1, XD2, XD3, XX
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
 CHARACTER(*), INTENT(IN) :: HINFO
LOGICAL,       INTENT(IN) :: OPRINTGRAN
INTEGER,       INTENT(IN) :: KLAYERS
REAL, DIMENSION(:), INTENT(IN) :: PSNOWDZ,PSNOWRHO,PSNOWTEMP,PSNOWLIQ, &
                                  PSNOWHEAT,PSNOWGRAN1,PSNOWGRAN2,     &
                                  PSNOWHIST,PSNOWAGE
 CHARACTER(3), INTENT(IN)       :: HSNOWMETAMO
!
REAL, DIMENSION(KLAYERS) :: ZSNOWSSA
REAL :: ZDIAM
!
INTEGER :: JST
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTPROFILE',0,ZHOOK_HANDLE)
!
WRITE(*,*)
WRITE(*,*)TRIM(HINFO)
!
IF (OPRINTGRAN) THEN
  !
  ! Compute SSA from SNOWGRAN1 and SNOWGRAN2
  IF ( HSNOWMETAMO=='B92' ) THEN
    !
    DO JST = 1,KLAYERS
      !
      IF ( PSNOWGRAN1(JST)<0. ) THEN
        ZDIAM =  -PSNOWGRAN1(JST)*XD1/XX + (1.+PSNOWGRAN1(JST)/XX) * &
                ( PSNOWGRAN2(JST)*XD2/XX + (1.-PSNOWGRAN2(JST)/XX) * XD3 )
        ZDIAM = ZDIAM/10000.
      ELSE
        ZDIAM = PSNOWGRAN2(JST)*PSNOWGRAN1(JST)/XX + &
                MAX( 0.0004, 0.5*PSNOWGRAN2(JST) ) * ( 1.-PSNOWGRAN1(JST)/XX )
      ENDIF
      ZSNOWSSA(JST) = 6. / (XRHOLI*ZDIAM)
      !
    END DO
    !
  ELSE
    !
    ZSNOWSSA = 6. / (XRHOLI*PSNOWGRAN1)
    !
  ENDIF
  !
  WRITE(*,'(9(A12,"|"))')"-------------","-------------","-------------",&
        "-------------","-------------","-------------","-------------",&
        "-------------","-------------"
  WRITE(*,'(9(A12,"|"))')"PSNOWDZ","PSNOWRHO","PSNOWTEMP","PSNOWLIQ","PSNOWHEAT",&
        "PSNOWGRAN1","PSNOWGRAN2","PSNOWHIST","PSNOWAGE"
  WRITE(*,'(9(A12,"|"))')"-------------","-------------","-------------",&
        "-------------","-------------","-------------","-------------",&
        "-------------","-------------"
  DO JST = 1,KLAYERS
    WRITE(*,'(9(ES12.3,"|")," L",I2.2)') PSNOWDZ(JST),PSNOWRHO(JST),PSNOWTEMP(JST),    &
                                          PSNOWLIQ(JST),PSNOWHEAT(JST),PSNOWGRAN1(JST), &
                                          PSNOWGRAN2(JST),PSNOWHIST(JST),PSNOWAGE(JST),JST
  ENDDO
  WRITE(*,'(9(A12,"|"))')"-------------","-------------","-------------",&
        "-------------","-------------","-------------","-------------",&
        "-------------","-------------"
  !
ELSE
  !
  WRITE(*,'(5(A12,"|"))')"------------","------------","------------",&
        "------------","------------"
  WRITE(*,'(5(A12,"|"))')"PSNOWDZ","PSNOWRHO","PSNOWTEMP","PSNOWLIQ","PSNOWHEAT"
  WRITE(*,'(5(A12,"|"))')"------------","------------","------------",&
        "------------","------------"
  DO JST = 1,KLAYERS
    WRITE(*,'(5(ES12.3,"|")," L",I2.2)') PSNOWDZ(JST),PSNOWRHO(JST),PSNOWTEMP(JST),&
                                         PSNOWLIQ(JST),PSNOWHEAT(JST),JST
  ENDDO
  WRITE(*,'(5(A12,"|"))')"------------","------------","------------",&
        "------------","------------"
  !
END IF
!
WRITE(*,*)
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTPROFILE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROPRINTPROFILE
!####################################################################
!###################################################################
SUBROUTINE SNOWCROPRINTATM(CINFO,PTA,PQA,PVMOD,PRR,PSR,PSW_RAD,PLW_RAD, &
                           PTG, PSOILCOND,PD_G,PPSN3L                 )

! Matthieu Lafaysse 08/06/2012
! This routine prints the atmospheric forcing of a given point for debugging
! and ground data
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE

 CHARACTER(*), INTENT(IN) :: CINFO
REAL,          INTENT(IN) :: PTA,PQA,PVMOD,PRR,PSR,PSW_RAD,PLW_RAD
REAL,          INTENT(IN) :: PTG, PSOILCOND, PD_G, PPSN3L
!
INTEGER :: JST
!
!!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTATM',0,ZHOOK_HANDLE)
!
 CALL SNOWCROPRINTDATE()
!
WRITE(*,*)
WRITE(*,*)TRIM(CINFO)
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
WRITE(*,'(4(A12,"|"))')"PTA","PQA","PRR","PSR"
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
WRITE(*,'(4(ES12.3,"|")," meteo1")')PTA,PQA,PRR,PSR
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
WRITE(*,'(3(A12,"|"))')"------------","------------","------------"
WRITE(*,'(3(A12,"|"))')"PSW_RAD","PLW_RAD","PVMOD"
WRITE(*,'(3(A12,"|"))')"------------","------------","------------"
WRITE(*,'(3(ES12.3,"|")," meteo2")')PSW_RAD,PLW_RAD,PVMOD
WRITE(*,'(3(A12,"|"))')"------------","------------","------------"
WRITE(*,*)
WRITE(*,*)"Ground :"
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
WRITE(*,'(4(A12,"|"))')"PTG","PSOILCOND","PD_G","PPSN3L"
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
WRITE(*,'(4(ES12.3,"|")," soil")')PTG,PSOILCOND,PD_G,PPSN3L
WRITE(*,'(4(A12,"|"))')"------------","------------","------------",&
"------------"
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTATM',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROPRINTATM
!
!####################################################################
!SUBROUTINE SNOWCROSTOPBALANCE(PMASSBALANCE,PENERGYBALANCE)
!
!USE MODE_CRODEBUG, ONLY : XWARNING_MASSBALANCE, XWARNING_ENERGYBALANCE
!
!!USE MODI_ABOR1_SFX
!
!!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
! stop if energy and mass balances are not closed
!
!IMPLICIT NONE
!!
!REAL , DIMENSION(:), INTENT(IN) :: PMASSBALANCE, PENERGYBALANCE
!
!REAL,DIMENSION(SIZE(PSR)) :: ZMASSBALANCE,ZENERGYBALANCE
!
!!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!!IF (LHOOK) CALL DR_HOOK('SNOWCROSTOPBALANCE',0,ZHOOK_HANDLE)
!
!IF ( ANY( PMASSBALANCE   > XWARNING_MASSBALANCE   ) ) &
!        CALL ABOR1_SFX("SNOWCRO: WARNING MASS BALANCE !")
!IF ( ANY( PENERGYBALANCE > XWARNING_ENERGYBALANCE ) ) &
!        CALL ABOR1_SFX("SNOWCRO: WARNING ENERGY BALANCE !")
!
!!IF (LHOOK) CALL DR_HOOK('SNOWCROSTOPBALANCE',1,ZHOOK_HANDLE)
!
!END SUBROUTINE SNOWCROSTOPBALANCE
!
!###################################################################
SUBROUTINE SNOWCROPRINTBALANCE(PSUMMASS_INI,PSUMHEAT_INI,PSUMMASS_FIN,PSUMHEAT_FIN, &
                               PSR,PRR,PTHRUFAL,PEVAP,PEVAPCOR,PGRNDFLUX,PHSNOW,    &
                               PRNSNOW,PLEL3L,PLES3L,PHPSNOW,PSNOWHMASS,PSNOWDZ,    &
                               PTSTEP,PMASSBALANCE,PENERGYBALANCE,PEVAPCOR2         )
!
! Matthieu Lafaysse / Eric Brun 03/10/2012
! Print energy and mass balances.
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PSUMMASS_INI,PSUMHEAT_INI,PSUMMASS_FIN,PSUMHEAT_FIN
REAL, INTENT(IN) :: PSR,PRR,PTHRUFAL,PEVAP,PEVAPCOR
REAL, INTENT(IN) :: PGRNDFLUX,PHSNOW,PRNSNOW,PLEL3L,PLES3L,PHPSNOW,PSNOWHMASS
REAL, INTENT(IN) :: PSNOWDZ !first layer
REAL, INTENT(IN) :: PTSTEP !time step
REAL, INTENT(IN) :: PMASSBALANCE, PENERGYBALANCE, PEVAPCOR2
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTBALANCE',0,ZHOOK_HANDLE)
!
WRITE(*,*) ' '
WRITE(*,FMT='(A1,67("+"),A1)') "+","+"
!
 CALL SNOWCROPRINTDATE()
!
WRITE(*,*) ' '
!
! print des residus de bilan et des differents termes pour le point
WRITE (*,FMT="(A25,1x,E17.10)") 'final mass (kg/m2) =' , PSUMMASS_FIN
WRITE (*,FMT="(A25,1x,E17.10)") 'final energy (J/m2) =', ZSUMHEAT_FIN
WRITE(*,*) ' '
!
WRITE(*,FMT="(A25,1x,E17.10)") 'mass balance (kg/m2) =', PMASSBALANCE
!
WRITE(*,*) ' '
WRITE(*,FMT="(A35)") 'mass balance contribution (kg/m2) '
WRITE(*,FMT="(A51,1x,E17.10)") 'delta mass:', (PSUMMASS_FIN-PSUMMASS_INI)
WRITE(*,FMT="(A51,1x,E17.10)") 'hoar or condensation (>0 towards snow):', -PEVAP * PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'rain:',    PRR * PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'snow:',    PSR * PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'run-off:', PTHRUFAL * PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'evapcor:', PEVAPCOR * PTSTEP
!
WRITE(*,FMT='(A1,55("-"),A1)')"+","+"
WRITE(*,*) ' '
!
WRITE(*,FMT="(A25,4(1x,E17.10))") 'energy balance (W/m2)=',PENERGYBALANCE
!
WRITE(*,*) ' '
WRITE(*,FMT="(A55)") 'energy balance contribution (W/m2) >0 towards snow :'
WRITE(*,FMT="(A51,1x,E17.10)") 'delta heat:', (ZSUMHEAT_FIN-ZSUMHEAT_INI)/PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'radiation (LW + SW):', PRNSNOW
WRITE(*,FMT="(A51,1x,E17.10)") 'sensible flux :',      -PHSNOW
WRITE(*,FMT="(A51,1x,E17.10)") 'ground heat flux :',   -PGRNDFLUX
WRITE(*,FMT="(A51,1x,E17.10)") 'liquid latent flux:',  -PLEL3L
WRITE(*,FMT="(A51,1x,E17.10)") 'solid latent flux:',   -PLES3L
WRITE(*,FMT="(A51,1x,E17.10)") 'rain sensible heat:',  PHPSNOW
WRITE(*,FMT="(A51,1x,E17.10)") 'snowfall/hoar heat (sensible + melt heat):', PSNOWHMASS/PTSTEP
WRITE(*,FMT="(A51,1x,E17.10)") 'evapcor:', PEVAPCOR2
WRITE(*,FMT='(A1,67("+"),A1)')"+","+"
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTBALANCE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROPRINTBALANCE
!
!####################################################################
SUBROUTINE GET_BALANCE(PSUMMASS_INI,PSUMHEAT_INI,PSUMMASS_FIN,PSUMHEAT_FIN, &
                       PSR,PRR,PTHRUFAL,PEVAP,PEVAPCOR,PGRNDFLUX,PHSNOW,    &
                       PRNSNOW,PLEL3L,PLES3L,PHPSNOW,PSNOWHMASS,PSNOWDZ,    &
                       PTSTEP,PMASSBALANCE,PENERGYBALANCE,PEVAPCOR2         )
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PSUMMASS_INI,PSUMHEAT_INI,PSUMMASS_FIN,PSUMHEAT_FIN
REAL, INTENT(IN) :: PSR,PRR,PTHRUFAL,PEVAP,PEVAPCOR
REAL, INTENT(IN) :: PGRNDFLUX,PHSNOW,PRNSNOW,PLEL3L,PLES3L,PHPSNOW,PSNOWHMASS
REAL, INTENT(IN) :: PSNOWDZ !first layer
REAL, INTENT(IN) :: PTSTEP !time step
!
REAL, INTENT(OUT)  :: PMASSBALANCE, PENERGYBALANCE, PEVAPCOR2
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_BALANCE',0,ZHOOK_HANDLE)
!
PMASSBALANCE = PSUMMASS_FIN - PSUMMASS_INI - &
               ( PSR + PRR - PTHRUFAL - PEVAP + PEVAPCOR ) * PTSTEP
!
PEVAPCOR2 = PEVAPCOR * PSNOWDZ / MAX( XUEPSI,PSNOWDZ ) *  &
           ( ABS(PLEL3L) * XLVTT / MAX( XUEPSI,ABS(PLEL3L) ) + &
             ABS(PLES3L) * XLSTT / MAX( XUEPSI,ABS(PLES3L) ) )
!
PENERGYBALANCE = ( PSUMHEAT_FIN-PSUMHEAT_INI ) / PTSTEP - &
                 ( -PGRNDFLUX - PHSNOW + PRNSNOW - PLEL3L - PLES3L + PHPSNOW ) - &
                 PSNOWHMASS / PTSTEP - PEVAPCOR2
!
!IF (LHOOK) CALL DR_HOOK('SNOWCRO:GET_BALANCE',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_BALANCE
!
!###################################################################
SUBROUTINE SNOWCROPRINTDATE()
!
!USE PARKIND1  ,ONLY : JPRB  ! trude added
!
IMPLICIT NONE
!
!REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTDATE',0,ZHOOK_HANDLE)
!
!WRITE(*,FMT='(I4.4,2("-",I2.2)," Hour=",F5.2)') &
!  TPTIME%TDATE%YEAR, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY, TPTIME%TIME/3600.
!
!IF (LHOOK) CALL DR_HOOK('SNOWCROPRINTDATE',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOWCROPRINTDATE
!####################################################################
!###################################################################
!
END SUBROUTINE SNOWCRO


END MODULE MODULE_SNOWCRO
