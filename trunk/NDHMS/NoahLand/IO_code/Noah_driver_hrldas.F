#define _DO_OUTPUT_
#define _DO_SYSTEM_CALLS_

program Noah_driver_hrldas

!  USE module_sf_Noah_grads_output
!  USE module_sf_Noahlsm_gridded_input
!  USE module_sf_Noahlsm_rtfdda_input
!  USE read_ihop
  USE module_sf_Noahlsm_hrldas_input
  USE module_sf_Noahlsm_param_init
  USE module_Noahlsm_utility
  USE module_sf_hrldas_noahlsm
  USE module_date_utilities

  IMPLICIT NONE

  ! Dummy parameterized dimension for maximum number of soil levels allowed
  INTEGER, PARAMETER :: NSOLDX = 100

  ! Intent (IN) to REDPRM
  ! ZSOIL, set through the namelist, is the BOTTOM of each soil layer (m)
  REAL, DIMENSION(NSOLDX) :: ZSOIL

  REAL :: T1V,TH2V

!-----------------------------------------------------------------
!  Dimensions from the input file:
!-----------------------------------------------------------------

!     IX: x-direction points, usually along latitude
!     JX: y-direction points, usually along longitude
  INTEGER :: IX
  INTEGER :: JX
  INTEGER :: LAND_CAT
  INTEGER :: SOIL_CAT

!-----------------------------------------------------------------
!  DECLARE VARIABLES FOR GRIDDED SIMULATION
!-----------------------------------------------------------------

! the following parameters are read from a namelist file

! setup model configuration
!   KHOUR: number of hours for the simulation
  INTEGER :: KHOUR
  INTEGER :: KDAY
! INTEGER :: NSOIL: number of soil layers for the LSM integration
! REAL    :: DT:    time-step in seconds
! REAL    :: ZLVL:  Level (m) at which atmospheric fields are taken to be valid

! Unaccounted for variables when compiling w/ implicit none F90

!KWM  REAL   :: VEGMIN,VEGMAX
  REAL   :: SNOFAC,ALBED
  REAL   :: RHO,CHKFF,T14,AET,FUP,S
!KWM  REAL   :: F, H
!KWM  REAL   :: SNOFLX, SNMAX, SNOEVP

  INTEGER   :: z,j,k,i,ierr
  INTEGER   :: NTIME

! Gridded fields

  INTEGER, allocatable, DIMENSION(:,:)   :: VEGTYP,SOLTYP
  REAL,    allocatable, DIMENSION(:,:)   :: TERRAIN, LATITUDE, LONGITUDE
  REAL,    allocatable, DIMENSION(:,:)   :: T2,XLONG,U,V,PRES,SHORT,PRCP1
  REAL,    allocatable, DIMENSION(:,:)   :: FPAR, LAI
  REAL,    allocatable, DIMENSION(:,:)   :: CMC,SNODEP,WEASD,T1
  REAL,    allocatable, DIMENSION(:,:)   :: ETPX,ETAX,SOILHX,CHX,RUNOFF1X,RUNOFF2X
  REAL,    allocatable, DIMENSION(:,:)   :: ETAKIN, QFX
  REAL,    allocatable, DIMENSION(:,:)   :: RUNOFF3X,EDIRX,ECX,ETTX,SNMAXX,RCX,HX
  REAL,    allocatable, DIMENSION(:,:)   :: Q2X,SFCSPDX,ALBEDX,SMCMAX1,SMCWLT1
  REAL,    allocatable, DIMENSION(:,:)   :: SMCREF1,FX,RESX
  REAL,    allocatable, DIMENSION(:,:)   :: SNOFLXX,SNOEVPX,ACSNOM,ESNOW2D,ACRAIN
  REAL,    allocatable, DIMENSION(:,:)   :: DRIP2D, DEWFALL, SOILMX, EMISS
!KWM  REAL,    allocatable, DIMENSION(:,:)   :: ETPNDX, SFCHEAD, INFXS1, PDDUM2, PCPDRP, SFCWATR2

  REAL,    allocatable, DIMENSION(:,:,:) :: SMC,STC,SH2OX
  REAL,    allocatable, DIMENSION(:,:,:) :: ZSOILX 

!---------------------------------------------------------------------
! NEW VARIABLES ADDED DURING NOAH F90 UPGRADE CALL to SFLX
!---------------------------------------------------------------------


! INTENT(IN) to SFLX:
  INTEGER, PARAMETER :: ICE = 0     ! Sea-ice flag  (=1: sea-ice, =0: land)
  REAL    :: DT       ! Timestep (s) (DT should not exceed 3600 S, recommend 1800 s or less)
  REAL    :: ZLVL     ! Height (m) above ground of atmospheric forcing variables
  INTEGER :: NSOIL    ! Number of soil layers
  REAL, ALLOCATABLE, DIMENSION(:) :: SLDPTH ! The THICKNESS (m) of each soil layer 
  CHARACTER(LEN=4) :: LLANDUSE ! (=USGS, using USGS landuse classification)
  CHARACTER(LEN=4) :: LSOIL    ! (=STAS, using FAO/STATSGO soil texture classification)

  REAL :: LWDN      ! LW downward radiation (W m-2; positive, not net longwave)
  REAL :: SOLDN     ! Solar downward radiation (W m-2; positive, not net solar)
  REAL :: SFCPRS    ! Pressure (Pa) at height ZLVL m above ground
  REAL :: PRCP      ! Precip rate (kg m-2 s-1) (NOTE: this is a rate)
  REAL :: SFCTMP    ! Air temperature (K) at height ZLVL m above ground
  REAL :: Q2        ! Mixing ratio (kg kg-1) at height ZLVL m above ground 
  REAL :: SFCSPD    ! Wind speed (m s-1) at height ZLVL m above ground
  REAL :: PRCPRAIN  ! Liquid-precipitation rate (KG M-2 S-1) (not used)
  REAL :: TH2       ! Air potential temperature (K) at height ZLVL m above ground
  REAL :: Q2SAT     ! Sat mixing ratio (kg kg-1) at height ZLVL m above ground
  REAL :: DQSDT2    ! Slope of sat specific humidity curve  (kg kg-1 K-1) at T=SFCTMP
  INTEGER :: VEGTYPX! Vegetation type (integer index)
  INTEGER :: SOILTYP! Soil type (integer index)
  INTEGER ::SLOPETYP! Class of sfc slope (integer index)
  REAL :: SHDMIN    ! Minimum areal fractional coverage of green vegetation
  !         (dimensionless fraction 0.0-1.0) <= SHDFAC
  REAL :: SHDMAX    !UNDOCUMENTED IN SFLX
  REAL :: TBOT      ! Bottom soil temperature (local yearly-mean of surfacc air temperature)


! INTENT(INOUT) to/from SFLX:
  REAL :: SNCOVR  ! Fractional snow cover (dimensionless fraction, 0.0-1.0)
  REAL :: COSZ    ! Solar zenith angle (not used for now)
  REAL :: SOLARDIRECT ! Direct component of downward solar radiation (W m-2) (not used)
  REAL :: Z0      ! Time varying roughness length (m) as function of snow depth
  REAL :: CMCX    ! Canopy moisture content (m)
  REAL :: T1X     ! Ground/Canopy/Snowpack effective skin temperature (K)
  REAL, allocatable, DIMENSION(:)  :: STC1    ! Soil temp (K)                            
  REAL, allocatable, DIMENSION(:)  :: SMC1    ! Total soil moisture content (volumetric fraction)
  REAL, allocatable, DIMENSION(:)  :: SH2O    ! Unfrozen soil moisture content (volumetric fraction)
  !          NOTE: Frozen soil moisture = SMC - SH2O
  REAL :: SNOWH   ! Actual snow depth (m)                                      
  REAL :: SNEQV   ! Liquid water-equivalent snow depth (m)                     
  !       NOTE: snow density = SNEQV/SNOWH                         
  REAL :: ALBEDO  ! Surface albedo including snow effect (unitless fraction)   
  !      =snow-free albedo (ALB) when SNEQV=0, or                 
  !      =FCT(MSNOALB,ALB,VEGTYP,SHDFAC,SHDMIN) when SNEQV>0      
  REAL :: CH      ! Surface exchange coefficient for heat and moisture         
  !   (m s-1); NOTE: CH is technically a conductance since     
  !   it has been multiplied by wind speed.                    
  REAL :: CM      ! Surface exchange coefficient for momentum (m s-1); NOTE:   
  !   CM is technically a conductance since it has been        
  !   multiplied by wind speed.                                

! INTENT(OUT) from SFLX:
  REAL :: ETT     ! Total plant transpiration (W m-2)
  REAL :: ETA     ! Actual latent heat flux (W m-2: negative, if up from surface)     
!KWM ???  Is the note about the sign of ETA right ???? !KWM
  REAL :: SHEAT   ! Sensible heat flux (W m-2: negative, if upward from surface)
  REAL :: ETA_KINEMATIC ! Actual latent heat flux (kg m/s)
  REAL :: FDOWN   ! Radiation forcing at the surface (W m-2) = SOLDN*(1-alb)+LWDN
  REAL :: EC      ! Canopy water evaporation (W m-2)
  REAL :: EDIR    ! Direct soil evaporation (W m-2)
  REAL, allocatable, DIMENSION(:) :: ET      ! Plant transpiration from a particular root (soil) layer (W m-2)
  REAL :: ESNOW   ! Sublimation from (or deposition to if <0) snowpack (W m-2)
  REAL :: DRIP    ! Through-fall of precip and/or dew in excess of canopy
  ! water-holding capacity (m)
  REAL :: DEW     ! Dewfall (or Frostfall for T<273.15) (m)

  REAL :: BETA    ! Ratio of actual to potential evap (dimensionless)
  REAL :: ETP     ! Potential Evaporation (w m-2)
  REAL :: SSOIL   ! Soil heat flux (W m-2: negative if downward from surface)
  REAL :: FLX1    ! Precip-snow sfc (W m-2)
  REAL :: FLX2    ! Freezing rain latent heat flux (W m-2)      
  REAL :: FLX3    ! Phase-change heat flux from snowmelt (W m-2)
  REAL :: SNOMLT  ! Snow melt (m) (water equivalent)
  REAL :: RUNOFF1 ! Surface runoff (m s-1), not infiltrating the surface
  REAL :: RUNOFF2 ! Subsurface runoff (m s-1), drainage out bottom of last
  !       soil layer (baseflow).  Note: RUNOFF2 is actually 
  !       the sum of RUNOFF2 and RUNOFF3
  REAL :: RUNOFF3 ! Numerical trunctation in excess of porosity (SMCMAX)
  ! for a given soil layer at the end of a time step (m s-1).
  REAL :: RC      ! Canopy resistance (s m-1)
  REAL :: PC      ! Plant coefficient (dimensionless fraction, 0.0-1.0) 
  ! where PC*ETP = actual transpiration
  REAL :: RCS     ! Incoming solar RC factor (dimensionless)
  REAL :: RCT     ! Air temperature RC factor (dimensionless)
  REAL :: RCQ     ! Atmos. vapor pressure deficit RC factor (dimensionless)
  REAL :: RCSOIL  ! Soil moisture RC factor (dimensionless)
  REAL :: SOILW   ! Available soil moisture in root zone (dimensionless fraction
  !      between SMCWLT and SMCMAX)
  REAL :: SOILM   ! Total soil column moisture content (frozen+unfrozen) (m)
  REAL :: Q1      ! Effective mixing ratio at surface (kg kg-1), used for
  !      diagnosing the mixing ratio at 2 meter for 
  !      coupled model


!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

  REAL, PARAMETER :: R=287.04
  REAL, PARAMETER :: CPHEAT=1004.5
  INTEGER, PARAMETER :: LAND=1

  character(len=19) :: olddate, newdate, startdate
  CHARACTER(len=256) :: inflnm, outflnm
  character :: hgrid

! NAMELIST:

  CHARACTER(len=256) :: indir
  integer :: igrid
  character(len=256) :: range = ""
  character(len=256) :: wrfsi_static_flnm = ""
  character(len=13)  :: restart_hdate = ""
  integer            :: split_output_count
  

  integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
  namelist / NOAHLSM_OFFLINE/ INDIR, NSOIL, ZSOIL, DT, TBOT, &
       START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN, &
       RESTART_HDATE, SPLIT_OUTPUT_COUNT, IGRID, RANGE, KHOUR, KDAY, ZLVL, Z, &
       wrfsi_static_flnm
!KWM       , VEGMIN, VEGMAX

  integer, parameter :: iunit = 10
  integer, parameter :: ounit = 11

  LLANDUSE = 'USGS'
  LSOIL = 'STAS'

  ! Initialize namelist variables to dummy values, so we can tell
  ! if they have not been set properly.

  nsoil = -999
  zsoil = -999.

  dt = -999.
  tbot = -999.

  start_year = -999
  start_month = -999
  start_day = -999
  start_hour = -999
  start_min = -999
  khour = -999
  kday = -999

  zlvl = -999.
!KWM  vegmin = -999.
!KWM  vegmax = -999.
  z = -999.

  open(30, file="noah_hrldas.namelist", form="FORMATTED")
  read(30, NOAHLSM_OFFLINE, iostat=ierr)
!  if (ierr /= 0) then
!     stop " ***** ERROR: Problem reading namelist"
!  endif
  close(30)

  if ((khour < 0) .and. (kday < 0)) then
     print*, 'Namelist problem:  Either KHOUR or KDAY must be defined.'
     stop
  else if (( khour < 0 ) .and. (kday > 0)) then
     khour = kday * 24
  else if ((khour > 0) .and. (kday > 0)) then
     print*, 'Namelist warning:  KHOUR and KDAY both defined.'
  else
     ! all is well.  KHOUR defined
  endif

  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":"I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate
  write(hgrid,'(I1)') igrid


#if defined (_DO_SYSTEM_CALLS_) 
     call system ('rm -f '//trim(indir)//"/*LDASIN*")
     call system ("/wahoo2/kmanning/HRLDAS_WRFDRIVER/collect_data/run/onetime.csh "//&
           olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//"_"//olddate(12:13)//&
           " "//hgrid//" "//trim(WRFSI_STATIC_FLNM))
#endif


  if (len_trim(range) == 0) then
     inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid
  else
     inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
  endif

!KWM  call read_rtfdda_hdrinfo(iunit, ix, jx)
  call read_hrldas_hdrinfo(wrfsi_static_flnm, ix, jx, land_cat, soil_cat)

  if (nsoil < 0) then
     stop " ***** ERROR: NSOIL must be set in the namelist."
  endif

  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":"I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate

!......................... end of model configuration (later in a namelist) .....

  CALL LSM_PARM_INIT

!----------------------------------------------------------------------
! Allocate arrays for our gridded domain, now that we know the size
!----------------------------------------------------------------------
  allocate( VEGTYP   (IX,JX) )
  allocate( SOLTYP   (IX,JX) )
  allocate( TERRAIN  (IX,JX) )
  allocate( LATITUDE (IX,JX) )
  allocate( LONGITUDE(IX,JX) )
  allocate( T2       (IX,JX) )
  allocate( XLONG    (IX,JX) )
  allocate( U        (IX,JX) )
  allocate( V        (IX,JX) )
  allocate( PRES     (IX,JX) )
  allocate( SHORT    (IX,JX) )
  allocate( PRCP1    (IX,JX) )
  allocate( FPAR     (IX,JX) )
  allocate( LAI      (IX,JX) )
  allocate( CMC      (IX,JX) )
  allocate( SNODEP   (IX,JX) )
  allocate( WEASD    (IX,JX) )
  allocate( T1       (IX,JX) )
  allocate( ETPX     (IX,JX) )
  allocate( ETAX     (IX,JX) )
  allocate( ETAKIN   (IX,JX) )
  allocate( SOILHX   (IX,JX) )
  allocate( CHX      (IX,JX) )
  allocate( RUNOFF1X (IX,JX) )
  allocate( RUNOFF2X (IX,JX) )
  allocate( RUNOFF3X (IX,JX) )
  allocate( EDIRX    (IX,JX) )
  allocate( ECX      (IX,JX) )
  allocate( ETTX     (IX,JX) )
  allocate( SNMAXX   (IX,JX) )
  allocate( RCX      (IX,JX) )
  allocate( HX       (IX,JX) )
  allocate( QFX      (IX,JX) )
  allocate( Q2X      (IX,JX) )
  allocate( SFCSPDX  (IX,JX) )
  allocate( ALBEDX   (IX,JX) )
  allocate( SMCMAX1  (IX,JX) )
  allocate( SMCWLT1  (IX,JX) )
  allocate( SMCREF1  (IX,JX) )
  allocate( ACSNOM   (IX,JX) )
  allocate( ACRAIN   (IX,JX) )
  allocate( ESNOW2D  (IX,JX) )
!KWM  allocate( SNOFLXX  (IX,JX) )
  allocate( SNOEVPX  (IX,JX) )
  allocate( FX       (IX,JX) )
  allocate( RESX     (IX,JX) )
  allocate( DRIP2D   (IX,JX) )
  allocate( DEWFALL  (IX,JX) )
  allocate( SOILMX   (IX,JX) )
  allocate( EMISS    (IX,JX) )
!KWM  allocate( ETPNDX   (IX,JX) )
!KWM  allocate( SFCHEAD  (IX,JX) )
!KWM  allocate( INFXS1   (IX,JX) )
!KWM  allocate( PDDUM2   (IX,JX) )
!KWM  allocate( PCPDRP   (IX,JX) )
!KWM  allocate( SFCWATR2 (IX,JX) )

  allocate( SMC  (IX,JX,NSOIL) ) 
  allocate( STC  (IX,JX,NSOIL) )
  allocate( SH2OX(IX,JX,NSOIL) )
  allocate( ZSOILX(IX,JX,NSOIL) )

  allocate( SH2O   (NSOIL) )
  allocate( SLDPTH (NSOIL) )
  allocate( SMC1   (NSOIL) )
  allocate( STC1   (NSOIL) )
  allocate( ET     (NSOIL) )

!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------

  ! SLDPTH is the thickness of each layer
  SLDPTH(1) = -ZSOIL(1)
  do i = 2, nsoil
     sldpth(i) = zsoil(i-1)-zsoil(i)
  enddo

  ETAX=0.0 ! -999.9
  ETAKIN=0.0 ! -999.9
  ETPX=-999.9
  CHX=-999.9
  FX=-999.9
  HX=-999.9
  RESX=-999.9
  SOILHX=-999.9
  CMC=0.0
  T2=-999.9
  T1=-999.9
  PRCP1=-999.9

  RUNOFF1X=0.0 
  RUNOFF2X=0.0 
  RUNOFF3X=0.0 
  EDIRX=0.0
  ETTX=0.0
!FC  ETPNDX=-999.9
  SNOEVPX=-999.9
  SNODEP=-999.9

  STC=-999.9
  SH2OX=-999.9
  SMC=-999.9

  ECX=0.0
  ACSNOM  = 0.0
  ACRAIN  = 0.0
  ESNOW2D = 0.0
  DRIP2D = 0.0
  DEWFALL = 0.0
  SOILMX = -999.

!---------------------------------------------------------------------
! Initialize static surface data
!---------------------------------------------------------------------


!   OK     VEGTYP     VEGETATION TYPE (INTEGER INDEX)
!   OK     SOILTYP    SOIL TYPE (INTEGER INDEX)
!   OK     SLOPETYP   CLASS OF SFC SLOPE (INTEGER INDEX)
!          SHDMIN     MINIMUM AREAL FRACTIONAL COVERAGE OF GREEN VEGETATION
!                (FRACTION= 0.0-1.0) <= SHDFAC
!   N/U    PTU        PHOTO THERMAL UNIT (PLANT PHENOLOGY FOR ANNUALS/CROPS)
!                (NOT YET USED, BUT PASSED TO REDPRM FOR FUTURE USE IN
!                VEG PARMS)
!   OK     ALB        BACKROUND SNOW-FREE SURFACE ALBEDO (FRACTION), FOR JULIAN
!                DAY OF YEAR (USUALLY FROM TEMPORAL INTERPOLATION OF
!                MONTHLY MEAN VALUES' CALLING PROG MAY OR MAY NOT
!                INCLUDE DIURNAL SUN ANGLE EFFECT)
!   OK     SNOALB     UPPER BOUND ON MAXIMUM ALBEDO OVER DEEP SNOW (E.G. FROM
!                ROBINSON AND KUKLA, 1985, J. CLIM. & APPL. METEOR.)
!   OK     TBOT       BOTTOM SOIL TEMPERATURE (LOCAL YEARLY-MEAN SFC AIR
!                TEMPERATURE)
!   ??     Z0BRD      Background fixed roughness length (M)
!   ??     Z0         Time varying roughness length (M) as function of snow depth




!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------

!KWM         CALL READLAND(IX,JX,VEGTYP,SOLTYP, TERRAIN, LATITUDE,LONGITUDE)
!KWM  CALL READLAND_IHOP(DIR,IX,JX,VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)
!KWM  CALL READLAND_RTFDDA(IUNIT,IX,JX,VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)
  CALL READLAND_HRLDAS(wrfsi_static_flnm,IX,JX,LAND_CAT,SOIL_CAT,      &
       VEGTYP,SOLTYP,TERRAIN,LATITUDE,LONGITUDE)

!----------------------------------------------------------------------
! Initialize Model State
!----------------------------------------------------------------------

!KWM         CALL READINIT(IX,JX,SMC,STC,SH2OX,ESDX,CMC,T1, SNODEP)
!KWM CALL READINIT_IHOP(DIR, IX, JX, NSOIL, SMC, STC, SH2OX, ESDX, CMC, T1, SNODEP)
!KWM  CALL READINIT_RTFDDA(IUNIT, IX, JX, NSOIL, OLDDATE, SMC, STC, SH2OX, CMC, T1, &
!KWM       WEASD, SNODEP)
  CALL READINIT_HRLDAS(inflnm, IX, JX, NSOIL, OLDDATE, SMC, STC, SH2OX, CMC, T1, &
       WEASD, SNODEP)


! *** Read lai, fveg etc.
!KWM        CALL READVEG(IX,JX,FPAR,XLAI)
!KWM  CALL READVEG_IHOP(DIR,IX,JX,FPAR,LAI)
!KWM  CALL READVEG_RTFDDA(IUNIT, IX, JX, OLDDATE, VEGTYP, FPAR, LAI)
  CALL READVEG_HRLDAS(inflnm, IX, JX, OLDDATE, VEGTYP, FPAR, LAI)

  SLOPETYP = 8

  close(iunit)
!  rewind(iunit)

! Output in V3 format at time zero:
  SOILMX = -1.0 * SMC (:,:,1)* ZSOIL (1)
  DO K = 2,NSOIL
     SOILMX = SOILMX + SMC(:,:,K) * (ZSOIL (K-1) - ZSOIL (K))
  ENDDO
  SOILMX = SOILMX * 1.E3 ! Convert from m to mm
  HX = 0.0
  QFX = 0.0

!KWM  call output_rtfdda(ounit, hgrid, range, ix, jx, nsoil, sldpth, startdate, startdate, &
!KWM       vegtyp, soltyp, t1, cmc, stc, smc, sh2ox, soilmx, runoff1x, runoff2x, &
!KWM       runoff3x, etax, etakin, ecx, edirx, ettx, albedx, weasd, acrain, acsnom, &
!KWM       esnow2d, drip2d, dewfall, snodep, fpar, hx, qfx)

  if (restart_hdate /= "") then

     olddate = restart_hdate(1:13)//":00:00"
     open(156, file="RESTART."//olddate(1:13), form='unformatted', action='read')
     read(156) stc
     read(156) smc
     read(156) qfx
     read(156) sh2ox
     read(156) snodep
     read(156) weasd
     read(156) fx
     read(156) etpx
     read(156) t1
     read(156) etax
     read(156) etakin
     read(156) CMC
     read(156) SOILHX
     read(156) CHX
     read(156) RUNOFF1X
     read(156) RUNOFF2X
     read(156) RUNOFF3X
     read(156) EDIRX
     read(156) ECX
     read(156) ETTX
     read(156) RCX
     read(156) HX
     read(156) SMCMAX1
     read(156) SMCREF1
     read(156) SMCWLT1
     read(156) ALBEDX
     read(156) ACRAIN
     read(156) ACSNOM
     read(156) ESNOW2D
     read(156) DRIP2D
     read(156) DEWFALL
     read(156) SOILMX
     read(156) SHORT
     read(156) FPAR
     close(156)
     
  endif

#if defined (_DO_OUTPUT_)
  call output_hrldas(ounit, split_output_count, hgrid, range, ix, jx, &
       nsoil, sldpth, startdate, olddate, &
       vegtyp, soltyp, t1, cmc, stc, smc, sh2ox, soilmx, runoff1x, runoff2x, &
       runoff3x, etax, etakin, ecx, edirx, ettx, albedx, weasd, acrain, acsnom, &
       esnow2d, drip2d, dewfall, snodep, fpar, hx, qfx, short)
#endif

!------------------------------------------------------------------------
! Begin Time Loop
!------------------------------------------------------------------------

  NTIME=KHOUR*3600./nint(dt)
  KLOOP : DO K=1,NTIME
!
! Read the forcing data.
!
!     CALL READFORC_IHOP(IX,JX,YEAR,MDT,MINUTE,DIR,DT,T2,Q2X,XLONG,U,V,   &
!          PRES,SHORT,PRCP1, LAI, FPAR)

!     CALL READFORC_RTFDDA(IUNIT,IX,JX,OLDDATE,T2,Q2X,U,V,   &
!          PRES,XLONG,SHORT,PRCP1,LAI,FPAR,IERR)

! For HRLDAS, we're assuming (for now) that each time period is in a 
! separate file.  So we can open a new one right now.

     if (len_trim(range) == 0) then
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid
     else
        inflnm = trim(indir)//"/"//&
             olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
             ".LDASIN_DOMAIN"//hgrid//"."//trim(range)
     endif

#if defined (_DO_SYSTEM_CALLS_)

     call system ("/wahoo2/kmanning/HRLDAS_WRFDRIVER/collect_data/run/onetime.csh "//&
           olddate(1:4)//"-"//olddate(6:7)//"-"//olddate(9:10)//"_"//olddate(12:13)//&
           " "//hgrid//" "//trim(WRFSI_STATIC_FLNM))

#endif

     CALL READFORC_HRLDAS(inflnm,IX,JX,OLDDATE,T2,Q2X,U,V,   &
          PRES,XLONG,SHORT,PRCP1,LAI,FPAR)

     close(iunit)

!------------------------------------------------------------------------
! Spatial Loop to Convert Gridded data to single point for 1-D SFLX call
!------------------------------------------------------------------------


     JLOOP : DO J=1,JX ! max(JX-1,1)
        ILOOP : DO I=1, IX ! max(IX-1,1)
!KWM     JLOOP : DO J=42,42 ! max(JX-1,1)
!KWM        ILOOP : DO I=44,44 ! max(IX-1,1)

           IF((VEGTYP(I,J).GT.0).AND.(VEGTYP(I,J).NE.16)) THEN
              VEGTYPX=VEGTYP(I,J)
              SOILTYP=SOLTYP(I,J)
              SFCTMP=T2(I,J)
              SFCSPD=SQRT(U(I,J)*U(I,J)+V(I,J)*V(I,J))
              SFCPRS=PRES(I,J)
              Q2=Q2X(I,J)
              PRCP=PRCP1(I,J)
              if (prcp < 0.0) prcp = 0.0 ! Just in case
              SOLDN=SHORT(I,J)
              LWDN=XLONG(I,J)

              CALL REDPRM (VEGTYPX,SOILTYP,SLOPETYP,SLDPTH,ZSOIL,NSOIL)

              XLAI =LAI(I,J)
              SHDFAC = FPAR(I,J)

!   snow albedo
              albedo = alb !?
              SNOFAC=MIN(SNODEP(I,J)*5.0, 1.0)
              ALBED=ALBEDO+(1.0-FPAR(I,J))*SNOFAC*(0.55-ALBEDO)
!   Net downward radiation
!              F= SRAD*(1.0-ALBED)+LWDN
!   
              CH = 0.1
              CM = 0.1

              Z0 = Z0BRD

!     Calculate a saturation mix ratio

              CALL CALTMP(T1(I,J), SFCTMP, SFCPRS, ZLVL, Q2, TH2, T1V, TH2V, RHO)
              CALL CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

!     Calculate the surface exchange coefficients CM, CH
              ! Intent (IN) :: ZLVL, Z0, T1V, TH2V, SFCSPD, CZIL
              ! Intent (INOUT) :: CM
              CALL SFCDIF_off (ZLVL,Z0,T1V,TH2V,SFCSPD,CZIL,CM,CH)                  

              IF (Q2 .LT. 0.0) THEN
                 print*,'Q2<0','I=',I, 'J=',J
                 Q2=.1E-5
              ENDIF
              IF (Q2 .GT. Q2SAT) THEN
                 ! print*, 'Q2 .GT. Q2SAT', 'I=',I, 'J=',J
                 ! print*,'Q2=',Q2,'Q2SAT=',Q2SAT
                 Q2=Q2SAT*0.99
              ENDIF


              CHKFF = CH * CPHEAT * RHO           

              STC1(1:NSOIL)=STC(I,J,1:NSOIL)

              SMC1(1:NSOIL)=SMC(I,J,1:NSOIL)
              SH2O(1:NSOIL)=SH2OX(I,J,1:NSOIL)
              ZSOILX(I,J,1:NSOIL)=ZSOIL(1:NSOIL)
! *** diagnostics 
              SFCSPDX(I,J)=SFCSPD


!--- Other conversions 'History (State) Variables

              CMCX=CMC(I,J)
              T1X=T1(I,J)
              SNOWH=SNODEP(I,J)

              SNEQV = WEASD(I,J)
              shdmin = 0.0

              esnow = 0.0

              CALL SFLX (ICE,DT,ZLVL,NSOIL,SLDPTH,                  &    !C
                   LWDN,SOLDN,SFCPRS,PRCP,SFCTMP,Q2,SFCSPD,         &    !F
                   COSZ,PRCPRAIN, SOLARDIRECT,                      &    !F
                   TH2,Q2SAT,DQSDT2,                                &    !I
                   VEGTYPX,SHDMIN,SHDMAX,                           &    !I
                   TBOT,Z0,                                         &    !S
                   CMCX,T1X,STC1,SMC1,SH2O,SNOWH,SNEQV,ALBEDO,CH,CM,&    !H
                   ETA,SHEAT, ETA_KINEMATIC,FDOWN,                  &    !O
                   EC,EDIR,ET,ETT,ESNOW,DRIP,DEW,                   &    !O
                   BETA,ETP,SSOIL,                                  &    !O
                   FLX1,FLX2,FLX3,                                  &    !O
                   SNOMLT,SNCOVR,                                   &    !O
                   RUNOFF1,RUNOFF2,RUNOFF3,                         &    !O
                   RC,PC,RCS,RCT,RCQ,RCSOIL,                        &    !O
                   SOILW,SOILM,Q1)                                       !P

!---------------------------------------------------------------------
! Begin Converting Data back to grid from 1-d and make units conversions
!---------------------------------------------------------------------

!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CONVERT ACTUAL EVAPOTRANSPIRATION VALUE FROM  KG M-2 S-1  TO
!     W M-2  (BY MULTIPLYING BY 2.501E+6) FOR USE IN ENERGY BALANCE.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              T14 = T1X * T1X * T1X * T1X
              AET = ETA
              FUP = 5.67E-8 * T14
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     CALCULATE RESIDUAL OF ALL SURFACE ENERGY BALANCE EQN TERMS.
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
              S = -S
!FC              RESX(I,J) = F - H - S - AET - FUP - FLX1 - FLX2 - FLX3
!KWM              SNOFLX=FLX3
!  Convert ETA and ETP from W M-2 to KG M-2 S-1

!KWM              ETA = ETA/2.501E+6
              ETP = ETP/2.501E+6  

              QFX(I,J) = (EDIR+EC+ETT) + ESNOW ! in W m{-2}

!KWM Overwrite ETA for now
              ETA = ((EDIR+EC+ETT)/2.501E+6) + (ESNOW/2.83E6)

! Fill output variable arrays and doing output

              STC(I,J,1:NSOIL)=STC1(1:NSOIL)    ! Updated Soil Temperature
              SMC(I,J,1:NSOIL)=SMC1(1:NSOIL)    ! Updated Soil Moisture
              SH2OX(I,J,1:NSOIL)=SH2O(1:NSOIL)  ! Updated Soil Liquid Water
              SNODEP(I,J)=SNOWH                 ! Updated snow depth !KWM
              WEASD(I,J)=SNEQV      ! ( m of water )
              FX(I,J)=FDOWN
              ETPX(I,J)=ETP
              T1(I,J)=T1X
              ETAX(I,J)=ETAX(I,J)+ETA*dt ! kg m{-2} s{-1} to mm liquid
              ETAKIN(I,J)=ETA_KINEMATIC*dt
              CMC(I,J)=CMCX
              SOILHX(I,J)=SSOIL
              CHX(I,J)=CH
              RUNOFF1X(I,J)=RUNOFF1X(I,J)+RUNOFF1*dt*1.E3
              RUNOFF2X(I,J)=RUNOFF2X(I,J)+RUNOFF2*dt*1.E3
              RUNOFF3X(I,J)=RUNOFF3X(I,J)+RUNOFF3*dt*1.E3
              EDIRX(I,J)=EDIRX(I,J)+(EDIR/2.501E6)*dt ! (W m{-2} to kg m{-2} s{-2} to mm)
              ECX(I,J)=ECX(I,J)+(EC/2.501E6)*dt       ! (W m{-2} to kg m{-2} s{-2} to mm)
              ETTX(I,J)=ETTX(I,J)+(ETT/2.501E6)*dt    ! (W m{-2} to kg m{-2} s{-2} to mm)
!KWM              ETPNDX(I,J)=ETPND
!KWM              SNMAXX(I,J)=SNMAX
!KWM              SNOFLXX(I,J)=SNOFLX
!FC             SNOEVPX(I,J)=SNOEVP
              RCX(I,J)=RC
              HX(I,J)=SHEAT
              SMCMAX1(I,J)=SMCMAX
              SMCREF1(I,J)=SMCREF
              SMCWLT1(I,J)=SMCWLT
              ALBEDX(I,J)=ALBEDO
              ACRAIN(I,J)=ACRAIN(I,J)+PRCP*dt      ! (mm/s to mm)
              ACSNOM(I,J)=ACSNOM(I,J)+snomlt*1.E3  ! Accumulated snow melt in mm
              ! Accumulated snow sublimation (converted from W m{-2} to kg m{-2} s{-2} to mm)
              ESNOW2D(I,J)=ESNOW2D(I,J)+(ESNOW/2.83E6)*dt
              DRIP2D(I,J)=DRIP2D(I,J)+DRIP*1.E3   ! convert m to mm
              DEWFALL(I,J)=DEWFALL(I,J)+DEW*1.E3  ! convert m to mm
              SOILMX(I,J)= SOILM*1.E3             ! convert m to mm
              EMISS(I,J) = EMISSI

!DG Convert from point to grid (units of SFHEAD & INFXS (mm)
!DG Temp assignment for parking lot runoff
!KWM              SFCHEAD(I,J)=SFHEAD
!DG       INFXS1(I,J)=PRCP1(I,J)
!KWM              INFXS1(I,J)=INFXS
! End temp assignments
!KWM              PDDUM2(I,J)=PDDUM
!KWM              PCPDRP(I,J)=DRIP
!KWM              SFCWATR2(I,J)=SFCWATR

! ***  endif of the land-point
           ENDIF

        ENDDO ILOOP
     ENDDO JLOOP
!-------------------------------------------------------------------
! END of 1-D NOAH processing
!-------------------------------------------------------------------

!------------------------------------------------------------------------
! Call to Output thermodynamic variables
!------------------------------------------------------------------------
!KWM          CALL PRTHM(K,IX,JX,MDT,MINUTE,NSOIL,CHX,FX,SOILHX,                &
!KWM         CMC,T2,T1,ETAX,ETPX,PRCP1,STC,SMC,DT,HX,RESX)

!------------------------------------------------------------------------
! Call to Output hydrological variables
!------------------------------------------------------------------------
!KWM          CALL PRTHYD(K,IX,JX,NSOIL,MDT,MINUTE,ETAX,ETPX,PRCP1,             &
!KWM        SMC,ZSOIL,RUNOFF1X,RUNOFF2X,RUNOFF3X,DT,EDIRX,ECX,                &
!KWM	ETTX,CMC,SLDPTH,SNOEVPX,SH2OX,ESDX,SNODEP,VEGTYP) 
!FC        QBDRYT,QSTRMVOLT,QBDRY,QSTRMVOL,SFCHEAD,INFXS1,DH,                &
!FC        CH_NET,PDDUM2,PCPDRP,SFCWATR2,QLAKVOL,QLAKVOLT)



!------------------------------------------------------------------------
! Update the time 
!------------------------------------------------------------------------

     call geth_newdate(newdate, olddate, nint(dt))
     olddate = newdate

     print*,' ***DATE=', olddate, stc(40,40,1)




!KWM! Output for restart
     if (olddate(12:13) == "00") then
        open(156, file="RESTART."//olddate(1:13), form='unformatted', action='write')
        write(156) stc
        write(156) smc
        write(156) qfx
        write(156) sh2ox
        write(156) snodep
        write(156) weasd
        write(156) fx
        write(156) etpx
        write(156) t1
        write(156) etax
        write(156) etakin
        write(156) CMC
        write(156) SOILHX
        write(156) CHX
        write(156) RUNOFF1X
        write(156) RUNOFF2X
        write(156) RUNOFF3X
        write(156) EDIRX
        write(156) ECX
        write(156) ETTX
        write(156) RCX
        write(156) HX
        write(156) SMCMAX1
        write(156) SMCREF1
        write(156) SMCWLT1
        write(156) ALBEDX
        write(156) ACRAIN
        write(156) ACSNOM
        write(156) ESNOW2D
        write(156) DRIP2D
        write(156) DEWFALL
        write(156) SOILMX
        write(156) SHORT
        write(156) FPAR
        close(156)
     endif

!KWM! Output in V3 format
!KWM     call output_rtfdda(ounit, hgrid, range, ix, jx, nsoil, sldpth, startdate, olddate, &
!KWM          vegtyp, soltyp, t1, cmc, stc, smc, sh2ox, soilmx, runoff1x, runoff2x, &
!KWM          runoff3x, etax, etakin, ecx, edirx, ettx, albedx, weasd, acrain, acsnom, &
!KWM          esnow2d, drip2d, dewfall, snodep, fpar, hx, qfx)


#if defined (_DO_OUTPUT_)
     call output_hrldas(ounit, split_output_count, hgrid, range, ix, jx, &
          nsoil, sldpth, startdate, olddate, &
          vegtyp, soltyp, t1, cmc, stc, smc, sh2ox, soilmx, runoff1x, runoff2x, &
          runoff3x, etax, etakin, ecx, edirx, ettx, albedx, weasd, acrain, acsnom, &
          esnow2d, drip2d, dewfall, snodep, fpar, hx, qfx, short)
#endif


!------------------------------------------------------------------------
! End of Time Loop  (do K) 
!------------------------------------------------------------------------

#if defined (_DO_SYSTEM_CALLS_) 
     call system ('rm -f '//trim(indir)//"/*LDASIN*")
#endif

  ENDDO KLOOP

END program Noah_driver_hrldas

