program Noah_hrldas_driver

  USE module_hrldas_netcdf_io
  USE module_Noahlsm_utility
  USE module_sf_noahmp_groundwater
  USE module_sf_noahmpdrv, only: noahmp_init, noahmplsm,SOIL_VEG_GEN_PARM
  USE module_sf_noahmplsm, only: READ_MP_VEG_PARAMETERS
  USE module_date_utilities
!  USE veg_parameters, ONLY : READ_MP_VEG_PARAMETERS, ISURBAN, ISWATER

  IMPLICIT NONE

  character(len=9), parameter :: version = "v20130401"
  integer :: LDASIN_VERSION

!------------------------------------------------------------------------
! Begin exact copy of declaration section from driver (substitute allocatable, remove intent)
!------------------------------------------------------------------------

! IN only (as defined in WRF)

  INTEGER                                 ::  ITIMESTEP ! timestep number
  INTEGER                                 ::  YR        ! 4-digit year
  REAL                                    ::  JULIAN_IN ! Julian day
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  COSZEN    ! cosine zenith angle
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  XLAT_URB2D! latitude [rad]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  DZ8W      ! thickness of atmo layers [m]
  REAL                                    ::  DTBL      ! timestep [s]
  REAL,    ALLOCATABLE, DIMENSION(:)      ::  DZS       ! thickness of soil layers [m]
  INTEGER                                 ::  NSOIL     ! number of soil layers
  INTEGER                                 ::  NUM_SOIL_LAYERS     ! number of soil layers
  REAL                                    ::  DX        ! horizontal grid spacing [m]
  INTEGER, ALLOCATABLE, DIMENSION(:,:)    ::  IVGTYP    ! vegetation type
  INTEGER, ALLOCATABLE, DIMENSION(:,:)    ::  ISLTYP    ! soil type
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  VEGFRA    ! vegetation fraction []
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SHDMAX    ! annual max vegetation fraction []
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TMN       ! deep soil temperature [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  XLAND     ! =2 ocean; =1 land/seaice
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  XICE      ! fraction of grid that is seaice
  REAL                                    ::  XICE_THRESHOLD! fraction of grid determining seaice
  INTEGER                                 ::  ISICE     ! land cover category for ice
  INTEGER                                 ::  ISURBAN   ! land cover category for urban
  INTEGER                                 ::  ISWATER   ! land cover category for water
  INTEGER                                 ::  IDVEG     ! dynamic vegetation (1 -> off ; 2 -> on) with opt_crs = 1   
  INTEGER                                 ::  IOPT_CRS  ! canopy stomatal resistance (1-> Ball-Berry; 2->Jarvis)
  INTEGER                                 ::  IOPT_BTR  ! soil moisture factor for stomatal resistance (1-> Noah; 2-> CLM; 3-> SSiB)
  INTEGER                                 ::  IOPT_RUN  ! runoff and groundwater (1->SIMGM; 2->SIMTOP; 3->Schaake96; 4->BATS)
  INTEGER                                 ::  IOPT_SFC  ! surface layer drag coeff (CH & CM) (1->M-O; 2->Chen97)
  INTEGER                                 ::  IOPT_FRZ  ! supercooled liquid water (1-> NY06; 2->Koren99)
  INTEGER                                 ::  IOPT_INF  ! frozen soil permeability (1-> NY06; 2->Koren99)
  INTEGER                                 ::  IOPT_RAD  ! radiation transfer (1->gap=F(3D,cosz); 2->gap=0; 3->gap=1-Fveg)
  INTEGER                                 ::  IOPT_ALB  ! snow surface albedo (1->BATS; 2->CLASS)
  INTEGER                                 ::  IOPT_SNF  ! rainfall & snowfall (1-Jordan91; 2->BATS; 3->Noah)
  INTEGER                                 ::  IOPT_TBOT ! lower boundary of soil temperature (1->zero-flux; 2->Noah)
  INTEGER                                 ::  IOPT_STC  ! snow/soil temperature time scheme
  INTEGER                                 ::  IZ0TLND   ! option of Chen adjustment of Czil (not used)
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  T_PHY     ! 3D atmospheric temperature valid at mid-levels [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  QV_CURR   ! 3D water vapor mixing ratio [kg/kg_dry]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  U_PHY     ! 3D U wind component [m/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  V_PHY     ! 3D V wind component [m/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SWDOWN    ! solar down at surface [W m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GLW       ! longwave down at surface [W m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  P8W       ! 3D pressure, valid at interface [Pa]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RAINBL    ! precipitation entering land model [mm]

! INOUT (with generic LSM equivalent) (as defined in WRF)

  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TSK       ! surface radiative temperature [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  HFX       ! sensible heat flux [W m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QFX       ! latent heat flux [kg s-1 m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  LH        ! latent heat flux [W m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GRDFLX    ! ground/snow heat flux [W m-2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SMSTAV    ! soil moisture avail. [not used]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SMSTOT    ! total soil water [mm][not used]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SFCRUNOFF ! accumulated surface runoff [m]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  UDRUNOFF  ! accumulated sub-surface runoff [m]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ALBEDO    ! total grid albedo []
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOWC     ! snow cover fraction []
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SMOISEQ   ! volumetric soil moisture [m3/m3]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SMOIS     ! volumetric soil moisture [m3/m3]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SH2O      ! volumetric liquid soil moisture [m3/m3]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  TSLB      ! soil temperature [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOW      ! snow water equivalent [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNOWH     ! physical snow depth [m]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CANWAT    ! total canopy water + ice [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACSNOM    ! accumulated snow melt leaving pack
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ACSNOW    ! accumulated snow on grid
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EMISS     ! surface bulk emissivity
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSFC      ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent) (as defined in WRF)

  INTEGER, ALLOCATABLE, DIMENSION(:,:)    ::  ISNOWXY   ! actual no. of snow layers
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TVXY      ! vegetation leaf temperature
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TGXY      ! bulk ground surface temperature
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CANICEXY  ! canopy-intercepted ice (mm)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CANLIQXY  ! canopy-intercepted liquid water (mm)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EAHXY     ! canopy air vapor pressure (pa)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TAHXY     ! canopy air temperature (k)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CMXY      ! bulk momentum drag coefficient
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHXY      ! bulk sensible heat exchange coefficient
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FWETXY    ! wetted or snowed fraction of the canopy (-)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SNEQVOXY  ! snow mass at last time step(mm h2o)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ALBOLDXY  ! snow albedo at last time step (-)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSNOWXY   ! snowfall on the ground [mm/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  WSLAKEXY  ! lake water storage [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ZWTXY     ! water table depth [m]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  WAXY      ! water in the "aquifer" [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  WTXY      ! groundwater storage [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SMCWTDXY  ! groundwater storage [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  DEEPRECHXY! groundwater storage [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RECHXY    ! groundwater storage [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  TSNOXY    ! snow temperature [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  ZSNSOXY   ! snow layer depth [m]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNICEXY   ! snow layer ice [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:,:)  ::  SNLIQXY   ! snow layer liquid water [mm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  LFMASSXY  ! leaf mass [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RTMASSXY  ! mass of fine roots [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  STMASSXY  ! stem mass [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  WOODXY    ! mass of wood (incl. woody roots) [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  STBLCPXY  ! stable carbon in deep soil [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FASTCPXY  ! short-lived carbon, shallow soil [g/m2]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  LAI       ! leaf area index
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  XSAIXY    ! stem area index
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TAUSSXY   ! snow age factor

! OUT (with no Noah LSM equivalent) (as defined in WRF)
   
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  T2MVXY    ! 2m temperature of vegetation part
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  T2MBXY    ! 2m temperature of bare ground part
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  Q2MVXY    ! 2m mixing ratio of vegetation part
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  Q2MBXY    ! 2m mixing ratio of bare ground part
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TRADXY    ! surface radiative temperature (k)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  NEEXY     ! net ecosys exchange (g/m2/s CO2)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GPPXY     ! gross primary assimilation [g/m2/s C]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  NPPXY     ! net primary productivity [g/m2/s C]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FVEGXY    ! Noah-MP vegetation fraction [-]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RUNSFXY   ! surface runoff [mm/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RUNSBXY   ! subsurface runoff [mm/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ECANXY    ! evaporation of intercepted water (mm/s)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EDIRXY    ! soil surface evaporation rate (mm/s]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  ETRANXY   ! transpiration rate (mm/s)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FSAXY     ! total absorbed solar radiation (w/m2)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FIRAXY    ! total net longwave rad (w/m2) [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  APARXY    ! photosyn active energy by canopy (w/m2)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  PSNXY     ! total photosynthesis (umol co2/m2/s) [+]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SAVXY     ! solar rad absorbed by veg. (w/m2)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SAGXY     ! solar rad absorbed by ground (w/m2)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RSSUNXY   ! sunlit leaf stomatal resistance (s/m)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RSSHAXY   ! shaded leaf stomatal resistance (s/m)
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  BGAPXY    ! between gap fraction
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  WGAPXY    ! within gap fraction
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TGVXY     ! under canopy ground temperature[K]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TGBXY     ! bare ground temperature [K]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHVXY     ! sensible heat exchange coefficient vegetated
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHBXY     ! sensible heat exchange coefficient bare-ground
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SHGXY     ! veg ground sen. heat [w/m2]   [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SHCXY     ! canopy sen. heat [w/m2]   [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SHBXY     ! bare sensible heat [w/m2]  [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EVGXY     ! veg ground evap. heat [w/m2]  [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EVBXY     ! bare soil evaporation [w/m2]  [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GHVXY     ! veg ground heat flux [w/m2]  [+ to soil]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GHBXY     ! bare ground heat flux [w/m2] [+ to soil]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  IRGXY     ! veg ground net LW rad. [w/m2] [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  IRCXY     ! canopy net LW rad. [w/m2] [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  IRBXY     ! bare net longwave rad. [w/m2] [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TRXY      ! transpiration [w/m2]  [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EVCXY     ! canopy evaporation heat [w/m2]  [+ to atm]
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHLEAFXY  ! leaf exchange coefficient 
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHUCXY    ! under canopy exchange coefficient 
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHV2XY    ! veg 2m exchange coefficient 
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHB2XY    ! bare 2m exchange coefficient 
  INTEGER   ::  ids,ide, jds,jde, kds,kde,  &  ! d -> domain
   &         ims,ime, jms,jme, kms,kme,  &  ! m -> memory
   &         its,ite, jts,jte, kts,kte   ! t -> tile


!    REAL, ALLOCATABLE, DIMENSION(:)     :: SLDPTH       ! depth to soil interfaces [m]

!------------------------------------------------------------------------
! Needed for NoahMP init
!------------------------------------------------------------------------

  LOGICAL                                 ::  FNDSOILW    ! soil water present in input
  LOGICAL                                 ::  FNDSNOWH    ! snow depth present in input
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  CHSTARXY    ! for consistency with MP_init; delete later
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  SEAICE      ! seaice fraction

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  MSFTX
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  MSFTY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  EQZWT
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RIVERBEDXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  RIVERCONDXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  PEXPXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  FDEPTHXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  AREAXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QRFSXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSPRINGSXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QRFXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSPRINGXY
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  QSLATXY
  REAL                                    ::  WTDDT  = 30.0    ! frequency of groundwater call [minutes]
  INTEGER                                 ::  STEPWTD          ! step of groundwater call

!------------------------------------------------------------------------
! 2D variables not used in WRF - should be removed?
!------------------------------------------------------------------------

  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  XLONIN      ! longitude
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  TERRAIN     ! terrain height
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GVFMIN      ! annual minimum in vegetation fraction
  REAL,    ALLOCATABLE, DIMENSION(:,:)    ::  GVFMAX      ! annual maximum in vegetation fraction

!------------------------------------------------------------------------
! End 2D variables not used in WRF
!------------------------------------------------------------------------

  CHARACTER(LEN=256) :: MMINSL  = 'STAS'  ! soil classification
  CHARACTER(LEN=256) :: LLANDUSE          ! (=USGS, using USGS landuse classification)

!------------------------------------------------------------------------
! Timing:
!------------------------------------------------------------------------

  INTEGER :: ITIME          ! timestep loop
  INTEGER :: NTIME          ! timesteps
  integer :: clock_count_1 = 0
  integer :: clock_count_2 = 0
  integer :: clock_rate    = 0
  real    :: timing_sum    = 0.0

  integer :: sflx_count_sum
  integer :: count_before_sflx
  integer :: count_after_sflx

!---------------------------------------------------------------------
!  DECLARE/Initialize constants
!---------------------------------------------------------------------

    INTEGER                             :: I
    INTEGER                             :: J
    INTEGER                             :: SLOPETYP
    INTEGER                             :: YEARLEN

    INTEGER, PARAMETER                  :: NSNOW = 3    ! number of snow layers fixed to 3
    REAL, PARAMETER                     :: undefined_value = -1.E36

!---------------------------------------------------------------------
!  File naming, parallel
!---------------------------------------------------------------------

  character(len=19)  :: olddate, newdate, startdate
  character          :: hgrid
  integer            :: igrid
  logical            :: lexist
  integer            :: imode
  integer            :: ixfull
  integer            :: jxfull
  integer            :: ixpar
  integer            :: jxpar
  integer            :: xstartpar
  integer            :: ystartpar
  integer            :: rank = 0
  CHARACTER(len=256) :: inflnm, outflnm, inflnm_template
  logical            :: restart_flag
  character(len=256) :: restart_flnm
  integer            :: ierr
  integer            :: ij
  integer, DIMENSION(2)  :: i_start, j_start, i_end, j_end

!---------------------------------------------------------------------
! Attributes from LDASIN input file (or HRLDAS_CONSTANTS_FILE, as the case may be)
!---------------------------------------------------------------------

  INTEGER           :: IX
  INTEGER           :: JX
  REAL              :: DY
  REAL              :: TRUELAT1
  REAL              :: TRUELAT2
  REAL              :: CEN_LON
  INTEGER           :: MAPPROJ
  REAL              :: LAT1
  REAL              :: LON1

!---------------------------------------------------------------------
!  NAMELIST start
!---------------------------------------------------------------------

  character(len=256) :: indir
  ! nsoil defined above
  integer            :: forcing_timestep
  integer            :: noah_timestep
  integer            :: start_year
  integer            :: start_month
  integer            :: start_day
  integer            :: start_hour
  integer            :: start_min
  character(len=256) :: outdir = "."
  character(len=256) :: restart_filename_requested = " "
  integer            :: restart_frequency_hours
  integer            :: output_timestep

  integer            :: dynamic_veg_option
  integer            :: canopy_stomatal_resistance_option
  integer            :: btr_option
  integer            :: runoff_option
  integer            :: surface_drag_option
  integer            :: supercooled_water_option
  integer            :: frozen_soil_option
  integer            :: radiative_transfer_option
  integer            :: snow_albedo_option
  integer            :: pcp_partition_option
  integer            :: tbot_option
  integer            :: temp_time_scheme_option

  integer            :: split_output_count = 1
  integer            :: khour
  integer            :: kday
  real               :: zlvl 
  character(len=256) :: hrldas_constants_file = " "
  character(len=256) :: mmf_runoff_file = " "
  character(len=256) :: external_fpar_filename_template = " "
  character(len=256) :: external_lai_filename_template = " "
  integer            :: xstart = 1
  integer            :: ystart = 1
  integer            ::   xend = 0
  integer            ::   yend = 0
  REAL, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist
  REAL, DIMENSION(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]

  namelist / NOAHLSM_OFFLINE / indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep, &
       start_year, start_month, start_day, start_hour, start_min, &
       outdir, &
       restart_filename_requested, restart_frequency_hours, output_timestep, &

       dynamic_veg_option, canopy_stomatal_resistance_option, &
       btr_option, runoff_option, surface_drag_option, supercooled_water_option, &
       frozen_soil_option, radiative_transfer_option, snow_albedo_option, &
       pcp_partition_option, tbot_option, temp_time_scheme_option, &

       split_output_count, & 
       khour, kday, zlvl, hrldas_constants_file, mmf_runoff_file, &
       external_fpar_filename_template, external_lai_filename_template, &
       xstart, xend, ystart, yend

  ! Initialize namelist variables to dummy values, so we can tell
  ! if they have not been set properly.

  nsoil                   = undefined_value
  soil_thick_input        = undefined_value
  dtbl                    = undefined_value
  start_year              = undefined_value
  start_month             = undefined_value
  start_day               = undefined_value
  start_hour              = undefined_value
  start_min               = undefined_value
  khour                   = undefined_value
  kday                    = undefined_value
  zlvl                    = undefined_value
  forcing_timestep        = undefined_value
  noah_timestep           = undefined_value
  output_timestep         = undefined_value
  restart_frequency_hours = undefined_value

  open(30, file="namelist.hrldas", form="FORMATTED")
  read(30, NOAHLSM_OFFLINE, iostat=ierr)
  if (ierr /= 0) then
     write(*,'(/," ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE",/)')
     rewind(30)
     read(30, NOAHLSM_OFFLINE)
     stop " ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE"
  endif
  close(30)

  dtbl = real(noah_timestep)
  num_soil_layers = nsoil      ! because surface driver uses the long form
  IDVEG = dynamic_veg_option ! transfer from namelist to driver format
  IOPT_CRS = canopy_stomatal_resistance_option
  IOPT_BTR = btr_option
  IOPT_RUN = runoff_option
  IOPT_SFC = surface_drag_option
  IOPT_FRZ = supercooled_water_option
  IOPT_INF = frozen_soil_option
  IOPT_RAD = radiative_transfer_option
  IOPT_ALB = snow_albedo_option
  IOPT_SNF = pcp_partition_option
  IOPT_TBOT = tbot_option
  IOPT_STC = temp_time_scheme_option

!---------------------------------------------------------------------
!  NAMELIST end
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  NAMELIST check begin
!---------------------------------------------------------------------

  if (nsoil < 0) then
     stop " ***** ERROR: NSOIL must be set in the namelist."
  endif

  if ((khour < 0) .and. (kday < 0)) then
     write(*, '(" ***** Namelist error: ************************************")')
     write(*, '(" ***** ")')
     write(*, '(" *****      Either KHOUR or KDAY must be defined.")')
     write(*, '(" ***** ")')
     stop
  else if (( khour < 0 ) .and. (kday > 0)) then
     khour = kday * 24
  else if ((khour > 0) .and. (kday > 0)) then
     write(*, '("Namelist warning:  KHOUR and KDAY both defined.")')
  else
     ! all is well.  KHOUR defined
  endif

  if (forcing_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       FORCING_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
  endif

  if (noah_timestep < 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *****************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       NOAH_TIMESTEP needs to be set greater than zero.")')
        write(*, '(" *****                     900 seconds is recommended.       ")')
        write(*, '(" ***** ")')
        write(*, *)
        stop
  endif

  !
  ! Check that OUTPUT_TIMESTEP fits into NOAH_TIMESTEP:
  !
  if (output_timestep /= 0) then
     if (mod(output_timestep, noah_timestep) > 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: *********************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       OUTPUT_TIMESTEP should set to an integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            OUTPUT_TIMESTEP = ", I12, " seconds")') output_timestep
        write(*, '(" *****            NOAH_TIMESTEP   = ", I12, " seconds")') noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     endif
  endif

  !
  ! Check that RESTART_FREQUENCY_HOURS fits into NOAH_TIMESTEP:
  !
  if (restart_frequency_hours /= 0) then
     if (mod(restart_frequency_hours*3600, noah_timestep) > 0) then
        write(*, *)
        write(*, '(" ***** Namelist error: ******************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       RESTART_FREQUENCY_HOURS (converted to seconds) should set to an ")')
        write(*, '(" *****       integer multiple of NOAH_TIMESTEP.")')
        write(*, '(" *****            RESTART_FREQUENCY_HOURS = ", I12, " hours:  ", I12, " seconds")') &
             restart_frequency_hours, restart_frequency_hours*3600
        write(*, '(" *****            NOAH_TIMESTEP           = ", I12, " seconds")') noah_timestep
        write(*, '(" ***** ")')
        write(*, *)
        stop
     endif
  endif

  if (dynamic_veg_option == 2) then
     if ( canopy_stomatal_resistance_option /= 1) then
        write(*, *)
        write(*, '(" ***** Namelist error: ******************************************************")')
        write(*, '(" ***** ")')
        write(*, '(" *****       CANOPY_STOMATAL_RESISTANCE_OPTION must be 1 when DYNAMIC_VEG_OPTION == 2")')
        write(*, *)
        stop
     endif
  endif

!---------------------------------------------------------------------
!  NAMELIST check end
!---------------------------------------------------------------------

!----------------------------------------------------------------------
! Initialize gridded domain
!----------------------------------------------------------------------

  call read_hrldas_hdrinfo(hrldas_constants_file, ix, jx, &
       xstart, xend, ystart, yend, &
       iswater, isurban, isice, llanduse, dx, dy, truelat1, truelat2, cen_lon, lat1, lon1, &
       igrid, mapproj)
  write(hgrid,'(I1)') igrid

  write(olddate,'(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
       start_year, start_month, start_day, start_hour, start_min, 0

  startdate = olddate
  ids = xstart
  ide = xend
  jds = ystart
  jde = yend
  kds = 1
  kde = 2
  its = xstart
  ite = xend
  jts = ystart
  jte = yend
  kts = 1
  kte = 2
  ims = xstart
  ime = xend
  jms = ystart
  jme = yend
  kms = 1
  kme = 2
  
!---------------------------------------------------------------------
!  Allocate multi-dimension fields for subwindow calculation
!---------------------------------------------------------------------

  ixfull = xend-xstart+1
  jxfull = yend-ystart+1

  ixpar = ixfull
  jxpar = jxfull
  xstartpar = 1
  ystartpar = 1

  ALLOCATE ( COSZEN    (XSTART:XEND,YSTART:YEND) )    ! cosine zenith angle
  ALLOCATE ( XLAT_URB2D(XSTART:XEND,YSTART:YEND) )    ! latitude [rad]
  ALLOCATE ( DZ8W      (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! thickness of atmo layers [m]
  ALLOCATE ( DZS       (1:NSOIL)                   )  ! thickness of soil layers [m]
  ALLOCATE ( IVGTYP    (XSTART:XEND,YSTART:YEND) )    ! vegetation type
  ALLOCATE ( ISLTYP    (XSTART:XEND,YSTART:YEND) )    ! soil type
  ALLOCATE ( VEGFRA    (XSTART:XEND,YSTART:YEND) )    ! vegetation fraction []
  ALLOCATE ( SHDMAX    (XSTART:XEND,YSTART:YEND) )    ! annual max vegetation fraction []
  ALLOCATE ( TMN       (XSTART:XEND,YSTART:YEND) )    ! deep soil temperature [K]
  ALLOCATE ( XLAND     (XSTART:XEND,YSTART:YEND) )    ! =2 ocean; =1 land/seaice
  ALLOCATE ( XICE      (XSTART:XEND,YSTART:YEND) )    ! fraction of grid that is seaice
  ALLOCATE ( T_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D atmospheric temperature valid at mid-levels [K]
  ALLOCATE ( QV_CURR   (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D water vapor mixing ratio [kg/kg_dry]
  ALLOCATE ( U_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D U wind component [m/s]
  ALLOCATE ( V_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D V wind component [m/s]
  ALLOCATE ( SWDOWN    (XSTART:XEND,YSTART:YEND) )    ! solar down at surface [W m-2]
  ALLOCATE ( GLW       (XSTART:XEND,YSTART:YEND) )    ! longwave down at surface [W m-2]
  ALLOCATE ( P8W       (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D pressure, valid at interface [Pa]
  ALLOCATE ( RAINBL    (XSTART:XEND,YSTART:YEND) )    ! precipitation entering land model [mm]

! INOUT (with generic LSM equivalent) (as defined in WRF)

  ALLOCATE ( TSK       (XSTART:XEND,YSTART:YEND) )  ! surface radiative temperature [K]
  ALLOCATE ( HFX       (XSTART:XEND,YSTART:YEND) )  ! sensible heat flux [W m-2]
  ALLOCATE ( QFX       (XSTART:XEND,YSTART:YEND) )  ! latent heat flux [kg s-1 m-2]
  ALLOCATE ( LH        (XSTART:XEND,YSTART:YEND) )  ! latent heat flux [W m-2]
  ALLOCATE ( GRDFLX    (XSTART:XEND,YSTART:YEND) )  ! ground/snow heat flux [W m-2]
  ALLOCATE ( SMSTAV    (XSTART:XEND,YSTART:YEND) )  ! soil moisture avail. [not used]
  ALLOCATE ( SMSTOT    (XSTART:XEND,YSTART:YEND) )  ! total soil water [mm][not used]
  ALLOCATE ( SFCRUNOFF (XSTART:XEND,YSTART:YEND) )  ! accumulated surface runoff [m]
  ALLOCATE ( UDRUNOFF  (XSTART:XEND,YSTART:YEND) )  ! accumulated sub-surface runoff [m]
  ALLOCATE ( ALBEDO    (XSTART:XEND,YSTART:YEND) )  ! total grid albedo []
  ALLOCATE ( SNOWC     (XSTART:XEND,YSTART:YEND) )  ! snow cover fraction []
  ALLOCATE ( SMOISEQ   (XSTART:XEND,1:NSOIL,YSTART:YEND) )     ! eq volumetric soil moisture [m3/m3]
  ALLOCATE ( SMOIS     (XSTART:XEND,1:NSOIL,YSTART:YEND) )     ! volumetric soil moisture [m3/m3]
  ALLOCATE ( SH2O      (XSTART:XEND,1:NSOIL,YSTART:YEND) )     ! volumetric liquid soil moisture [m3/m3]
  ALLOCATE ( TSLB      (XSTART:XEND,1:NSOIL,YSTART:YEND) )     ! soil temperature [K]
  ALLOCATE ( SNOW      (XSTART:XEND,YSTART:YEND) )  ! snow water equivalent [mm]
  ALLOCATE ( SNOWH     (XSTART:XEND,YSTART:YEND) )  ! physical snow depth [m]
  ALLOCATE ( CANWAT    (XSTART:XEND,YSTART:YEND) )  ! total canopy water + ice [mm]
  ALLOCATE ( ACSNOM    (XSTART:XEND,YSTART:YEND) )  ! accumulated snow melt leaving pack
  ALLOCATE ( ACSNOW    (XSTART:XEND,YSTART:YEND) )  ! accumulated snow on grid
  ALLOCATE ( EMISS     (XSTART:XEND,YSTART:YEND) )  ! surface bulk emissivity
  ALLOCATE ( QSFC      (XSTART:XEND,YSTART:YEND) )  ! bulk surface specific humidity

! INOUT (with no Noah LSM equivalent) (as defined in WRF)

  ALLOCATE ( ISNOWXY   (XSTART:XEND,YSTART:YEND) )  ! actual no. of snow layers
  ALLOCATE ( TVXY      (XSTART:XEND,YSTART:YEND) )  ! vegetation leaf temperature
  ALLOCATE ( TGXY      (XSTART:XEND,YSTART:YEND) )  ! bulk ground surface temperature
  ALLOCATE ( CANICEXY  (XSTART:XEND,YSTART:YEND) )  ! canopy-intercepted ice (mm)
  ALLOCATE ( CANLIQXY  (XSTART:XEND,YSTART:YEND) )  ! canopy-intercepted liquid water (mm)
  ALLOCATE ( EAHXY     (XSTART:XEND,YSTART:YEND) )  ! canopy air vapor pressure (pa)
  ALLOCATE ( TAHXY     (XSTART:XEND,YSTART:YEND) )  ! canopy air temperature (k)
  ALLOCATE ( CMXY      (XSTART:XEND,YSTART:YEND) )  ! bulk momentum drag coefficient
  ALLOCATE ( CHXY      (XSTART:XEND,YSTART:YEND) )  ! bulk sensible heat exchange coefficient
  ALLOCATE ( FWETXY    (XSTART:XEND,YSTART:YEND) )  ! wetted or snowed fraction of the canopy (-)
  ALLOCATE ( SNEQVOXY  (XSTART:XEND,YSTART:YEND) )  ! snow mass at last time step(mm h2o)
  ALLOCATE ( ALBOLDXY  (XSTART:XEND,YSTART:YEND) )  ! snow albedo at last time step (-)
  ALLOCATE ( QSNOWXY   (XSTART:XEND,YSTART:YEND) )  ! snowfall on the ground [mm/s]
  ALLOCATE ( WSLAKEXY  (XSTART:XEND,YSTART:YEND) )  ! lake water storage [mm]
  ALLOCATE ( ZWTXY     (XSTART:XEND,YSTART:YEND) )  ! water table depth [m]
  ALLOCATE ( WAXY      (XSTART:XEND,YSTART:YEND) )  ! water in the "aquifer" [mm]
  ALLOCATE ( WTXY      (XSTART:XEND,YSTART:YEND) )  ! groundwater storage [mm]
  ALLOCATE ( SMCWTDXY  (XSTART:XEND,YSTART:YEND) )  ! soil moisture below the bottom of the column (m3m-3)
  ALLOCATE ( DEEPRECHXY(XSTART:XEND,YSTART:YEND) )  ! recharge to the water table when deep (m)
  ALLOCATE ( RECHXY    (XSTART:XEND,YSTART:YEND) )  ! recharge to the water table (diagnostic) (m)
  ALLOCATE ( TSNOXY    (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow temperature [K]
  ALLOCATE ( ZSNSOXY   (XSTART:XEND,-NSNOW+1:NSOIL,YSTART:YEND) )  ! snow layer depth [m]
  ALLOCATE ( SNICEXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer ice [mm]
  ALLOCATE ( SNLIQXY   (XSTART:XEND,-NSNOW+1:0,    YSTART:YEND) )  ! snow layer liquid water [mm]
  ALLOCATE ( LFMASSXY  (XSTART:XEND,YSTART:YEND) )  ! leaf mass [g/m2]
  ALLOCATE ( RTMASSXY  (XSTART:XEND,YSTART:YEND) )  ! mass of fine roots [g/m2]
  ALLOCATE ( STMASSXY  (XSTART:XEND,YSTART:YEND) )  ! stem mass [g/m2]
  ALLOCATE ( WOODXY    (XSTART:XEND,YSTART:YEND) )  ! mass of wood (incl. woody roots) [g/m2]
  ALLOCATE ( STBLCPXY  (XSTART:XEND,YSTART:YEND) )  ! stable carbon in deep soil [g/m2]
  ALLOCATE ( FASTCPXY  (XSTART:XEND,YSTART:YEND) )  ! short-lived carbon, shallow soil [g/m2]
  ALLOCATE ( LAI       (XSTART:XEND,YSTART:YEND) )  ! leaf area index
  ALLOCATE ( XSAIXY    (XSTART:XEND,YSTART:YEND) )  ! stem area index
  ALLOCATE ( TAUSSXY   (XSTART:XEND,YSTART:YEND) )  ! snow age factor

! OUT (with no Noah LSM equivalent) (as defined in WRF)
   
  ALLOCATE ( T2MVXY    (XSTART:XEND,YSTART:YEND) )  ! 2m temperature of vegetation part
  ALLOCATE ( T2MBXY    (XSTART:XEND,YSTART:YEND) )  ! 2m temperature of bare ground part
  ALLOCATE ( Q2MVXY    (XSTART:XEND,YSTART:YEND) )  ! 2m mixing ratio of vegetation part
  ALLOCATE ( Q2MBXY    (XSTART:XEND,YSTART:YEND) )  ! 2m mixing ratio of bare ground part
  ALLOCATE ( TRADXY    (XSTART:XEND,YSTART:YEND) )  ! surface radiative temperature (k)
  ALLOCATE ( NEEXY     (XSTART:XEND,YSTART:YEND) )  ! net ecosys exchange (g/m2/s CO2)
  ALLOCATE ( GPPXY     (XSTART:XEND,YSTART:YEND) )  ! gross primary assimilation [g/m2/s C]
  ALLOCATE ( NPPXY     (XSTART:XEND,YSTART:YEND) )  ! net primary productivity [g/m2/s C]
  ALLOCATE ( FVEGXY    (XSTART:XEND,YSTART:YEND) )  ! Noah-MP vegetation fraction [-]
  ALLOCATE ( RUNSFXY   (XSTART:XEND,YSTART:YEND) )  ! surface runoff [mm/s]
  ALLOCATE ( RUNSBXY   (XSTART:XEND,YSTART:YEND) )  ! subsurface runoff [mm/s]
  ALLOCATE ( ECANXY    (XSTART:XEND,YSTART:YEND) )  ! evaporation of intercepted water (mm/s)
  ALLOCATE ( EDIRXY    (XSTART:XEND,YSTART:YEND) )  ! soil surface evaporation rate (mm/s]
  ALLOCATE ( ETRANXY   (XSTART:XEND,YSTART:YEND) )  ! transpiration rate (mm/s)
  ALLOCATE ( FSAXY     (XSTART:XEND,YSTART:YEND) )  ! total absorbed solar radiation (w/m2)
  ALLOCATE ( FIRAXY    (XSTART:XEND,YSTART:YEND) )  ! total net longwave rad (w/m2) [+ to atm]
  ALLOCATE ( APARXY    (XSTART:XEND,YSTART:YEND) )  ! photosyn active energy by canopy (w/m2)
  ALLOCATE ( PSNXY     (XSTART:XEND,YSTART:YEND) )  ! total photosynthesis (umol co2/m2/s) [+]
  ALLOCATE ( SAVXY     (XSTART:XEND,YSTART:YEND) )  ! solar rad absorbed by veg. (w/m2)
  ALLOCATE ( SAGXY     (XSTART:XEND,YSTART:YEND) )  ! solar rad absorbed by ground (w/m2)
  ALLOCATE ( RSSUNXY   (XSTART:XEND,YSTART:YEND) )  ! sunlit leaf stomatal resistance (s/m)
  ALLOCATE ( RSSHAXY   (XSTART:XEND,YSTART:YEND) )  ! shaded leaf stomatal resistance (s/m)
  ALLOCATE ( BGAPXY    (XSTART:XEND,YSTART:YEND) )  ! between gap fraction
  ALLOCATE ( WGAPXY    (XSTART:XEND,YSTART:YEND) )  ! within gap fraction
  ALLOCATE ( TGVXY     (XSTART:XEND,YSTART:YEND) )  ! under canopy ground temperature[K]
  ALLOCATE ( TGBXY     (XSTART:XEND,YSTART:YEND) )  ! bare ground temperature [K]
  ALLOCATE ( CHVXY     (XSTART:XEND,YSTART:YEND) )  ! sensible heat exchange coefficient vegetated
  ALLOCATE ( CHBXY     (XSTART:XEND,YSTART:YEND) )  ! sensible heat exchange coefficient bare-ground
  ALLOCATE ( SHGXY     (XSTART:XEND,YSTART:YEND) )  ! veg ground sen. heat [w/m2]   [+ to atm]
  ALLOCATE ( SHCXY     (XSTART:XEND,YSTART:YEND) )  ! canopy sen. heat [w/m2]   [+ to atm]
  ALLOCATE ( SHBXY     (XSTART:XEND,YSTART:YEND) )  ! bare sensible heat [w/m2]  [+ to atm]
  ALLOCATE ( EVGXY     (XSTART:XEND,YSTART:YEND) )  ! veg ground evap. heat [w/m2]  [+ to atm]
  ALLOCATE ( EVBXY     (XSTART:XEND,YSTART:YEND) )  ! bare soil evaporation [w/m2]  [+ to atm]
  ALLOCATE ( GHVXY     (XSTART:XEND,YSTART:YEND) )  ! veg ground heat flux [w/m2]  [+ to soil]
  ALLOCATE ( GHBXY     (XSTART:XEND,YSTART:YEND) )  ! bare ground heat flux [w/m2] [+ to soil]
  ALLOCATE ( IRGXY     (XSTART:XEND,YSTART:YEND) )  ! veg ground net LW rad. [w/m2] [+ to atm]
  ALLOCATE ( IRCXY     (XSTART:XEND,YSTART:YEND) )  ! canopy net LW rad. [w/m2] [+ to atm]
  ALLOCATE ( IRBXY     (XSTART:XEND,YSTART:YEND) )  ! bare net longwave rad. [w/m2] [+ to atm]
  ALLOCATE ( TRXY      (XSTART:XEND,YSTART:YEND) )  ! transpiration [w/m2]  [+ to atm]
  ALLOCATE ( EVCXY     (XSTART:XEND,YSTART:YEND) )  ! canopy evaporation heat [w/m2]  [+ to atm]
  ALLOCATE ( CHLEAFXY  (XSTART:XEND,YSTART:YEND) )  ! leaf exchange coefficient 
  ALLOCATE ( CHUCXY    (XSTART:XEND,YSTART:YEND) )  ! under canopy exchange coefficient 
  ALLOCATE ( CHV2XY    (XSTART:XEND,YSTART:YEND) )  ! veg 2m exchange coefficient 
  ALLOCATE ( CHB2XY    (XSTART:XEND,YSTART:YEND) )  ! bare 2m exchange coefficient 

  ALLOCATE ( XLONIN    (XSTART:XEND,YSTART:YEND) )  ! longitude
  ALLOCATE ( TERRAIN   (XSTART:XEND,YSTART:YEND) )  ! terrain height
  ALLOCATE ( GVFMIN    (XSTART:XEND,YSTART:YEND) )  ! annual minimum in vegetation fraction
  ALLOCATE ( GVFMAX    (XSTART:XEND,YSTART:YEND) )  ! annual maximum in vegetation fraction

!------------------------------------------------------------------------
! Needed for MMF_RUNOFF (IOPT_RUN = 5); not part of MP driver in WRF
!------------------------------------------------------------------------

  ALLOCATE ( MSFTX       (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( MSFTY       (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( EQZWT       (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( RIVERBEDXY  (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( RIVERCONDXY (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( PEXPXY      (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( FDEPTHXY    (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( AREAXY      (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( QRFSXY      (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( QSPRINGSXY  (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( QRFXY       (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( QSPRINGXY   (XSTART:XEND,YSTART:YEND) )  ! 
  ALLOCATE ( QSLATXY     (XSTART:XEND,YSTART:YEND) )  ! 

!------------------------------------------------------------------------

  ALLOCATE ( CHSTARXY  (XSTART:XEND,YSTART:YEND) )  ! for consistency with MP_init; delete later
  ALLOCATE ( SEAICE    (XSTART:XEND,YSTART:YEND) )  ! seaice fraction

  XLAND          = 1.0   ! water = 2.0, land = 1.0
  XICE           = 0.0   ! fraction of grid that is seaice
  XICE_THRESHOLD = 0.5   ! fraction of grid determining seaice

!----------------------------------------------------------------------
! Read Landuse Type and Soil Texture and Other Information
!----------------------------------------------------------------------

  CALL READLAND_HRLDAS(HRLDAS_CONSTANTS_FILE, XSTART, XEND, YSTART, YEND,     &
       ISWATER, IVGTYP, ISLTYP, TERRAIN, TMN, XLAT_URB2D, XLONIN, XLAND, SEAICE,MSFTX,MSFTY)
  
  WHERE(SEAICE > 0.0) XICE = 1.0

!------------------------------------------------------------------------
! For IOPT_RUN = 5 (MMF groundwater), read in necessary extra fields
!------------------------------------------------------------------------

  if (runoff_option == 5) then
    CALL READ_MMF_RUNOFF(MMF_RUNOFF_FILE, XSTART, XEND, YSTART, YEND,     &
                         ZWTXY,EQZWT,RIVERBEDXY,RIVERCONDXY,PEXPXY,FDEPTHXY)
  end if
   
!----------------------------------------------------------------------
! Initialize Model State
!----------------------------------------------------------------------

  SLOPETYP = 2
  DZS       =  SOIL_THICK_INPUT(1:NSOIL)

  if (restart_filename_requested /= " ") then
     restart_flag = .TRUE.

     call find_restart_file(rank, trim(restart_filename_requested), startdate, khour, olddate, restart_flnm)

     call read_restart(trim(restart_flnm), &
          xstart, xend, xstart, ixfull, jxfull, nsoil, olddate)

     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SOIL_T"  , TSLB     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOW_T"  , TSNOXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMC"     , SMOIS    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SH2O"    , SH2O     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZSNSO"   , ZSNSOXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNICE"   , SNICEXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNLIQ"   , SNLIQXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSNOW"   , QSNOWXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FWET"    , FWETXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQVO"  , SNEQVOXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EAH"     , EAHXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAH"     , TAHXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ALBOLD"  , ALBOLDXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CM"      , CMXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CH"      , CHXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ISNOW"   , ISNOWXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANLIQ"  , CANLIQXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "CANICE"  , CANICEXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNEQV"   , SNOW     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SNOWH"   , SNOWH    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TV"      , TVXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TG"      , TGXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ZWT"     , ZWTXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WA"      , WAXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WT"      , WTXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WSLAKE"  , WSLAKEXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LFMASS"  , LFMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RTMASS"  , RTMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STMASS"  , STMASSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "WOOD"    , WOODXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "STBLCP"  , STBLCPXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FASTCP"  , FASTCPXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "LAI"     , LAI      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SAI"     , XSAIXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FPAR"    , VEGFRA   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMIN"  , GVFMIN   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "GVFMAX"  , GVFMAX   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACMELT"  , ACSNOM   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "ACSNOW"  , ACSNOW   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "TAUSS"   , TAUSSXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSFC"    , QSFC     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SFCRUNOFF",SFCRUNOFF   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "UDRUNOFF" ,UDRUNOFF    )
    ! below for opt_run = 5
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMOISEQ"   , SMOISEQ    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "AREAXY"    , AREAXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "SMCWTDXY"  , SMCWTDXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFXY"     , QRFXY      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "DEEPRECHXY", DEEPRECHXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGXY" , QSPRINGXY  )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSLATXY"   , QSLATXY    )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QRFSXY"    , QRFSXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "QSPRINGSXY", QSPRINGSXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RECHXY"    , RECHXY     )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "FDEPTHXY"   ,FDEPTHXY   )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERCONDXY",RIVERCONDXY)
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "RIVERBEDXY" ,RIVERBEDXY )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "EQZWT"      ,EQZWT      )
     call get_from_restart(xstart, xend, xstart, ixfull, jxfull, "PEXPXY"     ,PEXPXY     )

     STEPWTD = nint(WTDDT*60./DTBL)
     STEPWTD = max(STEPWTD,1)

     call read_mp_veg_parameters(trim(LLANDUSE))
     CALL  SOIL_VEG_GEN_PARM( LLANDUSE, MMINSL )

  else

     restart_flag = .FALSE.

     SMOIS     =  undefined_value
     TSLB      =  undefined_value
     SH2O      =  undefined_value
     CANLIQXY  =  undefined_value
     TSK       =  undefined_value
     RAINBL    =  undefined_value
     SNOW      =  undefined_value
     SNOWH     =  undefined_value

     inflnm = trim(indir)//"/"//&
          startdate(1:4)//startdate(6:7)//startdate(9:10)//startdate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid

     CALL READINIT_HRLDAS(inflnm, xstart, xend, ystart, yend,  &
          NSOIL, DZS, OLDDATE, LDASIN_VERSION, SMOIS,       &
          TSLB, CANWAT, TSK, SNOW, SNOWH, FNDSNOWH)
	  
     VEGFRA    =  undefined_value
     LAI       =  undefined_value
     GVFMIN    =  undefined_value
     SHDMAX    =  undefined_value

     CALL READVEG_HRLDAS(inflnm, xstart, xend, ystart, yend,  &
          OLDDATE, IVGTYP, VEGFRA, LAI, GVFMIN, SHDMAX)

     SNOW = SNOW * 1000.    ! Convert snow water equivalent to mm.

     FNDSOILW = .FALSE.
     CALL NOAHMP_INIT(    LLANDUSE,     SNOW,    SNOWH,   CANWAT,   ISLTYP,   IVGTYP,ISURBAN, &   ! call from WRF phys_init
                    TSLB,    SMOIS,     SH2O,      DZS, FNDSOILW, FNDSNOWH,    ISICE,iswater  , &
                     TSK,  ISNOWXY,     TVXY,     TGXY, CANICEXY,      TMN,     XICE, &
                CANLIQXY,    EAHXY,    TAHXY,     CMXY,     CHXY,                     &
                  FWETXY, SNEQVOXY, ALBOLDXY,  QSNOWXY, WSLAKEXY,    ZWTXY,     WAXY, &
                    WTXY,   TSNOXY,  ZSNSOXY,  SNICEXY,  SNLIQXY, LFMASSXY, RTMASSXY, &
                STMASSXY,   WOODXY, STBLCPXY, FASTCPXY,   XSAIXY,                     &
                 SMOISEQ, SMCWTDXY,   RECHXY,DEEPRECHXY,  AREAXY,DX, DY, MSFTX, MSFTY, &
                   WTDDT,  STEPWTD,     DTBL,   QRFSXY,  QSPRINGSXY, QSLATXY,          &
                FDEPTHXY,  TERRAIN,RIVERBEDXY,   EQZWT, RIVERCONDXY, PEXPXY,           &
                  T2MVXY,   T2MBXY, CHSTARXY,                                         &
                   NSOIL,  .false.,                                                   &
                  .true.,runoff_option,                                                   &
                  ids,ide+1, jds,jde+1, kds,kde,                &  ! domain
                  ims,ime, jms,jme, kms,kme,                &  ! memory
                  its,ite, jts,jte, kts,kte                 )  ! tile

      TAUSSXY = 0.0   ! Need to be added to _INIT later
  endif

!------------------------------------------------------------------------
! Begin Time Loop
!------------------------------------------------------------------------

  NTIME=(KHOUR+1)*3600./nint(dtbl)
  
  call system_clock(count=clock_count_1)   ! Start a timer

  TIMELOOP : DO ITIME=1,NTIME

!--------------------------------------------------------------------------------
! Output for restart BEFORE the processing for this particular time has begun, 
! so that upon restart, we're ready to go on this time step.
!--------------------------------------------------------------------------------

     if (restart_frequency_hours > 0) then
        if ((ITIME > 1) .and. (mod(ITIME-1, int(restart_frequency_hours*3600./nint(dtbl))) == 0)) then
           if (rank == 0) print*, 'Write restart at '//olddate(1:13)
           call prepare_restart_file(trim(outdir), version, igrid, llanduse, olddate, startdate,                         & 
                ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                   &
                nsoil, nsnow, dx, dy, truelat1, truelat2, mapproj, lat1, lon1,        &
                cen_lon, iswater, ivgtyp)

           ! Since the restart files are not really for user consumption, the units and description are 
           ! a little superfluous.  I've made them optional arguments.  If not present, they both default to "-".

              call add_to_restart(TSLB      , "SOIL_T", layers="SOIL")
              call add_to_restart(TSNOXY    , "SNOW_T", layers="SNOW")
              call add_to_restart(SMOIS     , "SMC"   , layers="SOIL")
              call add_to_restart(SH2O      , "SH2O"  , layers="SOIL")
              call add_to_restart(ZSNSOXY   , "ZSNSO" , layers="SOSN")
              call add_to_restart(SNICEXY   , "SNICE" , layers="SNOW")
              call add_to_restart(SNLIQXY   , "SNLIQ" , layers="SNOW")
              call add_to_restart(QSNOWXY   , "QSNOW" )
              call add_to_restart(FWETXY    , "FWET"  )
              call add_to_restart(SNEQVOXY  , "SNEQVO")
              call add_to_restart(EAHXY     , "EAH"   )
              call add_to_restart(TAHXY     , "TAH"   )
              call add_to_restart(ALBOLDXY  , "ALBOLD")
              call add_to_restart(CMXY      , "CM"    )
              call add_to_restart(CHXY      , "CH"    )
              call add_to_restart(ISNOWXY   , "ISNOW" )
              call add_to_restart(CANLIQXY  , "CANLIQ")
              call add_to_restart(CANICEXY  , "CANICE")
              call add_to_restart(SNOW      , "SNEQV" )
              call add_to_restart(SNOWH     , "SNOWH" )
              call add_to_restart(TVXY      , "TV"    )
              call add_to_restart(TGXY      , "TG"    )
              call add_to_restart(ZWTXY     , "ZWT"   )
              call add_to_restart(WAXY      , "WA"    )
              call add_to_restart(WTXY      , "WT"    )
              call add_to_restart(WSLAKEXY  , "WSLAKE")
              call add_to_restart(LFMASSXY  , "LFMASS")
              call add_to_restart(RTMASSXY  , "RTMASS")
              call add_to_restart(STMASSXY  , "STMASS")
              call add_to_restart(WOODXY    , "WOOD"  )
              call add_to_restart(STBLCPXY  , "STBLCP")
              call add_to_restart(FASTCPXY  , "FASTCP")
              call add_to_restart(LAI       , "LAI"   )
              call add_to_restart(XSAIXY    , "SAI"   )
              call add_to_restart(VEGFRA    , "FPAR"  )
              call add_to_restart(GVFMIN    , "GVFMIN")
              call add_to_restart(GVFMAX    , "GVFMAX")
              call add_to_restart(ACSNOM    , "ACMELT")
              call add_to_restart(ACSNOW    , "ACSNOW")
              call add_to_restart(TAUSSXY   , "TAUSS" )
              call add_to_restart(QSFC      , "QSFC"  )
              call add_to_restart(SFCRUNOFF , "SFCRUNOFF")
              call add_to_restart(UDRUNOFF  , "UDRUNOFF" )
    ! below for opt_run = 5
              call add_to_restart(SMOISEQ   , "SMOISEQ"  , layers="SOIL"  )
              call add_to_restart(AREAXY    , "AREAXY"     )
              call add_to_restart(SMCWTDXY  , "SMCWTDXY"   )
              call add_to_restart(DEEPRECHXY, "DEEPRECHXY" )
              call add_to_restart(QSLATXY   , "QSLATXY"    )
              call add_to_restart(QRFSXY    , "QRFSXY"     )
              call add_to_restart(QSPRINGSXY, "QSPRINGSXY" )
              call add_to_restart(RECHXY    , "RECHXY"     )
              call add_to_restart(QRFXY     , "QRFXY"      )
              call add_to_restart(QSPRINGXY , "QSPRINGXY"  )
              call add_to_restart(FDEPTHXY , "FDEPTHXY"  )
              call add_to_restart(RIVERCONDXY , "RIVERCONDXY"  )
              call add_to_restart(RIVERBEDXY , "RIVERBEDXY"  )
              call add_to_restart(EQZWT , "EQZWT"  )
              call add_to_restart(PEXPXY , "PEXPXY"  )
          call finalize_restart_file()
        endif
     endif

!---------------------------------------------------------------------------------
! Read the forcing data.
!---------------------------------------------------------------------------------

! For HRLDAS, we're assuming (for now) that each time period is in a 
! separate file.  So we can open a new one right now.

     inflnm = trim(indir)//"/"//&
          olddate(1:4)//olddate(6:7)//olddate(9:10)//olddate(12:13)//&
          ".LDASIN_DOMAIN"//hgrid

     ! Build a filename template
     inflnm_template = trim(indir)//"/<date>.LDASIN_DOMAIN"//hgrid

     CALL READFORC_HRLDAS(INFLNM_TEMPLATE, FORCING_TIMESTEP, OLDDATE,  &
          XSTART, XEND, YSTART, YEND,                                  &
          T_PHY(:,1,:),QV_CURR(:,1,:),U_PHY(:,1,:),V_PHY(:,1,:),          &
	    P8W(:,1,:), GLW       ,SWDOWN      ,RAINBL, LAI, VEGFRA)

     P8W(:,2,:)     = P8W(:,1,:)      ! WRF uses lowest two layers
     T_PHY(:,2,:)   = T_PHY(:,1,:)    ! Only pressure is needed in two layer but fill the rest
     U_PHY(:,2,:)   = U_PHY(:,1,:)    ! 
     V_PHY(:,2,:)   = V_PHY(:,1,:)    ! 
     QV_CURR(:,2,:) = QV_CURR(:,1,:)  ! 
     RAINBL = RAINBL * DTBL           ! RAINBL in WRF is [mm]
     DZ8W = 2*ZLVL                    ! 2* to be consistent with WRF model level
!------------------------------------------------------------------------
! Noah-MP updates we can do before spatial loop.
!------------------------------------------------------------------------

   ! create a few fields that are IN in WRF - coszen, julian_in,yr

    DO J = YSTART,YEND
    DO I = XSTART,XEND
      CALL CALC_DECLIN(OLDDATE(1:13),XLAT_URB2D(I,J), XLONIN(I,J),COSZEN(I,J),JULIAN_IN)
    END DO
    END DO

    READ(OLDDATE(1:4),*)  YR
    YEARLEN = 365                            ! find length of year for phenology (also S Hemisphere)
    if (mod(YR,4) == 0) then
       YEARLEN = 366
       if (mod(YR,100) == 0) then
          YEARLEN = 365
          if (mod(YR,400) == 0) then
             YEARLEN = 366
          endif
       endif
    endif

    IF (ITIME == 1 .AND. .NOT. RESTART_FLAG ) THEN
      ! Initial guess only.  EAH gets updated in SFLX for later time steps.
         EAHXY = (P8W(:,1,:)*QV_CURR(:,1,:))/(0.622+QV_CURR(:,1,:))

      ! TAH:     Canopy air temperature (K)
      ! Initial guess only.  TAH gets updated in SFLX for later time steps.
         TAHXY = T_PHY(:,1,:)

        CHXY = 0.1
        CMXY = 0.1

    endif

!------------------------------------------------------------------------
! OpenMP i,j blocks
!------------------------------------------------------------------------
   
   i_start(1) = XSTART
   i_start(2) = (XEND - XSTART)/2 + 1
   i_end(1) = (XEND - XSTART)/2
   i_end(2) = XEND
   j_start(1) = YSTART
   j_start(2) = YSTART
   j_end(1) = YEND
   j_end(2) = YEND

!------------------------------------------------------------------------
! Call to Noah-MP driver same as surface_driver
!------------------------------------------------------------------------
    
     sflx_count_sum = 0 ! Timing

   ! Timing information for SFLX:
    call system_clock(count=count_before_sflx, count_rate=clock_rate)

!$OMP PARALLEL DO   &
!$OMP PRIVATE ( ij )
     DO ij = 1 , 2
         CALL noahmplsm(ITIMESTEP,       YR, JULIAN_IN,   COSZEN, XLAT_URB2D, &
	           DZ8W,     DTBL,      DZS,     NUM_SOIL_LAYERS,         DX, &
		 IVGTYP,   ISLTYP,   VEGFRA,   SHDMAX,       TMN,             &
		  XLAND,     XICE,     XICE_THRESHOLD,     ISICE,    ISURBAN, &
                  IDVEG, IOPT_CRS, IOPT_BTR, IOPT_RUN,  IOPT_SFC,   IOPT_FRZ, &
	       IOPT_INF, IOPT_RAD, IOPT_ALB, IOPT_SNF, IOPT_TBOT,   IOPT_STC, &
	        IZ0TLND,                                                      &
		  T_PHY,  QV_CURR,    U_PHY,    V_PHY,    SWDOWN,        GLW, &
		    P8W,   RAINBL,                                            &
		    TSK,      HFX,      QFX,       LH,    GRDFLX,     SMSTAV, &
		 SMSTOT,SFCRUNOFF, UDRUNOFF,   ALBEDO,     SNOWC,      SMOIS, &
		   SH2O,     TSLB,     SNOW,    SNOWH,    CANWAT,     ACSNOM, &
		 ACSNOW,    EMISS,     QSFC,                                  &
		ISNOWXY,     TVXY,     TGXY, CANICEXY,  CANLIQXY,      EAHXY, &
		  TAHXY,     CMXY,     CHXY,   FWETXY,  SNEQVOXY,   ALBOLDXY, &
		QSNOWXY, WSLAKEXY,    ZWTXY,     WAXY,      WTXY,     TSNOXY, &
		ZSNSOXY,  SNICEXY,  SNLIQXY, LFMASSXY,  RTMASSXY,   STMASSXY, &
		 WOODXY, STBLCPXY, FASTCPXY,      LAI,    XSAIXY,    TAUSSXY, &
	        SMOISEQ, SMCWTDXY,DEEPRECHXY,  RECHXY,                        &
	         T2MVXY,   T2MBXY,   Q2MVXY,   Q2MBXY,                        &
                 TRADXY,    NEEXY,    GPPXY,    NPPXY,    FVEGXY,    RUNSFXY, &
	        RUNSBXY,   ECANXY,   EDIRXY,  ETRANXY,     FSAXY,     FIRAXY, &
                 APARXY,    PSNXY,    SAVXY,    SAGXY,   RSSUNXY,    RSSHAXY, &
                 BGAPXY,   WGAPXY,    TGVXY,    TGBXY,     CHVXY,      CHBXY, &
		  SHGXY,    SHCXY,    SHBXY,    EVGXY,     EVBXY,      GHVXY, &
		  GHBXY,    IRGXY,    IRCXY,    IRBXY,      TRXY,      EVCXY, &
	       CHLEAFXY,   CHUCXY,   CHV2XY,   CHB2XY,                        &                          
                ids,ide, jds,jde, kds,kde,                      &
                ims,ime, jms,jme, kms,kme,                      &
                i_start(ij),i_end(ij), j_start(ij),j_end(ij), kts,kte, ij )
     end do
 !$OMP END PARALLEL DO

          call system_clock(count=count_after_sflx, count_rate=clock_rate)
          sflx_count_sum = sflx_count_sum + ( count_after_sflx - count_before_sflx )

  IF(RUNOFF_OPTION.EQ.5.AND.MOD(ITIME,STEPWTD).EQ.0)THEN
           CALL wrf_message('calling WTABLE' )

!gmm update wtable from lateral flow and shed water to rivers
           CALL WTABLE_MMF_NOAHMP(                                        &
	       NUM_SOIL_LAYERS,  XLAND, XICE,       XICE_THRESHOLD, ISICE,    &
               ISLTYP,      SMOISEQ,    DZS,        WTDDT,                &
               FDEPTHXY,    AREAXY,     TERRAIN,    ISURBAN,    IVGTYP,   &
               RIVERCONDXY, RIVERBEDXY, EQZWT,      PEXPXY,               &
               SMOIS,       SH2O,       SMCWTDXY,   ZWTXY,                &
	       QRFXY,       DEEPRECHXY, QSPRINGXY,                        &
               QSLATXY,     QRFSXY,     QSPRINGSXY, RECHXY,               &
               IDS,IDE, JDS,JDE, KDS,KDE,                                 &
               IMS,IME, JMS,JME, KMS,KME,                                 &
               ITS,ITE, JTS,JTE, KTS,KTE )

 ENDIF

!------------------------------------------------------------------------
! END of surface_driver consistent code
!------------------------------------------------------------------------

! Output for history
     OUTPUT_FOR_HISTORY: if (output_timestep > 0) then
        if (mod((ITIME-1)*noah_timestep, output_timestep) == 0) then

           call prepare_output_file(trim(outdir), version, &
                igrid, output_timestep, llanduse, split_output_count, hgrid,                &
                ixfull, jxfull, ixpar, jxpar, xstartpar, ystartpar,                         &
                iswater, mapproj, lat1, lon1, dx, dy, truelat1, truelat2, cen_lon,          &
                nsoil, nsnow, dzs, startdate, olddate, IVGTYP, ISLTYP)

           DEFINE_MODE_LOOP : do imode = 1, 2

              call set_output_define_mode(imode)

              ! For 3D arrays, we need to know whether the Z dimension is snow layers, or soil layers.

        ! Properties - Assigned or predicted
              call add_to_output(IVGTYP     , "IVGTYP"  , "Dominant vegetation category"         , "category"              )
              call add_to_output(ISLTYP     , "ISLTYP"  , "Dominant soil category"               , "category"              )
              call add_to_output(FVEGXY     , "FVEG"    , "Green Vegetation Fraction"              , "-"                   )
              call add_to_output(LAI        , "LAI"     , "Leaf area index"                      , "-"                     )
              call add_to_output(XSAIXY     , "SAI"     , "Stem area index"                      , "-"                     )
        ! Forcing
              call add_to_output(SWDOWN     , "SWFORC"  , "Shortwave forcing"                    , "W m{-2}"               )
              call add_to_output(COSZEN     , "COSZ"    , "Cosine of zenith angle"                    , "W m{-2}"               )
              call add_to_output(GLW        , "LWFORC"  , "Longwave forcing"                    , "W m{-2}"               )
              call add_to_output(RAINBL     , "RAINRATE", "Precipitation rate"                   , "kg m{-2} s{-1}"        )
        ! Grid energy budget terms
              call add_to_output(EMISS      , "EMISS"   , "Grid emissivity"                    , ""               )
              call add_to_output(FSAXY      , "FSA"     , "Total absorbed SW radiation"          , "W m{-2}"               )         
              call add_to_output(FIRAXY     , "FIRA"    , "Total net LW radiation to atmosphere" , "W m{-2}"               )
              call add_to_output(GRDFLX     , "GRDFLX"  , "Heat flux into the soil"              , "W m{-2}"               )
              call add_to_output(HFX        , "HFX"     , "Total sensible heat to atmosphere"    , "W m{-2}"               )
              call add_to_output(LH         , "LH"      , "Total latent heat to atmosphere"    , "W m{-2}"               )
              call add_to_output(ECANXY     , "ECAN"    , "Canopy water evaporation rate"        , "kg m{-2} s{-1}"        )
              call add_to_output(ETRANXY    , "ETRAN"   , "Transpiration rate"                   , "kg m{-2} s{-1}"        )
              call add_to_output(EDIRXY     , "EDIR"    , "Direct from soil evaporation rate"    , "kg m{-2} s{-1}"        )
              call add_to_output(ALBEDO     , "ALBEDO"  , "Surface albedo"                         , "-"                   )
        ! Grid water budget terms - in addition to above
              call add_to_output(UDRUNOFF   , "UGDRNOFF", "Accumulated underground runoff"       , "mm"                    )
              call add_to_output(SFCRUNOFF  , "SFCRNOFF", "Accumulatetd surface runoff"          , "mm"                    )
              call add_to_output(CANLIQXY   , "CANLIQ"  , "Canopy liquid water content"          , "mm"                    )
              call add_to_output(CANICEXY   , "CANICE"  , "Canopy ice water content"             , "mm"                    )
              call add_to_output(ZWTXY      , "ZWT"     , "Depth to water table"                 , "m"                     )
              call add_to_output(WAXY       , "WA"      , "Water in aquifer"                     , "kg m{-2}"              )
              call add_to_output(WTXY       , "WT"      , "Water in aquifer and saturated soil"  , "kg m{-2}"              )
        ! Additional needed to close the canopy energy budget
              call add_to_output(SAVXY      , "SAV"     , "Solar radiative heat flux absorbed by vegetation", "W m{-2}"    )
              call add_to_output(TRXY       , "TR"      , "Transpiration heat"                     , "W m{-2}"             )
              call add_to_output(EVCXY      , "EVC"     , "Canopy evap heat"                       , "W m{-2}"             )
              call add_to_output(IRCXY      , "IRC"     , "Canopy net LW rad"                      , "W m{-2}"             )
              call add_to_output(SHCXY      , "SHC"     , "Canopy sensible heat"                   , "W m{-2}"             )
        ! Additional needed to close the under canopy ground energy budget
              call add_to_output(IRGXY      , "IRG"     , "Ground net LW rad"                      , "W m{-2}"             )
              call add_to_output(SHGXY      , "SHG"     , "Ground sensible heat"                   , "W m{-2}"             )
              call add_to_output(EVGXY      , "EVG"     , "Ground evap heat"                       , "W m{-2}"             )
              call add_to_output(GHVXY      , "GHV"     , "Ground heat flux + to soil vegetated"   , "W m{-2}"             )
        ! Needed to close the bare ground energy budget
              call add_to_output(SAGXY      , "SAG"     , "Solar radiative heat flux absorbed by ground", "W m{-2}"        )
              call add_to_output(IRBXY      , "IRB"     , "Net LW rad to atm bare"                 , "W m{-2}"             )
              call add_to_output(SHBXY      , "SHB"     , "Sensible heat to atm bare"              , "W m{-2}"             )
              call add_to_output(EVBXY      , "EVB"     , "Evaporation heat to atm bare"           , "W m{-2}"             )
              call add_to_output(GHBXY      , "GHB"     , "Ground heat flux + to soil bare"        , "W m{-2}"             )
        ! Above-soil temperatures
              call add_to_output(TRADXY     , "TRAD"    , "Surface radiative temperature"        , "K"                     )
              call add_to_output(TGXY       , "TG"      , "Ground temperature"                   , "K"                     )
              call add_to_output(TVXY       , "TV"      , "Vegetation temperature"               , "K"                     )
              call add_to_output(TAHXY      , "TAH"     , "Canopy air temperature"               , "K"                     )
              call add_to_output(TGVXY      , "TGV"     , "Ground surface Temp vegetated"          , "K"                   )
              call add_to_output(TGBXY      , "TGB"     , "Ground surface Temp bare"               , "K"                   )
              call add_to_output(T2MVXY     , "T2MV"    , "2m Air Temp vegetated"                  , "K"                   )
              call add_to_output(T2MBXY     , "T2MB"    , "2m Air Temp bare"                       , "K"                   )
        ! Above-soil moisture
              call add_to_output(Q2MVXY     , "Q2MV"    , "2m mixing ratio vegetated"              , "kg/kg"               )
              call add_to_output(Q2MBXY     , "Q2MB"    , "2m mixing ratio bare"                   , "kg/kg"               )
              call add_to_output(EAHXY      , "EAH"     , "Canopy air vapor pressure"            , "Pa"                    )
              call add_to_output(FWETXY     , "FWET"    , "Wetted or snowed fraction of canopy"  , "fraction"              )
        ! Snow and soil - 3D terms
              call add_to_output(ZSNSOXY(:,-nsnow+1:0,:),  "ZSNSO_SN" , "Snow layer depths from snow surface", "m", "SNOW")
              call add_to_output(SNICEXY    , "SNICE"   , "Snow layer ice"                       , "mm"             , "SNOW")
              call add_to_output(SNLIQXY    , "SNLIQ"   , "Snow layer liquid water"              , "mm"             , "SNOW")
              call add_to_output(TSLB       , "SOIL_T"  , "soil temperature"                     , "K"              , "SOIL")
              call add_to_output(SMOIS      , "SOIL_M"  , "volumetric soil moisture"             , "m{3} m{-3}"     , "SOIL")
              call add_to_output(SH2O       , "SOIL_W"  , "liquid volumetric soil moisture"      , "m3 m-3"         , "SOIL")
              call add_to_output(TSNOXY     , "SNOW_T"  , "snow temperature"                     , "K"              , "SNOW")
        ! Snow - 2D terms
              call add_to_output(SNOWH      , "SNOWH"   , "Snow depth"                           , "m"                     )
              call add_to_output(SNOW       , "SNEQV"   , "Snow water equivalent"                , "kg m{-2}"              )
              call add_to_output(QSNOWXY    , "QSNOW"   , "Snowfall rate"                        , "mm s{-1}"              )
              call add_to_output(ISNOWXY    , "ISNOW"   , "Number of snow layers"                , "count"                 )
              call add_to_output(SNOWC      , "FSNO"    , "Snow-cover fraction on the ground"      , ""                    )
              call add_to_output(ACSNOW     , "ACSNOW"  , "accumulated snow fall"                  , "mm"                  )
              call add_to_output(ACSNOM     , "ACSNOM"  , "accumulated melting water out of snow bottom" , "mm"            )
        ! Exchange coefficients
              call add_to_output(CMXY       , "CM"      , "Momentum drag coefficient"            , ""                      )
              call add_to_output(CHXY       , "CH"      , "Sensible heat exchange coefficient"   , ""                      )
              call add_to_output(CHVXY      , "CHV"     , "Exchange coefficient vegetated"         , "m s{-1}"             )
              call add_to_output(CHBXY      , "CHB"     , "Exchange coefficient bare"              , "m s{-1}"             )
              call add_to_output(CHLEAFXY   , "CHLEAF"  , "Exchange coefficient leaf"              , "m s{-1}"             )
              call add_to_output(CHUCXY     , "CHUC"    , "Exchange coefficient bare"              , "m s{-1}"             )
              call add_to_output(CHV2XY     , "CHV2"    , "Exchange coefficient 2-meter vegetated" , "m s{-1}"             )
              call add_to_output(CHB2XY     , "CHB2"    , "Exchange coefficient 2-meter bare"      , "m s{-1}"             )
        ! Carbon allocation model
              call add_to_output(LFMASSXY   , "LFMASS"  , "Leaf mass"                            , "g m{-2}"               )
              call add_to_output(RTMASSXY   , "RTMASS"  , "Mass of fine roots"                   , "g m{-2}"               )
              call add_to_output(STMASSXY   , "STMASS"  , "Stem mass"                            , "g m{-2}"               )
              call add_to_output(WOODXY     , "WOOD"    , "Mass of wood and woody roots"         , "g m{-2}"               )
              call add_to_output(STBLCPXY   , "STBLCP"  , "Stable carbon in deep soil"           , "g m{-2}"               )
              call add_to_output(FASTCPXY   , "FASTCP"  , "Short-lived carbon in shallow soil"   , "g m{-2}"               )
              call add_to_output(NEEXY      , "NEE"     , "Net ecosystem exchange"                 , "g m{-2} s{-1} CO2"   )
              call add_to_output(GPPXY      , "GPP"     , "Net instantaneous assimilation"         , "g m{-2} s{-1} C"     )
              call add_to_output(NPPXY      , "NPP"     , "Net primary productivity"               , "g m{-2} s{-1} C"     )
              call add_to_output(PSNXY      , "PSN"     , "Total photosynthesis"                   , "umol CO@ m{-2} s{-1}")
              call add_to_output(APARXY     , "APAR"    , "Photosynthesis active energy by canopy" , "W m{-2}"             )

        ! Carbon allocation model
	    IF(RUNOFF_OPTION == 5) THEN
              call add_to_output(SMCWTDXY   , "SMCWTD"   , "Leaf mass"                            , "g m{-2}"               )
              call add_to_output(RECHXY     , "RECH"     , "Mass of fine roots"                   , "g m{-2}"               )
              call add_to_output(QRFSXY     , "QRFS"     , "Stem mass"                            , "g m{-2}"               )
              call add_to_output(QSPRINGSXY , "QSPRINGS" , "Mass of wood and woody roots"         , "g m{-2}"               )
              call add_to_output(QSLATXY    , "QSLAT"    , "Stable carbon in deep soil"           , "g m{-2}"               )
	    ENDIF

           enddo DEFINE_MODE_LOOP
           call finalize_output_file(split_output_count)
        endif
     endif OUTPUT_FOR_HISTORY

     if (IVGTYP(xstart,ystart)==ISWATER) then
        write(*,'(" ***DATE=", A19)', advance="NO") olddate
     else
     write(*,'(" ***DATE=", A19, 6F10.5)', advance="NO") olddate, &
          TSLB(xstart,1,ystart), LAI(xstart,ystart)
     endif


!------------------------------------------------------------------------
! Update the time 
!------------------------------------------------------------------------

     call geth_newdate(newdate, olddate, nint(dtbl))
     olddate = newdate

!------------------------------------------------------------------------
! End of Time Loop  (do ITIME) 
!------------------------------------------------------------------------

     ! update the timer
     call system_clock(count=clock_count_2, count_rate=clock_rate)
     timing_sum = timing_sum + float(clock_count_2-clock_count_1)/float(clock_rate)
     write(*,'("    Timing: ",f6.2," Cumulative:  ", f10.2, "  SFLX: ", f6.2 )') &
          float(clock_count_2-clock_count_1)/float(clock_rate), &
          timing_sum, real(sflx_count_sum) / real(clock_rate)
     clock_count_1 = clock_count_2

  ENDDO TIMELOOP

END program Noah_hrldas_driver

subroutine wrf_message(msg)
  implicit none
  character(len=*), intent(in) :: msg
  print*, msg
end subroutine wrf_message

logical function wrf_dm_on_monitor() result(l)
  l = .TRUE.
  return
end function wrf_dm_on_monitor

!------------------------------------------------------------------------------------
!------------------------------------------------------------------------------------

SUBROUTINE CALC_DECLIN ( NOWDATE, LATITUDE, LONGITUDE, COSZ, JULIAN )

  USE MODULE_DATE_UTILITIES
!---------------------------------------------------------------------
   IMPLICIT NONE
!---------------------------------------------------------------------

   REAL, PARAMETER :: DEGRAD = 3.14159265/180.
   REAL, PARAMETER :: DPD    = 360./365.
! !ARGUMENTS:
   CHARACTER(LEN=19), INTENT(IN)  :: NOWDATE    ! YYYY-MM-DD_HH:MM:SS
   REAL,              INTENT(IN)  :: LATITUDE
   REAL,              INTENT(IN)  :: LONGITUDE
   REAL,              INTENT(OUT) :: COSZ
   REAL,              INTENT(OUT) :: JULIAN
   REAL                           :: HRANG
   REAL                           :: DECLIN
   REAL                           :: OBECL
   REAL                           :: SINOB
   REAL                           :: SXLONG
   REAL                           :: ARG
   REAL                           :: TLOCTIM
   INTEGER                        :: IDAY
   INTEGER                        :: IHOUR
   INTEGER                        :: IMINUTE
   INTEGER                        :: ISECOND

   CALL GETH_IDTS(NOWDATE(1:10), NOWDATE(1:4)//"-01-01", IDAY)
   READ(NOWDATE(12:13), *) IHOUR
   READ(NOWDATE(15:16), *) IMINUTE
   READ(NOWDATE(18:19), *) ISECOND
   JULIAN = REAL(IDAY) + REAL(IHOUR)/24.

!
! FOR SHORT WAVE RADIATION

   DECLIN=0.

!-----OBECL : OBLIQUITY = 23.5 DEGREE.

   OBECL=23.5*DEGRAD
   SINOB=SIN(OBECL)

!-----CALCULATE LONGITUDE OF THE SUN FROM VERNAL EQUINOX:

   IF(JULIAN.GE.80.)SXLONG=DPD*(JULIAN-80.)*DEGRAD
   IF(JULIAN.LT.80.)SXLONG=DPD*(JULIAN+285.)*DEGRAD
   ARG=SINOB*SIN(SXLONG)
   DECLIN=ASIN(ARG)

   TLOCTIM = REAL(IHOUR) + REAL(IMINUTE)/60.0 + REAL(ISECOND)/3600.0 + LONGITUDE/15.0 ! LOCAL TIME IN HOURS
   TLOCTIM = AMOD(TLOCTIM+24.0, 24.0)
   HRANG=15.*(TLOCTIM-12.)*DEGRAD
   COSZ=SIN(LATITUDE*DEGRAD)*SIN(DECLIN)+COS(LATITUDE*DEGRAD)*COS(DECLIN)*COS(HRANG)

 END SUBROUTINE CALC_DECLIN

!
!------------------------------------------------------------------------------------------
