module config_base
  !use netcdf_layer_base
  implicit none

  integer, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist
  !REAL                  ::  DTBL      ! timestep [s]

  type NOAHLSM_OFFLINE_DT
     character(len=256) :: indir
     integer            :: nsoil ! number of soil layers
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
     integer            :: glacier_option
     integer            :: surface_resistance_option
     integer            :: split_output_count = 1
     integer            :: khour
     integer            :: kday
     real               :: zlvl 
     character(len=256) :: hrldas_setup_file = " "
     character(len=256) :: mmf_runoff_file = " "
     character(len=256) :: external_veg_filename_template = " "
     character(len=256) :: external_lai_filename_template = " "
     integer            :: xstart = 1
     integer            :: ystart = 1
     integer            :: xend = 0
     integer            :: yend = 0
     REAL, DIMENSION(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
     integer :: rst_bi_out, rst_bi_in !0: default netcdf format. 1: binary write/read by each core.
     CHARACTER(LEN = 256) :: spatial_filename
  end type NOAHLSM_OFFLINE_DT

  type WRF_HYDRO_OFFLINE_DT
     integer  :: finemesh
     integer  :: finemesh_factor
     integer  :: forc_typ
     integer  :: snow_assim
  end type WRF_HYDRO_OFFLINE_DT

  TYPE namelist_rt_field  
      
     integer :: nsoil, SOLVEG_INITSWC
     real,allocatable,dimension(:) :: ZSOIL8
     real*8 :: out_dt, rst_dt
     real   :: dt  !! dt is NOAH_TIMESTEP
     integer :: START_YEAR, START_MONTH, START_DAY, START_HOUR, START_MIN
     character(len=256)  :: restart_file = ""
     integer            :: split_output_count
     integer :: igrid
     integer :: rst_bi_in   ! used for parallel io with large restart file.
     integer :: rst_bi_out   ! used for parallel io with large restart file.
     ! each process will output the restart tile.
     character(len=256) :: geo_static_flnm = ""
     character(len=1024) :: land_spatial_meta_flnm = ""
     integer  :: DEEPGWSPIN
     integer ::  order_to_write, rst_typ
     character(len=256)  :: upmap_file = ""    ! user defined mapping file for NHDPLUS
     character(len=256)  :: hydrotbl_f = ""    ! hydrotbl file

     !      additional character
     character :: hgrid
     character(len=19) :: olddate="123456"
     character(len=19) :: startdate="123456"
     character(len=19) :: sincedate="123456"

     integer :: io_config_outputs  ! used for NCEP REALTIME OUTPUT
     integer :: io_form_outputs ! Flag to turn specify level of internal compression
     integer :: t0OutputFlag  
     integer :: channel_only, channelBucket_only
     integer :: output_channelBucket_influx ! used for FORCE_TYPE 9 and 10

     integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
          SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
          GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
          sys_cpl, gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, &
          gwsoilcpl, UDMP_OPT
     logical:: GwPreDiag, GwSpinUp
     real:: DTRT_TER,DTRT_CH, DTCT, dxrt0,  gwChanCondConstIn, gwChanCondConstOut, gwIhShift
     character(len=256) :: route_topo_f=""
     character(len=256) :: route_chan_f=""
     character(len=256) :: route_link_f=""
     character(len=256) :: route_lake_f=""
     character(len=256) :: route_direction_f=""
     character(len=256) :: route_order_f=""
     character(len=256) :: gwbasmskfil =""
     character(len=256) :: gwstrmfil =""
     character(len=256) :: geo_finegrid_flnm =""
     character(len=256) :: udmap_file =""
     character(len=256) :: GWBUCKPARM_file = ""
     integer :: reservoir_data_ingest ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE. 
     character(len=1024) :: reservoir_obs_dir = ""

     logical :: compound_channel
     integer ::frxst_pts_out            ! ASCII point timeseries output at user specified points
     integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
     integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
     integer ::CHANOBS_DOMAIN             ! NetCDF point timeseries of output at forecast/gage points
     integer ::LSMOUT_DOMAIN              ! Netcdf grid of variables passed between LSM and routing components
     integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
     integer ::output_gw                   ! Netcdf grid of GW
     integer ::outlake                   ! Netcdf grid of lake
     integer :: rtFlag
     integer ::khour

     character(len=256) :: nudgingParamFile
     character(len=256) :: netwkReExFile
     logical            :: readTimesliceParallel
     logical            :: temporalPersistence
     logical            :: persistBias
     logical            :: biasWindowBeforeT0
     character(len=256) :: nudgingLastObsFile
     integer            :: minNumPairsBiasPersist
     integer            :: maxAgePairsBiasPersist
     logical            :: invDistTimeWeightBias
     logical            :: noConstInterfBias  
     character(len=256) :: timeSlicePath
     integer            :: nLastObs

   contains

     procedure, pass(self) :: check => rt_nlst_check
     
  END TYPE namelist_rt_field

  type, public :: Configuration_
   contains
     procedure, nopass :: init => config_init
     procedure, nopass :: noah_lsm => copy_noah_lsm
     procedure, nopass :: noah_lsm_sync => noah_lsm_sync
     procedure, nopass :: wrf_hydro => copy_wrf_hydro
  end type Configuration_

  type(NOAHLSM_OFFLINE_DT), private, save :: noah_lsm_file
  type(WRF_HYDRO_OFFLINE_DT), private, save :: wrf_hydro_file
  type(namelist_rt_field), private, save :: nlst

contains

  subroutine config_init()
    implicit none

    call init_noah_lsm()
    call init_wrf_hydro()
    call init_namelist_rt_field()

  end subroutine config_init

  subroutine rt_nlst_check(self)
    ! Subroutine to check namelist options specified by the user.
    implicit none

    class(namelist_rt_field) self

    ! Local variables
    logical :: fileExists = .false.
    integer :: i

    ! Go through and make some logical checks for each hydro.namelist option.
    ! Some of these checks will depend on specific options chosen by the user.

    if( (nlst%sys_cpl .lt. 1) .or. (nlst%sys_cpl .gt. 4) ) then
       call hydro_stop("hydro.namelist ERROR: Invalid sys_cpl value specified.")
    endif
    if(len(trim(nlst%geo_static_flnm)) .eq. 0) then
       call hydro_stop("hydro.namelist ERROR: Please specify a GEO_STATIC_FLNM file.")
    else
       inquire(file=trim(nlst%geo_static_flnm),exist=fileExists)
       if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GEO_STATIC_FLNM not found.')
    endif
    if(len(trim(nlst%geo_finegrid_flnm)) .eq. 0) then
       call hydro_stop("hydro.namelist ERROR: Please specify a GEO_FINEGRID_FLNM file.")
    else
       inquire(file=trim(nlst%geo_finegrid_flnm),exist=fileExists)
       if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GEO_FINEGRID_FLNM not found.')
    endif
    !if(len(trim(nlst%land_spatial_meta_flnm)) .eq. 0) then
    !   call hydro_stop("hydro.namelist ERROR: Please specify a LAND_SPATIAL_META_FLNM file.")
    !else
    !   inquire(file=trim(nlst%land_spatial_meta_flnm),exist=fileExists)
    !   if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: LAND_SPATIAL_META_FLNM not found.')
    !endif
    if(len(trim(nlst%RESTART_FILE)) .ne. 0) then
       inquire(file=trim(nlst%RESTART_FILE),exist=fileExists)
       if (.not. fileExists) call hydro_stop('hydro.namelist ERROR:= Hydro RESTART_FILE not found.')
    endif
    if(nlst%igrid .le. 0) call hydro_stop('hydro.namelist ERROR: Invalid IGRID specified.')
    if(nlst%out_dt .le. 0) call hydro_stop('hydro_namelist ERROR: Invalid out_dt specified.')
    if( (nlst%split_output_count .lt. 0 ) .or. (nlst%split_output_count .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid SPLIT_OUTPUT_COUNT specified')
    endif
    if( (nlst%rst_typ .lt. 0 ) .or. (nlst%rst_typ .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid rst_typ specified')
    endif
    if( (nlst%rst_bi_in .lt. 0 ) .or. (nlst%rst_bi_in .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid rst_bi_in specified')
    endif
    if( (nlst%rst_bi_out .lt. 0 ) .or. (nlst%rst_bi_out .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid rst_bi_out specified')
    endif
    if( (nlst%RSTRT_SWC .lt. 0 ) .or. (nlst%RSTRT_SWC .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid RSTRT_SWC specified')
    endif
    if( (nlst%GW_RESTART .lt. 0 ) .or. (nlst%GW_RESTART .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid GW_RESTART specified')
    endif
    if( (nlst%order_to_write .lt. 1 ) .or. (nlst%order_to_write .gt. 12) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid order_to_write specified')
    endif
    if( (nlst%io_form_outputs .lt. 0 ) .or. (nlst%io_form_outputs .gt. 4) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid io_form_outputs specified')
    endif
    if( (nlst%io_config_outputs .lt. 0 ) .or. (nlst%io_config_outputs .gt. 6) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid io_config_outputs specified')
    endif
    if( (nlst%t0OutputFlag .lt. 0 ) .or. (nlst%t0OutputFlag .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid t0OutputFlag specified')
    endif
    if( (nlst%output_channelBucket_influx .lt. 0 ) .or. (nlst%output_channelBucket_influx .gt. 3) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid output_channelBucket_influx specified')
    endif
    if( (nlst%CHRTOUT_DOMAIN .lt. 0 ) .or. (nlst%CHRTOUT_DOMAIN .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid CHRTOUT_DOMAIN specified')
    endif
    if( (nlst%CHANOBS_DOMAIN .lt. 0 ) .or. (nlst%CHANOBS_DOMAIN .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid CHANOBS_DOMAIN specified')
    endif
    if( (nlst%CHRTOUT_GRID .lt. 0 ) .or. (nlst%CHRTOUT_GRID .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid CHRTOUT_GRID specified')
    endif
    if( (nlst%LSMOUT_DOMAIN .lt. 0 ) .or. (nlst%LSMOUT_DOMAIN .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid LSMOUT_DOMAIN specified')
    endif
    if( (nlst%RTOUT_DOMAIN .lt. 0 ) .or. (nlst%RTOUT_DOMAIN .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid RTOUT_DOMAIN specified')
    endif
    if( (nlst%output_gw .lt. 0 ) .or. (nlst%output_gw .gt. 2) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid output_gw specified')
    endif
    if( (nlst%outlake .lt. 0 ) .or. (nlst%outlake .gt. 2) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid outlake specified')
    endif
    if( (nlst%frxst_pts_out .lt. 0 ) .or. (nlst%frxst_pts_out .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid frxst_pts_out specified')
    endif
    if(nlst%TERADJ_SOLAR .ne. 0) then
       call hydro_stop('hydro.namelist ERROR: Invalid TERADJ_SOLAR specified')
    endif

    ! The default value of nsoil == -999. When channel-only is used,
    ! nsoil ==  -999999. In the case of channel-only, skip following block of code.
    if(nlst%NSOIL .le. 0 .and. nlst%NSOIL .ne. -999999) then
       call hydro_stop('hydro.namelist ERROR: Invalid NSOIL specified.')
    endif
    do i = 1,nlst%NSOIL
       if(nlst%ZSOIL8(i) .gt. 0) then
          call hydro_stop('hydro.namelist ERROR: Invalid ZSOIL layer depth specified.')
       endif
       if(i .gt. 1) then
          if(nlst%ZSOIL8(i) .ge. nlst%ZSOIL8(i-1)) then
             call hydro_stop('hydro.namelist ERROR: Invalid ZSOIL layer depth specified.')
          endif
       endif
    end do

    if(nlst%dxrt0 .le. 0) then
       call hydro_stop('hydro.namelist ERROR: Invalid DXRT specified.')
    endif
    if(nlst%AGGFACTRT .le. 0) then
       call hydro_stop('hydro.namelist ERROR: Invalid AGGFACTRT specified.')
    endif
    if(nlst%DTRT_CH .le. 0) then
       call hydro_stop('hydro.namelist ERROR: Invalid DTRT_CH specified.')
    endif
    if(nlst%DTRT_TER .le. 0) then
       call hydro_stop('hydro.namelist ERROR: Invalid DTRT_TER specified.')
    endif
    if( (nlst%SUBRTSWCRT .lt. 0 ) .or. (nlst%SUBRTSWCRT .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid SUBRTSWCRT specified')
    endif
    if( (nlst%OVRTSWCRT .lt. 0 ) .or. (nlst%OVRTSWCRT .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid OVRTSWCRT specified')
    endif
    if( (nlst%OVRTSWCRT .eq. 1 ) .or. (nlst%SUBRTSWCRT .eq. 1) ) then
       if( (nlst%rt_option .lt. 1 ) .or. (nlst%rt_option .gt. 2) ) then
          !if(nlst%rt_option .ne. 1) then
          call hydro_stop('hydro.namelist ERROR: Invalid rt_option specified')
       endif
    endif
    if( (nlst%CHANRTSWCRT .lt. 0 ) .or. (nlst%CHANRTSWCRT .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid CHANRTSWCRT specified')
    endif
    if(nlst%CHANRTSWCRT .eq. 1) then
       if( (nlst%channel_option .lt. 1 ) .or. (nlst%channel_option .gt. 3) ) then
          call hydro_stop('hydro.namelist ERROR: Invalid channel_option specified')
       endif
    endif
    if( (nlst%CHANRTSWCRT .eq. 1) .and. (nlst%channel_option .lt. 3) ) then
       if(len(trim(nlst%route_link_f)) .eq. 0) then
          call hydro_stop("hydro.namelist ERROR: Please specify a route_link_f file.")
       else
          inquire(file=trim(nlst%route_link_f),exist=fileExists)
          if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: route_link_f not found.')
       endif
    endif
    if( (nlst%GWBASESWCRT .lt. 0 ) .or. (nlst%GWBASESWCRT .gt. 2) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid GWBASESWCRT specified')
    endif
    if(nlst%GWBASESWCRT .eq. 1) then
       if(len(trim(nlst%GWBUCKPARM_file)) .eq. 0) then
          call hydro_stop("hydro.namelist ERROR: Please specify a GWBUCKPARM_file file.")
       else
          inquire(file=trim(nlst%GWBUCKPARM_file),exist=fileExists)
          if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: GWBUCKPARM_file not found.')
       endif
    endif
    if( (nlst%GWBASESWCRT .gt. 0) .and. (nlst%UDMP_OPT .ne. 1) ) then
       if(len(trim(nlst%gwbasmskfil)) .eq. 0) then
          call hydro_stop("hydro.namelist ERROR: Please specify a gwbasmskfil file.")
       else
          inquire(file=trim(nlst%gwbasmskfil),exist=fileExists)
          if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: gwbasmskfil not found.')
       endif
    endif
    if( (nlst%UDMP_OPT .lt. 0 ) .or. (nlst%UDMP_OPT .gt. 1) ) then
       call hydro_stop('hydro.namelist ERROR: Invalid UDMP_OPT specified')
    endif
    if(nlst%UDMP_OPT .gt. 0) then
       if(len(trim(nlst%udmap_file)) .eq. 0) then
          call hydro_stop("hydro.namelist ERROR: Please specify a udmap_file file.")
       else
          inquire(file=trim(nlst%udmap_file),exist=fileExists)
          if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: udmap_file not found.')
       endif
    endif
    if( (nlst%UDMP_OPT .eq. 1) .and. (nlst%CHANRTSWCRT .eq. 0) ) then
       call hydro_stop('hydro.namelist ERROR: User-defined mapping requires channel routing on.')
    endif
    if(nlst%outlake .ne. 0) then
       if(len(trim(nlst%route_lake_f)) .eq. 0) then
          call hydro_stop('hydro.namelist ERROR: You MUST specify a route_lake_f to ouptut and run lakes.')
       endif
    endif
    if(len(trim(nlst%route_lake_f)) .ne. 0) then
       inquire(file=trim(nlst%route_lake_f),exist=fileExists)
       if (.not. fileExists) call hydro_stop('hydro.namelist ERROR: route_lake_f not found.')
    endif
    ! Only allow lakes to be ran with gridded routing or NWM routing
    if(len(trim(nlst%route_lake_f)) .ne. 0) then
       if(nlst%channel_option .ne. 3) then
          if(nlst%UDMP_OPT .ne. 1) then
             call hydro_stop('hydro.namelist ERROR: Currently lakes only work with gridded channel routing or UDMP=1. Please change your namelist settings.')
          endif
       endif
    endif

    if((nlst%channel_option .eq. 3) .and. (nlst%compound_channel)) then
       call hydro_stop("Compound channel option not available for diffusive wave routing. ")
    end if

  end subroutine rt_nlst_check

  subroutine init_namelist_rt_field()
    implicit none

    integer ierr
    integer:: RT_OPTION, CHANRTSWCRT, channel_option, &
         SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, &
         GWBASESWCRT,  GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, &
         sys_cpl, rst_typ, rst_bi_in, rst_bi_out, &
         gwChanCondSw, GwPreCycles, GwSpinCycles, GwPreDiagInterval, gwsoilcpl, &
         UDMP_OPT, io_form_outputs
    real:: DTRT_TER,DTRT_CH,dxrt, gwChanCondConstIn, gwChanCondConstOut, gwIhShift
    character(len=256) :: route_topo_f=""
    character(len=256) :: route_chan_f=""
    character(len=256) :: route_link_f=""
    logical            :: compound_channel
    character(len=256) :: route_lake_f=""
    character(len=256) :: route_direction_f=""
    character(len=256) :: route_order_f=""
    character(len=256) :: gwbasmskfil =""
    character(len=256) :: gwstrmfil =""
    character(len=256) :: geo_finegrid_flnm =""
    character(len=256) :: udmap_file =""
    character(len=256) :: GWBUCKPARM_file = ""
    integer :: reservoir_data_ingest ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE.
    integer :: SOLVEG_INITSWC
    real*8 :: out_dt, rst_dt
    character(len=256)  :: RESTART_FILE = ""
    character(len=256)  :: hydrotbl_f   = ""
    logical            :: GwPreDiag, GwSpinUp
    integer            :: split_output_count, order_to_write
    integer :: igrid, io_config_outputs, t0OutputFlag, output_channelBucket_influx
    character(len=256) :: geo_static_flnm = ""
    character(len=1024) :: land_spatial_meta_flnm = ""
    integer  :: DEEPGWSPIN

    integer :: i

    integer ::CHRTOUT_DOMAIN           ! Netcdf point timeseries output at all channel points
    integer ::CHRTOUT_GRID                ! Netcdf grid of channel streamflow values
    integer ::LSMOUT_DOMAIN              ! Netcdf grid of variables passed between LSM and routing components
    integer ::RTOUT_DOMAIN                ! Netcdf grid of terrain routing variables on routing grid
    integer  :: output_gw
    integer  :: outlake
    integer :: frxst_pts_out            ! ASCII text file of streamflow at forecast points
    integer :: CHANOBS_DOMAIN           ! NetCDF point timeseries output at forecast points.

!!! add the following two dummy variables
    integer  :: NSOIL
    real :: ZSOIL8(8)

    logical            :: dir_e
    character(len=1024) :: reservoir_obs_dir
#ifdef WRF_HYDRO_NUDGING
    character(len=256) :: nudgingParamFile
    character(len=256) :: netwkReExFile
    logical            :: readTimesliceParallel
    logical            :: temporalPersistence
    logical            :: persistBias
    logical            :: biasWindowBeforeT0
    character(len=256) :: nudgingLastObsFile
    character(len=256) :: timeSlicePath
    integer            :: nLastObs
    integer            :: minNumPairsBiasPersist
    integer            :: maxAgePairsBiasPersist
    logical            :: invDistTimeWeightBias
    logical            :: noConstInterfBias
#endif

    namelist /HYDRO_nlist/ NSOIL, ZSOIL8,&
         RESTART_FILE,SPLIT_OUTPUT_COUNT,IGRID,&
         geo_static_flnm, &
         land_spatial_meta_flnm, &
         out_dt, rst_dt, &
         DEEPGWSPIN, SOLVEG_INITSWC, &
         RT_OPTION, CHANRTSWCRT, channel_option, &
         SUBRTSWCRT,OVRTSWCRT,AGGFACTRT, dtrt_ter,dtrt_ch,dxrt,&
         GwSpinCycles, GwPreCycles, GwSpinUp, GwPreDiag, GwPreDiagInterval, gwIhShift, &
         GWBASESWCRT, gwChanCondSw, gwChanCondConstIn, gwChanCondConstOut , &
         route_topo_f,route_chan_f,route_link_f, compound_channel, route_lake_f, &
         route_direction_f,route_order_f,gwbasmskfil, geo_finegrid_flnm,&
         gwstrmfil,GW_RESTART,RSTRT_SWC,TERADJ_SOLAR, sys_cpl, &
         order_to_write , rst_typ, rst_bi_in, rst_bi_out, gwsoilcpl, &
         CHRTOUT_DOMAIN,CHANOBS_DOMAIN,CHRTOUT_GRID,LSMOUT_DOMAIN,&
         RTOUT_DOMAIN, output_gw, outlake, &
         frxst_pts_out, udmap_file, UDMP_OPT, GWBUCKPARM_file, &
         io_config_outputs, io_form_outputs, hydrotbl_f, t0OutputFlag, output_channelBucket_influx

#ifdef WRF_HYDRO_NUDGING
    namelist /NUDGING_nlist/ nudgingParamFile,       netwkReExFile,          &
         readTimesliceParallel,  temporalPersistence,    &
         persistBias,            nudgingLastObsFile,     &
         timeSlicePath,          nLastObs,               &
         minNumPairsBiasPersist, maxAgePairsBiasPersist, &
         biasWindowBeforeT0,     invDistTimeWeightBias,  &
         noConstInterfBias
#endif

    !! ---- End definitions ----

    ! Default values for HYDRO_nlist
    UDMP_OPT = 0
    rst_bi_in = 0
    rst_bi_out = 0
    io_config_outputs = 0
    io_form_outputs = 0
    frxst_pts_out = 0
    CHANOBS_DOMAIN = 0
    t0OutputFlag = 1
    output_channelBucket_influx = 0
    TERADJ_SOLAR = 0
    reservoir_data_ingest = 0 ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE.
    compound_channel = .FALSE.

#ifdef WRF_HYDRO_NUDGING
    ! Default values for NUDGING_nlist
    nudgingParamFile = "DOMAIN/nudgingParams.nc"
    netwkReExFile    = "DOMAIN/netwkReExFile.nc"
    readTimesliceParallel  = .true.
    temporalPersistence    = .true.
    persistBias            = .false.
    biasWindowBeforeT0     = .false.
    nudgingLastObsFile     = ""
    timeSlicePath          = "./nudgingTimeSliceObs/"
    nLastObs               = 960
    minNumPairsBiasPersist = 8
    maxAgePairsBiasPersist = -99999
    invDistTimeWeightBias  = .false.
    noConstInterfBias      = .false.
#endif

! #ifdef MPP_LAND
!     if(IO_id .eq. my_id) then
! #endif
#ifndef NCEP_WCOSS
    open(12, file="hydro.namelist", form="FORMATTED")
#else
    open(12, form="FORMATTED")
#endif
    read(12, HYDRO_nlist, iostat=ierr)
    if(ierr .ne. 0) call hydro_stop("HYDRO_nlst namelist error in read_rt_nlst")

#ifdef WRF_HYDRO_NUDGING
    read(12, NUDGING_nlist, iostat=ierr)
    if(ierr .ne. 0) call hydro_stop("NUDGING_nlst namelist error in read_rt_nlst")
    !! Conditional default values for nuding_nlist
    if(maxAgePairsBiasPersist .eq. -99999) maxAgePairsBiasPersist = -1*nLastObs
#endif
    close(12)

! #ifdef MPP_LAND
!     endif
! #endif

    ! ADCHANGE: move these checks to more universal namelist checks...
    if ( io_config_outputs .eq. 4 ) RTOUT_DOMAIN = 0

    if(output_channelBucket_influx .ne. 0) then
       if(nlst%dt .ne. out_dt*60) &
            call hydro_stop("read_rt_nlst:: output_channelBucket_influx =! 0 inconsistent with out_dt and NOAH_TIMESTEP choices.")
       if(output_channelBucket_influx .eq. 2 .and. GWBASESWCRT .ne. 1) &
            call hydro_stop("read_rt_nlst:: output_channelBucket_influx = 2 but GWBASESWCRT != 1.")
    end if

    if(CHANRTSWCRT .eq. 0 .and. channel_option .lt. 3) channel_option = 3

    nlst%RESTART_FILE = RESTART_FILE
    nlst%hydrotbl_f = trim(hydrotbl_f)
    nlst%SPLIT_OUTPUT_COUNT = SPLIT_OUTPUT_COUNT
    nlst%IGRID = IGRID
    nlst%io_config_outputs = io_config_outputs
    nlst%io_form_outputs = io_form_outputs
    nlst%t0OutputFlag = t0OutputFlag
    nlst%output_channelBucket_influx = output_channelBucket_influx
    nlst%geo_static_flnm = geo_static_flnm
    nlst%land_spatial_meta_flnm = land_spatial_meta_flnm
    nlst%out_dt = out_dt
    nlst%rst_dt = rst_dt
    nlst%DEEPGWSPIN = DEEPGWSPIN
    nlst%SOLVEG_INITSWC = SOLVEG_INITSWC
    nlst%reservoir_obs_dir = "testDirectory"

    write(nlst%hgrid,'(I1)') igrid

    if(RESTART_FILE .eq. "") rst_typ = 0

    if(rst_bi_out .eq. 1) then
       ! This part works for intel not pgi
       !     inquire(directory='restart', exist=dir_e)
       inquire(file='restart/.', exist=dir_e)
       if(.not. dir_e) then
          call system('mkdir restart')
       endif
    endif


    if(channel_option .eq. 4) then
       CHANRTSWCRT = 0
       OVRTSWCRT = 0
       SUBRTSWCRT = 0
    endif

    nlst%CHRTOUT_DOMAIN = CHRTOUT_DOMAIN
    nlst%CHANOBS_DOMAIN = CHANOBS_DOMAIN
    nlst%output_gw      = output_gw
    nlst%outlake      = outlake
    nlst%frxst_pts_out = frxst_pts_out
    nlst%CHRTOUT_GRID = CHRTOUT_GRID
    nlst%LSMOUT_DOMAIN = LSMOUT_DOMAIN
    nlst%RTOUT_DOMAIN = RTOUT_DOMAIN
    nlst%RT_OPTION = RT_OPTION
    nlst%CHANRTSWCRT = CHANRTSWCRT
    nlst%GW_RESTART  = GW_RESTART
    nlst%RSTRT_SWC   = RSTRT_SWC
    nlst%channel_option = channel_option
    nlst%DTRT_TER   = DTRT_TER
    nlst%DTRT_CH   = DTRT_CH
    nlst%DTCT      = DTRT_CH   ! small time step for grid based channel routing

    nlst%SUBRTSWCRT = SUBRTSWCRT
    nlst%OVRTSWCRT = OVRTSWCRT
    nlst%dxrt0 = dxrt
    nlst%AGGFACTRT = AGGFACTRT
    nlst%GWBASESWCRT = GWBASESWCRT
    nlst%GWSOILCPL= GWSOILCPL
    nlst%gwChanCondSw = gwChanCondSw
    nlst%gwChanCondConstIn = gwChanCondConstIn
    nlst%gwChanCondConstOut = gwChanCondConstOut
    nlst%gwIhShift = gwIhShift
    nlst%GwSpinCycles = GwSpinCycles
    nlst%GwPreCycles = GwPreCycles
    nlst%GwPreDiag = GwPreDiag
    nlst%GwSpinUp = GwSpinUp
    nlst%GwPreDiagInterval = GwPreDiagInterval
    nlst%TERADJ_SOLAR = TERADJ_SOLAR
    nlst%sys_cpl = sys_cpl
    nlst%rst_typ = rst_typ
    nlst%rst_bi_in = rst_bi_in
    nlst%rst_bi_out = rst_bi_out
    nlst%order_to_write = order_to_write
    nlst%compound_channel = compound_channel

    ! files
    nlst%route_topo_f   =  route_topo_f
    nlst%route_chan_f = route_chan_f
    nlst%route_link_f = route_link_f
    nlst%route_lake_f =route_lake_f
    nlst%route_direction_f =  route_direction_f
    nlst%route_order_f =  route_order_f
    nlst%gwbasmskfil =  gwbasmskfil
    nlst%gwstrmfil =  gwstrmfil
    nlst%geo_finegrid_flnm =  geo_finegrid_flnm
    nlst%udmap_file =  udmap_file
    nlst%UDMP_OPT = UDMP_OPT
    nlst%GWBUCKPARM_file =  GWBUCKPARM_file
    nlst%reservoir_data_ingest = 0 ! STUB FOR USE OF REALTIME RESERVOIR DISCHARGE DATA. CURRENTLY NOT IN USE.
    nlst%reservoir_obs_dir = 'testDirectory'
#ifdef WRF_HYDRO_NUDGING
    nlst%nudgingParamFile       = nudgingParamFile
    nlst%netWkReExFile          = netWkReExFile
    nlst%readTimesliceParallel  = readTimesliceParallel
    nlst%temporalPersistence    = temporalPersistence
    nlst%persistBias            = persistBias
    nlst%biasWindowBeforeT0     = biasWindowBeforeT0
    nlst%nudgingLastObsFile     = nudgingLastObsFile
    nlst%timeSlicePath          = timeSlicePath
    nlst%nLastObs               = nLastObs
    nlst%minNumPairsBiasPersist = minNumPairsBiasPersist
    nlst%maxAgePairsBiasPersist = maxAgePairsBiasPersist
    nlst%invDistTimeWeightBias  = invDistTimeWeightBias
    nlst%noConstInterfBias      = noConstInterfBias
#endif

    call nlst%check()

    ! derive rtFlag
    nlst%rtFlag = 1
    if(channel_option .eq. 4) nlst%rtFlag = 0
    !      if(CHANRTSWCRT .eq. 0 .and.  SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0
    if(SUBRTSWCRT .eq. 0 .and. OVRTSWCRT .eq. 0 .and. GWBASESWCRT .eq. 0) nlst%rtFlag = 0

  end subroutine init_namelist_rt_field

  subroutine noah_lsm_sync(mod_noah_lsm)
    implicit none

    type(NOAHLSM_OFFLINE_DT) :: mod_noah_lsm

    noah_lsm_file%indir = mod_noah_lsm%indir
    noah_lsm_file%nsoil = mod_noah_lsm%nsoil ! number of soil layers
    noah_lsm_file%forcing_timestep = mod_noah_lsm%forcing_timestep
    noah_lsm_file%noah_timestep = mod_noah_lsm%noah_timestep
    noah_lsm_file%start_year = mod_noah_lsm%start_year
    noah_lsm_file%start_month = mod_noah_lsm%start_month
    noah_lsm_file%start_day = mod_noah_lsm%start_day
    noah_lsm_file%start_hour = mod_noah_lsm%start_hour
    noah_lsm_file%start_min = mod_noah_lsm%start_min
    noah_lsm_file%outdir = mod_noah_lsm%outdir
    noah_lsm_file%restart_filename_requested = mod_noah_lsm%restart_filename_requested
    noah_lsm_file%restart_frequency_hours = mod_noah_lsm%restart_frequency_hours
    noah_lsm_file%output_timestep = mod_noah_lsm%output_timestep
    noah_lsm_file%dynamic_veg_option = mod_noah_lsm%dynamic_veg_option
    noah_lsm_file%canopy_stomatal_resistance_option = mod_noah_lsm%canopy_stomatal_resistance_option
    noah_lsm_file%btr_option = mod_noah_lsm%btr_option
    noah_lsm_file%runoff_option = mod_noah_lsm%runoff_option
    noah_lsm_file%surface_drag_option = mod_noah_lsm%surface_drag_option
    noah_lsm_file%supercooled_water_option = mod_noah_lsm%supercooled_water_option
    noah_lsm_file%frozen_soil_option = mod_noah_lsm%frozen_soil_option
    noah_lsm_file%radiative_transfer_option = mod_noah_lsm%radiative_transfer_option
    noah_lsm_file%snow_albedo_option = mod_noah_lsm%snow_albedo_option
    noah_lsm_file%pcp_partition_option = mod_noah_lsm%pcp_partition_option
    noah_lsm_file%tbot_option = mod_noah_lsm%tbot_option
    noah_lsm_file%temp_time_scheme_option = mod_noah_lsm%temp_time_scheme_option
    noah_lsm_file%glacier_option = mod_noah_lsm%glacier_option
    noah_lsm_file%surface_resistance_option = mod_noah_lsm%surface_resistance_option
    noah_lsm_file%split_output_count = mod_noah_lsm%split_output_count
    noah_lsm_file%khour = mod_noah_lsm%khour
    noah_lsm_file%kday = mod_noah_lsm%kday
    noah_lsm_file%zlvl = mod_noah_lsm%zlvl
    noah_lsm_file%hrldas_setup_file = mod_noah_lsm%hrldas_setup_file
    noah_lsm_file%mmf_runoff_file = mod_noah_lsm%mmf_runoff_file
    noah_lsm_file%external_veg_filename_template = mod_noah_lsm%external_veg_filename_template
    noah_lsm_file%external_lai_filename_template = mod_noah_lsm%external_lai_filename_template
    noah_lsm_file%xstart = mod_noah_lsm%xstart
    noah_lsm_file%ystart = mod_noah_lsm%ystart
    noah_lsm_file%xend = mod_noah_lsm%xend
    noah_lsm_file%yend = mod_noah_lsm%yend
    noah_lsm_file%soil_thick_input = mod_noah_lsm%soil_thick_input
    noah_lsm_file%rst_bi_out = mod_noah_lsm%rst_bi_out
    noah_lsm_file%rst_bi_in = mod_noah_lsm%rst_bi_in
    noah_lsm_file%spatial_filename = mod_noah_lsm%spatial_filename

  end subroutine noah_lsm_sync

  type(WRF_HYDRO_OFFLINE_DT) function copy_wrf_hydro()
    implicit none
    
    copy_wrf_hydro%finemesh = wrf_hydro_file%finemesh
    copy_wrf_hydro%finemesh_factor = wrf_hydro_file%finemesh_factor
    copy_wrf_hydro%forc_typ = wrf_hydro_file%forc_typ
    copy_wrf_hydro%snow_assim = wrf_hydro_file%snow_assim
    
  end function copy_wrf_hydro

  type(NOAHLSM_OFFLINE_DT) function copy_noah_lsm()
    implicit none

    copy_noah_lsm%indir = noah_lsm_file%indir
    copy_noah_lsm%nsoil = noah_lsm_file%nsoil ! number of soil layers
    copy_noah_lsm%forcing_timestep = noah_lsm_file%forcing_timestep
    copy_noah_lsm%noah_timestep = noah_lsm_file%noah_timestep
    copy_noah_lsm%start_year = noah_lsm_file%start_year
    copy_noah_lsm%start_month = noah_lsm_file%start_month
    copy_noah_lsm%start_day = noah_lsm_file%start_day
    copy_noah_lsm%start_hour = noah_lsm_file%start_hour
    copy_noah_lsm%start_min = noah_lsm_file%start_min
    copy_noah_lsm%outdir = noah_lsm_file%outdir
    copy_noah_lsm%restart_filename_requested = noah_lsm_file%restart_filename_requested
    copy_noah_lsm%restart_frequency_hours = noah_lsm_file%restart_frequency_hours
    copy_noah_lsm%output_timestep = noah_lsm_file%output_timestep
    copy_noah_lsm%dynamic_veg_option = noah_lsm_file%dynamic_veg_option
    copy_noah_lsm%canopy_stomatal_resistance_option = noah_lsm_file%canopy_stomatal_resistance_option
    copy_noah_lsm%btr_option = noah_lsm_file%btr_option
    copy_noah_lsm%runoff_option = noah_lsm_file%runoff_option
    copy_noah_lsm%surface_drag_option = noah_lsm_file%surface_drag_option
    copy_noah_lsm%supercooled_water_option = noah_lsm_file%supercooled_water_option
    copy_noah_lsm%frozen_soil_option = noah_lsm_file%frozen_soil_option
    copy_noah_lsm%radiative_transfer_option = noah_lsm_file%radiative_transfer_option
    copy_noah_lsm%snow_albedo_option = noah_lsm_file%snow_albedo_option
    copy_noah_lsm%pcp_partition_option = noah_lsm_file%pcp_partition_option
    copy_noah_lsm%tbot_option = noah_lsm_file%tbot_option
    copy_noah_lsm%temp_time_scheme_option = noah_lsm_file%temp_time_scheme_option
    copy_noah_lsm%glacier_option = noah_lsm_file%glacier_option
    copy_noah_lsm%surface_resistance_option = noah_lsm_file%surface_resistance_option
    copy_noah_lsm%split_output_count = noah_lsm_file%split_output_count
    copy_noah_lsm%khour = noah_lsm_file%khour
    copy_noah_lsm%kday = noah_lsm_file%kday
    copy_noah_lsm%zlvl = noah_lsm_file%zlvl
    copy_noah_lsm%hrldas_setup_file = noah_lsm_file%hrldas_setup_file
    copy_noah_lsm%mmf_runoff_file = noah_lsm_file%mmf_runoff_file
    copy_noah_lsm%external_veg_filename_template = noah_lsm_file%external_veg_filename_template
    copy_noah_lsm%external_lai_filename_template = noah_lsm_file%external_lai_filename_template
    copy_noah_lsm%xstart = noah_lsm_file%xstart
    copy_noah_lsm%ystart = noah_lsm_file%ystart
    copy_noah_lsm%xend = noah_lsm_file%xend
    copy_noah_lsm%yend = noah_lsm_file%yend
    copy_noah_lsm%soil_thick_input = noah_lsm_file%soil_thick_input
    copy_noah_lsm%rst_bi_out = noah_lsm_file%rst_bi_out
    copy_noah_lsm%rst_bi_in = noah_lsm_file%rst_bi_in
    copy_noah_lsm%spatial_filename = noah_lsm_file%spatial_filename
    
  end function copy_noah_lsm

  subroutine init_wrf_hydro()
    implicit none

    integer  :: ierr
    integer  :: finemesh, finemesh_factor
    integer  :: forc_typ, snow_assim

    namelist /WRF_HYDRO_OFFLINE/ &
         !LRK - Remove HRLDAS_ini_typ and GEO_STATIC_FLNM for WRF-Hydro
         finemesh,finemesh_factor,forc_typ, snow_assim
    !finemesh,finemesh_factor,forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ

#ifndef NCEP_WCOSS
    read(30, NML=WRF_HYDRO_OFFLINE, iostat=ierr)
#else
    read(11, NML=WRF_HYDRO_OFFLINE, iostat=ierr)
#endif
    if (ierr /= 0) then
       write(*,'(/," ***** ERROR: Problem reading namelist WRF_HYDRO_OFFLINE",/)')
       call hydro_stop (" FATAL ERROR: Problem reading namelist WRF_HYDRO_OFFLINE")
    endif

#ifndef NCEP_WCOSS
    close(30)
#else
    close(11)
#endif

    wrf_hydro_file%finemesh = finemesh
    wrf_hydro_file%finemesh_factor = finemesh_factor
    wrf_hydro_file%forc_typ = forc_typ
    wrf_hydro_file%snow_assim = snow_assim

  end subroutine init_wrf_hydro

  subroutine init_noah_lsm()
    implicit none
     character(len=256) :: indir
     integer            :: nsoil ! number of soil layers
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
     integer            :: glacier_option
     integer            :: surface_resistance_option
     integer            :: split_output_count = 1
     integer            :: khour
     integer            :: kday
     real               :: zlvl 
     character(len=256) :: hrldas_setup_file = " "
     character(len=256) :: mmf_runoff_file = " "
     character(len=256) :: external_veg_filename_template = " "
     character(len=256) :: external_lai_filename_template = " "
     integer            :: xstart = 1
     integer            :: ystart = 1
     integer            :: xend = 0
     integer            :: yend = 0
     REAL, DIMENSION(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
     integer :: rst_bi_out, rst_bi_in !0: default netcdf format. 1: binary write/read by each core.
     CHARACTER(LEN = 256)                    ::  spatial_filename
     integer :: ierr = 0

    namelist / NOAHLSM_OFFLINE /    &
         indir, nsoil, soil_thick_input, forcing_timestep, &
         noah_timestep, start_year, start_month, start_day, &
         start_hour, start_min, outdir, restart_filename_requested, &
         restart_frequency_hours, output_timestep, dynamic_veg_option, &
         canopy_stomatal_resistance_option, btr_option, runoff_option, &
         surface_drag_option, supercooled_water_option, &
         frozen_soil_option, radiative_transfer_option, snow_albedo_option, &
         pcp_partition_option, tbot_option, temp_time_scheme_option, &
         glacier_option, surface_resistance_option, &
         split_output_count, & 
         khour, kday, zlvl, hrldas_setup_file, mmf_runoff_file, &
         spatial_filename, &
         external_veg_filename_template, external_lai_filename_template, &
         xstart, xend, ystart, yend, rst_bi_out, rst_bi_in

    noah_lsm_file%nsoil                   = -999
    noah_lsm_file%soil_thick_input        = -999
    ! dtbl                             = -999
    noah_lsm_file%start_year              = -999
    noah_lsm_file%start_month             = -999
    noah_lsm_file%start_day               = -999
    noah_lsm_file%start_hour              = -999
    noah_lsm_file%start_min               = -999
    noah_lsm_file%khour                   = -999
    noah_lsm_file%kday                    = -999
    noah_lsm_file%zlvl                    = -999
    noah_lsm_file%forcing_timestep        = -999
    noah_lsm_file%noah_timestep           = -999
    noah_lsm_file%output_timestep         = -999
    noah_lsm_file%restart_frequency_hours = -999

#ifndef NCEP_WCOSS
    open(30, file="namelist.hrldas", form="FORMATTED")
    read(30, NML=NOAHLSM_OFFLINE, iostat=ierr)
#else
    open(11, form="FORMATTED")
    read(11, NML=NOAHLSM_OFFLINE, iostat=ierr)
#endif

    if (ierr /= 0) then
       write(*,'(/," ***** ERROR: Problem reading namelist NOAHLSM_OFFLINE",/)')
#ifndef NCEP_WCOSS
       rewind(30)
       read(30, NOAHLSM_OFFLINE)
#else
            rewind(11)
            read(11, NOAHLSM_OFFLINE)
#endif
       stop "FATAL ERROR: Problem reading namelist NOAHLSM_OFFLINE"
    endif

    noah_lsm_file%indir = indir 
    noah_lsm_file%nsoil = nsoil ! number of soil layers
    noah_lsm_file%forcing_timestep = forcing_timestep
    noah_lsm_file%noah_timestep = noah_timestep
    noah_lsm_file%start_year = start_year
    noah_lsm_file%start_month = start_month
    noah_lsm_file%start_day = start_day
    noah_lsm_file%start_hour = start_hour
    noah_lsm_file%start_min = start_min
    noah_lsm_file%outdir = outdir
    noah_lsm_file%restart_filename_requested = restart_filename_requested
    noah_lsm_file%restart_frequency_hours = restart_frequency_hours
    noah_lsm_file%output_timestep = output_timestep
    noah_lsm_file%dynamic_veg_option = dynamic_veg_option
    noah_lsm_file%canopy_stomatal_resistance_option = canopy_stomatal_resistance_option
    noah_lsm_file%btr_option = btr_option
    noah_lsm_file%runoff_option = runoff_option
    noah_lsm_file%surface_drag_option = surface_drag_option
    noah_lsm_file%supercooled_water_option = supercooled_water_option
    noah_lsm_file%frozen_soil_option = frozen_soil_option
    noah_lsm_file%radiative_transfer_option = radiative_transfer_option
    noah_lsm_file%snow_albedo_option = snow_albedo_option
    noah_lsm_file%pcp_partition_option = pcp_partition_option
    noah_lsm_file%tbot_option = tbot_option
    noah_lsm_file%temp_time_scheme_option = temp_time_scheme_option
    noah_lsm_file%glacier_option = glacier_option
    noah_lsm_file%surface_resistance_option = surface_resistance_option
    noah_lsm_file%split_output_count = split_output_count
    noah_lsm_file%khour = khour
    noah_lsm_file%kday = kday
    noah_lsm_file%zlvl = zlvl
    noah_lsm_file%hrldas_setup_file = hrldas_setup_file
    noah_lsm_file%mmf_runoff_file = mmf_runoff_file
    noah_lsm_file%external_veg_filename_template = external_veg_filename_template
    noah_lsm_file%external_lai_filename_template = external_lai_filename_template
    noah_lsm_file%xstart = xstart
    noah_lsm_file%ystart = ystart
    noah_lsm_file%xend = xend
    noah_lsm_file%yend = yend
    noah_lsm_file%soil_thick_input = soil_thick_input
    noah_lsm_file%rst_bi_out = rst_bi_out
    noah_lsm_file%rst_bi_in = rst_bi_in
    noah_lsm_file%spatial_filename = spatial_filename

      !dtbl = real(noah_timestep)

  end subroutine init_noah_lsm
  
end module config_base
