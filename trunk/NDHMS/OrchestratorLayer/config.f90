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

  type, public :: Configuration_
   contains
     procedure, nopass :: init => config_init
     procedure, nopass :: noah_lsm => copy_noah_lsm
     procedure, nopass :: noah_lsm_sync => noah_lsm_sync
     procedure, nopass :: wrf_hydro => copy_wrf_hydro
  end type Configuration_

  type(NOAHLSM_OFFLINE_DT), private, save :: noah_lsm_file
  type(WRF_HYDRO_OFFLINE_DT), private, save :: wrf_hydro_file
    
contains

  subroutine config_init()
    implicit none

    call init_noah_lsm()
    call init_wrf_hydro()

  end subroutine config_init

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
