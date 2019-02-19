module config_base
  use netcdf_layer_base
  implicit none

  private

  type(NOAHLSM_OFFLINE_DT), save :: noah_lsm

  public
  
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
     integer            ::   xend = 0
     integer            ::   yend = 0
     integer, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist
     REAL, DIMENSION(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
     integer :: rst_bi_out, rst_bi_in !0: default netcdf format. 1: binary write/read by each core.
  end type NOAHLSM_OFFLINE_DT

  type :: Configuration_
   contains
     subroutine config_init()
  end type Configuration_

  namelist / NOAHLSM_OFFLINE /    &
       noah_lsm%indir, noah_lsm%nsoil, noah_lsm%soil_thick_input, noah_lsm%forcing_timestep, &
       & noah_lsm%noah_timestep, noah_lsm%start_year, noah_lsm%start_month, noah_lsm%start_day, &
       & noah_lsm%start_hour, noah_lsm%start_min, noah_lsm%outdir, noah_lsm%restart_filename_requested, &
       & noah_lsm%restart_frequency_hours, noah_lsm%output_timestep, noah_lsm%dynamic_veg_option, &
       & noah_lsm%canopy_stomatal_resistance_option, noah_lsm%btr_option, noah_lsm%runoff_option, &
       & noah_lsm%surface_drag_option, noah_lsm%supercooled_water_option, &
       & noah_lsm%frozen_soil_option, noah_lsm%radiative_transfer_option, noah_lsm%snow_albedo_option, &
       & noah_lsm%pcp_partition_option, noah_lsm%tbot_option, noah_lsm%temp_time_scheme_option, &
       & noah_lsm%glacier_option, noah_lsm%surface_resistance_option, &
       & noah_lsm%split_output_count, & 
       & noah_lsm%khour, noah_lsm%kday, noah_lsm%zlvl, noah_lsm%hrldas_setup_file, noah_lsm%mmf_runoff_file, &
       & noah_lsm%spatial_filename, &
       & noah_lsm%external_veg_filename_template, noah_lsm%external_lai_filename_template, &
       & noah_lsm%xstart, noah_lsm%xend, noah_lsm%ystart, noah_lsm%yend, noah_lsm%rst_bi_out, noah_lsm%rst_bi_in
    
contains

  subroutine config_init()
    implicit none

    call init_noah_lsm()
    
  end subroutine config_init

  subroutine init_noah_lsm()
    implicit none
    integer :: ierr = 0

    noah_lsm%nsoil                   = -999
    noah_lsm%soil_thick_input        = -999
    noah_lsm%dtbl                    = -999
    noah_lsm%start_year              = -999
    noah_lsm%start_month             = -999
    noah_lsm%start_day               = -999
    noah_lsm%start_hour              = -999
    noah_lsm%start_min               = -999
    noah_lsm%khour                   = -999
    noah_lsm%kday                    = -999
    noah_lsm%zlvl                    = -999
    noah_lsm%forcing_timestep        = -999
    noah_lsm%noah_timestep           = -999
    noah_lsm%output_timestep         = -999
    noah_lsm%restart_frequency_hours = -999
    
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

  end subroutine init_noah_lsm
  
end module config_base
