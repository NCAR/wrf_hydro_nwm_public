#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#ifndef DEBUG_LVL
#define DEBUG_LVL 3
#endif
#define UNINITIALIZED -999

module wrfhydro_nuopc_gluecode
! !MODULE: wrfhydro_nuopc_gluecode
!
! !DESCRIPTION:
!   This module connects NUOPC initialize, advance,
!   and finalize to WRFHYDRO.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
!--------- WRF-Hydro Field Connections -------------
! Standard Name: moisture_content_of_soil_layer
! Description  : TOTAL SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION) 3D Field
! Status       : Hooked Up / Hooked Up
! Hookup       : RT_DOMAIN(did)%SMC (noah%smc)
! ------------------------------------------
! Standard Name: temperature_of_soil_layer
! Description  : SOIL TEMP (K) 3D Field
! Status       : Hooked Up / Hooked Up
! Hookup       : RT_DOMAIN(did)%stc (noah%stc)
! ------------------------------------------
! Standard Name: liquid_water_content_of_soil_layer
! Description  : UNFROZEN SOIL MOISTURE CONTENT (VOLUMETRIC FRACTION) 3D Field
! Status       : Hooked Up / Hooked Up
! Hookup       : RT_DOMAIN(did)%SH2OX (noah%sh2o)
! ------------------------------------------
! Standard Name: surface_runoff_flux
! Description  : Surface Runoff 2D Field
! Status       : Hooked Up / Not Needed
! Hookup       : RT_DOMAIN(did)%infxsrt (INFXS1RT)
! ------------------------------------------
! Standard Name: subsurface_runoff_flux
! Description  : Subsurface Runoff 2D Field
! Status       : Hooked Up / Not Needed
! Hookup       : RT_DOMAIN(did)%soldrain (SOLDRAIN)
! ------------------------------------------
! Standard Name: water_surface_height_above_reference_datum
! Description  : Surface Head 2D Field
! Status       : Not Needed / Hooked Up
! Hookup       : RT_DOMAIN(did)%sfcheadrt (SFHEAD1RT)
! ------------------------------------------
! !USES:
  use ESMF
  use NUOPC
  use module_mpp_land, only: &
    HYDRO_COMM_WORLD, &
    global_nx, &
    global_ny, &
    decompose_data_real, &
    write_io_real, my_id, &
    mpp_land_bcast_real1, &
    IO_id, &
    mpp_land_bcast_real, &
    mpp_land_bcast_int1, &
    MPP_LAND_INIT
  use module_HYDRO_drv, only: &
    HYDRO_ini, &
    HYDRO_exe
  use module_HYDRO_io, only: &
    get_file_dimension
  use module_CPL_LAND, only: &
    CPL_LAND_INIT, &
    cpl_outdate
  use module_rt_data, only: &
    rt_domain
  use module_namelist, only: &
    nlst_rt
  use wrfhydro_nuopc_addonutils
!  use module_gw_gw2d_data, only: &
!    gw2d
!  use module_domain, only: &
!    domain, &
!    domain_clock_get
!  use module_configure, only: &
!    grid_config_rec_type
!  use module_configure, only: &
!    config_flags
!  use module_configure, only: &
!    model_config_rec
  use module_mpp_land, only: &
    HYDRO_COMM_WORLD, &
    global_nx, &
    global_ny, &
    decompose_data_real, &
    write_io_real, &
    my_id, &
    mpp_land_bcast_real1, &
    IO_id, &
    mpp_land_bcast_real, &
    mpp_land_bcast_int1, &
    MPP_LAND_INIT

  private

  public :: wrfhydro_nuopc_ini
  public :: wrfhydro_nuopc_run
  public :: wrfhydro_nuopc_fin
  public :: WRFHYDRO_distgrid
  public :: WRFHYDRO_grid
  public :: WRFHYDRO_soilarrayspec
  public :: WRFHYDRO_nsoil

  ! HRLDAS Configuration
  character(len=ESMF_MAXPATHLEN) :: hrldasConfigFile = "namelist.hrldas"
  INTEGER, PARAMETER             :: hrldasConfigFH = 30

  ! PARAMETERS
  INTEGER, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist

  ! WRFHYDRO Config File
  type :: WRFHYDRO_ConfigFile
    character(len=256)                  :: indir = "UNINITIALIZED"
    character(len=256)                  :: GEO_STATIC_FLNM = "UNINITIALIZED"
    integer                             :: nsoil = UNINITIALIZED
    integer                             :: start_year = UNINITIALIZED
    integer                             :: start_month = UNINITIALIZED
    integer                             :: start_day = UNINITIALIZED
    integer                             :: start_hour = UNINITIALIZED
    integer                             :: start_min = UNINITIALIZED
    integer                             :: FORCING_TIMESTEP = UNINITIALIZED
    integer                             :: NOAH_TIMESTEP = UNINITIALIZED
    integer                             :: OUTPUT_TIMESTEP = UNINITIALIZED
    real, dimension(MAX_SOIL_LEVELS)    :: soil_thick_input = UNINITIALIZED
  end type WRFHYDRO_ConfigFile

  ! Configuration
  type(WRFHYDRO_ConfigFile) :: configFile
  integer               :: slice = UNINITIALIZED
  integer               :: num_nests = UNINITIALIZED
  integer               :: num_tiles = UNINITIALIZED
  integer               :: did = UNINITIALIZED
  integer               :: nx_global = UNINITIALIZED
  integer               :: ny_global = UNINITIALIZED
  integer               :: x_start = UNINITIALIZED
  integer               :: x_end = UNINITIALIZED
  integer               :: y_start = UNINITIALIZED
  integer               :: y_end = UNINITIALIZED
  integer               :: nx_local = UNINITIALIZED
  integer               :: ny_local = UNINITIALIZED
  integer               :: sf_surface_physics = UNINITIALIZED
  real,dimension(:),allocatable       :: zs ! zoil layer depths
  integer,dimension(:,:), allocatable :: IVGTYP, isltyp

  ! Public State
  type(ESMF_DistGrid), save   :: WRFHYDRO_distgrid
  type(ESMF_Grid), save       :: WRFHYDRO_grid
  type(ESMF_ArraySpec), save  :: WRFHYDRO_soilarrayspec
  integer, save               :: WRFHYDRO_nsoil = UNINITIALIZED

  ! added to consider the adaptive time step from driver.
  real                  :: dt0 = UNINITIALIZED
  real                  :: dtrt0 = UNINITIALIZED
  integer               :: mm0 = UNINITIALIZED
  integer               :: mm = UNINITIALIZED
  ! added for check soil moisture and soiltype
  integer               :: checkSOIL_flag = UNINITIALIZED
  ! added to track the driver clock
  character(len=19)     :: start_time = "0000-00-00_00:00:00"

  ! some temporary debug variables
  character(len=256)  :: msgString
  INTEGER, PARAMETER  :: dbug_flag = 5

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(vm,rc)
    type(ESMF_VM)                   :: vm
    integer, intent(out)            :: rc

    ! local variables
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                     :: ntime
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    real                        :: dt, dtrt
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:nuopc_cpl_HYDRO_ini)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    ! Initialize run values (TBD Read from Config)
    did = 1
    num_nests = 0
    sf_surface_physics = 0
    num_tiles = 1
    slice = 0

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Read information from config file
    call WRFHYDRO_ConfigFileRead(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call WRFHYDRO_ConfigFilePrint(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Check number of soil layers
    if(configFile%nsoil .lt. 1) then
      call ESMF_LogWrite(msg=SUBNAME//": nsoil layers less than 1", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    elseif(configFile%nsoil .gt. MAX_SOIL_LEVELS) then
      call ESMF_LogWrite(msg=SUBNAME//": nsoil layers greater than MAX_SOIL_LEVELS", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    ! Allocate Memory & Initialize Soil Layer Depths
    allocate(zs(configFile%nsoil))
    zs(1) = 0-configFile%soil_thick_input(1)
    do i=2,configFile%nsoil
      zs(i) = zs(i-1)-configFile%soil_thick_input(i)
    enddo

    ! Set Model Soil Depths (Must be negative)
    nlst_rt(did)%nsoil = configFile%nsoil
    call mpp_land_bcast_int1 (nlst_rt(did)%nsoil)
    allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
    if(zs(1) <  0) then
      call ESMF_LogWrite(msg=SUBNAME//": zs(1) negative - no change", &
        logmsgFlag=ESMF_LOGMSG_INFO)
      nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = zs(1:nlst_rt(did)%nsoil)
    else
      call ESMF_LogWrite(msg=SUBNAME//": zs(1) positive - converting to negative", &
        logmsgFlag=ESMF_LOGMSG_INFO)
      nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = -1*zs(1:nlst_rt(did)%nsoil)
    endif
    ! Set Soil Layer Value
    WRFHYDRO_nsoil = configFile%nsoil

    ! Set Soil Array Spec
    call ESMF_ArraySpecSet(WRFHYDRO_soilarrayspec, rank=3, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call MPP_LAND_INIT()  ! required before get_file_dimension
    call get_file_dimension(fileName=configFile%GEO_STATIC_FLNM,ix=nx_global,jx=ny_global)

    allocate(connectionList(1))
    call ESMF_DistGridConnectionSet(connectionList(1), tileIndexA=1, &
      tileIndexB=1, positionVector=(/nx_global, 0/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Create DistGrid based on WRFHDYRO Config NX,NY
    WRFHYDRO_distgrid = ESMF_DistGridCreate(minIndex=(/1,1/), maxIndex=(/nx_global,ny_global/), &
!      indexflag = ESMF_INDEX_DELOCAL, &
!      deBlockList=deBlockList, &
!      deLabelList=deLabelList, &
!      delayout=delayout, &
!      connectionList=connectionList, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get the Local Decomp Incides
    call WRFHYDRO_SetLocalIndices(WRFHYDRO_distgrid,x_start,x_end, &
      y_start,y_end,nx_local,ny_local,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Allocate Memory & Initialize Vegetation Type and Soil Type
    ! To be implemented - Replace with read from config file read or coupling
    allocate(IVGTYP(x_start:x_end,y_start:y_end))
    allocate(isltyp(x_start:x_end,y_start:y_end))
    IVGTYP = 0
    isltyp = 0

    ! Create Grid using DistGrid and Set Local Decomp Coordinates
    WRFHYDRO_grid = WRFHYDRO_GridCreate(WRFHYDRO_distgrid, &
      configFile%GEO_STATIC_FLNM,nx_local,ny_local,x_start,y_start,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Initialize the time using WRFHYDRO Config File
    call ESMF_TimeSet(startTime, &
      yy = configFile%start_year, & ! Implicit kind conversions int to I4
      mm = configFile%start_month, &
      dd = configFile%start_day, &
      h = configFile%start_hour, & ! Implicit kind conversions int to I4
      m = configFile%start_min, & ! Implicit kind conversions int to I4
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call time_to_string(startTime,timestr=start_time,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    cpl_outdate = start_time(1:19)
    nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
    nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

!    ! Initialize the timestep using WRFHYDRO Config File
    dt = real(configFile%NOAH_TIMESTEP)
    nlst_rt(did)%dt = dt ! TBD pass in timestep from driver

    if(dt .le. 0) then
      call ESMF_LogWrite(msg=SUBNAME//": TimeStep less than 1 is not supported.", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(x_start, x_end, y_start, y_end)

    ! ntime used in HYDRO_ini
    ! Routing timestep set in HYDRO_ini
    ntime = 1
    if(sf_surface_physics .eq. 5) then
      ! clm4
      call HYDRO_ini(ntime,did=did,ix0=1,jx0=1)
    else
      call HYDRO_ini(ntime,did,ix0=nx_local,jx0=ny_local,vegtyp=IVGTYP,soltyp=isltyp)
    endif

    ! Adjust the routing timestep and factor
    ! At this point the coupling driver timestep is unknown
    ! and uses WRFHYDRO Config as best guess
    if(nlst_rt(did)%dtrt .ge. nlst_rt(did)%dt) then
       nlst_rt(did)%dtrt = nlst_rt(did)%dt
       mm = 1
    else
      if(mod(nlst_rt(did)%dt,nlst_rt(did)%dtrt) /= 0) then
        call ESMF_LogWrite(msg=SUBNAME//": Driver timestep is not a multiple of routing timestep", &
          logmsgFlag=ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
      endif
      mm = nlst_rt(did)%dt/nlst_rt(did)%dtrt
    endif
    dt0 = nlst_rt(did)%dt
    dtrt0 = nlst_rt(did)%dtrt
    mm0 = mm

    RT_DOMAIN(did)%initialized = .true.

    call WRFHYDRO_ConfigPrint(rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call grid_print(WRFHYDRO_grid,'wrfhydro_',rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call grid_write(WRFHYDRO_grid, 'array_wrfhydro', rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_run(clock,importState,exportState,rc)
    type(ESMF_Clock),intent(in)     :: clock
    type(ESMF_State),intent(inout)  :: importState
    type(ESMF_State),intent(inout)  :: exportState
    integer, intent(out)            :: rc

    ! local variables
    character                   :: hgrid
    type(ESMF_TimeInterval)     :: timeStep
    real                        :: dt
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:nuopc_cpl_HYDRO_run)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS
    slice = slice+1

    if(.not. RT_DOMAIN(did)%initialized) then
      call ESMF_LogWrite(msg=SUBNAME//": Model has not been initialized!", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call clock_to_string(clock,timestr=cpl_outdate,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19) ! Current time is the

    dt = timeinterval_to_real(timeInterval=timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if(dt .le. 0) then
      call ESMF_LogWrite(msg=SUBNAME//": dt less than 1 is not supported.", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    nlst_rt(did)%dt = dt

    if((mm*nlst_rt(did)%dtrt) .ne. nlst_rt(did)%dt) then   ! NUOPC driver time step changed.
      call ESMF_LogWrite(msg=SUBNAME//": Driver timestep changed.", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      if(nlst_rt(did)%dtrt .ge. nlst_rt(did)%dt) then
        nlst_rt(did)%dtrt = nlst_rt(did)%dt
        call ESMF_LogWrite(msg=SUBNAME//": Routing timestep set to driver timestep.", &
          logmsgFlag=ESMF_LOGMSG_ERROR)
        mm = 1
      else
        if(mod(nlst_rt(did)%dt,nlst_rt(did)%dtrt) /= 0) then
          call ESMF_LogWrite(msg=SUBNAME//": New driver timestep is not a multiple of routing timestep", &
            logmsgFlag=ESMF_LOGMSG_ERROR)
          rc = ESMF_RC_ARG_OUTOFRANGE
          return  ! bail out
        endif
        mm = nlst_rt(did)%dt/nlst_rt(did)%dtrt
      endif
    endif

    if(nlst_rt(did)%SUBRTSWCRT .eq.0  .and. &
      nlst_rt(did)%OVRTSWCRT .eq. 0 .and. &
      nlst_rt(did)%GWBASESWCRT .eq. 0) then
      call ESMF_LogWrite(msg=SUBNAME//": SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    if((.not. RT_DOMAIN(did)%initialized) .and. (nlst_rt(did)%rst_typ .eq. 1) ) then
      call ESMF_LogWrite(msg=SUBNAME//": Restart initial data from offline file.", &
        logmsgFlag=ESMF_LOGMSG_INFO)
    else

      call WRFHYDRO_ModelStatePrint(did, slice,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      ! Convert domain integer id to string
      write(hgrid,"(I1)") nlst_rt(did)%IGRID
      ! Read offline forcing data
      call ESMF_LogWrite(msg=SUBNAME//": Calling read_forc_ldasout", &
        logmsgFlag=ESMF_LOGMSG_INFO)
      call read_forc_ldasout(nlst_rt(did)%olddate(1:19),hgrid, &
        trim(configFile%indir), nlst_rt(did)%dt, &
        rt_domain(did)%ix,rt_domain(did)%jx, &
        rt_domain(did)%infxsrt,rt_domain(did)%soldrain)

      ! Copy the data from NUOPC fields
      call WRFHYDRO_CopyImportFields(did, importState, rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! Call the WRF-HYDRO run routine
    call ESMF_LogWrite(msg=SUBNAME//": Calling HYDRO_exe", &
      logmsgFlag=ESMF_LOGMSG_INFO)
    call HYDRO_exe(did=did)

    !! Copy the data to NUOPC fields
    call WRFHYDRO_CopyExportFields(did, exportState, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
    !if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
      !Wei Yu: comment the following two lines. Not ready
    !yw     qsgw(x_start(1):x_end(1),y_start(1):y_end(1)) = gw2d(did)%qsgw
    !yw     config_flags%gwsoilcpl = nlst_rt(did)%gwsoilcpl
    !end if

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_fin(rc)
    ! ARGUMENTES
    integer, intent(out)            :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:nuopc_cpl_HYDRO_fin)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    deallocate(zs)
    deallocate(IVGTYP)
    deallocate(isltyp)

    RT_DOMAIN(did)%initialized = .false.

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_CopyImportFields(did,importState, rc)
    ! ARGUMENTS
    integer, intent(in)             :: did
    type(ESMF_State), intent(inout) :: importState
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_CopyImportFields)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    !! Land forcing fields
    if(state_isfieldconnected(importState, "temperature_of_soil_layer_1", rc)) then
      call copy_data_layer(importState, "temperature_of_soil_layer_1", rt_domain(did)%STC, 'i',1, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "temperature_of_soil_layer_2", rc)) then
      call copy_data_layer(importState, "temperature_of_soil_layer_2", rt_domain(did)%STC, 'i',2, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "temperature_of_soil_layer_3", rc)) then
      call copy_data_layer(importState, "temperature_of_soil_layer_3", rt_domain(did)%STC, 'i',3, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "temperature_of_soil_layer_4", rc)) then
      call copy_data_layer(importState, "temperature_of_soil_layer_4", rt_domain(did)%STC, 'i',4, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "moisture_content_of_soil_layer_1", rc)) then
      call copy_data_layer(importState, "moisture_content_of_soil_layer_1", rt_domain(did)%smc, 'i',1, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "moisture_content_of_soil_layer_2", rc)) then
      call copy_data_layer(importState, "moisture_content_of_soil_layer_2", rt_domain(did)%smc, 'i',2, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "moisture_content_of_soil_layer_3", rc)) then
      call copy_data_layer(importState, "moisture_content_of_soil_layer_3", rt_domain(did)%smc, 'i',3, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "moisture_content_of_soil_layer_4", rc)) then
      call copy_data_layer(importState, "moisture_content_of_soil_layer_4", rt_domain(did)%smc, 'i',4, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "liquid_water_content_of_soil_layer_1", rc)) then
      call copy_data_layer(importState, "liquid_water_content_of_soil_layer_1", rt_domain(did)%sh2ox, 'i',1, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "liquid_water_content_of_soil_layer_2", rc)) then
      call copy_data_layer(importState, "liquid_water_content_of_soil_layer_2", rt_domain(did)%sh2ox, 'i',2, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "liquid_water_content_of_soil_layer_3", rc)) then
      call copy_data_layer(importState, "liquid_water_content_of_soil_layer_3", rt_domain(did)%sh2ox, 'i',3, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "liquid_water_content_of_soil_layer_4", rc)) then
      call copy_data_layer(importState, "liquid_water_content_of_soil_layer_4", rt_domain(did)%sh2ox, 'i',4, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "surface_runoff_flux", rc)) then
      call copy_data_2D(importState, "surface_runoff_flux", rt_domain(did)%infxsrt, 'i', rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(importState, "subsurface_runoff_flux", rc)) then
      call copy_data_2D(importState, "subsurface_runoff_flux", rt_domain(did)%soldrain, 'i', rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    !! Meterological forcing fields
!    if(state_isfieldconnected(importState, "inst_down_lw_flx", rc)) then
!      call copy_data_2D(importState, "inst_down_lw_flx", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_down_sw_flx", rc)) then
!      call copy_data_2D(importState, "inst_down_sw_flx", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_merid_wind_height_lowest", rc)) then
!      call copy_data_2D(importState, "inst_merid_wind_height_lowest", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_pres_height_surface", rc)) then
!      call copy_data_2D(importState, "inst_pres_height_surface", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_spec_humid_height_lowest", rc)) then
!      call copy_data_2D(importState, "inst_spec_humid_height_lowest", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_temp_height_lowest", rc)) then
!      call copy_data_2D(importState, "inst_temp_height_lowest", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "inst_zonal_wind_height_lowest", rc)) then
!      call copy_data_2D(importState, "inst_zonal_wind_height_lowest", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif
!    if(state_isfieldconnected(importState, "mean_prec_rate", rc)) then
!      call copy_data_2D(importState, "mean_prec_rate", UNKNOWN, 'i', rc)
!      if (ESMF_STDERRORCHECK(rc)) return
!    endif

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_CopyExportFields(did, exportState, rc)
    ! ARGUMENTS
    integer, intent(in)             :: did
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_CopyExportFields)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    !! Feedback for land
    if(state_isfieldconnected(exportState, "liquid_water_content_of_soil_layer_1", rc)) then
      call copy_data_layer(exportState, "liquid_water_content_of_soil_layer_1", rt_domain(did)%sh2ox, 'e',1, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(exportState, "liquid_water_content_of_soil_layer_2", rc)) then
      call copy_data_layer(exportState, "liquid_water_content_of_soil_layer_2", rt_domain(did)%sh2ox, 'e',2, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(exportState, "liquid_water_content_of_soil_layer_3", rc)) then
      call copy_data_layer(exportState, "liquid_water_content_of_soil_layer_3", rt_domain(did)%sh2ox, 'e',3, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
    if(state_isfieldconnected(exportState, "liquid_water_content_of_soil_layer_4", rc)) then
      call copy_data_layer(exportState, "liquid_water_content_of_soil_layer_4", rt_domain(did)%sh2ox, 'e',4, rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif
!    if(state_isfieldconnected(exportState, "volume_fraction_of_total_water_in_soil", rc)) then
!      call copy_data_2D(exportState, "volume_fraction_of_total_water_in_soil", UNKNOWN, 'e', rc)
!      if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    endif
!    if(state_isfieldconnected(exportState, "surface_snow_thickness", rc)) then
!      call copy_data_2D(exportState, "surface_snow_thickness", UNKNOWN, 'e', rc)
!      if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    endif
!    if(state_isfieldconnected(exportState, "liquid_water_content_of_surface_snow", rc)) then
!      call copy_data_2D(exportState, "liquid_water_content_of_surface_snow", UNKNOWN, 'e', rc)
!      if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    endif

    !! Feedback for atmosphere
!    if(state_isfieldconnected(exportState, "dummyfield", rc)) then
!      call copy_data_2D(exportState, "dummyfield", UNKNOWN, 'e', rc)
!      if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    endif

    !! Other export fields
!    if(state_isfieldconnected(exportState, "water_surface_height_above_reference_datum", rc)) then
!      call copy_data_2D(exportState, "water_surface_height_above_reference_datum", rt_domain(did)%sfcheadrt, 'e', rc)
!      if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    endif

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_ConfigFileRead(rc)
    integer, intent(out)            :: rc

    ! Local Variables
    integer                     :: ierr
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_ConfigFileRead)'

    ! NOAHLSM_OFFLINE namelist variables for NoahMP

    integer  :: finemesh, finemesh_factor
    integer :: forc_typ, snow_assim
    character(len=256) :: GEO_STATIC_FLNM
    integer :: HRLDAS_ini_typ
    character(len=256) :: indir
    integer            :: nsoil
    real, dimension(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
    integer            :: forcing_timestep, noah_timestep
    integer            :: start_year, start_month, start_day, start_hour, start_min
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
    integer            :: khour, kday
    real               :: zlvl
    character(len=256) :: hrldas_constants_file = " "
    character(len=256) :: mmf_runoff_file = " "
    character(len=256) :: external_fpar_filename_template = " "
    character(len=256) :: external_lai_filename_template = " "
    integer            :: xstart = 1, ystart = 1, xend = 0, yend = 0

    namelist / NOAHLSM_OFFLINE /    &
    finemesh,finemesh_factor,forc_typ, snow_assim , GEO_STATIC_FLNM, HRLDAS_ini_typ, &
    indir, nsoil, soil_thick_input, forcing_timestep, noah_timestep, &
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

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS
    configFile%nsoil = 0
    configFile%indir = " "
    configFile%GEO_STATIC_FLNM = " "
    configFile%start_year = 0
    configFile%start_month = 0
    configFile%start_day = 0
    configFile%start_hour = 0
    configFile%start_min = 0
    configFile%FORCING_TIMESTEP = 0
    configFile%NOAH_TIMESTEP = 0
    configFile%OUTPUT_TIMESTEP = 0
    configFile%soil_thick_input = 0

    open(hrldasConfigFH, file=trim(hrldasConfigFile), form="FORMATTED", iostat=ierr)
    if (ierr /= 0) then
      write (msgString,"(I5)") ierr
      call ESMF_LogWrite(msg=SUBNAME//": Error opening HRLDAS config file = " // trim(msgString), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    read(hrldasConfigFH, NOAHLSM_OFFLINE, iostat=ierr)
    if (ierr /= 0) then
      write (msgString,"(I5)") ierr
      call ESMF_LogWrite(msg=SUBNAME//": Error reading HRLDAS config file = " // trim(msgString), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    close (hrldasConfigFH, iostat=ierr )
    if (ierr /= 0) then
      write (msgString,"(I5)") ierr
      call ESMF_LogWrite(msg=SUBNAME//": Error closing hydro config file = " // trim(msgString), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    configFile%indir = trim(indir)
    configFile%GEO_STATIC_FLNM = trim(GEO_STATIC_FLNM)
    configFile%nsoil = nsoil
    configFile%start_year = start_year
    configFile%start_month = start_month
    configFile%start_day = start_day
    configFile%start_hour = start_hour
    configFile%start_min = start_min
    configFile%FORCING_TIMESTEP = forcing_timestep
    configFile%NOAH_TIMESTEP = noah_timestep
    configFile%OUTPUT_TIMESTEP = output_timestep
    configFile%soil_thick_input = soil_thick_input

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  function WRFHYDRO_GridCreate(distgrid,GEO_STATIC_FLNM,nx_local,ny_local,x_start,y_start,rc)
    ! ARGUMENTS
    type(ESMF_DistGrid),intent(in)  :: distgrid
    character(len=*),intent(in)     :: GEO_STATIC_FLNM
    integer,intent(in)              :: nx_local
    integer,intent(in)              :: ny_local
    integer,intent(in)              :: x_start
    integer,intent(in)              :: y_start
    integer, intent(out)            :: rc
    ! RETURN VALUE
    type(ESMF_Grid)                 :: WRFHYDRO_GridCreate
    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)          :: min_lat, max_lat, min_lon, max_lon
    real, allocatable           :: latitude(:,:), longitude(:,:)
    integer                     :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcorner(:,:)
    integer                     :: i,j, i1,j1
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_GridCreate)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    WRFHYDRO_GridCreate = ESMF_GridCreate(name='WRFHYDRO Grid', &
      distgrid=distgrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local))
    call get_geostatic_array("XLAT_M", GEO_STATIC_FLNM, latitude, &
      x_start, y_start, nx_local, ny_local, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local))
    call get_geostatic_array("XLONG_M", GEO_STATIC_FLNM, longitude, &
      x_start, y_start, nx_local, ny_local, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Centers
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local,ny_local)
    write (msgString, "(2F10.5)") min_lat, max_lat
    call ESMF_LogWrite(msg=SUBNAME//": XLAT_M(lower-left), XLAT_M(upper-right) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local,ny_local)
    write (msgString, "(2F10.5)") min_lon, max_lon
    call ESMF_LogWrite(msg=SUBNAME//": XLONG_M(lower-left), XLONG_M(upper-right) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    write (msgString, "(4I5)") lbnd(1), lbnd(2), ubnd(1), ubnd(2)
    call ESMF_LogWrite(msg=SUBNAME//": Center lbnd(1), lbnd(2), ubnd(1), ubnd(2) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcenter(i,j) = longitude(i,j)
      coordYcenter(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude)
    deallocate(longitude)

    ! CORNERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local+1,ny_local+1))
    call get_geostatic_array("XLAT_CORNER", GEO_STATIC_FLNM, latitude, &
      x_start, y_start, nx_local+1, ny_local+1, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local+1,ny_local+1))
    call get_geostatic_array("XLONG_CORNER", GEO_STATIC_FLNM, longitude, &
      x_start, y_start, nx_local+1, ny_local+1, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Corners
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local+1,ny_local+1)
    write (msgString, "(2F10.5)") min_lat, max_lat
    call ESMF_LogWrite(msg=SUBNAME//": XLAT_CORNER(lower-left), XLAT_CORNER(upper-right) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local+1,ny_local+1)
    write (msgString, "(2F10.5)") min_lon, max_lon
    call ESMF_LogWrite(msg=SUBNAME//": XLONG_CORNER(lower-left), XLONG_CORNER(upper-right) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    ! Add Corner Coordinates to Grid
    call ESMF_GridAddCoord(WRFHYDRO_GridCreate, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(WRFHYDRO_GridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    write (msgString, "(4I5)") lbnd(1), lbnd(2), ubnd(1), ubnd(2)
    call ESMF_LogWrite(msg=SUBNAME//": Corner lbnd(1), lbnd(2), ubnd(1), ubnd(2) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcorner(i,j) = longitude(i,j)
      coordYcorner(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude)
    deallocate(longitude)

    call WRFHYDRO_AddArea(WRFHYDRO_GridCreate, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end function

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_AddArea(grid, rc)
    type(ESMF_Grid), intent(inout)  :: grid
    integer, intent(out)            :: rc

    ! Local Variables
    integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
    type(ESMF_Field)                :: fieldArea
    type(ESMF_Array)                :: areaArray
    integer                         :: i,j
    integer                         :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer     :: radianarea(:,:)
    real(ESMF_KIND_R8), pointer     :: gridarea(:,:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_AddArea)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    fieldArea = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(fieldArea, localDE=0, &
      farrayPtr=radianarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridAddItem(grid, itemFlag=ESMF_GRIDITEM_AREA, itemTypeKind=ESMF_TYPEKIND_R8, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=gridarea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

     do j = lbnd(2),ubnd(2)
     do i = lbnd(1),ubnd(1)
       gridarea(i,j) = radianarea(i,j) * R * R
     enddo
     enddo

     call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine WRFHYDRO_SetLocalIndices(distgrid,x_start,x_end,y_start,y_end,nx_local,ny_local,rc)
    ! ARGUMENTS
    type(ESMF_DistGrid),intent(in)  :: distgrid
    integer, intent(out)            :: x_start
    integer, intent(out)            :: x_end
    integer, intent(out)            :: y_start
    integer, intent(out)            :: y_end
    integer, intent(out)            :: nx_local
    integer, intent(out)            :: ny_local
    integer, intent(out)            :: rc

    ! LOCAL VARIABLES
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_SetLocalIndices)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    !! Get VM Info to see if this will give me the PET info I need
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Get the grid distribution for this pet
    allocate(dimExtent(2, 0:(petCount - 1))) ! (dimCount, deCount)
    call ESMF_DistGridGet(distgrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(iIndexList(dimExtent(1, localPet)))
    call ESMF_DistGridGet(distgrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(jIndexList(dimExtent(2, localPet)))
    call ESMF_DistGridGet(distgrid, localDe=0, dim=2, &
      indexList=jIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    x_start = minVal(iIndexList)
    x_end   = maxVal(iIndexList)
    y_start = minVal(jIndexList)
    y_end   = maxVal(jIndexList)

    nx_local = x_end - x_start + 1
    ny_local = y_end - y_start + 1

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)

  end subroutine

subroutine WRFHYDRO_ConfigFilePrint(rc)
    ! ARGUMENTS
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_ConfigFilePrint)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    write (msgString, *) configFile%indir
    call ESMF_LogWrite(msg=SUBNAME//": INDIR = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%GEO_STATIC_FLNM
    call ESMF_LogWrite(msg=SUBNAME//": GEO_STATIC_FLNM = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%nsoil
    call ESMF_LogWrite(msg=SUBNAME//": nsoil = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%start_year
    call ESMF_LogWrite(msg=SUBNAME//": configFile%start_year = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%start_month
    call ESMF_LogWrite(msg=SUBNAME//": configFile%start_month = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%start_day
    call ESMF_LogWrite(msg=SUBNAME//": configFile%start_day = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%start_hour
    call ESMF_LogWrite(msg=SUBNAME//": configFile%start_hour = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%start_min
    call ESMF_LogWrite(msg=SUBNAME//": configFile%start_min = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%FORCING_TIMESTEP
    call ESMF_LogWrite(msg=SUBNAME//": configFile%FORCING_TIMESTEP = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%NOAH_TIMESTEP
    call ESMF_LogWrite(msg=SUBNAME//": configFile%NOAH_TIMESTEP = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%OUTPUT_TIMESTEP
    call ESMF_LogWrite(msg=SUBNAME//": configFile%OUTPUT_TIMESTEP = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write (msgString, *) configFile%soil_thick_input(1), configFile%soil_thick_input(configFile%nsoil)
    call ESMF_LogWrite(msg=SUBNAME//": configFile%soil_thick_input (1) (nsoil) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  subroutine WRFHYDRO_ConfigPrint(rc)
    ! ARGUMENTS
    integer         , intent(out) :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_ConfigPrint)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    write (msgString, *) did
    call ESMF_LogWrite(msg=SUBNAME//": did = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) num_nests
    call ESMF_LogWrite(msg=SUBNAME//": num_nests = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) num_tiles
    call ESMF_LogWrite(msg=SUBNAME//": num_tiles = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) sf_surface_physics
    call ESMF_LogWrite(msg=SUBNAME//": sf_surface_physics = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) checkSOIL_flag
    call ESMF_LogWrite(msg=SUBNAME//": checkSOIL_flag = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) zs(1), zs(configFile%nsoil)
    call ESMF_LogWrite(msg=SUBNAME//": zs(1), zs(configFile%nsoil) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) nx_global, ny_global
    call ESMF_LogWrite(msg=SUBNAME//": nx_global, ny_global = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) nx_local, ny_local
    call ESMF_LogWrite(msg=SUBNAME//": nx_local, ny_local = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) x_start, y_start, x_end, y_end
    call ESMF_LogWrite(msg=SUBNAME//": x_start, y_start, x_end, y_end = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) IVGTYP(x_start,y_start), IVGTYP(x_end,y_end)
    call ESMF_LogWrite(msg=SUBNAME//": IVGTYP(x_start,y_start), IVGTYP(x_end,y_end) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) isltyp(x_start,y_start), isltyp(x_end,y_end)
    call ESMF_LogWrite(msg=SUBNAME//": isltyp(x_start,y_start), isltyp(x_end,y_end) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, *) cpl_outdate
    call ESMF_LogWrite(msg=SUBNAME//": cpl_outdate = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  subroutine WRFHYDRO_ModelStatePrint(did,slice,rc)
    ! ARGUMENTS
    integer         , intent(in)  :: did
    integer         , intent(in)  :: slice
    integer         , intent(out) :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc_gluecode:WRFHYDRO_ModelStatePrint)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    write (msgString, *) slice
    call ESMF_LogWrite(msg=SUBNAME//": slice "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString, "(I2)") nlst_rt(did)%sys_cpl
    call ESMF_LogWrite(msg=SUBNAME//": sys_cpl is "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString,*) nlst_rt(did)%zsoil8
    call ESMF_LogWrite(msg=SUBNAME//": soil depths nlst_rt(did)%zsoil8 = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString,*) nlst_rt(did)%startdate(1:19)
    call ESMF_LogWrite(msg=SUBNAME//": nlst_rt(did)%startdate(1:19) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString,*) nlst_rt(did)%olddate(1:19)
    call ESMF_LogWrite(msg=SUBNAME//": nlst_rt(did)%olddate(1:19) = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString,*) nlst_rt(did)%dt
    call ESMF_LogWrite(msg=SUBNAME//": nlst_rt(did)%dt = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write (msgString,*) nlst_rt(did)%dtrt
    call ESMF_LogWrite(msg=SUBNAME//": nlst_rt(did)%dtrt = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write(msgString,*) nlst_rt(did)%IGRID
    call ESMF_LogWrite(msg=SUBNAME//": nlst_rt(did)%igrid = " // trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write(msgString,*) rt_domain(did)%ix, rt_domain(did)%jx
    call ESMF_LogWrite(msg=SUBNAME//": rt_domain(did)%ix, rt_domain(did)%jx = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write(msgString,*) shape(rt_domain(did)%infxsrt)
    call ESMF_LogWrite(msg=SUBNAME//": shape(rt_domain(did)%infxsrt) = " // trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)
    write(msgString,*) shape(rt_domain(did)%soldrain)
    call ESMF_LogWrite(msg=SUBNAME//": shape(rt_domain(did)%soldrain) = " // trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

end module
