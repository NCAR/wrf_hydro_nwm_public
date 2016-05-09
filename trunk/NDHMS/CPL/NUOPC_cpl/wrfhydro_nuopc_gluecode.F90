#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "wrfhydro_nuopc_gluecode"
#define MODNAME "wrfhydro_nuopc_gluecode"

#define VERBOSITY_MIN 0
#define VERBOSITY_MAX 255
#define VERBOSITY_DBG 1023
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

  implicit none

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
  integer               :: num_nests = UNINITIALIZED
  integer               :: num_tiles = UNINITIALIZED
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
  type(ESMF_DistGrid)   :: WRFHYDRO_distgrid
  type(ESMF_Grid)       :: WRFHYDRO_grid
  type(ESMF_ArraySpec)  :: WRFHYDRO_soilarrayspec
  integer               :: WRFHYDRO_nsoil = UNINITIALIZED

  ! added to consider the adaptive time step from driver.
  real                  :: dt0 = UNINITIALIZED
  real                  :: dtrt0 = UNINITIALIZED
  integer               :: dt_factor0 = UNINITIALIZED
  integer               :: dt_factor = UNINITIALIZED
  ! added for check soil moisture and soiltype
  integer               :: checkSOIL_flag = UNINITIALIZED
  ! added to track the driver clock
  character(len=19)     :: start_time = "0000-00-00_00:00:00"

  ! some temporary debug variables
  character(len=ESMF_MAXSTR)  :: logMsg

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  subroutine wrfhydro_nuopc_ini(is,vm,rc)
    type(type_InternalState), intent(inout) :: is
    type(ESMF_VM),intent(in)                :: vm
    integer, intent(out)                    :: rc

    ! local variables
    type(ESMF_DistGridConnection), allocatable :: connectionList(:)
    integer                     :: ntime
    integer                     :: i
    type(ESMF_Time)             :: startTime
    type(ESMF_TimeInterval)     :: timeStep
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='nuopc_cpl_HYDRO_ini'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! Set mpiCommunicator for WRFHYDRO
    call ESMF_VMGet(vm, mpiCommunicator=HYDRO_COMM_WORLD, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Read information from config file
    call config_file_read(is,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call config_file_print(is,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Check number of soil layers
    if(configFile%nsoil .lt. 1) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Number of soil layers less than 1!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    elseif(configFile%nsoil .gt. MAX_SOIL_LEVELS) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Number of soil layers greater than MAX!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    ! Allocate Memory & Initialize Soil Layer Depths
    allocate(zs(configFile%nsoil))
    zs(1) = 0-configFile%soil_thick_input(1)
    do i=2,configFile%nsoil
      zs(i) = zs(i-1)-configFile%soil_thick_input(i)
    enddo

    ! Set Model Soil Depths (Must be negative)
    nlst_rt(is%wrap%nest)%nsoil = configFile%nsoil
    call mpp_land_bcast_int1 (nlst_rt(is%wrap%nest)%nsoil)
    allocate(nlst_rt(is%wrap%nest)%zsoil8(nlst_rt(is%wrap%nest)%nsoil))
    if(zs(1) <  0) then
      if (is%wrap%verbosity >= VERBOSITY_MAX) then
        call ESMF_LogWrite("zs(1) negative - no change", &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      endif
      nlst_rt(is%wrap%nest)%zsoil8(1:nlst_rt(is%wrap%nest)%nsoil) = zs(1:nlst_rt(is%wrap%nest)%nsoil)
    else
      if (is%wrap%verbosity >= VERBOSITY_MAX) then
        call ESMF_LogWrite("zs(1) positive - converting to negative", &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      endif
      nlst_rt(is%wrap%nest)%zsoil8(1:nlst_rt(is%wrap%nest)%nsoil) = -1*zs(1:nlst_rt(is%wrap%nest)%nsoil)
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
    call set_local_indices(is,WRFHYDRO_distgrid,x_start,x_end, &
      y_start,y_end,nx_local,ny_local,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Allocate Memory & Initialize Vegetation Type and Soil Type
    ! To be implemented - Replace with read from config file read or coupling
    allocate(IVGTYP(x_start:x_end,y_start:y_end))
    allocate(isltyp(x_start:x_end,y_start:y_end))
    IVGTYP = 0
    isltyp = 0

    ! Create Grid using DistGrid and Set Local Decomp Coordinates
    WRFHYDRO_grid = create_grid(is,WRFHYDRO_distgrid, &
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
    call time_to_string(is, startTime,timestr=start_time,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    cpl_outdate = start_time(1:19)
    nlst_rt(is%wrap%nest)%startdate(1:19) = cpl_outdate(1:19)
    nlst_rt(is%wrap%nest)%olddate(1:19) = cpl_outdate(1:19)

    ! Initialize the timestep using WRFHYDRO Config File
    ! TBD pass in timestep from driver
    nlst_rt(is%wrap%nest)%dt = real(configFile%NOAH_TIMESTEP)

    if(nlst_rt(is%wrap%nest)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Timestep less than 1 is not supported!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    ! Initialize the internal Land <-> Hydro Coupling
    call CPL_LAND_INIT(x_start, x_end, y_start, y_end)

    ! ntime used in HYDRO_ini
    ! Routing timestep set in HYDRO_ini
    ntime = 1
    if(sf_surface_physics .eq. 5) then
      ! clm4
      call HYDRO_ini(ntime,did=is%wrap%nest,ix0=1,jx0=1)
    else
      call HYDRO_ini(ntime,is%wrap%nest,ix0=nx_local,jx0=ny_local,vegtyp=IVGTYP,soltyp=isltyp)
    endif

    ! Adjust the routing timestep and factor
    ! At this point the coupling driver timestep is unknown
    ! and uses WRFHYDRO Config as best guess
    if(nlst_rt(is%wrap%nest)%dtrt .ge. nlst_rt(is%wrap%nest)%dt) then
       nlst_rt(is%wrap%nest)%dtrt = nlst_rt(is%wrap%nest)%dt
       dt_factor = 1
    else
      if(mod(nlst_rt(is%wrap%nest)%dt,nlst_rt(is%wrap%nest)%dtrt) /= 0) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="Driver timestep is not a multiple of routine timestep!", &
          file=FILENAME,method=SUBNAME,rcToReturn=rc)
        return  ! bail out
      endif
      dt_factor = nlst_rt(is%wrap%nest)%dt/nlst_rt(is%wrap%nest)%dtrt
    endif
    dt0 = nlst_rt(is%wrap%nest)%dt
    dtrt0 = nlst_rt(is%wrap%nest)%dtrt
    dt_factor0 = dt_factor

    RT_DOMAIN(is%wrap%nest)%initialized = .true.

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      call config_print(is,rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      call grid_print(is, WRFHYDRO_grid,'wrfhydro',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call nlst_rt_print(is,rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call rt_domain_print(is,"Initial",rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call grid_write(is,WRFHYDRO_grid, 'array_wrfhydro', rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_run(is,clock,importState,exportState,rc)
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Clock),intent(in)             :: clock
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc

    ! local variables
    type(ESMF_TimeInterval)     :: timeStep
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='nuopc_cpl_HYDRO_run'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if(.not. RT_DOMAIN(is%wrap%nest)%initialized) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Model has not been initialized!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call clock_to_string(is,clock,timestr=cpl_outdate,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    nlst_rt(is%wrap%nest)%olddate(1:19) = cpl_outdate(1:19) ! Current time is the

    nlst_rt(is%wrap%nest)%dt = timeinterval_to_real(is,timeInterval=timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if(nlst_rt(is%wrap%nest)%dt .le. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Timestep less than 1 is not supported!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    if((dt_factor*nlst_rt(is%wrap%nest)%dtrt) .ne. nlst_rt(is%wrap%nest)%dt) then   ! NUOPC driver time step changed.
      if (is%wrap%verbosity >= VERBOSITY_MAX) then
        call ESMF_LogWrite("Driver timestep changed.", &
          ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
      endif
      if(nlst_rt(is%wrap%nest)%dtrt .ge. nlst_rt(is%wrap%nest)%dt) then
        nlst_rt(is%wrap%nest)%dtrt = nlst_rt(is%wrap%nest)%dt
        if (is%wrap%verbosity >= VERBOSITY_MAX) then
          call ESMF_LogWrite("Routing timestep set to driver timestep.", &
            ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
        endif
        dt_factor = 1
      else
        if(mod(nlst_rt(is%wrap%nest)%dt,nlst_rt(is%wrap%nest)%dtrt) /= 0) then
          call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
            msg="New driver timestep is not a multiple of routing timestep!", &
            file=FILENAME,method=SUBNAME,rcToReturn=rc)
          return  ! bail out
        endif
        dt_factor = nlst_rt(is%wrap%nest)%dt/nlst_rt(is%wrap%nest)%dtrt
      endif
    endif

    if(nlst_rt(is%wrap%nest)%SUBRTSWCRT .eq.0  .and. &
      nlst_rt(is%wrap%nest)%OVRTSWCRT .eq. 0 .and. &
      nlst_rt(is%wrap%nest)%GWBASESWCRT .eq. 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    if((.not. RT_DOMAIN(is%wrap%nest)%initialized) .and. (nlst_rt(is%wrap%nest)%rst_typ .eq. 1) ) then
      if (is%wrap%verbosity >= VERBOSITY_MAX) then
        call ESMF_LogWrite("Restart initial data from offline file.", &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
        endif
    else

      select case (is%wrap%mode)
        case (mode_Offline)
          if (is%wrap%verbosity >= VERBOSITY_MAX) then
            call ESMF_LogWrite("Reading LDAS forcing data in offline mode.", &
              ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
            write(logMsg,"(A,I0)") "slice: ",is%wrap%slice
            call read_forc_ldasout_print(is,trim(logMsg),rc=rc)
          endif
          call read_forc_ldasout(nlst_rt(is%wrap%nest)%olddate(1:19), &
            nlst_rt(is%wrap%nest)%hgrid, &
            trim(configFile%indir), nlst_rt(is%wrap%nest)%dt, &
            rt_domain(is%wrap%nest)%ix,rt_domain(is%wrap%nest)%jx, &
            rt_domain(is%wrap%nest)%infxsrt,rt_domain(is%wrap%nest)%soldrain)
        case (mode_Coupled)
          call copy_import_fields(is, importState, rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case (mode_Hybrid)
          if (is%wrap%verbosity >= VERBOSITY_MAX) then
            call ESMF_LogWrite("Reading LDAS forcing data in hybrid mode.", &
              ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
            write(logMsg,"(A,I0)") "slice: ",is%wrap%slice
            call read_forc_ldasout_print(is,trim(logMsg),rc=rc)
          endif
          call read_forc_ldasout(nlst_rt(is%wrap%nest)%olddate(1:19), &
            nlst_rt(is%wrap%nest)%hgrid, &
            trim(configFile%indir), nlst_rt(is%wrap%nest)%dt, &
            rt_domain(is%wrap%nest)%ix,rt_domain(is%wrap%nest)%jx, &
            rt_domain(is%wrap%nest)%infxsrt,rt_domain(is%wrap%nest)%soldrain)
          call copy_import_fields(is, importState, rc)
          if (ESMF_STDERRORCHECK(rc)) return
        case default
          call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
            msg="Running mode is unknown.", &
            file=FILENAME, method=SUBNAME, rcToReturn=rc)
          return  ! bail out
      end select
    endif

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write(logMsg,"(A,I0)") "slice: ",is%wrap%slice
      call rt_domain_print(is,trim(logMsg),rc)
      if (ESMF_STDERRORCHECK(rc)) return   
    endif
  
    ! Call the WRF-HYDRO run routine
    call HYDRO_exe(did=is%wrap%nest)

    !! Copy the data to NUOPC fields
    call copy_export_fields(is, exportState, rc)
    if (ESMF_STDERRORCHECK(rc)) return

    ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
    !if(nlst_rt(is%wrap%nest)%GWBASESWCRT .eq. 3 ) then
      !Wei Yu: comment the following two lines. Not ready
    !yw     qsgw(x_start(1):x_end(1),y_start(1):y_end(1)) = gw2d(is%wrap%nest)%qsgw
    !yw     config_flags%gwsoilcpl = nlst_rt(is%wrap%nest)%gwsoilcpl
    !end if

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine wrfhydro_nuopc_fin(is,rc)
    ! ARGUMENTES
    type(type_InternalState), intent(inout) :: is
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='nuopc_cpl_HYDRO_fin'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    deallocate(zs)
    deallocate(IVGTYP)
    deallocate(isltyp)

    RT_DOMAIN(is%wrap%nest)%initialized = .false.

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine rt_domain_print(is,label,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*),intent(in)             :: label
    integer,          intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                    :: layerIndex
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='rt_domain_print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("Routing Domain "//trim(label),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    !! Land forcing fields
    do layerIndex = 1, nlst_rt(is%wrap%nest)%nsoil
      call array_print(is," Temperature of soil", rt_domain(is%wrap%nest)%STC,layerIndex, rc)
      call array_print(is," Moisture content of soil", rt_domain(is%wrap%nest)%smc,layerIndex, rc)
      call array_print(is," Liquid water content of soil", rt_domain(is%wrap%nest)%sh2ox,layerIndex, rc)
    enddo
    call array_print(is," Surface runoff flux", rt_domain(is%wrap%nest)%infxsrt, rc)
    call array_print(is," Subsurface runoff flux", rt_domain(is%wrap%nest)%soldrain, rc)
    
    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      do layerIndex = 1, nlst_rt(is%wrap%nest)%nsoil
        write(logMsg,"(A,I0,A,F0.3,A)") " Soil layer depth (layer,depth): (", &
          layerIndex,",",rt_domain(is%wrap%nest)%SLDPTH(layerIndex),")"
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      enddo
      write(logMsg,"(A,2(I0,A))") " RT domain dimensions (IX,JX): (", &
        rt_domain(is%wrap%nest)%ix,",",rt_domain(is%wrap%nest)%jx,")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      call array_print(is," SMCMAX1", rt_domain(is%wrap%nest)%SMCMAX1, rc)
      call array_print(is," SMCWLT1", rt_domain(is%wrap%nest)%SMCWLT1, rc)
      call array_print(is," SMCREF1", rt_domain(is%wrap%nest)%SMCREF1, rc)
      call array_print(is," Vegetation type", rt_domain(is%wrap%nest)%VEGTYP, rc)
      call array_print(is," Node area", rt_domain(is%wrap%nest)%node_area, rc)
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine copy_import_fields(is,importState, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(inout)         :: importState
    integer,          intent(out)           :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='copy_import_fields'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    !! Land forcing fields
    call copy_data(is,importState,"temperature_of_soil_layer_1", &
      rt_domain(is%wrap%nest)%STC,'i',1,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"temperature_of_soil_layer_2", &
      rt_domain(is%wrap%nest)%STC,'i',2,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"temperature_of_soil_layer_3", &
      rt_domain(is%wrap%nest)%STC,'i',3,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"temperature_of_soil_layer_4", &
      rt_domain(is%wrap%nest)%STC,'i',4,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"moisture_content_of_soil_layer_1", &
      rt_domain(is%wrap%nest)%smc,'i',1,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"moisture_content_of_soil_layer_2", &
      rt_domain(is%wrap%nest)%smc,'i',2,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"moisture_content_of_soil_layer_3", &
      rt_domain(is%wrap%nest)%smc,'i',3,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"moisture_content_of_soil_layer_4", &
      rt_domain(is%wrap%nest)%smc,'i',4,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"liquid_water_content_of_soil_layer_1", &
      rt_domain(is%wrap%nest)%sh2ox,'i',1,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"liquid_water_content_of_soil_layer_2", &
      rt_domain(is%wrap%nest)%sh2ox,'i',2,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"liquid_water_content_of_soil_layer_3", &
      rt_domain(is%wrap%nest)%sh2ox,'i',3,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"liquid_water_content_of_soil_layer_4", &
      rt_domain(is%wrap%nest)%sh2ox,'i',4,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"surface_runoff_flux", &
      rt_domain(is%wrap%nest)%infxsrt,'i',.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,importState,"subsurface_runoff_flux", &
      rt_domain(is%wrap%nest)%soldrain,'i',.true.,rc)
    if (ESMF_STDERRORCHECK(rc)) return

    !! Meterological forcing fields
!    call copy_data(is,importState,"inst_down_lw_flx", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_down_sw_flx", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_merid_wind_height_lowest", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_pres_height_surface", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_spec_humid_height_lowest", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_temp_height_lowest", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"inst_zonal_wind_height_lowest", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call copy_data(is,importState,"mean_prec_rate", &
!      UNKNOWN,'i',.true.,rc)
!    if (ESMF_STDERRORCHECK(rc)) return

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine copy_export_fields(is, exportState, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(inout)         :: exportState
    integer,          intent(out)           :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='copy_export_fields'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    !! Feedback for land
    call copy_data(is,exportState,"liquid_water_content_of_soil_layer_1", &
      rt_domain(is%wrap%nest)%sh2ox,'e',1,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,exportState,"liquid_water_content_of_soil_layer_2", &
      rt_domain(is%wrap%nest)%sh2ox,'e',2,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,exportState,"liquid_water_content_of_soil_layer_3", &
      rt_domain(is%wrap%nest)%sh2ox,'e',3,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call copy_data(is,exportState,"liquid_water_content_of_soil_layer_4", &
      rt_domain(is%wrap%nest)%sh2ox,'e',4,.true.,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    call copy_data(is,exportState,"volume_fraction_of_total_water_in_soil", &
!      UNKNOWN,'e',.true.,rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    call copy_data(is,exportState,"surface_snow_thickness", &
!      UNKNOWN,'e',.true.,rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    call copy_data(is,exportState,"liquid_water_content_of_surface_snow", &
!      UNKNOWN,'e',.true.,rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Feedback for atmosphere
!    call copy_data(is,exportState,"dummyfield", &
!      UNKNOWN,'e',.true.,rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Other export fields
!    call copy_data(is,exportState,"water_surface_height_above_reference_datum", &
!      rt_domain(is%wrap%nest)%sfcheadrt,'e',.true.,rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done",ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine config_file_read(is,rc)
    type(type_InternalState), intent(inout) :: is
    integer, intent(out)                    :: rc

    ! Local Variables
    integer                     :: ierr
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='config_file_read'

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

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

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
      call ESMF_LogSetError(ESMF_RC_FILE_OPEN, &
        msg="Error opening HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    read(hrldasConfigFH, NOAHLSM_OFFLINE, iostat=ierr)
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error reading HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    close (hrldasConfigFH, iostat=ierr )
    if (ierr /= 0) then
      call ESMF_LogSetError(ESMF_RC_FILE_CLOSE, &
        msg="Error closing HRLDAS config file: "//trim(hrldasConfigFile), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
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

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  function create_grid(is,distgrid,GEO_STATIC_FLNM,nx_local,ny_local,x_start,y_start,rc)
    ! RETURN VALUE
    type(ESMF_Grid) :: create_grid
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_DistGrid),intent(in)          :: distgrid
    character(len=*),intent(in)             :: GEO_STATIC_FLNM
    integer,intent(in)                      :: nx_local
    integer,intent(in)                      :: ny_local
    integer,intent(in)                      :: x_start
    integer,intent(in)                      :: y_start
    integer, intent(out)                    :: rc
    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)          :: min_lat, max_lat, min_lon, max_lon
    real, allocatable           :: latitude(:,:), longitude(:,:)
    integer                     :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_R8), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_R8), pointer :: coordYcorner(:,:)
    integer                     :: i,j, i1,j1
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='create_grid'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    create_grid = ESMF_GridCreate(name='WRFHYDRO_Grid', &
      distgrid=distgrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! CENTERS

    ! Get Local Latitude (lat)
    allocate(latitude(nx_local,ny_local))
    call get_geostatic_array(is,"XLAT_M", GEO_STATIC_FLNM, latitude, &
      x_start, y_start, nx_local, ny_local, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local,ny_local))
    call get_geostatic_array(is,"XLONG_M", GEO_STATIC_FLNM, longitude, &
      x_start, y_start, nx_local, ny_local, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Centers
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local,ny_local)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local,ny_local)
    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write (logMsg, "(A,2(F0.3,A))") "XLAT_M (LOWER-LEFT,UPPER-RIGHT): (", &
        min_lat,",",max_lat,")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write (logMsg, "(A,2(F0.3,A))") "XLONG_M (LOWER-LEFT,UPPER-RIGHT): (", &
        min_lon,",",max_lon,")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(create_grid, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(create_grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(create_grid, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write (logMsg, "(A,4(I0,A))") "Center bounds (DIM1,DIM2): (", &
        lbnd(1),":",ubnd(1),",",lbnd(2),":",ubnd(2),")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

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
    call get_geostatic_array(is,"XLAT_CORNER", GEO_STATIC_FLNM, latitude, &
      x_start, y_start, nx_local+1, ny_local+1, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(nx_local+1,ny_local+1))
    call get_geostatic_array(is,"XLONG_CORNER", GEO_STATIC_FLNM, longitude, &
      x_start, y_start, nx_local+1, ny_local+1, rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Print Local Lat Lon Lower Left / Upper Right Corners
    min_lat = latitude(1,1)
    max_lat = latitude(nx_local+1,ny_local+1)
    min_lon = longitude(1,1)
    max_lon = longitude(nx_local+1,ny_local+1)
    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write (logMsg, "(A,2(F0.3,A))") "XLAT_CORNER (LOWER-LEFT,UPPER-RIGHT): (", &
        min_lat,",",max_lat,")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write (logMsg, "(A,2(F0.3,A))") "XLONG_CORNER (LOWER-LEFT,UPPER-RIGHT): (", &
        min_lon,",",max_lon,")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! Add Corner Coordinates to Grid
    call ESMF_GridAddCoord(create_grid, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_GridGetCoord(create_grid, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call ESMF_GridGetCoord(create_grid, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write (logMsg, "(A,4(I0,A))") "Corner bounds (DIM1,DIM2): (", &
        lbnd(1),":",ubnd(1),",",lbnd(2),":",ubnd(2),")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcorner(i,j) = longitude(i,j)
      coordYcorner(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude)
    deallocate(longitude)

    call add_area(is,create_grid, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end function

  !-----------------------------------------------------------------------------

  subroutine add_area(is,grid,rc)
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Grid), intent(inout)          :: grid
    integer, intent(out)                    :: rc

    ! Local Variables
    integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
    type(ESMF_Field)                 :: fieldArea
    type(ESMF_Array)                 :: areaArray
    integer                          :: i,j
    integer                          :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer      :: radianarea(:,:)
    real(ESMF_KIND_R8), pointer      :: gridarea(:,:)
    CHARACTER(LEN=*),PARAMETER       :: SUBNAME='add_area'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    fieldArea = ESMF_FieldCreate(grid=grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_FieldRegridGetArea(fieldArea, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

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

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine set_local_indices(is,distgrid,x_start,x_end,y_start,y_end,nx_local,ny_local,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_DistGrid),intent(in)          :: distgrid
    integer, intent(out)                    :: x_start
    integer, intent(out)                    :: x_end
    integer, intent(out)                    :: y_start
    integer, intent(out)                    :: y_end
    integer, intent(out)                    :: nx_local
    integer, intent(out)                    :: ny_local
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='set_local_indices'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

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

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

subroutine config_file_print(is,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='config_file_print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("Configuration File Information",ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(2A)") " INDIR: ",trim(configFile%indir)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(2A)") " Geostatic filename: ",trim(configFile%GEO_STATIC_FLNM)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Number of soil layers: ",configFile%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,5(I0,A))") " Start (yr-mn-dy_hr:mn): (", &
      configFile%start_year,"-", &
      configFile%start_month,"-", &
      configFile%start_day,"_", &
      configFile%start_hour,":", &
      configFile%start_min,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Forcing timestep: ",configFile%FORCING_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Noah timestep: ",configFile%NOAH_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Output timestep: ",configFile%OUTPUT_TIMESTEP
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,2(F0.3,A))") " Soil thickness (1:MAX): (", &
      configFile%soil_thick_input(1),",", &
      configFile%soil_thick_input(configFile%nsoil),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  subroutine config_print(is,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='config_print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("Configuration",ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,I0)") " Nest/Domain ID: ",is%wrap%nest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,I0)") " Number of nests: ",num_nests
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,I0)") " Number of tiles: ",num_tiles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,I0)") " Surface physics: ",sf_surface_physics
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,L1)") " Check soil: ",checkSOIL_flag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,2(I0,A))") " Soil depth (1,MAX): (", &
     zs(1),",", &
     zs(configFile%nsoil),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,2(I0,A))") " Global (NX,NY): (", &
      nx_global,",",ny_global,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,2(I0,A))") " Local (NX,NY): (", &
      nx_local,",",ny_local,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,4(I0,A))") " Start (X,Y) End (X,Y): (", &
      x_start,",",y_start,") (", &
      x_end,",",y_end,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,2(I0,A))") " Vegetation type (START,END): (", &
      IVGTYP(x_start,y_start),",",IVGTYP(x_end,y_end),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(A,2(I0,A))") " SL type (START,END): (", &
      isltyp(x_start,y_start),",",isltyp(x_end,y_end),")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg, "(2A)") " Couple outdate: ",cpl_outdate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  subroutine nlst_rt_print(is,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    integer         , intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                     :: layerIndex
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='nlst_rt_print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("NLST_RT",ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Nest: ",is%wrap%nest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Soil Layers: ",nlst_rt(is%wrap%nest)%nsoil
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " SOLVEG_INITSWC: ",nlst_rt(is%wrap%nest)%SOLVEG_INITSWC
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    do layerIndex=1,nlst_rt(is%wrap%nest)%nsoil
      write (logMsg,"(A,I0,A,F0.3,A)") " Soil layer depth (layer,depth): (", &
        layerIndex,",",nlst_rt(is%wrap%nest)%ZSOIL8(layerIndex),")"
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    enddo
 
    write (logMsg,"(A,3(F0.3,A))") " Timestep (out_dt,rst_dt,dt): (", &
      nlst_rt(is%wrap%nest)%out_dt,",",nlst_rt(is%wrap%nest)%rst_dt,",", &
      nlst_rt(is%wrap%nest)%dt,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,5(I0,A))") " Start (YR-MN-DY_HR:MN): ", &
      nlst_rt(is%wrap%nest)%START_YEAR,"-",nlst_rt(is%wrap%nest)%START_MONTH,"-", &
      nlst_rt(is%wrap%nest)%START_DAY,"_", &
      nlst_rt(is%wrap%nest)%START_HOUR,":",nlst_rt(is%wrap%nest)%START_MIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Restart file: ",trim(nlst_rt(is%wrap%nest)%restart_file)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Split output count: ",nlst_rt(is%wrap%nest)%split_output_count
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Grid ID: ",nlst_rt(is%wrap%nest)%igrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,2(I0,A))") " Parallel IO (in,out): (", &
      nlst_rt(is%wrap%nest)%rst_bi_in,",",nlst_rt(is%wrap%nest)%rst_bi_out,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Geo static filename: ",trim(nlst_rt(is%wrap%nest)%geo_static_flnm)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " DEEPGWSPIN: ",nlst_rt(is%wrap%nest)%DEEPGWSPIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Order to write: ",nlst_rt(is%wrap%nest)%order_to_write
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Restart type: ",nlst_rt(is%wrap%nest)%rst_typ
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Hydro Grid: ",nlst_rt(is%wrap%nest)%hgrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Old date: ",nlst_rt(is%wrap%nest)%olddate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " Start date: ",nlst_rt(is%wrap%nest)%startdate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " Since date: ",nlst_rt(is%wrap%nest)%sincedate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Routing option: ",nlst_rt(is%wrap%nest)%RT_OPTION
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " CHANRTSWCRT: ",nlst_rt(is%wrap%nest)%CHANRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Channel option: ",nlst_rt(is%wrap%nest)%channel_option
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " SUBRTSWCRT: ",nlst_rt(is%wrap%nest)%SUBRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " OVRTSWCRT: ",nlst_rt(is%wrap%nest)%OVRTSWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " AGGFACTRT: ",nlst_rt(is%wrap%nest)%AGGFACTRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " GWBASESWCRT: ",nlst_rt(is%wrap%nest)%GWBASESWCRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " GW_RESTART: ",nlst_rt(is%wrap%nest)%GW_RESTART
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " RSTRT_SWC: ",nlst_rt(is%wrap%nest)%RSTRT_SWC
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " TERADJ_SOLAR: ",nlst_rt(is%wrap%nest)%TERADJ_SOLAR
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " System coupling: ",nlst_rt(is%wrap%nest)%sys_cpl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " gwChanCondSw: ",nlst_rt(is%wrap%nest)%gwChanCondSw
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " GwPreCycles: ",nlst_rt(is%wrap%nest)%GwPreCycles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " GwSpinCycles: ",nlst_rt(is%wrap%nest)%GwSpinCycles
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " GwPreDiagInterval: ",nlst_rt(is%wrap%nest)%GwPreDiagInterval
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " gwsoilcpl: ",nlst_rt(is%wrap%nest)%gwsoilcpl
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,L1)") " GwPreDiag: ",nlst_rt(is%wrap%nest)%GwPreDiag
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,L1)") " GwSpinUp: ",nlst_rt(is%wrap%nest)%GwSpinUp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " DTRT: ",nlst_rt(is%wrap%nest)%DTRT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " dxrt0: ",nlst_rt(is%wrap%nest)%dxrt0
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " DTCT: ",nlst_rt(is%wrap%nest)%DTCT
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " gwChanCondConstIn: ",nlst_rt(is%wrap%nest)%gwChanCondConstIn
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " gwChanCondConstOut: ",nlst_rt(is%wrap%nest)%gwChanCondConstOut
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,F0.3)") " gwIhShift: ",nlst_rt(is%wrap%nest)%gwIhShift
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " route_topo_f: ",trim(nlst_rt(is%wrap%nest)%route_topo_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " route_chan_f: ",trim(nlst_rt(is%wrap%nest)%route_chan_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " route_link_f: ",trim(nlst_rt(is%wrap%nest)%route_link_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " route_lake_f: ",trim(nlst_rt(is%wrap%nest)%route_lake_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " route_direction_f: ",trim(nlst_rt(is%wrap%nest)%route_direction_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " route_order_f: ",trim(nlst_rt(is%wrap%nest)%route_order_f)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " gwbasmskfil: ",trim(nlst_rt(is%wrap%nest)%gwbasmskfil)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " gwstrmfil: ",trim(nlst_rt(is%wrap%nest)%gwstrmfil)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,A)") " geo_finegrid_flnm: ",trim(nlst_rt(is%wrap%nest)%geo_finegrid_flnm)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,I0)") " Point timeseries output at user specified points: ",nlst_rt(is%wrap%nest)%frxst_pts_out
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Point timeseries output at all channel points: ",nlst_rt(is%wrap%nest)%CHRTOUT_DOMAIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Grid of channel streamflow values: ",nlst_rt(is%wrap%nest)%CHRTOUT_GRID
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Grid of variables passed between LSM and routing components: ",nlst_rt(is%wrap%nest)%LSMOUT_DOMAN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Grid of terrain routing variables on routing grid: ",nlst_rt(is%wrap%nest)%RTOUT_DOMAIN
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Grid of GW: ",nlst_rt(is%wrap%nest)%output_gw
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Grid of lakes: ",nlst_rt(is%wrap%nest)%outlake
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  subroutine read_forc_ldasout_print(is,label,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in)            :: label
    integer         , intent(out)           :: rc

    ! LOCAL VARIABLES
    integer                     :: layerIndex
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='read_forc_ldasout_print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("Read forcing LDASOUT parameters "//trim(label),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (logMsg,"(A,I0)") " Nest: ",is%wrap%nest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Old date: ",nlst_rt(is%wrap%nest)%olddate
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Hydro grid: ",nlst_rt(is%wrap%nest)%hgrid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,A)") " Input directory: ",trim(configFile%indir)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write (logMsg,"(A,F0.3)") " Timestep: ",nlst_rt(is%wrap%nest)%dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    write(logMsg,"(A,2(I0,A))") " RT domain dimensions (IX,JX): (", &
      rt_domain(is%wrap%nest)%ix,",",rt_domain(is%wrap%nest)%jx,")"
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    call array_print(is," Surface runoff flux", rt_domain(is%wrap%nest)%infxsrt, rc)
    call array_print(is," Subsurface runoff flux", rt_domain(is%wrap%nest)%soldrain, rc)    

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

end module
