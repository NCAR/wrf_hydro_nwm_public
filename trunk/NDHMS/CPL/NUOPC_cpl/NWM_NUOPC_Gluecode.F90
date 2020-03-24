#define FILENAME "NWM_NUOPC_Gluecode"
#define MODNAME "NWM_NUOPC_Gluecode.F90"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

module NWM_NUOPC_Gluecode
! !MODULE: NWM_NUOPC_Gluecode
!
! !DESCRIPTION:
!   This module connects NUOPC initialize, advance,
!   and finalize to NWM.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
! !USES:
  use ESMF
  use NUOPC
  use NWM_ESMF_Extensions
  use module_mpp_land, only: &
    numprocs, &        ! total number of requested process to run the model 
    global_nx, &       ! grid total cols, number of cells in x direction
    global_ny, &       ! grid total rows, number of cells in y direction

    ! left_right_np, &   ! number of subgrids on x direction (i.e num PET in x)
    ! up_down_np,    &   ! number of subgrids on y direction (i.e num PET in y)
                       ! if there are 3 subgrids in x direction, there are 3 PETs
                       ! if each subgrid contains 15 cells, each PET operates on 
                       ! all 15 cells at once
    startx, &          ! array of cells in x direction per local DE (i.e. subgrids)
    starty, &          ! array of cells in y direction per local DE (i.e. subgrids)
    local_nx_size, &   ! array that holds all local_nxs
    local_ny_size, &   ! array that holds all local_nys

    ! local_nx, &        ! num cells in x direction for a local DE or subgrid   ! not needed
    ! local_ny, &        ! num cells in y direction for a local DE or subgrid   ! not needed

    IO_id, &           ! 1st processor number, where all the writing/broadcasting occurs
    my_id              ! place holder for processor number (i.e. PET, MPI rank, DE id,)
  use module_NoahMP_hrldas_driver, only: ITIME, NTIME
  use module_hrldas_driver, only : noahMP_init, noahMP_exe, noahMP_finish
  use state_module, only   : state_type
  use module_rt_data, only : rt_domain
  use module_namelist, only: &
    nlst_rt, &         ! NWM data structure holding metadata and data
    read_rt_nlst 

  implicit none

  private

  public :: NWM_NUOPC_Init
  public :: NWM_GetHgrid
  public :: NWM_LSMGridCreate
  ! public :: NWM_RTGridCreate
  public :: NWM_LocStreamCreate
  public :: NWM_NUOPC_Run
  public :: NWM_NUOPC_Fin
  public :: NWM_GetTimestep
  public :: NWM_SetTimestep
  public :: NWM_RunModeGet
  public :: NWM_Field
  public :: NWM_FieldList
  public :: NWM_FieldDictionaryAdd
  public :: NWM_FieldCreate


  type NWM_Field
    character(len=64)   :: stdname        = ' '  ! human readable name
    character(len=64)   :: shortname      = ' '  ! NWM variable of the field
    character(len=64)   :: desc           = ' '
    character(len=10)   :: units          = ' '
    character(len=64)   :: transferOffer  = 'will provide'
    logical             :: adImport       = .FALSE.
    logical             :: realizedImport = .FALSE.
    logical             :: adExport       = .FALSE.
    logical             :: realizedExport = .FALSE.
    logical             :: assoc          = .FALSE. 
    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr => null()
  end type NWM_Field

  type(NWM_Field),dimension(19) :: NWM_FieldList = (/ & 
    ! NWM output - file: 201905160600.CHRTOUT_DOMAIN1
    NWM_Field( & !(1)
      stdname='flow_rate', units='m3 s-1', &
      desc='volume of fluid passing by some location through an area during a period of time.', shortname='streamflow', &
      adImport=.FALSE.,adExport=.TRUE.), &
    NWM_Field( & !(2) 
      stdname='surface_runoff', units='m3 s-1', &
      desc='water, from rain, snowmelt, or other sources, that flows over the land surface.', shortname='qSfcLatRunoff', &
      adImport=.FALSE.,adExport=.TRUE.), &
    NWM_Field( & !(3)
      stdname='river_velocity', units='m s-1', &
      desc='vector filed of river or flow velocity.', shortname='velocity', &
      adImport=.FALSE.,adExport=.TRUE.), &
    NWM_Field( & !(4 - file ???)
      stdname='waterlevel', units='m', &
      desc='water level from dflow to check on this', shortname='wl', &
      adImport=.FALSE.,adExport=.TRUE.), &

    ! Atmospheric Forcing, HWRF - file: 2011082720.LDASIN_DOMAIN1, func: READFORC_HRLDAS
    NWM_Field( & !(5) U_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D U wind component [m/s]
      stdname='wind_velocity_u', units='m s-1', &
      desc='UGRD, 10-m eastward wind', shortname='U2D', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(6) V_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D V wind component [m/s]
      stdname='wind_velocity_v', units='m s-1', &
      desc='VGRD, 10-m northward wind', shortname='V2D', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(7) 
      stdname='surface_pressure', units='Pa', &
      desc='surface pressure.', shortname='PSFC', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(8) T_PHY     (XSTART:XEND,KDS:KDE,YSTART:YEND) )  ! 3D atmospheric temperature valid at mid-levels [K]- TSK, (XSTART:XEND,YSTART:YEND) )  ! surface radiative temperature [K]
      stdname='air_temperature', units='K', &
      desc='2-m air temperature.', shortname='T2D', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(9) 
      stdname='specific_humidity', units='kg kg-1', &
      desc='2-m specific humidity.', shortname='Q2D', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(10) SWDOWN    (XSTART:XEND,YSTART:YEND) )    ! solar down at surface [W m-2]
      stdname='shortwave_height', units='w m-2', &
      desc='surface downward shortwave radiation flux', shortname='SWDOWN', & 
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(11) GLW       (XSTART:XEND,YSTART:YEND) )    ! longwave down at surface [W m-2]
      stdname='longwave_height', units='w m-2', & 
      desc='surface downward longwave radiation flux', shortname='LWDOWN', &
      adImport=.TRUE.,adExport=.FALSE.), &

    NWM_Field( & !(12 file: ??)
      stdname='precipitation_rate', units='kg/m2s', & 
      desc='15-min surface precipitation rate', shortname='PRATE', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(13)
      stdname='cloud_cover', units='??', & 
      desc='total cloud cover, fraction: 0-1', shortname='TCC', &
      adImport=.TRUE.,adExport=.FALSE.), &
    
    NWM_Field( & !(14 file: ???) 
      stdname='mean_sea_level', units='Pa', &
      desc='PRMSL, Pressure Reduced to MSL, atm. pressure', shortname='PMSL', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(15) 
      stdname='rediation_stress_xy', units='m s-1', &
      desc='Rediation stress Instantanous fields', shortname='SXY', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(16)
      stdname='rediation_stress_xx', units='m s-1', &
      desc='Rediation stress Instantanous fields', shortname='SXX', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(17)
      stdname='rediation_stress_yy', units='m s-1', &
      desc='Rediation stress Instantanous fields', shortname='SYY', &
      adImport=.TRUE.,adExport=.FALSE.), &
    NWM_Field( & !(18)
      stdname='wave_peak_mean_dir', units='degree', &
      desc='Peak/Mean wave direction', shortname='??', &
      adImport=.TRUE.,adExport=.FALSE.), &
    ! what is next
    NWM_Field( & !(19)
      stdname='next', units='??', &
      desc='what is next', shortname='nxt', &
      adImport=.FALSE.,adExport=.FALSE.) /)

  ! Local version variables from NWM LSM grid
  type(ESMF_DistGrid)        :: NWM_DistGrid
  character(len=ESMF_MAXSTR) :: indir = 'FORCING'

  integer                    :: nx_global(1)     ! 4608  num grids in x direction
  integer                    :: ny_global(1)     ! 3840  num grids in y direction
  integer                    :: x_start
  integer                    :: x_end
  integer                    :: y_start
  integer                    :: y_end
  integer                    :: nx_local               
  integer                    :: ny_local

  ! Added to consider the adaptive time step from driver.
  real                  :: dt0        = UNINITIALIZED
  real                  :: dtrt_ter0  = UNINITIALIZED
  real                  :: dtrt_ch0   = UNINITIALIZED
  integer               :: dt_factor0 = UNINITIALIZED
  integer               :: dt_factor  = UNINITIALIZED
  ! Added to track the driver clock
  character(len=19)     :: startTimeStr = "0000-00-00_00:00:00"

  character(len=512)    :: logMsg

  !-----------------------------------------------------------------------------
  ! Model Glue Code
  !-----------------------------------------------------------------------------
contains

  !-----------------------------------------------------------------------------
  ! Calls NWM land_driver_ini and saves timing variables 
  ! needed for further processing.
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_NUOPC_Init"

  subroutine NWM_NUOPC_Init(did,vm,clock,state,rc)

    !use module_NoahMP_hrldas_driver, only: land_driver_ini
    use module_hrldas_driver, only: noahMP_init
    use state_module, only: state_type
    
    ! arguments
    integer, intent(in)         :: did
    type(ESMF_VM),intent(in)    :: vm
    type(ESMF_Clock),intent(in) :: clock
    integer, intent(out)        :: rc
    type(state_type)            :: state

    ! Local variables
    integer                     :: localPet       ! current process number
    integer                     :: stat       
    integer                     :: esmf_comm, nuopc_comm
    integer                     :: itime, ntime1, ntimestep
    character(32)               :: startdate
    ! clock coming from NEMS
    type(ESMF_Time)             :: startTime
    type(ESMF_Time)             :: stopTime
    type(ESMF_TimeInterval)     :: timeStep         ! comes in sec. from NEMS
    type(ESMF_TimeInterval)     :: runDuration      ! comes in sec. from NEMS
    real(ESMF_KIND_R8)          :: dt 
    integer                     :: YY, MM, DD, H, M, S

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Set mpiCommunicator for NWM
    call ESMF_VMGet(vm, localPet=localPet, mpiCommunicator=esmf_comm, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    !call NWM_VMPrint(vm)

    call MPI_Comm_dup(esmf_comm, nuopc_comm, rc)
    ! Duplicate the MPI communicator not to interfere with ESMF communications.
    ! The duplicate MPI communicator can be used in any MPI call in the user
    ! code. Here the MPI_Barrier() routine is called.
    call MPI_Barrier(nuopc_comm, rc) 

    ! Get the models timestep(at this time is NEMS default = 3600 s),
    ! startTime and runDuration are model's set in model_configure
    call ESMF_ClockGet(clock,timeStep=timeStep,startTime=startTime, &
                       runDuration=runDuration,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_TimeIntervalGet(timeStep,s_r8=dt,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    write(logMsg,*) dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,rc=RC)

    call NWM_TimeToString(startTime,timestr=startTimeStr,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_LogWrite(startTimeStr,ESMF_LOGMSG_INFO,rc=RC)

    call ESMF_TimeIntervalGet(runDuration, s_r8=dt, rc=rc)
    write(logMsg,*) dt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,rc=RC)

 
    ! Initialize NWM before setting up fields
    ! getting back ntime and state from NWM here
    call noahMP_init(ntime1, state, nuopc_comm=esmf_comm)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Passing to calling method for model clock
    startdate = nlst_rt(did)%startdate(1:19)
    ntimestep = nlst_rt(did)%dt      ! noah_timestep   
    itime = 0                        ! starts from 1 in cap and in NWM
    print *, startdate, ntimestep

    ! Broadcasts a contiguous data array from rootPet to all other PETs of 
    ! the ESMF_VM object. root_pet = IO_id = pet0 on 1st node.
    call ESMF_VMBroadcast(vm, startx, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, starty, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, local_nx_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMBroadcast(vm, local_ny_size, count=numprocs, rootPet=IO_id, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out 

  end subroutine


  !-----------------------------------------------------------------------------
  ! Calls NWM land_driver_exe(itime) command per timestep.
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_NUOPC_Run"

  subroutine NWM_NUOPC_Run(did,mode,clock,state,itime, ntime, importState,exportState,rc)
    !use module_NoahMP_hrldas_driver, only: land_driver_exe
    use module_hrldas_driver, only: noahMP_exe
    use state_module, only: state_type

    integer, intent(in)                     :: did
    integer, intent(in)                     :: mode
    type(ESMF_Clock),intent(in)             :: clock
    type(ESMF_State),intent(inout)          :: importState
    type(ESMF_State),intent(inout)          :: exportState
    integer, intent(out)                    :: rc
    type(state_type)                        :: state
    integer, intent(in)                     :: itime,ntime
    !character(32), intent(in)               :: startdate_str
   
    ! local variables
    type(ESMF_TimeInterval)     :: timeStep
    integer                     :: dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    
    dt = NWM_TimeIntervalGetReal(timeInterval=timeStep,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
 
    ! dt is the driver requested time step. NWM regardless of time step
    ! only runs once per hour (noah_timestep = 3600 sec).
    ! at this time we should keep track of dt's until 3600 sec is passed
    ! then call this method. Meanwhile, previous data is being passed
    ! around until new data set comes. We have to emulate this:
    ! do ITIME = 1, NTIME land_driver_exe(ITIME)

    ! TO DO
    ! if (exeTimeFlag) then
    !    call land_driver_exe(itime)
    ! endif

    !call land_driver_exe(itime, state)
    call noahMp_exe(itime, state)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! At the end of number of iterations, stops the program.
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_NUOPC_Fin"

  subroutine NWM_NUOPC_Fin(did,state,rc)
    use module_hrldas_driver, only: noahMP_finish

    ! ARGUMENTES
    integer, intent(inout)      :: did
    integer, intent(out)        :: rc
    type(state_type)            :: state

    ! LOCAL VARIABLES
    integer                     :: stat

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! WRF-Hydro finish routine cannot be called because it stops MPI

!    DCR - Turned off to let the model deallocate memory
!    deallocate(nlst_rt(did)%zsoil8)
!    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
!      msg=METHOD//': Deallocation of model soil depth memory failed.', &
!      file=FILENAME,rcToReturn=rc)) return ! bail out

#ifdef WRF_HYDRO
    !call hydro_finish(stat)  !?????? see above note
#endif


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine


  !-----------------------------------------------------------------------------
  ! Create field using internal memory
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_FieldCreate"

  function NWM_FieldCreate(stdName,grid,did,rc)
    ! RETURN VALUE
    type(ESMF_Field) :: NWM_FieldCreate
    ! ARGUMENTS
    character(*), intent(in)                :: stdName
    type(ESMF_Grid), intent(in)             :: grid
    integer, intent(in)                     :: did
    integer,          intent(out)           :: rc
    ! LOCAL VARIABLES

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    SELECT CASE (trim(stdName))
      CASE ('flow_rate')
        NWM_FieldCreate = ESMF_FieldCreate(name=stdName, grid=grid, &
          farray=rt_domain(did)%qlink(:,1), &  
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      CASE ('surface_runoff')
        NWM_FieldCreate = ESMF_FieldCreate(name=stdName, grid=grid, &
          farray=rt_domain(did)%qSfcLatRunoff, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      CASE ('river_velocity')
        NWM_FieldCreate = ESMF_FieldCreate(name=stdName, grid=grid, &
          farray=rt_domain(did)%velocity, &
          indexflag=ESMF_INDEX_DELOCAL, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
      CASE DEFAULT
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg=METHOD//": Field hookup missing: "//trim(stdName), &
          file=FILENAME,rcToReturn=rc)
        return  ! bail out
    END SELECT

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

 !-----------------------------------------------------------------------------

! read_rt_nlst(nlst)
! nlst%geo_finegrid_flnm
! ./DOMAIN/Fulldom_hires.nc
! rt_domain(did)%g_IXRT, rt_domain(did)%g_JXRT, rt_domain(did)%ixrt, rt_domain(did)%jxrt
!          60          64          22          21
! rt_domain(did)%ix, rt_domain(did)%jx
!           5           5
! global_nx, global_ny, local_nx, local_ny
!          15          16           5     
! read LINKID for CH_LNKRT from ./DOMAIN/Fulldom_hires.nc
! NLINKS IS          560
! read file to get NLINKSL from./DOMAIN/Route_Link.nc
! getting dimension from file: ./DOMAIN/Route_Link.nc
!
  !-----------------------------------------------------------------------------
  ! Creating an ESMF domain data structure capable of data transfer from 
  ! point to point using points coordinates called a Location Stream. 
  ! A LocStream differs from a Grid in that no topological structure is 
  ! maintained between the points (e.g. the class contains no information  
  ! about which point is the neighbor of which other point). 
  ! This is needed for adding location information to the 1D channel data 
  ! structure that will be used to transfer streamflow data from NWM to
  ! ADCIRC. (i.e. station "5" doesn't have to be located next to station 
  ! "6" because they can be stations in different rivers)
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_LocStreamCreate"

  function NWM_LocStreamCreate(did,rc)
    use module_NWM_io, only: output_chrt_NWM
    ! Return value
    type(ESMF_LocStream)       :: NWM_LocStreamCreate

    ! Aguments
    integer, intent(in)   :: did
    integer, intent(out)  :: rc

    ! Local variables  
    type(ESMF_TimeInterval) :: stabilityTimeStep
    type(ESMF_VM)        :: vm
    integer              :: localPet
    integer              :: petCount
    integer              :: globalNumloc     ! total number of chrouting points
    integer              :: quotient         ! integer part of a division
    integer              :: rem              ! remainder part of a division
    integer              :: localLocBeg      ! pet begin point
    integer              :: localNumloc      ! number of points on each pet
    integer              :: i
    real, allocatable    :: lat(:), lon(:)
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_Field)     :: field_streamflow
    type(ESMF_LocStream) :: locStream
    type(ESMF_LocStream) :: locStreamOut
    real(ESMF_KIND_R8), dimension(:), pointer :: farrayPtr => null()
    real, allocatable, dimension(:) :: streamflow, velocity

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif
 
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !-------------------------------------------------------------------
    ! Get parallel information. Here petCount is the total number of 
    ! running PETs, and localPet is the number of this particular PET.
    !-------------------------------------------------------------------
    call ESMF_VMGetCurrent(vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call ESMF_VMGet(vm=vm, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    !-------------------------------------------------------------------
    ! Allocate and set equal number of points the destination LocStream 
    ! will have on each PET.  
    !  
    ! Assumptions: 1) nlst_rt(1)%io_config_outputs=1, assimilation, 
    !              2) mppFlag=1,
    !              3) nlst_rt(domainId)%channel_option!=3,
    !              4) one DE per PET
    !              5) #ifndef NCEP_WCOSS
    !-------------------------------------------------------------------
    if(nlst_rt(did)%channel_option .ne. 3) then
      globalNumloc = rt_domain(did)%gnlinksl
    else
      globalNumloc = rt_domain(did)%gnlinks
    endif
    
    quotient = globalNumloc/petCount
    rem = globalNumloc - quotient
    if (localPet.eq.(petCount-1)) then
      localNumloc = quotient + rem
    else
      localNumloc = quotient
    endif
    localLocBeg = 1 + (quotient*localPet)
    !  g_chlon = RT_DOMAIN(domainId)%CHLON
    !  g_chlat = RT_DOMAIN(domainId)%CHLAT
    allocate(lon(globalNumloc))
    allocate(lat(globalNumloc))

    do i=1,globalNumloc
      lon(i)=360.0*i/globalNumloc
      lat(i)=100*REAL(localPet,ESMF_KIND_R8)/REAL(petCount,ESMF_KIND_R8)-50.0
    enddo

    !-------------------------------------------------------------------
    ! Allocate and set stream flow field data
    !-------------------------------------------------------------------
    ! g_qlink = RT_DOMAIN(domainId)%QLINK
    allocate(streamflow(globalNumloc))

    do i=1,globalNumloc
      streamflow(i)= 300 - abs(lat(i))
    enddo

    !-------------------------------------------------------------------
    ! Create the LocStream:  Allocate space for the LocStream object, 
    ! define the number and distribution of the locations. 
    !-------------------------------------------------------------------
    locStream=ESMF_LocStreamCreate(name="NWM_streamflow",          &
                                   localCount=localNumloc,         &
                                   coordSys=ESMF_COORDSYS_SPH_DEG, &
                                   rc=rc)

    !-------------------------------------------------------------------
    ! Add key data, referencing a user data pointer. By changing the 
    ! datacopyflag to ESMF_DATACOPY_VALUE an internally allocated copy  
    ! of the user data may also be set. 
    !-------------------------------------------------------------------
    call ESMF_LocStreamAddKey(locStream,             &
                             keyName="ESMF:Lat",     &
                             farray=lat,             &
                             datacopyflag=ESMF_DATACOPY_REFERENCE, &
                             keyUnits="Degrees",     &
                             keyLongName="Latitude", rc=rc)

    call ESMF_LocStreamAddKey(locStream,             &
                             keyName="ESMF:Lon",     &
                             farray=lon,             &
                             datacopyflag=ESMF_DATACOPY_REFERENCE, &
                             keyUnits="Degrees",     &
                             keyLongName="Longitude", rc=rc)
    !-------------------------------------------------------------------
    ! Create a Field on the Location Stream. In this case the 
    ! Field is created from a user array, but any of the other
    ! Field create methods (e.g. from ArraySpec) would also apply.
    !-------------------------------------------------------------------       
    field_streamflow = ESMF_FieldCreate(locStream=locStream,  &
                             typekind=ESMF_TYPEKIND_R8,       &
                             name="flow_rate", &
                             rc=rc)

    ! RT_DOMAIN(domainId)%QLINK(:,1)  g_qlink(:1)
 
     
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function


  !-----------------------------------------------------------------------------
  ! Creates NWM Land Surface grid locally for the purpose of
  ! importing/exporting data between models.  
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "NWM_LSMGridCreate"

  function NWM_LSMGridCreate(did,rc)
    ! Return value
    type(ESMF_Grid)       :: NWM_LSMGridCreate
  
    ! Aguments
    integer, intent(in)   :: did
    integer, intent(out)  :: rc

    ! Local variables
    integer                     :: stat
    real                        :: min_lat, max_lat, min_lon, max_lon
    real, allocatable           :: latitude(:,:), longitude(:,:)
    integer, allocatable        :: mask(:,:)
    integer                     :: lbnd(2),ubnd(2)
    real(ESMF_KIND_COORD), pointer :: coordXcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcenter(:,:)
    real(ESMF_KIND_COORD), pointer :: coordXcorner(:,:)
    real(ESMF_KIND_COORD), pointer :: coordYcorner(:,:)
    integer(ESMF_KIND_I4), pointer :: gridmask(:,:)
    integer                     :: i,j, i1,j1
    character(len=16)           :: xlat_corner_name, xlon_corner_name

    integer, allocatable        :: deBlockList(:,:,:)   ! there is one global for use on other methods,
                                                        ! delete this or that one, after more progress

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif


#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    allocate(deBlockList(2,2,numprocs))
    ! Dimension one is each dimension of the element
    ! Dimension 2 is always the lower and upper indices per 
    ! dimension per deBlock
    ! Dimension 3 is each deBlock (1 to deCount)
    do i = 1, numprocs
 
      deBlockList(:,1,i) = (/startx(i),starty(i)/)
      deBlockList(:,2,i) = (/startx(i)+local_nx_size(i)-1, &
                             starty(i)+local_ny_size(i)-1/)
      write (logMsg,"(A,I0,A,4(I0,A))") MODNAME//": deBlockList ", i, " = (", &
        deBlockList(1,1,i),":",deBlockList(1,2,i),",", &
        deBlockList(2,1,i),":",deBlockList(2,2,i),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo
    ! Create a DistGrid
    NWM_DistGrid = ESMF_DistGridCreate( &
      minIndex=(/1,1/), maxIndex=(/global_nx,global_ny/), &
      deBlockList=deBlockList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    deallocate(deBlockList)

    ! Create a esmf grid per domain 
    NWM_LSMGridCreate = ESMF_GridCreate(name='NWM_LSMGrid_D'//trim(nlst_rt(did)%hgrid), &
      distgrid=NWM_DistGrid, coordSys = ESMF_COORDSYS_SPH_DEG, &
      coordTypeKind=ESMF_TYPEKIND_COORD, &
!      gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/0,1/), &
      rc = rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! CENTER
    ! Get Local Latitude (lat), (i.e. DE local, per subgrid size)
    allocate(latitude(local_nx_size(my_id+1),local_ny_size(my_id+1)),stat=stat)    
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of latitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out


    call NWM_ESMF_NetcdfReadIXJX("XLAT_M",nlst_rt(did)%geo_static_flnm, &
        (/startx(my_id+1),starty(my_id+1)/),latitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get Local Longitude (lon)
    allocate(longitude(local_nx_size(my_id+1),local_ny_size(my_id+1)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of longitude memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out

    call NWM_ESMF_NetcdfReadIXJX("XLONG_M",nlst_rt(did)%geo_static_flnm, &
      (/startx(my_id+1),starty(my_id+1)/),longitude,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
    ! Print Local Lat Lon Lower Left / Upper Right Centers
    write(logMsg,"(A,4(F0.3,A))") MODNAME//": Center Coordinates = (", &
      longitude(1,1),":",longitude(local_nx_size(my_id+1),local_ny_size(my_id+1)),",", &
      latitude(1,1),":",latitude(local_nx_size(my_id+1),local_ny_size(my_id+1)),")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

    ! Add Center Coordinates to Grid
    call ESMF_GridAddCoord(NWM_LSMGridCreate, staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! Get coordinates and put into an Array
    call ESMF_GridGetCoord(NWM_LSMGridCreate, coordDim=1, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      computationalLBound=lbnd, computationalUBound=ubnd, &
      farrayPtr=coordXcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
   
    call ESMF_GridGetCoord(NWM_LSMGridCreate, coordDim=2, localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=coordYcenter, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      coordXcenter(i,j) = longitude(i,j)
      coordYcenter(i,j) = latitude(i,j)
    enddo
    enddo

    deallocate(latitude,longitude,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of longitude and latitude memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    ! Get Local Mask
    allocate(mask(local_nx_size(my_id+1),local_ny_size(my_id+1)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of mask memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out

    call NWM_ESMF_NetcdfReadIXJX("LANDMASK",nlst_rt(did)%geo_static_flnm, &
      (/startx(my_id+1),starty(my_id+1)/),mask,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out


    ! Add Grid Mask
    call ESMF_GridAddItem(NWM_LSMGridCreate, itemFlag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    ! Get pointer to Grid Mask array
    call ESMF_GridGetItem(NWM_LSMGridCreate, itemflag=ESMF_GRIDITEM_MASK, &
      localDE=0, &
      staggerloc=ESMF_STAGGERLOC_CENTER, &
      farrayPtr=gridmask, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do j = lbnd(2),ubnd(2)
    do i = lbnd(1),ubnd(1)
      gridmask(i,j) = mask(i,j)
      gridmask(i,j) = mask(i,j)
    enddo
    enddo

    deallocate(mask,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of mask memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    ! CORNERS
    ! The original WPS implementation used the _CORNER names
    ! but it was then changes to the _C names.  Support both
    ! options.
    if (NWM_ESMF_NetcdfIsPresent("XLAT_CORNER",nlst_rt(did)%geo_static_flnm) .AND. &
        NWM_ESMF_NetcdfIsPresent("XLONG_CORNER",nlst_rt(did)%geo_static_flnm)) then
        xlat_corner_name = "XLAT_CORNER"
        xlon_corner_name = "XLONG_CORNER"
    else if (NWM_ESMF_NetcdfIsPresent("XLAT_C",nlst_rt(did)%geo_static_flnm) .AND. &
       NWM_ESMF_NetcdfIsPresent("XLONG_C",nlst_rt(did)%geo_static_flnm)) then
       xlat_corner_name = "XLAT_C"
       xlon_corner_name = "XLONG_C"
    else
       xlat_corner_name = ""
       xlon_corner_name = ""
    endif

    if (trim(xlat_corner_name) /= "") then
      ! Get Local Latitude (lat)
      allocate(latitude(local_nx_size(my_id+1)+1,local_ny_size(my_id+1)+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
        msg=METHOD//': Allocation of corner latitude memory failed.', &
        file=FILENAME, rcToReturn=rc)) return ! bail out

      call NWM_ESMF_NetcdfReadIXJX(trim(xlat_corner_name),nlst_rt(did)%geo_static_flnm, &
        (/startx(my_id+1),starty(my_id+1)/),latitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      ! Get Local Longitude (lon)
      allocate(longitude(local_nx_size(my_id+1)+1,local_ny_size(my_id+1)+1),stat=stat)
      if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg=METHOD//': Allocation of corner longitude memory failed.', &
          file=FILENAME, rcToReturn=rc)) return ! bail out

      call NWM_ESMF_NetcdfReadIXJX(trim(xlon_corner_name),nlst_rt(did)%geo_static_flnm, &
                                   (/startx(my_id+1),starty(my_id+1)/),longitude,rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
      ! Print Local Lat Lon Lower Left / Upper Right Corners
      write(logMsg,"(A,4(F0.3,A))") MODNAME//": Corner Coordinates = (", &
            longitude(1,1),":",longitude(local_nx_size(my_id+1)+1,local_ny_size(my_id+1)+1),",", &
            latitude(1,1),":",latitude(local_nx_size(my_id+1)+1,local_ny_size(my_id+1)+1),")"
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

      ! Add Corner Coordinates to Grid
      call ESMF_GridAddCoord(NWM_LSMGridCreate, staggerLoc=ESMF_STAGGERLOC_CORNER, rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      call ESMF_GridGetCoord(NWM_LSMGridCreate, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, &
           computationalLBound=lbnd, computationalUBound=ubnd, &
           farrayPtr=coordXcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call ESMF_GridGetCoord(NWM_LSMGridCreate, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CORNER, farrayPtr=coordYcorner, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      do j = lbnd(2),ubnd(2)
      do i = lbnd(1),ubnd(1)
        coordXcorner(i,j) = longitude(i,j)
        coordYcorner(i,j) = latitude(i,j)
      enddo
      enddo

      deallocate(latitude,longitude,stat=stat)
      if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
          msg=METHOD//': Deallocation of corner longitude and latitude memory failed.', &
          file=FILENAME,rcToReturn=rc)) return ! bail out

      call NWM_Add_Area(NWM_LSMGridCreate, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

    else
#ifdef DEBUG
      ! Warning no corners in domain file
      call ESMF_LogWrite(MODNAME//": No Corner Coordinates.", ESMF_LOGMSG_WARNING)
#endif
   endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_Add_Area"

  subroutine NWM_Add_Area(grid,rc)
    type(ESMF_Grid), intent(inout)   :: grid
    integer, intent(out)             :: rc

    ! Local Variables
    integer(ESMF_KIND_I4), PARAMETER :: R = 6376000 ! metres
    type(ESMF_Field)                 :: fieldArea
    type(ESMF_Array)                 :: areaArray
    integer                          :: i,j
    integer                          :: lbnd(2),ubnd(2)
    real(ESMF_KIND_R8), pointer      :: radianarea(:,:)
    real(ESMF_KIND_R8), pointer      :: gridarea(:,:)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

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

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_SetLocalIndices"

  subroutine NWM_SetLocalIndices(rc)
    ! ARGUMENTS
    integer, intent(out)                    :: rc

    ! LOCAL VARIABLES
    integer                     :: stat
    type(ESMF_VM)               :: currentVM
    integer                     :: localPet
    integer                     :: petCount
    type(ESMF_DELayout)         :: delayout
    integer, allocatable        :: dimExtent(:,:)
    integer, allocatable        :: iIndexList(:), jIndexList(:)
#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    !! Get VM Info to see if this will give me the PET info I need
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Get the grid distribution for this pet
    allocate(dimExtent(2, 0:(petCount - 1)),stat=stat) ! (dimCount, deCount)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of indexCountPDe memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(NWM_DistGrid, delayout=delayout, &
      indexCountPDe=dimExtent, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(iIndexList(dimExtent(1, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of iIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(NWM_DistGrid, localDe=0, dim=1, &
      indexList=iIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate(jIndexList(dimExtent(2, localPet)),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg=METHOD//': Allocation of jIndexList memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_DistGridGet(NWM_DistGrid, localDe=0, dim=2, &
      indexList=jIndexList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    x_start = minVal(iIndexList)
    x_end   = maxVal(iIndexList)
    y_start = minVal(jIndexList)
    y_end   = maxVal(jIndexList)

    nx_local = x_end - x_start + 1
    ny_local = y_end - y_start + 1

#ifdef DEBUG
    write (logMsg,"(A,6(I0,A))") MODNAME//": Local Indices = (", &
      x_start,":",x_end,",",y_start,":",y_end,") Local Size = (", &
      nx_local,"x",ny_local,")"
    call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
#endif

    deallocate(iIndexList,jIndexList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of IndexList memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

    deallocate(dimExtent,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=METHOD//': Deallocation of indexCountPDeo memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------


#undef METHOD
#define METHOD "NWM_RunModeGet"

  function NWM_RunModeGet(importState,rc)
    ! RETURN
    integer                          :: NWM_RunModeGet
    ! ARGUMENTS
    type(ESMF_State), intent(in)     :: importState
    integer, intent(out), optional   :: rc
    ! LOCAL VARIABLES
    integer                    :: fieldIndex
    integer                    :: forcingCount
    integer                    :: connectedCount
    type(ESMF_StateItem_Flag)  :: itemType

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS

    NWM_RunModeGet = 1
    forcingCount = 0
    connectedCount = 0

    do fieldIndex=1, size(NWM_FieldList)
      if(NWM_FieldList(fieldIndex)%adImport) then
        forcingCount = forcingCount + 1
        ! Check itemType to see if field exists in state
        call ESMF_StateGet(importState, &
          itemName=trim(NWM_FieldList(fieldIndex)%stdname), &
          itemType=itemType, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return

        if (itemType == ESMF_STATEITEM_FIELD) then
          if (NUOPC_IsConnected(importState, &
          fieldName=trim(NWM_FieldList(fieldIndex)%stdname))) then
            connectedCount = connectedCount + 1
          endif
        endif
      endif
    enddo

    if ( connectedCount == forcingCount ) then
      NWM_RunModeGet = 1
    end if
  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_GetHgrid"

  subroutine NWM_GetHgrid(did,hgrid,rc)
    ! ARGUMENTS
    integer, intent(in)         :: did
    character, intent(out)      :: hgrid
    integer, intent(out)        :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    hgrid = nlst_rt(did)%hgrid

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !------------------------------Time Utility -----------------------------------------------

#undef METHOD
#define METHOD "NWM_ClockToString"

  subroutine NWM_ClockToString(clock, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Clock)                :: clock
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    type(ESMF_Time)            :: currTime

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    ! Get the current time from the clock
    call ESMF_ClockGet(clock=clock,currTime=currTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call NWM_TimeToString(currTime,timestr,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

 !-----------------------------------------------------------------------------


#undef METHOD
#define METHOD "NWM_TimeToString"

  subroutine NWM_TimeToString(time, timestr, rc)
    ! ARGUMENTS
    type(ESMF_Time)                 :: time
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    character (len=256)        :: tmpstr = ''
    integer                    :: strlen

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize

    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg=METHOD//": Time string is too short!", &
        file=FILENAME,rcToReturn=rc)
      return  ! bail out
    endif

    CALL ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_TimeIntervalGetReal"

  function NWM_TimeIntervalGetReal(timeInterval,rc)
    ! RETURN VALUE:
    real                                :: NWM_TimeIntervalGetReal
    ! ARGUMENTS
    type(ESMF_TimeInterval),intent(in)  :: timeInterval
    integer, intent(out), optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)                  :: s_r8

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    if(present(rc)) rc = ESMF_SUCCESS

    NWM_TimeIntervalGetReal = -9999

    call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    NWM_TimeIntervalGetReal = s_r8

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------

    ! Get driver's clock properites  - TO DO
!    call ESMF_ClockGet(clock,timeStep=timestep,startTime=starttime,rc=rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    

    ! The ESMF_Clock's time step interval, receives timestep
    ! in double precision seconds,dt variable, where timestep 
    ! was set in driver code
!    call ESMF_TimeIntervalGet(timestep,s_r8=dt,rc=rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! The ESMF_Clock's starting time, receives in startTimeStr
!    call NWM_TimeToString(startdate,timestr=startTimeStr,rc=rc)
!    if(ESMF_STDERRORCHECK(rc)) return ! bail out
!    print*, "startTimeStr: ", startTimeStr
! --------------------------------------------------


#undef METHOD
#define METHOD "NWM_GetTimestep"

  function NWM_GetTimestep(did,rc)
    ! RETURN VALUE
    real :: NWM_GetTimestep
    ! ARGUMENTS
    integer, intent(in)         :: did
    integer, intent(out)        :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    NWM_GetTimestep = nlst_rt(did)%dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end function

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_SetTimestep"

  subroutine NWM_SetTimestep(did,dt,rc)
    ! ARGUMENTS
    integer, intent(in)           :: did
    real                          :: dt
    integer, intent(out)          :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    nlst_rt(did)%dt = dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  ! Dictionary Utility
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_FieldDictionaryAdd"

  subroutine NWM_FieldDictionaryAdd(fd, rc)
    ! ARGUMENTS
    character(len=64), optional :: fd
    integer,intent(out)         :: rc

    ! LOCAL VARIABLES
    integer                    :: fIndex
    logical                    :: isPresent

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(fd)) then
        ! read the field file and keep a local import/export field
        ! TO DO
    else    
      do fIndex=1,size(NWM_FieldList)
        isPresent = NUOPC_FieldDictionaryHasEntry( &
          trim(NWM_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        if (.not.isPresent) then
          call NUOPC_FieldDictionaryAddEntry( &
            trim(NWM_FieldList(fIndex)%stdname), &
            trim(NWM_FieldList(fIndex)%units), &
            rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
        endif
      enddo
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! Log Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_nlstLog"

  subroutine NWM_nlstLog(did,label,rc)
    ! ARGUMENTS
    integer,intent(in)                   :: did
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    integer                     :: layerIndex
    character(len=64)           :: l_label

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = METHOD
    endif

    write (logMsg,"(A,I0)") ": Domain ID      = ",did
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,5(I0,A))") ": Start Date     = ", &
      nlst_rt(did)%START_YEAR,"-",nlst_rt(did)%START_MONTH,"-", &
      nlst_rt(did)%START_DAY,"_",nlst_rt(did)%START_HOUR,":", &
      nlst_rt(did)%START_MIN
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Timestep       = ",nlst_rt(did)%dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Output Step    = ",nlst_rt(did)%out_dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Restart Step   = ",nlst_rt(did)%rst_dt
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Ter Routing Step   = ",nlst_rt(did)%dtrt_ter
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,F0.3)") ": Ch Routing Step   = ",nlst_rt(did)%dtrt_ch
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Grid ID        = ",nlst_rt(did)%igrid
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Hydro Grid     = ",nlst_rt(did)%hgrid
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Geo Grid File  = ",nlst_rt(did)%geo_static_flnm
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Fine Grid File = ",nlst_rt(did)%geo_finegrid_flnm
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": GW Basin File  = ",nlst_rt(did)%gwbasmskfil
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Restart Type   = ",nlst_rt(did)%rst_typ
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,A)") ": Restart file   = ",nlst_rt(did)%restart_file
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Coupling       = ",nlst_rt(did)%sys_cpl
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Channel RT     = ",nlst_rt(did)%CHANRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Subsurface RT  = ",nlst_rt(did)%SUBRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Overland RT    = ",nlst_rt(did)%OVRTSWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": GW Baseflow RT = ",nlst_rt(did)%GWBASESWCRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Routing Option = ",nlst_rt(did)%RT_OPTION
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Channel Option = ",nlst_rt(did)%channel_option
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Aggr Factor    = ",nlst_rt(did)%AGGFACTRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": GW Restart     = ",nlst_rt(did)%GW_RESTART
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": SWC Restart    = ",nlst_rt(did)%RSTRT_SWC
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,I0)") ": Soil Layers    = ",nlst_rt(did)%nsoil
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    do layerIndex=1,nlst_rt(did)%nsoil
      write (logMsg,"(A,I0,A,F0.3)") ": Soil layer depth (", &
        layerIndex,") = ",nlst_rt(did)%ZSOIL8(layerIndex)
      call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "NWM_DomainLog"

  subroutine NWM_DomainLog(did,label,rc)
    ! ARGUMENTS
    integer,intent(in)                   :: did
    character(len=*),intent(in),optional :: label
    integer,intent(out)                  :: rc

    ! LOCAL VARIABLES
    character(len=64)           :: l_label

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    if (present(label)) then
      l_label = label
    else
      l_label = METHOD
    endif

    write (logMsg,"(A,I0)") ": Domain ID      = ",did
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

    write (logMsg,"(A,L1)") ": Domain Init    = ",rt_domain(did)%initialized
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain IX      = ",rt_domain(did)%IX
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain JX      = ",rt_domain(did)%JX
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain IXRT    = ",rt_domain(did)%IXRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain JXRT    = ",rt_domain(did)%JXRT
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Domain Forc    = ",rt_domain(did)%FORC_TYP
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Max Links      = ",rt_domain(did)%NLINKS
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Num Lakes      = ",rt_domain(did)%NLAKES
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)
    write (logMsg,"(A,I0)") ": Num Basins     = ",rt_domain(did)%numbasns
    call ESMF_LogWrite(trim(l_label)//logMsg,ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

end module
