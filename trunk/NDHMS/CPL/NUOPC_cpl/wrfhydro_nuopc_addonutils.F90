#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "wrfhydro_nuopc_addonutils"
#define MODNAME "wrfhydro_nuopc_addonutils"

#define VERBOSITY_MIN 0
#define VERBOSITY_MAX 255
#define VERBOSITY_DBG 1023

module wrfhydro_nuopc_addonutils
  use ESMF
  use NUOPC
  use NETCDF

  implicit none

  private

  public :: mode_Unknown
  public :: mode_Offline
  public :: mode_Coupled
  public :: mode_Hybrid
  public :: type_FieldDesc
  public :: type_InternalStateStruct
  public :: type_InternalState
  public :: label_InternalState
  public :: clock_to_string
  public :: time_to_string
  public :: timeinterval_to_real
  public :: get_geostatic_array
  public :: grid_write
  public :: grid_print
  public :: field_print
  public :: array_print
  public :: copy_data
  public :: state_reset
  public :: field_list_add
  public :: field_list_print
  public :: set_runmode

  INTEGER, PARAMETER :: mode_Unknown = -1
  INTEGER, PARAMETER :: mode_Offline =  0
  INTEGER, PARAMETER :: mode_Coupled =  1
  INTEGER, PARAMETER :: mode_Hybrid  =  2

  INTEGER, PARAMETER :: FLDSMAX = 100

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  ! module interfaces
  interface array_print
    module procedure fortran_array_print_R2D
    module procedure fortran_array_print_R2D_layer
    module procedure fortran_array_print_I2D
    module procedure fortran_array_print_R1D
    module procedure esmf_array_print
  end interface
  interface copy_data
    module procedure copy_data_2D
    module procedure copy_data_2D_layer
  end interface

  type type_FieldDesc
    character(len=64)   :: stdname =" "
    character(len=64)   :: shortname = " "
    character(len=64)   :: transferOffer = " "
    logical             :: forcing = .FALSE.
    logical             :: import = .FALSE.
    logical             :: export = .FALSE.
    logical             :: assoc = .FALSE. ! is the farrayPtr associated with internal data
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr => null()
  endtype type_FieldDesc

  type type_InternalStateStruct
    integer               :: verbosity = VERBOSITY_MAX
    integer               :: mode = mode_Unknown
    logical               :: coldstart = .FALSE.
    logical               :: statewrite_flag = .FALSE.
    logical               :: profile_memory = .FALSE.
    integer               :: slice = 0
    integer               :: nest = 1
    real                  :: timestep = 0.0
    integer               :: fields_total = 0
    integer               :: fields_forcing = 0
    integer               :: fields_import = 0
    integer               :: fields_export = 0
    type (type_FieldDesc) :: field_list(fldsMax)
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  subroutine print_connected_fields(is,state,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(in)            :: state
    integer, intent(out), optional          :: rc
    ! LOCAL VARIABLES
    integer                                 :: stat
    character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)
    integer                                 :: i, itemCount
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='print_connected_fields'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(fieldNameList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of fieldNameList memory failed.', &
      method=SUBNAME, file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do i=1, itemCount
      if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
        call ESMF_LogWrite("Field IS connected: " // trim(fieldNameList(i)), &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      else
        call ESMF_LogWrite("Field IS NOT connected: " // trim(fieldNameList(i)), &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      endif
    enddo

    deallocate(fieldNameList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of fieldNameList memory failed.', &
      method=SUBNAME,file=FILENAME,rcToReturn=rc)) return ! bail out

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

  end subroutine

  subroutine state_reset(is,state, value, rc)
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(inout) :: state
    real(ESMF_KIND_R8)    , intent(in), optional :: value
    integer               , intent(out)   :: rc

    ! local variables
    integer                     :: stat
    real(ESMF_KIND_R8)          :: lvalue
    integer                     :: n
    integer                     :: itemCount
    character(ESMF_MAXSTR),allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)            :: field
    type(ESMF_TypeKind_Flag)    :: typekind
    integer                     :: rank
    real(ESMF_KIND_R8), pointer :: dataPtrR82D(:,:)
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='state_reset_2d'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    lvalue = 0.0_ESMF_KIND_R8
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(itemNameList(itemCount),itemTypeList(itemCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of itemList memory failed.', &
      method=SUBNAME, file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_StateGet(state, itemNameList=itemNameList, itemTypeList=itemTypeList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    do n = 1, itemCount
      if(itemTypeList(n) .EQ. ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(state, itemName=itemNameList(n), field=field, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
        call ESMF_FieldGet(field,typekind=typekind, rank=rank, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out
        if(typekind .EQ. ESMF_TYPEKIND_R8 .AND. rank .EQ. 2) then
          call ESMF_FieldGet(field,farrayPtr=dataPtrR82D,rc=rc)
          if(ESMF_STDERRORCHECK(rc)) return ! bail out
          dataPtrR82D = lvalue
        else
          call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
            msg="Typekind / Rank reset not implemented for "//trim(itemNameList(n)), &
            file=FILENAME,method=SUBNAME,rcToReturn=rc)
          return
        endif
      endif
    enddo
    deallocate(itemNameList,itemTypeList,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of itemList memory failed.', &
      method=SUBNAME,file=FILENAME,rcToReturn=rc)) return ! bail out

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine state_reset

  subroutine clock_to_string(is,clock, timestr, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Clock)                :: clock
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    type(ESMF_Time)                 :: currTime
    character (len=ESMF_MAXSTR)     :: tmpstr = ''
    integer                         :: strlen
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='clock_to_string'

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize
    timestr = '' ! clear string

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Time string is too short!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    ! Get the current time from the clock
    call ESMF_ClockGet(clock=clock,currTime=currTime,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    CALL ESMF_TimeGet(currTime,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

!-----------------------------------------------------------------------------

  subroutine time_to_string(is,time, timestr, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Time)                :: time
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    character (len=256)     :: tmpstr = ''
    integer                         :: strlen
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='time_to_string'

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize
    timestr = '' ! clear string

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (len(timestr) < 19) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="Time string is too short!", &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    CALL ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Time string: "//trim(timestr), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  function timeinterval_to_real(is,timeInterval,rc)
    ! RETURN VALUE:
    real                                :: timeinterval_to_real
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_TimeInterval),intent(in)  :: timeInterval
    integer, intent(out), optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)                  :: s_r8
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='timeinterval_to_real'

    if(present(rc)) rc = ESMF_SUCCESS
    timeinterval_to_real = -9999

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    timeinterval_to_real = s_r8

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end function

  !-----------------------------------------------------------------------------

  subroutine get_geostatic_array(is,name, geo_static_flnm, array, istart, jstart, ix, jx, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in)        :: name
    character(len=*), intent(in)        :: geo_static_flnm
    integer, intent(in)                 :: istart, jstart
    integer, intent(in)                 :: ix, jx
    real, dimension(ix,jx), intent(out) :: array
    integer, intent(out)                :: rc

    ! LOCAL VARIABLES
    integer                             :: iret, varid
    integer                             :: num_times, num_ix, num_jx
    integer, dimension(nf90_max_var_dims) :: dimIDs
    integer                             :: ncid
    character(len=256)                  :: msgString
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='get_geostatic_array'

    ! INITIALIZE
    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    iret = nf90_open(geo_static_flnm,nf90_NoWrite,ncid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_OPEN, &
        msg="Error opening domain file: "//trim(geo_static_flnm), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    iret = nf90_inq_varid(ncid,name,varid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error finding variable: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    iret = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error retrieving dimension IDs: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(1), len = num_ix)
    if(iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error retrieving dimension 1 size: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(2), len = num_jx)
    if(iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error retrieving dimension 2 size: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(3), len = num_times)
    if(iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error retrieving dimension 3 size: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      write(msgString,"(3A,3(I0,A))") "geostatic array: ",trim(name), &
        " (num_ix,num_jx,num_times): (", &
        num_ix,",",num_jx,",",num_times,")"
      call ESMF_LogWrite(trim(msgString),ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    iret = nf90_get_var(ncid, varid, values=array, start=(/istart,jstart/), count=(/ix,jx/))
    if (iret /= nf90_NoErr) then
        call ESMF_LogSetError(ESMF_RC_FILE_READ, &
        msg="Error retrieving variable: "//trim(name), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
        return  ! bail out
    endif

    iret = nf90_close(ncid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogSetError(ESMF_RC_FILE_CLOSE, &
        msg="Error closing domain file: "//trim(geo_static_flnm), &
        file=FILENAME,method=SUBNAME,rcToReturn=rc)
      return  ! bail out
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine grid_print(is,grid,label, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Grid), intent(in)   :: grid
    character(len=*), intent(in)  :: label
    integer         , intent(out) :: rc
    ! LOCAL VARIABLES
    integer                     :: stat
    character(len=256)          :: msgString
    type(ESMF_DistGrid)         :: distgrid
    character(ESMF_MAXSTR)      :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer                     :: dimIndex, tileIndex
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='Grid_Print'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,"(2A,I0)") trim(label)," Local decomposition count: ",localDeCount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,"(2A,I0)") trim(label)," Dimension count: ",dimCount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    write (msgString,"(2A,I0)") trim(label)," Tile count: ",tileCount
    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount),stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of indexPTile memory failed.', &
      method=SUBNAME, file=FILENAME, rcToReturn=rc)) return ! bail out

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do tileIndex=1,tileCount
    do dimIndex=1,dimCount
      write (msgString,"(2A,4(I0,A))") trim(label)," (tile,dim,minIndexPTile,maxIndexPTile): (", &
        tileIndex,",",dimIndex,",", &
        minIndexPTile(dimIndex,tileIndex),",", &
        maxIndexPTile(dimIndex,tileIndex),")"
      call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    enddo
    enddo

    deallocate(minIndexPTile, maxIndexPTile,stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of indexPTile memory failed.', &
      method=SUBNAME,file=FILENAME,rcToReturn=rc)) return ! bail out

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine grid_print

  !-----------------------------------------------------------------------------

  subroutine grid_write(is,grid,label,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Grid) , intent(in)    :: grid
    character(len=*), intent(in)    :: label
    integer         , intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_Array)                :: array
    logical                         :: isPresent
    CHARACTER(LEN=*), PARAMETER :: SUBNAME='grid_write'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! -- centers --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
      isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="lon_center", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call array_print(is,array,trim(label)//"_grid_coord1", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_coord1.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="lat_center", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call array_print(is,array,trim(label)//"_grid_coord2", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_coord2.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! -- corners --

    call ESMF_GridGetCoord(grid, staggerLoc=ESMF_STAGGERLOC_CORNER, &
      isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetCoord(grid, coordDim=1, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_STDERRORCHECK(rc)) then
        call ESMF_ArraySet(array, name="lon_corner", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call array_print(is,array,trim(label)//"_grid_corner1", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_corner1.nc", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_STDERRORCHECK(rc)) then
        call ESMF_ArraySet(array, name="lat_corner", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call array_print(is,array,trim(label)//"_grid_corner2", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_corner2.nc", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
    endif

    ! -- mask --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_MASK, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_MASK, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="mask", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call array_print(is,array,trim(label)//"_grid_mask", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_mask.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    ! -- area --

    call ESMF_GridGetItem(grid, itemflag=ESMF_GRIDITEM_AREA, &
      staggerLoc=ESMF_STAGGERLOC_CENTER, isPresent=isPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    if (isPresent) then
      call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        itemflag=ESMF_GRIDITEM_AREA, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="area", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call array_print(is,array,trim(label)//"_grid_area", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(label)//"_grid_area.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  subroutine field_print(is, field, label, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Field), intent(in)            :: field
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    type(ESMF_Array)            :: array
    type(ESMF_TypeKind_Flag)    :: typekind
    integer                     :: rank
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_1D(:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_2D(:,:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_3D(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_1D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_2D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_3D(:,:,:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='FieldPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (min,max,sum): ("
    else
      llabel = "ESMF_Field (min,max,sum): ("
    endif

    call ESMF_FieldGet(field,array=array, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    call ESMF_ArrayGet(array, typekind=typekind,rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (typekind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_1D),",", &
          maxval(dataPtr_R4_1D),",", &
          sum(dataPtr_R4_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_2D),",", &
          maxval(dataPtr_R4_2D),",", &
          sum(dataPtr_R4_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R4_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_3D),",", &
          maxval(dataPtr_R4_3D),",", &
          sum(dataPtr_R4_3D),")"
      else
        write(msgString,"(2A)") trim(llabel), "rank out of range)"
      endif
    elseif (typekind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_1D),",", &
          maxval(dataPtr_R8_1D),",", &
          sum(dataPtr_R8_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_2D),",", &
          maxval(dataPtr_R8_2D),",", &
          sum(dataPtr_R8_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(array, farrayPtr=dataPtr_R8_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_3D),",", &
          maxval(dataPtr_R8_3D),",", &
          sum(dataPtr_R8_3D),")"
      else
        write(msgString,"(2A)") trim(llabel), "rank out of range)"
      endif
    else
      write(msgString,"(2A)") trim(llabel), "typekind out of range)"
    endif

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine field_print

  ! ----------------------------------------------
  ! Print status of array
  ! ----------------------------------------------

subroutine fortran_array_print_R2D_layer(is, label, array, layer, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in), optional  :: label
    real, intent(in)                        :: array(:,:,:)
    integer, intent(in)                     :: layer
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ArrayPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (layer,min,max,sum): ("
    else
      llabel = "FORTRAN_Array (layer,min,max,sum): ("
    endif

    write(msgString,'(A,I0,A,3(F0.3,A))') trim(llabel), &
      layer,",", &
      minval(array(:,:,layer)),",", &
      maxval(array(:,:,layer)),",", &
      sum(array(:,:,layer)),")"

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

  end subroutine

  subroutine fortran_array_print_R2D(is, label, array, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in), optional  :: label
    real, intent(in)                        :: array(:,:)
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ArrayPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (min,max,sum): ("
    else
      llabel = "FORTRAN_Array (min,max,sum): ("
    endif

    write(msgString,'(A,3(F0.3,A))') trim(llabel), &
      minval(array),",", &
      maxval(array),",", &
      sum(array),")"

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

  end subroutine

subroutine fortran_array_print_I2D(is, label, array, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in), optional  :: label
    integer, intent(in)                     :: array(:,:)
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ArrayPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (min,max,sum): ("
    else
      llabel = "FORTRAN_Array (min,max,sum): ("
    endif

    write(msgString,'(A,3(I0,A))') trim(llabel), &
      minval(array),",", &
      maxval(array),",", &
      sum(array),")"

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

  end subroutine

subroutine fortran_array_print_R1D(is, label, array, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    character(len=*), intent(in), optional  :: label
    real, intent(in)                        :: array(:)
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ArrayPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (min,max,sum): ("
    else
      llabel = "FORTRAN_Array (min,max,sum): ("
    endif

    write(msgString,'(A,3(F0.3,A))') trim(llabel), &
      minval(array),",", &
      maxval(array),",", &
      sum(array),")"

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

  end subroutine
    
  subroutine esmf_array_print(is, array, label, rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_Array), intent(in)            :: array
    character(len=*), intent(in), optional  :: label
    integer, intent(out), optional          :: rc

    ! LOCAL VARIABLES
    character(len=256)          :: llabel
    character(len=256)          :: msgString
    type(ESMF_TypeKind_Flag)    :: typekind
    integer                     :: rank
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_1D(:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_2D(:,:)
    real(ESMF_KIND_R4), pointer :: dataPtr_R4_3D(:,:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_1D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_2D(:,:)
    real(ESMF_KIND_R8), pointer :: dataPtr_R8_3D(:,:,:)
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ArrayPrint'

    if(present(rc)) rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (present(label)) then
      llabel = trim(label)//" (min,max,sum): ("
    else
      llabel = "ESMF_Array (min,max,sum): ("
    endif

    call ESMF_ArrayGet(array, typekind=typekind,rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (typekind == ESMF_TYPEKIND_R4) then
      if (rank == 1) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R4_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_1D),",", &
          maxval(dataPtr_R4_1D),",", &
          sum(dataPtr_R4_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R4_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_2D),",", &
          maxval(dataPtr_R4_2D),",", &
          sum(dataPtr_R4_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R4_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R4_3D),",", &
          maxval(dataPtr_R4_3D),",", &
          sum(dataPtr_R4_3D),")"
      else
        write(msgString,"(2A)") trim(llabel), "rank out of range)"
      endif
    elseif (typekind == ESMF_TYPEKIND_R8) then
      if (rank == 1) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R8_1D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_1D),",", &
          maxval(dataPtr_R8_1D),",", &
          sum(dataPtr_R8_1D),")"
      elseif (rank == 2) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R8_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_2D),",", &
          maxval(dataPtr_R8_2D),",", &
          sum(dataPtr_R8_2D),")"
      elseif (rank == 3) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R8_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(A,3(F0.3,A))') trim(llabel), &
          minval(dataPtr_R8_3D),",", &
          maxval(dataPtr_R8_3D),",", &
          sum(dataPtr_R8_3D),")"
      else
        write(msgString,"(2A)") trim(llabel), "rank out of range)"
      endif
    else
      write(msgString,"(2A)") trim(llabel), "typekind out of range)"
    endif

    call ESMF_LogWrite(trim(msgString), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine esmf_array_print

  !-----------------------------------------------------------------------------

  subroutine copy_data_2D_layer(is,state,fieldName,modelArray,stateType,layer,relaxedFlag,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: modelArray(:,:,:)
    character,        intent(in)    :: stateType
    integer,          intent(in)    :: layer
    logical,          intent(in)    :: relaxedFlag
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_StateItem_Flag)       :: itemType
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R8),pointer :: farrayPtr(:,:)
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='copy_data'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      if (stateType .eq. 'i') then
        call ESMF_LogWrite("Copying field data to array from NUOPC%"//trim(fieldName), &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      elseif (stateType .eq. 'e') then
        call ESMF_LogWrite("Copying field data from array to NUOPC%"//trim(fieldName), &
          ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      endif
    endif

    call ESMF_StateGet(state, itemName=fieldName, itemType=itemType, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (itemType == ESMF_STATEITEM_FIELD) then
      if(NUOPC_IsConnected(state,trim(fieldName),rc=rc)) then
        call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out

        ! retrieve the Fortran data pointer from the Field and bounds
        call ESMF_FieldGet(field=field,farrayPtr=farrayPtr,rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out

        if (stateType .eq. 'e') then
          farrayPtr = modelArray(:,:,layer)
          if (is%wrap%verbosity >= VERBOSITY_DBG) then
            call ESMF_LogWrite("Field data copied from model array to NUOPC%"//trim(fieldName), &
              ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
        else
          modelArray(:,:,layer) = farrayPtr
          if (is%wrap%verbosity >= VERBOSITY_DBG) then
            call ESMF_LogWrite("Field data copied to model array from NUOPC%"//trim(fieldName), &
              ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
        endif
      else
        if (.not. relaxedFlag) then
          call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
            msg="Field is not connected in state. "//trim(fieldName), &
            rcToReturn=rc)
          return
        endif
      endif
    else
      if (.not. relaxedFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="Field does not exist in state. "//trim(fieldName), &
          rcToReturn=rc)
        return
      endif
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine copy_data_2D(is,state,fieldName,modelArray,stateType,relaxedFlag,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: modelArray(:,:)
    character,        intent(in)    :: stateType
    logical,          intent(in)    :: relaxedFlag
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_StateItem_Flag)       :: itemType
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R8),pointer :: farrayPtr(:,:)
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='copy_data'

    rc = ESMF_SUCCESS

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_StateGet(state, itemName=fieldName, itemType=itemType, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (itemType == ESMF_STATEITEM_FIELD) then
      if(NUOPC_IsConnected(state,trim(fieldName),rc=rc)) then
        call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out

        ! retrieve the Fortran data pointer from the Field and bounds
        call ESMF_FieldGet(field=field,farrayPtr=farrayPtr,rc=rc)
        if(ESMF_STDERRORCHECK(rc)) return ! bail out

        if (stateType .eq. 'e') then
          farrayPtr = modelArray
          if (is%wrap%verbosity >= VERBOSITY_DBG) then
            call ESMF_LogWrite("Field data copied to NUOPC: "//trim(fieldName), &
              ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
        else
          modelArray = farrayPtr
          if (is%wrap%verbosity >= VERBOSITY_DBG) then
            call ESMF_LogWrite("Field data copied to model: "//trim(fieldName), &
              ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
        endif
      else
        if (.not. relaxedFlag) then
          call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
            msg="Field is not connected in state. "//trim(fieldName), &
            rcToReturn=rc)
          return
        endif
      endif
    else
      if (.not. relaxedFlag) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="Field does not exist in state. "//trim(fieldName), &
          rcToReturn=rc)
        return
      endif
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine

!-----------------------------------------------------------------------------

  subroutine field_list_add(is,stdname,transferOffer,forcing,import,export,shortname,data,rc)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! ----------------------------------------------
    type(type_InternalState),intent(inout)          :: is
    character(len=*),intent(in)                     :: stdname
    character(len=*),intent(in)                     :: transferOffer
    logical,intent(in)                              :: forcing
    logical,intent(in)                              :: import
    logical,intent(in)                              :: export
    character(len=*),intent(in),optional            :: shortname
    real(ESMF_KIND_R8),intent(in), target, optional :: data(:,:,:)
    integer,intent(out),optional                    :: rc

    ! local variables
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='field_list_add'

    if(present(rc)) rc = ESMF_SUCCESS

    if ( is%wrap%verbosity >= VERBOSITY_DBG ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! fill in the new entry

    is%wrap%fields_total = is%wrap%fields_total + 1
    if (is%wrap%fields_total > size(is%wrap%field_list)) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
         msg="field_list_add ERROR num gt fldsMax "//trim(stdname), &
         rcToReturn=rc)
      return  ! bail out
    endif

    is%wrap%field_list(is%wrap%fields_total)%stdname            = trim(stdname)
        is%wrap%field_list(is%wrap%fields_total)%transferOffer  = trim(transferOffer)
    is%wrap%field_list(is%wrap%fields_total)%forcing            = forcing
    if (forcing) is%wrap%fields_forcing = is%wrap%fields_forcing + 1
    is%wrap%field_list(is%wrap%fields_total)%import             = import
    if (import) is%wrap%fields_import = is%wrap%fields_import + 1
    is%wrap%field_list(is%wrap%fields_total)%export             = export
    if (export) is%wrap%fields_export = is%wrap%fields_export + 1
    if (present(shortname)) then
       is%wrap%field_list(is%wrap%fields_total)%shortname   = trim(shortname)
    else
       is%wrap%field_list(is%wrap%fields_total)%shortname   = trim(stdname)
    endif
    if (present(data)) then
      is%wrap%field_list(is%wrap%fields_total)%assoc        = .true.
      is%wrap%field_list(is%wrap%fields_total)%farrayPtr    => data
    else
      is%wrap%field_list(is%wrap%fields_total)%assoc        = .false.
    endif

    if ( is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine field_list_add

  subroutine field_list_print(is,rc)
    type(type_InternalState),intent(inout)          :: is
    integer,intent(out),optional                    :: rc

    ! local variables
    integer                    :: fieldIndex
    character(len=ESMF_MAXSTR) :: logMsg
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='field_list_print'

    if(present(rc)) rc = ESMF_SUCCESS

    if ( is%wrap%verbosity >= VERBOSITY_DBG ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_LogWrite("Forcing field list:",ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    do fieldIndex = 1, is%wrap%fields_total
      if(is%wrap%field_list(fieldIndex)%forcing) then
        call ESMF_LogWrite(" "//trim(is%wrap%field_list(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
      endif
    enddo

    call ESMF_LogWrite("Import field list:",ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    do fieldIndex = 1, is%wrap%fields_total
      if(is%wrap%field_list(fieldIndex)%import) then
        call ESMF_LogWrite(" "//trim(is%wrap%field_list(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
      endif
    enddo

    call ESMF_LogWrite("Export field list:",ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    do fieldIndex = 1, is%wrap%fields_total
      if(is%wrap%field_list(fieldIndex)%export) then
        call ESMF_LogWrite(" "//trim(is%wrap%field_list(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
      endif
    enddo

    write (logMsg,"(A,I0)") "Total field count: ",is%wrap%fields_total
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    write (logMsg,"(A,I0)") "Forcing field count: ",is%wrap%fields_forcing
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    write (logMsg,"(A,I0)") "Import field count: ",is%wrap%fields_import
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    write (logMsg,"(A,I0)") "Export field count: ",is%wrap%fields_export
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)

    if ( is%wrap%verbosity >= VERBOSITY_DBG ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine field_list_print

  subroutine set_runmode(is,importState,rc)
    ! ARGUMENTS
    type(type_InternalState), intent(inout) :: is
    type(ESMF_State), intent(in)            :: importState
    integer, intent(out), optional          :: rc
    ! LOCAL VARIABLES
    integer                    :: fieldIndex, connectedCount
    type(ESMF_StateItem_Flag)  :: itemType
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='set_runmode'

    if(present(rc)) rc = ESMF_SUCCESS
    is%wrap%mode = mode_Unknown
    connectedCount = 0

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    do fieldIndex=1, is%wrap%fields_total
      if(is%wrap%field_list(fieldIndex)%forcing) then
        ! Check itemType to see if field exists in state
        call ESMF_StateGet(importState, &
          itemName=trim(is%wrap%field_list(fieldIndex)%stdname), &
          itemType=itemType, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return

        if (itemType == ESMF_STATEITEM_FIELD) then
          if (NUOPC_IsConnected(importState, fieldName=trim(is%wrap%field_list(fieldIndex)%stdname))) then
            connectedCount = connectedCount + 1
          endif
        endif
      endif
    enddo

    if( connectedCount == 0 ) then
      is%wrap%mode = mode_Offline
    elseif ( connectedCount == is%wrap%fields_forcing ) then
      is%wrap%mode = mode_Coupled
    elseif ( connectedCount < is%wrap%fields_forcing ) then
      is%wrap%mode = mode_Hybrid
    endif

    if (is%wrap%verbosity >= VERBOSITY_DBG) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif
  end subroutine set_runmode

end module
