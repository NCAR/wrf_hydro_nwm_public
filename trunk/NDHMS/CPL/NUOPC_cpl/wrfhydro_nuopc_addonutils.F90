#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)

module wrfhydro_nuopc_addonutils

#include <netcdf.inc>

  private

  public :: count_connected_fields
  public :: clock_to_string
  public :: time_to_string
  public :: timeinterval_to_real
  public :: get_geostatic_array
  public :: state_isfieldconnected
  public :: grid_write
  public :: grid_print
  public :: copy_data_layer
  public :: copy_data_2D
  public :: state_reset

contains

  !-----------------------------------------------------------------------------
  ! Utilities
  !-----------------------------------------------------------------------------

  function count_connected_fields(state, rc)
    ! USES
    use ESMF
    use NUOPC

    implicit none

    ! ARGUMENTS
    type(ESMF_State), intent(in)            :: state
    integer, intent(out), optional          :: rc
    ! RETURN
    integer                                 :: count_connected_fields
    ! LOCAL VARIABLES
    character(len=ESMF_MAXSTR), allocatable :: fieldNameList(:)
    integer                                 :: i, itemCount
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:count_connected_fields)'

    if(present(rc)) rc = ESMF_SUCCESS
    count_connected_fields = 0

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(fieldNameList(itemCount))
    call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do i=1, itemCount
      if (NUOPC_IsConnected(state, fieldName=fieldNameList(i))) then
        call ESMF_LogWrite(msg=SUBNAME // ": Field IS connected: " // trim(fieldNameList(i)), &
          logmsgFlag=ESMF_LOGMSG_INFO)
        count_connected_fields = count_connected_fields + 1
      else
        call ESMF_LogWrite(msg=SUBNAME//": Field IS NOT connected: " // trim(fieldNameList(i)), &
          logmsgFlag=ESMF_LOGMSG_INFO)
      endif
    enddo
  end function

  !-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

  subroutine state_reset(state, value, rc)
    use ESMF
    ! ----------------------------------------------
    ! Set all fields to value in State
    ! If value is not provided, reset to 0.0
    ! ----------------------------------------------
    type(ESMF_State), intent(inout) :: state
    real(ESMF_KIND_R8)    , intent(in), optional :: value
    integer               , intent(out)   :: rc

    ! local variables
    real(ESMF_KIND_R8)          :: lvalue
    integer                     :: n
    integer                     :: itemCount
    character(ESMF_MAXSTR),allocatable :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)            :: field
    type(ESMF_TypeKind_Flag)    :: typekind
    integer                     :: rank
    real(ESMF_KIND_R8), pointer :: dataPtrR82D(:,:)
    character(len=*),parameter :: subname='(module_MEDIATOR:state_reset_2d)'

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    endif
    rc = ESMF_SUCCESS

    lvalue = 0.0_ESMF_KIND_R8
    if (present(value)) then
      lvalue = value
    endif

    call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    allocate(itemNameList(itemCount))
    allocate(itemTypeList(itemCount))
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
            msg="Typekind / Rank reset not implemented.",rcToReturn=rc)
          call ESMF_LogWrite(SUBNAME//": State reset failed on "//trim(itemNameList(n)), ESMF_LOGMSG_ERROR)
          return
        endif
      endif
    enddo
    deallocate(itemNameList)
    deallocate(itemTypeList)

    if (dbug_flag > 1) then
      call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)
    endif

  end subroutine state_reset

  subroutine clock_to_string(clock, timestr, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_Clock)                :: clock
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    type(ESMF_Time)                 :: currTime
    character (len=ESMF_MAXSTR)     :: tmpstr = ''
    integer                         :: strlen
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:clock_to_string)'

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize
    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogWrite(msg=SUBNAME//": timestr is too short!", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      if(present(rc)) rc = ESMF_RC_ARG_OUTOFRANGE
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
  end subroutine

!-----------------------------------------------------------------------------

  subroutine time_to_string(time, timestr, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_Time)                :: time
    integer, intent(out),optional   :: rc
    character (len=*), intent(out)  :: timestr

    ! LOCAL VARIABLES
    character (len=256)     :: tmpstr = ''
    integer                         :: strlen
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:clock_to_string)'

    if(present(rc)) rc = ESMF_SUCCESS  ! Initialize
    timestr = '' ! clear string

    if (len(timestr) < 19) then
      call ESMF_LogWrite(msg=SUBNAME//": timestr is too short!", &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      if(present(rc)) rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    CALL ESMF_TimeGet(time,timeString=tmpstr,rc=rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    strlen = min(len(timestr),len_trim(tmpstr))
    timestr(1:strlen) = tmpstr(1:strlen)
    timestr(11:11) = '_'
  end subroutine

  !-----------------------------------------------------------------------------

  function timeinterval_to_real(timeInterval,rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_TimeInterval),intent(in)  :: timeInterval
    real                                :: timeinterval_to_real
    integer, intent(out), optional      :: rc

    ! LOCAL VARIABLES
    real(ESMF_KIND_R8)                  :: s_r8
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:timeinterval_to_real)'

    if(present(rc)) rc = ESMF_SUCCESS
    timeinterval_to_real = -9999

    call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    timeinterval_to_real = s_r8

  end function

  !-----------------------------------------------------------------------------

  subroutine get_geostatic_array(name, geo_static_flnm, array, istart, jstart, ix, jx, rc)
    ! USES
    use ESMF
    use NETCDF

    implicit none

    ! ARGUMENTS
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
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:get_geostatic_array)'

    call ESMF_LogWrite(msg=SUBNAME//": called ", logmsgFlag=ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()

    ! INITIALIZE
    rc = ESMF_SUCCESS

    iret = nf90_open(geo_static_flnm,nf90_NoWrite,ncid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error opening domain file: "//trim(geo_static_flnm), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    iret = nf90_inq_varid(ncid,name,varid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error finding variable: "//trim(name), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    ! How big is the netCDF variable, that is, what are the lengths of
    !   its constituent dimensions?
    iret = nf90_inquire_variable(ncid, varid, dimids = dimIDs)
    if(iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error retrieving dimension IDs: "//trim(name), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(1), len = num_ix)
    if(iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error retrieving dimension 1 size: "//trim(name), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(2), len = num_jx)
    if(iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error retrieving dimension 2 size: "//trim(name), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif
    iret = nf90_inquire_dimension(ncid, dimIDs(3), len = num_times)
    if(iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error retrieving dimension 3 size: "//trim(name), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    write(msgString,*) num_ix, num_jx, num_times
    call ESMF_LogWrite(msg=SUBNAME // ": num_ix, num_jx, num_times = "//trim(msgString), &
      logmsgFlag=ESMF_LOGMSG_INFO)

    iret = nf90_get_var(ncid, varid, values=array, start=(/istart,jstart/), count=(/ix,jx/))
    if (iret /= nf90_NoErr) then
        call ESMF_LogWrite(msg=SUBNAME // ": Error retrieving variable: "//trim(name), &
            logmsgFlag=ESMF_LOGMSG_ERROR)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
    endif

    iret = nf_close(ncid)
    if (iret /= nf90_NoErr) then
      call ESMF_LogWrite(msg=SUBNAME // ": Error closing domain file: "//trim(geo_static_flnm), &
        logmsgFlag=ESMF_LOGMSG_ERROR)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    call ESMF_LogWrite(msg=SUBNAME//": called ", logmsgFlag=ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()

  end subroutine

  !-----------------------------------------------------------------------------
  !BOP
  ! !ROUTINE: state_isfieldconnected(state, fieldName, rc)
  !
  ! !INTERFACE:
  function state_isfieldconnected(state, fieldName, rc)
    ! !USES:
    use ESMF
    use NUOPC

    implicit none

    ! !RETURN VALUE:
    logical                                 :: state_isfieldconnected
    ! !ARGUMENTS:
    type(ESMF_State), intent(in)            :: state
    character(*),     intent(in)            :: fieldName
    integer,          intent(out), optional :: rc
    ! !DESCRIPTION:
    !   Return TRUE if the field with name fieldName contained in
    !   state is contained in state and connected. Otherwise return FALSE.
    !
    !EOP
    !-----------------------------------------------------------------------------
    ! local variables
    type(ESMF_Field)                        :: field
    type(ESMF_StateItem_Flag)               :: itemType
    character(ESMF_MAXSTR)                  :: connectedValue
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:state_isfieldconnected)'

    if (present(rc)) rc = ESMF_SUCCESS

    state_isfieldconnected = .false. ! initialize

    call ESMF_StateGet(state, itemName=fieldName, itemType=itemType, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (itemType == ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(state, itemName=fieldName, field=field, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      call NUOPC_GetAttribute(field,name="Connected", &
        value=connectedValue,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return

      if (connectedValue=="true") then
        state_isfieldconnected = .true.
      endif
    endif

  end function

  !-----------------------------------------------------------------------------

  subroutine grid_print(grid,string, rc)
    ! USES
    use ESMF
    ! ARGUMENTS
    type(ESMF_Grid), intent(in)   :: grid
    character(len=*), intent(in)  :: string
    integer         , intent(out) :: rc
    ! LOCAL VARIABLES
    character(len=256)          :: msgString
    type(ESMF_DistGrid)         :: distgrid
    character(ESMF_MAXSTR)      :: transferAction
    integer                     :: localDeCount
    integer                     :: dimCount, tileCount
    integer, allocatable        :: minIndexPTile(:,:), maxIndexPTile(:,:)

    character(len=*),parameter  :: subname='(lis_nuopc_addonutils:Grid_Print)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    ! access localDeCount to show this is a real Grid
    call ESMF_GridGet(grid, localDeCount=localDeCount, distgrid=distgrid, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,*) localDeCount
    call ESMF_LogWrite(SUBNAME//": "//trim(string)//": localDeCount = "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! get dimCount and tileCount
    call ESMF_DistGridGet(distgrid, dimCount=dimCount, tileCount=tileCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,*) dimCount
    call ESMF_LogWrite(SUBNAME//": "//trim(string)//": dimCount = "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,*) tileCount
    call ESMF_LogWrite(SUBNAME//": "//trim(string)//": tileCount = "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! allocate minIndexPTile and maxIndexPTile accord. to dimCount and tileCount
    allocate(minIndexPTile(dimCount, tileCount), &
      maxIndexPTile(dimCount, tileCount))

    ! get minIndex and maxIndex arrays
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
       maxIndexPTile=maxIndexPTile, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,*) minIndexPTile
    call ESMF_LogWrite(SUBNAME//": "//trim(string)//": minIndexPTile = "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (msgString,*) maxIndexPTile
    call ESMF_LogWrite(SUBNAME//": "//trim(string)//": maxIndexPTile = "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(minIndexPTile, maxIndexPTile)

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
  end subroutine grid_print

  !-----------------------------------------------------------------------------

  subroutine grid_write(grid, string, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_Grid) , intent(in)    :: grid
    character(len=*), intent(in)    :: string
    integer         , intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_Array)                :: array
    logical                         :: isPresent
    CHARACTER(LEN=*), PARAMETER :: SUBNAME='(nuopc_addon_utilities:grid_write)'

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(SUBNAME // ": called", ESMF_LOGMSG_INFO)

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
      call array_diagnose(array,trim(string)//"_grid_coord1", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_coord1.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CENTER, array=array, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArraySet(array, name="lat_center", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call array_diagnose(array,trim(string)//"_grid_coord2", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_coord2.nc", rc=rc)
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
        call array_diagnose(array,trim(string)//"_grid_corner1", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_corner1.nc", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif
      call ESMF_GridGetCoord(grid, coordDim=2, &
        staggerLoc=ESMF_STAGGERLOC_CORNER, array=array, rc=rc)
      if (.not. ESMF_STDERRORCHECK(rc)) then
        call ESMF_ArraySet(array, name="lat_corner", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call array_diagnose(array,trim(string)//"_grid_corner2", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_corner2.nc", rc=rc)
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
      call array_diagnose(array,trim(string)//"_grid_mask", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_mask.nc", rc=rc)
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
      call array_diagnose(array,trim(string)//"_grid_area", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
      call ESMF_ArrayWrite(array, fileName=trim(string)//"_grid_area.nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return
    endif

    call ESMF_LogWrite(SUBNAME // ": done", ESMF_LOGMSG_INFO)
  end subroutine

  ! ----------------------------------------------
  ! Diagnose status of array
  ! ----------------------------------------------
  subroutine array_diagnose(array, string, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_Array), intent(inout)         :: array
    character(len=*), intent(in), optional  :: string
    integer         , intent(out)           :: rc

    ! LOCAL VARIABLES
    character(len=256)              :: lstring
    character(len=256)              :: msgString
    type(ESMF_TypeKind_Flag)                :: typekind
    integer                                 :: rank
    real(ESMF_KIND_R4), pointer             :: dataPtr_R4_2D(:,:)
    real(ESMF_KIND_R4), pointer             :: dataPtr_R4_3D(:,:,:)
    real(ESMF_KIND_R8), pointer             :: dataPtr_R8_2D(:,:)
    real(ESMF_KIND_R8), pointer             :: dataPtr_R8_3D(:,:,:)
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:array_diagnose)'

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(SUBNAME // ": called", ESMF_LOGMSG_INFO)

    lstring = '' ! clear lstring
    if (present(string)) then
      lstring = string//': '
    endif

    call ESMF_ArrayGet(Array, typekind=typekind,rank=rank, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if (typekind == ESMF_TYPEKIND_R4) then
      if (rank == 2) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R4_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(3g14.7)') &
          minval(dataPtr_R4_2D),maxval(dataPtr_R4_2D),sum(dataPtr_R4_2D)
      else if (rank == 3) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R4_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(3g14.7)') &
          minval(dataPtr_R4_3D),maxval(dataPtr_R4_3D),sum(dataPtr_R4_3D)
      else
        write(msgString,*) "rank out of range"
      endif
    else if (typekind == ESMF_TYPEKIND_R8) then
      if (rank == 2) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R8_2D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(3g14.7)') &
          minval(dataPtr_R8_2D),maxval(dataPtr_R8_2D),sum(dataPtr_R8_2D)
      else if (rank == 3) then
        call ESMF_ArrayGet(Array, farrayPtr=dataPtr_R8_3D, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        write(msgString,'(3g14.7)') &
          minval(dataPtr_R8_3D),maxval(dataPtr_R8_3D),sum(dataPtr_R8_3D)
      else
        write(msgString,*) "rank out of range"
      endif
    else
      write(msgString,*) "typekind out of range"
    end if

    call ESMF_LogWrite(SUBNAME//': '//trim(lstring)//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)

  end subroutine array_diagnose

  !-----------------------------------------------------------------------------

  subroutine copy_data_layer(state, fieldName, modelArray, stateType, layer, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: modelArray(:,:,:)
    character,        intent(in)    :: stateType
    integer,          intent(in)    :: layer
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R8),pointer :: farrayPtr(:,:)
    integer                         :: compLBnd(2)
    integer                         :: compUBnd(2)
    character(len=5)                :: numString
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:copy_data_3D)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! retrieve the Fortran data pointer from the Field and bounds
    call ESMF_FieldGet(field=field, farrayPtr=farrayPtr, &
      computationalLBound=compLBnd, computationalUBound=compUBnd, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (stateType .eq. 'e') then
      farrayPtr = modelArray(:,:,layer)
    else
      modelArray(:,:,layer) = farrayPtr
    end if

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine copy_data_2D(state, fieldName, modelArray, stateType, rc)
    ! USES
    use ESMF

    implicit none

    ! ARGUMENTS
    type(ESMF_State), intent(inout) :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: modelArray(:,:)
    character,        intent(in)    :: stateType
    integer,          intent(out)   :: rc

    ! LOCAL VARIABLES
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R8),pointer :: farrayPtr(:,:)
    integer                         :: compLBnd(2)
    integer                         :: compUBnd(2)
    integer                         :: i, j
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='(nuopc_addon_utilities:copy_data_2D)'

    call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)

    rc = ESMF_SUCCESS

    call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! retrieve the Fortran data pointer from the Field and bounds
    call ESMF_FieldGet(field=field, farrayPtr=farrayPtr, &
      computationalLBound=compLBnd, computationalUBound=compUBnd, &
      rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (stateType .eq. 'e') then
      farrayPtr(compLBnd(1):compUBnd(1), &
        compLBnd(2):compUBnd(2)) = &
        modelArray(compLBnd(1):compUBnd(1), &
        compLBnd(2):compUBnd(2))
    else
      modelArray(compLBnd(1):compUBnd(1), &
        compLBnd(2):compUBnd(2)) = &
        farrayPtr(compLBnd(1):compUBnd(1), &
        compLBnd(2):compUBnd(2))
    endif

    call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    call ESMF_LogFlush()
  end subroutine

end module
