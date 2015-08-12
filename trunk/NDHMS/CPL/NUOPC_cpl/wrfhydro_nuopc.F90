module wrfhydro_nuopc

  !-----------------------------------------------------------------------------
  ! HYDRO Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance

  use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                mpp_land_bcast_real, mpp_land_bcast_int1

  use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe
  use module_CPL_LAND, only: CPL_LAND_INIT
  use module_rt_data, only: rt_domain
  use module_namelist, only: nlst_rt


  implicit none

#include <netcdf.inc>


  private

    real(ESMF_KIND_R8) :: min_lat, max_lat, min_lon, max_lon
    integer :: i_count, j_count
    integer :: did
    integer :: nsoil

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv00p2"/), userRoutine=InitializeP2, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables

!print *, 'KDS: HYDRO InitializeP1 - begin'
    rc = ESMF_SUCCESS

    !!
    !! import fields
    !!
    call NUOPC_StateAdvertiseField(importState, &
      StandardName="soil_temperature_b", name="soil_temperature_b", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, &
      StandardName="soil_moisture", name="soil_moisture", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, &
      StandardName="soil_water_content", name="soil_water_content", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, &
      StandardName="surface_runoff", name="surface_runoff", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(importState, &
      StandardName="subsurface_runoff", name="subsurface_runoff", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !!
    !! export fields
    !!
    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="soil_temperature", name="soil_temperature", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="soil_moisture", name="soil_moisture", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="soil_water_content", name="soil_water_content", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_StateAdvertiseField(exportState, &
      StandardName="surface_head", name="surface_head", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!print *, 'KDS: HYDRO InitializeP1 - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP2(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_ArraySpec)    :: soilArraySpec
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    type(ESMF_DistGrid)     :: distGrid


    min_lat = 38.60428
    max_lat = 40.94429
    min_lon = -106.6588
    max_lon = -103.5294

    i_count = 268
    j_count = 260
    nsoil = 4

!print *, 'KDS: HYDRO InitializeP2 - begin'
    rc = ESMF_SUCCESS

    ! create a Grid object for Fields
    gridIn = NUOPC_GridCreateSimpleXY( &
      min_lat, max_lat, &
      min_lon, max_lon, &
      i_count, j_count, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    gridOut = gridIn ! for now out same as in

    ! See if the grid create has a dist grid already
    call ESMF_GridGet(gridOut, distgrid=distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_DistGridPrint(distgrid=distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create an ArraySpec object for those Fields that have a 3rd
    ! dimension for soil
    call ESMF_ArraySpecSet(soilArraySpec, 3, ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !!
    !! import fields
    !!
    field = ESMF_FieldCreate(name="soil_temperature_b", grid=gridIn, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="soil_moisture", grid=gridIn, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="soil_water_content", grid=gridIn, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="surface_runoff", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="subsurface_runoff", grid=gridIn, &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !!
    !! export fields
    !!
    field = ESMF_FieldCreate(name="soil_temperature", grid=gridOut, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="soil_moisture", grid=gridOut, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="soil_water_content", grid=gridOut, &
      arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
      ungriddedLBound=(/1/), ungriddedUBound=(/nsoil/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    field = ESMF_FieldCreate(name="surface_head", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R4, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_StateRealizeField(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


    call InitHydro(distGrid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!print *, 'KDS: HYDRO InitializeP2 - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState


!print *, 'KDS: ModelAdvance - begin'
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(gcomp, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call NUOPC_ClockPrintCurrTime(clock, &
      "------>Advancing HYDRO from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_ClockPrintStopTime(clock, &
      "--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !! Copy data from import fields to RT_Domain object
    call CopyImportData(importState, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !!
    !! Call the WRF-HYDRO run routine
    !!
    call HYDRO_exe(did=did)

    !! Copy data from RT_Domain object to export fields
    call CopyExportData(exportState, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!print *, 'KDS: ModelAdvance - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CopyImportData(importState, rc)
    type(ESMF_State), intent(in)  :: importState
    integer,          intent(out) :: rc

    ! local variables


!print *, 'KDS: CopyImportData - begin'
    rc = ESMF_SUCCESS

    call CopyData3d(importState, "soil_temperature_b", rt_domain(did)%STC, 'i', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData3d(importState, "soil_moisture", rt_domain(did)%smc, 'i', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData3d(importState, "soil_water_content", rt_domain(did)%sh2ox, 'i', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData2d(importState, "surface_runoff", rt_domain(did)%infxsrt, 'i', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!!
!! KDS: This is the code giving me fits... uncomment it and the code will
!!      seg fault.  Basically, it's copying the value from the importState
!!      to the soldrain array.  I've checked the soldrain array and it is
!!      allocated and it's got the same size as the importState array.
!!      It gets through this call, but causes a seg fault elsewhere (and that
!!      elsewhere is not consistent... last time I ran it, it was crashing in
!!      the NUOPC compliance checker).  I've tried tracking it down in
!!      TotalView, but TV crashes during the HYDRO_ini when trying to read
!!      in a NetCDF file not related to this problem (I think that may be a
!!      memory size issue).  I've also tried re-arranging the calls, putting
!!      infxsrt as the last import field... but that didn't affect it at all.
!!      Not sure what to try next.
!    call CopyData2d(importState, "subsurface_runoff", rt_domain(did)%soldrain, 'i', rc)
!    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!      line=__LINE__, &
!      file=__FILE__)) &
!      return  ! bail out

!print *, 'KDS: CopyImportData - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CopyExportData(exportState, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(out)   :: rc

    ! local variables


    rc = ESMF_SUCCESS
!print *, 'KDS: CopyExportData - begin'

    call CopyData3d(exportState, "soil_temperature", rt_domain(did)%STC, 'e', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData3d(exportState, "soil_moisture", rt_domain(did)%smc, 'e', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData3d(exportState, "soil_water_content", rt_domain(did)%sh2ox, 'e', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call CopyData2d(exportState, "surface_head", rt_domain(did)%sfcheadrt, 'e', rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!print *, 'KDS: CopyExportData - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CopyData3d(state, fieldName, hydroArray, stateType, rc)
    type(ESMF_State), intent(in)    :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: hydroArray(:,:,:)
    character,        intent(in)    :: stateType
    integer,          intent(out)   :: rc

    ! local variables
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R4),pointer :: farrayPtr(:,:,:)
    integer                         :: compLBnd(3)
    integer                         :: compUBnd(3)


    rc = ESMF_SUCCESS
!print *, 'KDS: CopyData3d - begin'

    call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! retrieve the Fortran data pointer from the Field and bounds
    call ESMF_FieldGet(field=field, farrayPtr=farrayPtr, &
        computationalLBound=compLBnd, computationalUBound=compUBnd, &
        rc=rc)

!print *, 'KDS - Field Info - compLBnd(1): ', compLBnd(1)
!print *, 'KDS - Field Info - compLBnd(2): ', compLBnd(2)
!print *, 'KDS - Field Info - compLBnd(3): ', compLBnd(3)
!print *, 'KDS - Field Info - compUBnd(1): ', compUBnd(1)
!print *, 'KDS - Field Info - compUBnd(2): ', compUBnd(2)
!print *, 'KDS - Field Info - compUBnd(3): ', compUBnd(3)

    if (stateType .eq. 'e') then
      farrayPtr(compLBnd(1):compUBnd(1), &
                compLBnd(2):compUBnd(2), &
                compLBnd(3):compUBnd(3)) = &
        hydroArray(compLBnd(1):compUBnd(1), &
                   compLBnd(2):compUBnd(2), &
                   compLBnd(3):compUBnd(3))
    else
      hydroArray(compLBnd(1):compUBnd(1), &
                 compLBnd(2):compUBnd(2), &
                 compLBnd(3):compUBnd(3)) = &
        farrayPtr(compLBnd(1):compUBnd(1), &
                  compLBnd(2):compUBnd(2), &
                  compLBnd(3):compUBnd(3))
    end if
!print *, 'KDS: CopyData3d - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CopyData2d(state, fieldName, hydroArray, stateType, rc)
    type(ESMF_State), intent(in)    :: state
    character(len=*), intent(in)    :: fieldName
    real,             intent(inout) :: hydroArray(:,:)
    character,        intent(in)    :: stateType
    integer,          intent(out)   :: rc

    ! local variables
    type(ESMF_Field)                :: field
    real(kind=ESMF_KIND_R4),pointer :: farrayPtr(:,:)
    integer                         :: compLBnd(2)
    integer                         :: compUBnd(2)
    integer                         :: i, j


    rc = ESMF_SUCCESS
!print *, 'KDS: CopyData2d - begin'

    call ESMF_StateGet(state, itemName=trim(fieldName), field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_FieldGet(field, farrayPtr=farrayPtr, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! retrieve the Fortran data pointer from the Field and bounds
    call ESMF_FieldGet(field=field, farrayPtr=farrayPtr, &
        computationalLBound=compLBnd, computationalUBound=compUBnd, &
        rc=rc)

!print *, 'KDS - Field Info - compLBnd(1): ', compLBnd(1)
!print *, 'KDS - Field Info - compLBnd(2): ', compLBnd(2)
!print *, 'KDS - Field Info - compUBnd(1): ', compUBnd(1)
!print *, 'KDS - Field Info - compUBnd(2): ', compUBnd(2)
!print *, 'KDS - Array Info - size hydroArray: ', size(hydroArray)
!print *, 'KDS - Array Info - size fArrayPtr: ', size(farrayPtr)

    if (stateType .eq. 'e') then
      farrayPtr(compLBnd(1):compUBnd(1), &
                compLBnd(2):compUBnd(2)) = &
        hydroArray(compLBnd(1):compUBnd(1), &
                   compLBnd(2):compUBnd(2))
    else
      hydroArray(compLBnd(1):compUBnd(1), &
                 compLBnd(2):compUBnd(2)) = &
        farrayPtr(compLBnd(1):compUBnd(1), &
                  compLBnd(2):compUBnd(2))
    end if
!print *, 'KDS: CopyData2d - end'

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitHydro(distGrid, rc)
    type(ESMF_DistGrid), intent(in)    :: distGrid
    integer,             intent(out)   :: rc

    ! local variables
    integer               :: de, dim
    integer               :: localDeCount, localDe
    type(ESMF_DELayout)   :: delayout
    integer, allocatable  :: dimExtent(:,:), localIndexList(:)
    integer, allocatable  :: localDeToDeMap(:)
    integer, allocatable  :: iIndexList(:), jIndexList(:)



    integer             :: kk
    real                :: dt
    real, dimension(4)  :: zsoil
    character(len = 19) :: olddate
    integer, dimension(268, 260) :: vegtyp, soltyp

    character(160)      :: msgString
    type(ESMF_VM)       :: currentVM
    integer             :: localPet
    integer             :: petCount
    integer             :: peCount
    integer             :: iSize, jSize
    integer             :: iStart, iEnd
    integer             :: jStart, jEnd


!print *, 'KDS: InitHydro - begin'
    rc = ESMF_SUCCESS

    !!
    !! Get VM Info to see if this will give me the PET info I need
    !!
    call ESMF_VMGetCurrent(currentVM, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_VMGet(currentVM, localPet=localPet, petCount=petCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

!print *, "KDS - InitHydro: localPet = ", localPet
!print *, "KDS - InitHydro: ", localPet, "/petCount = ", petCount

    !!
    !! Get the grid distribution for this pet
    !!
    allocate(dimExtent(2, 0:(petCount - 1))) ! (dimCount, deCount)
    call ESMF_DistGridGet(distgrid, delayout=delayout, &
                          indexCountPDe=dimExtent, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    allocate(iIndexList(dimExtent(1, localPet)))
    call ESMF_DistGridGet(distgrid, localDe=0, dim=1, &
                          indexList=iIndexList, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    allocate(jIndexList(dimExtent(2, localPet)))
    call ESMF_DistGridGet(distgrid, localDe=0, dim=2, &
                          indexList=jIndexList, rc=rc)
    if (rc /= ESMF_SUCCESS) call ESMF_Finalize(endflag=ESMF_END_ABORT)

    iStart = minVal(iIndexList)
    iEnd   = maxVal(iIndexList)
    jStart = minVal(jIndexList)
    jEnd   = maxVal(jIndexList)
!print *, "KDS - InitHydro: ", localPet, ": iStart - ", iStart
!print *, "KDS - InitHydro: ", localPet, ": iEnd - ", iEnd
!print *, "KDS - InitHydro: ", localPet, ": jStart - ", jStart
!print *, "KDS - InitHydro: ", localPet, ": jEnd - ", jEnd

    iSize = (iEnd - iStart) + 1
    jSize = (jEnd - jStart) + 1

    deallocate(dimExtent)
    deallocate(iIndexList)
    deallocate(jIndexList)

    !!
    !! Setup the values that the HYDRO_ini is expecting to be set
    !! KDS NOTE: Most of this stuff is temporary and will need to be defined
    !!           at runtime
    !!
    did = 1
    dt = 60.0
    olddate = '2008-08-08_00:00:00'
    kk = 4
    zsoil(1) = -0.1
    zsoil(2) = -0.4
    zsoil(3) = -1.0
    zsoil(4) = -2.0

    nlst_rt(did)%dt = dt
    nlst_rt(did)%olddate(1:19) = olddate(1:19)
    nlst_rt(did)%startdate(1:19) = olddate(1:19)

    nlst_rt(did)%nsoil = kk
    call mpp_land_bcast_int1(nlst_rt(did)%nsoil)
    allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
    nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = zsoil(1:nlst_rt(did)%nsoil)

    vegtyp = 0
    soltyp = 0


    call CPL_LAND_INIT(iStart, iEnd, jStart, jEnd)

    !!
    !! Call the WRF-HYDRO initialization routine
    !!
    call HYDRO_ini(ntime=1, did=did, ix0=iSize, jx0=jSize, &
      vegtyp=vegtyp, soltyp=soltyp)

    rt_domain(did)%initialized = .true.
!print *, 'KDS: InitHydro - end'

  end subroutine

end module


