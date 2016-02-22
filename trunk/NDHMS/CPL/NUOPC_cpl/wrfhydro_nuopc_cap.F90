#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#ifndef DEBUG_LVL
#define DEBUG_LVL 3
#endif

module wrfhydro_nuopc
! !MODULE: wrfhydro_nuopc
!
! !DESCRIPTION:
!   This modules creates a specialized the NUOPC_Model
!   for WRFHYDRO.  This is also referred to as the NUOPC Cap.
!
! !REVISION HISTORY:
!  13Oct15    Dan Rosen  Initial Specification
!
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_SetClock    => label_SetClock, &
    model_label_SetRunClock => label_SetRunClock, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize
  use wrfhydro_nuopc_gluecode, only: &
    wrfhydro_nuopc_ini, &
    wrfhydro_nuopc_run, &
    wrfhydro_nuopc_fin, &
    WRFHYDRO_distgrid, &
    WRFHYDRO_grid, &
    WRFHYDRO_soilarrayspec, &
    WRFHYDRO_nsoil
  use wrfhydro_nuopc_addonutils, only: &
    state_reset

  implicit none

  private

  public SetServices

  integer   :: import_slice = 0
  integer   :: export_slice = 0

  type fld_list_type
    character(len=64)   :: stdname =" "
    character(len=64)   :: shortname = " "
    character(len=64)   :: transferOffer = " "
    logical             :: import = .FALSE.
    logical             :: export = .FALSE.
    logical             :: assoc = .FALSE. ! is the farrayPtr associated with internal data
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
  end type fld_list_type

  integer,parameter     :: nest = 1
  integer,parameter     :: fldsMax = 100
  integer               :: fldsHyd_num = 0
  type (fld_list_type)  :: fldsHyd(fldsMax)

  ! some temporary debug variables
  character(len=256) :: msgString
  integer, parameter :: dbug_flag = DEBUG_LVL

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:SetServices)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(hydroGridComp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p1"/), userRoutine=InitializeAdvertise, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSetEntryPoint(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv01p3"/), userRoutine=InitializeRealize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! attach specializing method(s)
    ! No need to change clock settings
    call ESMF_MethodAdd(hydroGridComp, label=model_label_SetClock, &
      userRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_MethodAdd(hydroGridComp, label=model_label_Advance, &
      userRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSpecialize(hydroGridComp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: hydroGridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:InitializeP0)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

 !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    integer                     :: fieldIndex
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:InitializeAdvertise)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    !! Land surface forcing fields
    !! liquid_water_content_of_soil_layer also feedback to land
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="moisture_content_of_soil_layer_1", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="moisture_content_of_soil_layer_2", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="moisture_content_of_soil_layer_3", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="moisture_content_of_soil_layer_4", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="temperature_of_soil_layer_1", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="temperature_of_soil_layer_2", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="temperature_of_soil_layer_3", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="temperature_of_soil_layer_4", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="liquid_water_content_of_soil_layer_1", &
      transferOffer="will provide", import=.TRUE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="liquid_water_content_of_soil_layer_2", &
      transferOffer="will provide", import=.TRUE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="liquid_water_content_of_soil_layer_3", &
      transferOffer="will provide", import=.TRUE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="liquid_water_content_of_soil_layer_4", &
      transferOffer="will provide", import=.TRUE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="surface_runoff_flux", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="subsurface_runoff_flux", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)

    !! Feedback to land
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="volume_fraction_of_total_water_in_soil", &
      transferOffer="will provide", import=.FALSE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="surface_snow_thickness", &
      transferOffer="will provide", import=.FALSE., export=.TRUE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="liquid_water_content_of_surface_snow", &
      transferOffer="will provide", import=.FALSE., export=.TRUE.)

    !! Meterological forcing fields
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_down_lw_flx", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_down_sw_flx", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_merid_wind_height_lowest", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_pres_height_surface", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_spec_humid_height_lowest", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_temp_height_lowest", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="inst_zonal_wind_height_lowest", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)
    call fld_list_add(fldsHyd_num, fldsHyd, &
      stdname="mean_prec_rate", &
      transferOffer="will provide", import=.TRUE., export=.FALSE.)

!    !! Feedback to atmosphere
!    call fld_list_add(fldsHyd_num, fldsHyd, &
!      stdname="dummyfield", &
!      transferOffer="will provide", import=.TRUE., export=.FALSE.)

!    !! Other fields
!    call fld_list_add(fldsHyd_num, fldsHyd, &
!      stdname="water_surface_height_above_reference_datum", &
!      transferOffer="will provide", import=.FALSE., export=.TRUE.)

    !!
    !! advertise import and export fields
    !!
    do fieldIndex = 1, fldsHyd_num
      if (fldsHyd(fieldIndex)%import) then
        call ESMF_LogWrite(SUBNAME//': Advertise IMPORT '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call NUOPC_Advertise(importState, &
          standardName=trim(fldsHyd(fieldIndex)%stdname), &
          name=trim(fldsHyd(fieldIndex)%shortname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (fldsHyd(fieldIndex)%export) then
        call ESMF_LogWrite(SUBNAME//': Advertise EXPORT '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call NUOPC_Advertise(exportState, &
          standardName=trim(fldsHyd(fieldIndex)%stdname), &
          name=trim(fldsHyd(fieldIndex)%shortname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    ! set Component name so it becomes identifiable
    call ESMF_GridCompSet(hydroGridComp, name="WRFHYDRO", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: hydroGridComp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    type(ESMF_Field)            :: field
    integer                     :: importCount, exportCount
    type(ESMF_VM)               :: vm
    logical                     :: importConnected, exportConnected
    character(len=256)          :: msgString
    character(len=10)           :: numString
    integer                     :: fieldIndex
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:InitializeRealize)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    call ESMF_GridCompGet(hydroGridComp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call wrfhydro_nuopc_ini(vm, rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    do fieldIndex = 1, fldsHyd_num
      if (fldsHyd(fieldIndex)%import) then
        importConnected = NUOPC_IsConnected(importState, &
          fieldName=fldsHyd(fieldIndex)%stdname)
      else
        importConnected = .FALSE.
      endif
      if (fldsHyd(fieldIndex)%export) then
        exportConnected = NUOPC_IsConnected(exportState, &
          fieldName=fldsHyd(fieldIndex)%stdname)
      else
        exportConnected = .FALSE.
      endif

      if (importConnected .or. exportConnected) then
        if (fldsHyd(fieldIndex)%assoc) then
          write(msgString,"(A)") "Boundaries: ("
          write(numString, "(I10)") lbound(fldsHyd(fieldIndex)%farrayPtr,1)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//":"
          write(numString, "(I10)") ubound(fldsHyd(fieldIndex)%farrayPtr,1)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//","
          write(numString, "(I10)") lbound(fldsHyd(fieldIndex)%farrayPtr,2)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//":"
          write(numString, "(I10)") ubound(fldsHyd(fieldIndex)%farrayPtr,2)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//","
          write(numString, "(I10)") lbound(fldsHyd(fieldIndex)%farrayPtr,3)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//":"
          write(numString, "(I10)") ubound(fldsHyd(fieldIndex)%farrayPtr,3)
          write(msgString,"(A)") trim(msgString)//trim(adjustl(numString))//")"
          call ESMF_LogWrite(SUBNAME//": Create field from existing array: "// &
            trim(fldsHyd(fieldIndex)%shortname)//": "//trim(msgString), ESMF_LOGMSG_INFO, rc=rc)
          field = ESMF_FieldCreate(name=fldsHyd(fieldIndex)%shortname, &
            grid=WRFHYDRO_grid, farray=fldsHyd(fieldIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          call ESMF_LogWrite(SUBNAME//": Create field from new array: "// &
            trim(fldsHyd(fieldIndex)%shortname), ESMF_LOGMSG_INFO, rc=rc)
          field = ESMF_FieldCreate(name=fldsHyd(fieldIndex)%shortname, &
            grid=WRFHYDRO_grid, typekind=ESMF_TYPEKIND_R8, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!         Create Soil Field with ungridded soil layer dimension
!          field = ESMF_FieldCreate(name="temperature_of_soil_layer", grid=WRFHYDRO_grid, &
!            arrayspec=WRFHYDRO_soilarrayspec, gridToFieldMap=(/1,2/), &
!            ungriddedLBound=(/1/), ungriddedUBound=(/WRFHYDRO_nsoil/), rc=rc)
!          if (ESMF_STDERRORCHECK(rc)) return
        endif
      endif

      if (importConnected) then
        call ESMF_LogWrite(SUBNAME//': Realizing IMPORT: '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(fldsHyd(fieldIndex)%import) then
        call ESMF_LogWrite(SUBNAME//': Removing IMPORT: '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_StateRemove(importState, (/trim(fldsHyd(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (exportConnected) then
        call ESMF_LogWrite(SUBNAME//': Realizing EXPORT: '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call NUOPC_Realize(exportState, field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(fldsHyd(fieldIndex)%export) then
        call ESMF_LogWrite(SUBNAME//': Removing EXPORT: '//trim(fldsHyd(fieldIndex)%stdname), &
          ESMF_LOGMSG_INFO, rc=rc)
        call ESMF_StateRemove(exportState,(/trim(fldsHyd(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(fldsHyd(fieldIndex)%farrayPtr) ) fldsHyd(fieldIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

    call ESMF_StateGet(importState, itemCount=importCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    write (numString, "(I10)") importCount
    call ESMF_LogWrite(SUBNAME//": Connected IMPORT fields: " // trim(adjustl(numString)), ESMF_LOGMSG_INFO)

    call ESMF_StateGet(exportState, itemCount=exportCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    write (numString, "(I10)") exportCount
    call ESMF_LogWrite(SUBNAME//": Connected EXPORT fields: " // trim(adjustl(numString)), ESMF_LOGMSG_INFO)

    call state_reset(importState,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call state_reset(exportState,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:ModelAdvance)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(hydroGridComp, clock=clock, importState=importState, &
        exportState=exportState, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="-------->Advancing WRFHYDRO from: ", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! write out the Fields in the importState
    import_slice = import_slice + 1
    call NUOPC_Write(importState, fileNamePrefix="field_wrfhydro_import_", &
      timeslice=import_slice, relaxedFlag=.true., rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Gluecode ModelAdvance
    call wrfhydro_nuopc_run(clock,importState,exportState, rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! write out the Fields in the importState
    export_slice = export_slice + 1
    call NUOPC_Write(exportState, fileNamePrefix="field_wrfhydro_export_", &
      timeslice=export_slice, relaxedFlag=.true., rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(hydroGridComp,rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! Local Variables
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:ModelFinalize)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    call wrfhydro_nuopc_fin(rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_TimeInterval)     :: stabilityTimeStep, timestep
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:SetClock)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(hydroGridComp, clock=clock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeIntervalSet(timestep, m=60, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=60, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSetClock(hydroGridComp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine fld_list_add(num, fldlist, stdname, transferOffer, import, export, shortname, data, rc)
    ! ----------------------------------------------
    ! Set up a list of field information
    ! ----------------------------------------------
    integer,             intent(inout)  :: num
    type(fld_list_type), intent(inout)  :: fldlist(:)
    character(len=*),    intent(in)     :: stdname
    character(len=*),    intent(in)     :: transferOffer
    logical,             intent(in)     :: import
    logical,             intent(in)     :: export
    character(len=*),    intent(in),optional :: shortname
    real(ESMF_KIND_R8), dimension(:,:,:), optional, target :: data
    integer, optional,   intent(out)    :: rc

    ! local variables
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='(wrfhydro_nuopc:fld_list_add)'

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": called", ESMF_LOGMSG_INFO)
    endif

    if(present(rc)) rc = ESMF_SUCCESS

    ! fill in the new entry

    num = num + 1
    if (num > fldsMax) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
         msg=SUBNAME//": fld_list_add ERROR num gt fldsMax "//trim(stdname), &
         rcToReturn=rc)
      return  ! bail out
    endif

    fldlist(num)%stdname            = trim(stdname)
        fldlist(num)%transferOffer  = trim(transferOffer)
    fldlist(num)%import             = import
    fldlist(num)%export             = export
    if (present(shortname)) then
       fldlist(num)%shortname   = trim(shortname)
    else
       fldlist(num)%shortname   = trim(stdname)
    endif
    if (present(data)) then
      fldlist(num)%assoc        = .true.
      fldlist(num)%farrayPtr    => data
    else
      fldlist(num)%assoc        = .false.
    endif

    if ( dbug_flag > 2) then
      call ESMF_LogWrite(SUBNAME//": done", ESMF_LOGMSG_INFO)
    endif
  end subroutine fld_list_add

end module
