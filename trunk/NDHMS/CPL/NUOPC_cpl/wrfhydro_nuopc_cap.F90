#define ESMF_STDERRORCHECK(rc) ESMF_LogFoundError(rcToCheck=rc,msg=ESMF_LOGERR_PASSTHRU,line=__LINE__,file=__FILE__)
#define FILENAME "wrfhydro_nuopc_cap"
#define MODNAME "wrfhydro_nuopc"

#define VERBOSITY_MIN 0
#define VERBOSITY_MAX 255
#define VERBOSITY_DBG 1023

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
    WRFHYDRO_GridCreate, &
    WRFHYDRO_get_timestep
  use wrfhydro_nuopc_addonutils, only: &
    mode_Unknown, &
    mode_Offline, &
    mode_Coupled, &
    mode_Hybrid, &
    type_FieldDesc, &
    type_InternalStateStruct, &
    type_InternalState, &
    label_InternalState, &
    state_reset, &
    field_list_add, &
    field_list_print, &
    grid_print, &
    grid_write, &
    set_runmode

  implicit none

  private

  public SetServices

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='SetServices'

    rc = ESMF_SUCCESS

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='Allocation of internal state memory failed.', &
      method=SUBNAME, file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif

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

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: hydroGridComp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is
    character(len=10)          :: value
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='InitializeP0'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_AttributeGet(hydroGridComp, name="Verbosity", value=value, defaultValue="max", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","debug"/), &
      specialValueList=(/VERBOSITY_MIN,VERBOSITY_MAX,VERBOSITY_DBG/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_AttributeGet(hydroGridComp, name="ColdStart", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%coldstart = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="DumpFields", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%statewrite_flag = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="ProfileMemory", value=value, defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%profile_memory = (trim(value)=="true")

    call ESMF_AttributeGet(hydroGridComp, name="Nest", value=value, defaultValue="1", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%nest = ESMF_UtilString2Int(value, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_AttributeGet(hydroGridComp, name="Timestep", value=value, defaultValue="0.0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    read(value,*,iostat=stat) is%wrap%timestep
    if ( stat /= 0) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="Timestep format error: "//trim(value), &
        file=FILENAME, method=SUBNAME, rcToReturn=rc)
      return  ! bail out
    endif

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write(logMsg,'(A,I0)') 'Verbosity: ',is%wrap%verbosity
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write(logMsg,'(A,l6)') 'Coldstart: ',is%wrap%coldstart
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write(logMsg,'(A,l6)') 'DumpFields: ',is%wrap%statewrite_flag
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write(logMsg,'(A,l6)') 'ProfileMemory: ',is%wrap%profile_memory
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write(logMsg,'(A,I0)') 'Nest: ',is%wrap%nest
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      if ( is%wrap%timestep /= 0.0 ) then
        write(logMsg,'(A,F0.0,A)') 'Timestep: ',is%wrap%timestep, &
          ' (Note: This will overwrite value in HRLDAS config file.)'
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      else
        write(logMsg,'(A,F0.0)') 'Timestep: ',is%wrap%timestep
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      endif
    endif

    ! Switch to IPDv01 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(hydroGridComp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv01p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

 !-----------------------------------------------------------------------------

  subroutine InitializeAdvertise(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)    :: is
    integer                     :: fieldIndex
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='InitializeAdvertise'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call field_list_add(is,stdname='aerodynamic_roughness_length', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='canopy_moisture_storage', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='carbon_dioxide', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='cosine_zenith_angle', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='exchange_coefficient_heat', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='exchange_coefficient_heat_height2m', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='exchange_coefficient_moisture_height2m', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='ice_mask', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_down_lw_flx', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_down_sw_flx', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_merid_wind_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_pres_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_pres_height_surface', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_spec_humid_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_temp_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_temp_height_surface', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_wind_speed_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='inst_zonal_wind_height_lowest', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='liquid_water_content_of_soil_layer_1', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='liquid_water_content_of_soil_layer_2', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='liquid_water_content_of_soil_layer_3', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='liquid_water_content_of_soil_layer_4', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='mean_cprec_rate', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='mean_down_lw_flx', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='mean_down_sw_flx', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='mean_fprec_rate', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='mean_prec_rate', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='mean_surface_albedo', &
      transferOffer='will provide',forcing=.FALSE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='moisture_content_of_soil_layer_1', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='moisture_content_of_soil_layer_2', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='moisture_content_of_soil_layer_3', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='moisture_content_of_soil_layer_4', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.FALSE.)
    call field_list_add(is,stdname='subsurface_runoff_flux', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='surface_runoff_flux', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='surface_snow_thickness', &
      transferOffer='will provide',forcing=.FALSE.,import=.FALSE.,export=.TRUE.)
    call field_list_add(is,stdname='temperature_of_soil_layer_1', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='temperature_of_soil_layer_2', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='temperature_of_soil_layer_3', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='temperature_of_soil_layer_4', &
      transferOffer='will provide',forcing=.TRUE.,import=.TRUE.,export=.TRUE.)
    call field_list_add(is,stdname='volume_fraction_of_total_water_in_soil', &
      transferOffer='will provide',forcing=.FALSE.,import=.FALSE.,export=.TRUE.)
    call field_list_add(is,stdname='water_surface_height_above_reference_datum', &
      transferOffer='will provide',forcing=.FALSE.,import=.FALSE.,export=.TRUE.)

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call field_list_print(is,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    !!
    !! advertise import and export fields
    !!
    do fieldIndex = 1, is%wrap%fields_total
      if (is%wrap%field_list(fieldIndex)%import) then
        call NUOPC_Advertise(importState, &
          standardName=trim(is%wrap%field_list(fieldIndex)%stdname), &
          name=trim(is%wrap%field_list(fieldIndex)%shortname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (is%wrap%field_list(fieldIndex)%export) then
        call NUOPC_Advertise(exportState, &
          standardName=trim(is%wrap%field_list(fieldIndex)%stdname), &
          name=trim(is%wrap%field_list(fieldIndex)%shortname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    ! set Component name so it becomes identifiable
    call ESMF_GridCompSet(hydroGridComp, name="WRFHYDRO", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeRealize(hydroGridComp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: hydroGridComp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: WRFHYDRO_Grid
    type(ESMF_Field)           :: field
    type(ESMF_VM)              :: vm
    logical                    :: importConnected, exportConnected
    integer                    :: importCount, exportCount
    integer                    :: fieldIndex
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='InitializeRealize'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call ESMF_GridCompGet(hydroGridComp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call wrfhydro_nuopc_ini(is,vm, rc )
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    WRFHYDRO_Grid = WRFHYDRO_GridCreate(is,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      call grid_print(is, WRFHYDRO_Grid,'WRFHYDRO',rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call grid_write(is,WRFHYDRO_grid, 'array_wrfhydro', rc=rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
    endif

    do fieldIndex = 1, is%wrap%fields_total
      if (is%wrap%field_list(fieldIndex)%import) then
        importConnected = NUOPC_IsConnected(importState, &
          fieldName=is%wrap%field_list(fieldIndex)%stdname)
      else
        importConnected = .FALSE.
      endif
      if (is%wrap%field_list(fieldIndex)%export) then
        exportConnected = NUOPC_IsConnected(exportState, &
          fieldName=is%wrap%field_list(fieldIndex)%stdname)
      else
        exportConnected = .FALSE.
      endif

      if (importConnected .or. exportConnected) then
        if (is%wrap%field_list(fieldIndex)%assoc) then
          if(is%wrap%verbosity >= VERBOSITY_MAX) then
            write(logMsg,"(3A,6(I0,A))") "Create field: ",trim(is%wrap%field_list(fieldIndex)%shortname), &
              " from exiting array with boundaries: (", &
              lbound(is%wrap%field_list(fieldIndex)%farrayPtr,1),":", &
              ubound(is%wrap%field_list(fieldIndex)%farrayPtr,1),",", &
              lbound(is%wrap%field_list(fieldIndex)%farrayPtr,2),":", &
              ubound(is%wrap%field_list(fieldIndex)%farrayPtr,2),",", &
              lbound(is%wrap%field_list(fieldIndex)%farrayPtr,3),":", &
              ubound(is%wrap%field_list(fieldIndex)%farrayPtr,3),")"
            call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
          field = ESMF_FieldCreate(name=is%wrap%field_list(fieldIndex)%shortname, &
            grid=WRFHYDRO_grid, farray=is%wrap%field_list(fieldIndex)%farrayPtr, &
            indexflag=ESMF_INDEX_DELOCAL, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        else
          if(is%wrap%verbosity >= VERBOSITY_MAX) then
            call ESMF_LogWrite("Create field from new array: "// &
              trim(is%wrap%field_list(fieldIndex)%shortname),ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
          endif
          field = ESMF_FieldCreate(name=is%wrap%field_list(fieldIndex)%shortname, &
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
        if(is%wrap%verbosity >= VERBOSITY_MIN) then
          call ESMF_LogWrite('Realizing IMPORT: '//trim(is%wrap%field_list(fieldIndex)%stdname), &
            ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
        endif
        call NUOPC_Realize(importState, field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(is%wrap%field_list(fieldIndex)%import) then
        if(is%wrap%verbosity >= VERBOSITY_MIN) then
          call ESMF_LogWrite('Removing IMPORT: '//trim(is%wrap%field_list(fieldIndex)%stdname), &
            ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
        endif
        call ESMF_StateRemove(importState, (/trim(is%wrap%field_list(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (exportConnected) then
        if(is%wrap%verbosity >= VERBOSITY_MIN) then
          call ESMF_LogWrite('Realizing EXPORT: '//trim(is%wrap%field_list(fieldIndex)%stdname), &
            ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
        endif
        call NUOPC_Realize(exportState, field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(is%wrap%field_list(fieldIndex)%export) then
        if(is%wrap%verbosity >= VERBOSITY_MIN) then
          call ESMF_LogWrite('Removing EXPORT: '//trim(is%wrap%field_list(fieldIndex)%stdname), &
            ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
        endif
        call ESMF_StateRemove(exportState,(/trim(is%wrap%field_list(fieldIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(is%wrap%field_list(fieldIndex)%farrayPtr) ) is%wrap%field_list(fieldIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      call ESMF_StateGet(importState, itemCount=importCount, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_StateGet(exportState, itemCount=exportCount, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      write (logMsg, "(A,I0)") "Connected IMPORT fields: ", importCount
      call ESMF_LogWrite(trim(logMsg), &
        ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
      write (logMsg, "(A,I0)") "Connected EXPORT fields: ", exportCount
      call ESMF_LogWrite(trim(logMsg), &
        ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call set_runmode(is,importState,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_MAX) then
      select case (is%wrap%mode)
        case(mode_Offline)
          write (logMsg,"(A)") "WRF-Hydro coupled mode: Offline"
        case (mode_Coupled)
          write (logMsg,"(A)") "WRF-Hydro coupled mode: Coupled"
        case (mode_Hybrid)
          write (logMsg,"(A)") "WRF-Hydro coupled mode: Hybrid"
        case default
          write (logMsg,"(A)") "WRF-Hydro coupled mode: Unknown"
      end select
      call ESMF_LogWrite(trim(logMsg), &
        ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call state_reset(is,importState,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return
    call state_reset(is,exportState,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)    :: is
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    CHARACTER(LEN=*),PARAMETER  :: SUBNAME='ModelAdvance'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

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

    is%wrap%slice = is%wrap%slice + 1

    ! write out the Fields in the importState
    if ( is%wrap%statewrite_flag) then
      call NUOPC_Write(importState, fileNamePrefix="field_wrfhydro_import_", &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Gluecode ModelAdvance
    call wrfhydro_nuopc_run(is,clock,importState,exportState,rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! write out the Fields in the importState
    if ( is%wrap%statewrite_flag) then
      call NUOPC_Write(exportState, fileNamePrefix="field_wrfhydro_export_", &
        timeslice=is%wrap%slice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(hydroGridComp,rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! Local Variables
    type(type_InternalState)   :: is
    integer                    :: stat
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='ModelFinalize'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    call wrfhydro_nuopc_fin(is,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='Deallocation of internal state memory failed.', &
      method=SUBNAME,file=FILENAME,rcToReturn=rc)) return ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(hydroGridComp, rc)
    type(ESMF_GridComp)  :: hydroGridComp
    integer, intent(out) :: rc

    ! local variables
    type(type_InternalState)   :: is
    real                       :: dt
    type(ESMF_Clock)           :: clock
    type(ESMF_TimeInterval)    :: stabilityTimeStep, timestep
    CHARACTER(LEN=*),PARAMETER :: SUBNAME='SetClock'

    rc = ESMF_SUCCESS

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(hydroGridComp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Called", ESMF_LOGMSG_INFO, file=FILENAME, method=SUBNAME)
    endif

    ! query the Component for its clock
    call ESMF_GridCompGet(hydroGridComp, clock=clock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    dt = WRFHYDRO_get_timestep(is,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeIntervalSet(timestep, s=nint(dt), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_ClockSet(clock, timestep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    call ESMF_TimeIntervalSet(stabilityTimeStep, s=nint(dt), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSetClock(hydroGridComp, clock, stabilityTimeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if ( is%wrap%verbosity >= VERBOSITY_MAX ) then
      call ESMF_LogWrite("Done", ESMF_LOGMSG_INFO,file=FILENAME,method=SUBNAME)
    endif
  end subroutine

end module
