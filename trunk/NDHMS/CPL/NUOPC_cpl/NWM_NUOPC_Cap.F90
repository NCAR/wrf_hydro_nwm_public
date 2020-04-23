#define FILENAME "NWM_NUOPC_Cap"
#define MODNAME "NWM_NUOPC_Cap"
#include "NWM_NUOPC_Macros.h"

#define DEBUG=on

module NWM_NUOPC_Cap
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize

  use state_module, only: state_type
  use NWM_NUOPC_Gluecode
  use NWM_ESMF_Extensions

  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    integer                  :: did           = 1
    character                :: hgrid         = '0'
    character(len=64)        :: fdYamlFile    = 'fd.yaml'
    integer                  :: nfields       = size(NWM_FieldList)
    integer                  :: timeSlice     = 0 ! total timesteps in unit of hours, calc. based on KDAY/KHOUR [RUNDURATION in NEMS]    
    integer                  :: timeStepInt   = 0 ! 1 timestep per hour
    type(state_type)         :: hydroState         ! state passed in init, exec, finish

    logical                  :: lwrite_grid   = .TRUE.
    integer                  :: verbosity     = VERBOSITY_LV1
    logical                  :: llog_memory   = .FALSE.
    logical                  :: lwrite_debug  = .TRUE.
    integer                  :: debugIntvlInt = 0
    type(ESMF_TimeInterval)  :: debugIntvl
    type(ESMF_TimeInterval)  :: debugImpAccum
    type(ESMF_TimeInterval)  :: debugExpAccum
    integer                  :: debugImpSlice = 1
    integer                  :: debugExpSlice = 1

    type (ESMF_Clock)        :: clock(1)          ! same as model clock 
    type (ESMF_TimeInterval) :: stepTimer(1)      ! for displying timestep 

    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    integer                  :: mode(1) = 1    ! NWM in NUOPC mode
    type(state_type)         :: hydro_state    ! state passed in init, exec, finish
    ! added to track the driver clock
    character(len=19)        :: startTimeStr = "0000-00-00_00:00:00"


  end type

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  !! The NUOPC specialized subroutines are registered during SetServices
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "SetServices"

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is


#ifdef DEBUG
    call ESMF_LogSet(flush=.true., rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='NWM: Allocation of internal state memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
!
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! The NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! Set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! Attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! check why is removed??
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP0"
  
  !! phase 0: (REQUIRED, NUOPC PROVIDED)
  !! Initialize the InitializePhaseMap Attribute according to the NUOPC 
  !! Initialize Phase Definition (IPD) version 00 
  !! During initialize phase 0 the runtime configuration is read in from 
  !! model attributes and the initialization phase definition version is 
  !! set to IPDv03.
  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(32)              :: cname
    integer                    :: stat
    logical                    :: configIsPresent
    type(ESMF_Config)          :: config
    type(NUOPC_FreeFormat)     :: attrFF
    type(type_InternalState)   :: is
    character(len=64)          :: value
 
#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name, gcomp was created through driver
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !! Beheen - at this time timeStep=runDuration=259200

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! Check gcomp for config - where this is coming from -- driver??
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    if (configIsPresent) then

      ! Read and ingest free format component attributes
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
     
      !call  PRINT_ESMF_CONFIG(config,label="EARTH_attributes::")

      attrFF = NUOPC_FreeFormatCreate(config, &
        label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    endif
!
    ! Determine Domain ID
    call ESMF_AttributeGet(gcomp, name="did", value=value, &
                           defaultValue="1", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
!
    ! Update internalstate per domain
    is%wrap%did = ESMF_UtilString2Int(value, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Time step initialization
    call ESMF_AttributeGet(gcomp, name="time_step", value=value, defaultValue="0", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    !! at this time time step is set to 0, clock is not changed
    read (value,*,iostat=stat) is%wrap%timeStepInt    !nwm timestep - 1hr=3600s
    if (stat /= 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Cannot convert "//trim(value)//" to integer.", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    endif
!
    ! Debug Write Interval
    call ESMF_AttributeGet(gcomp, name="debug_interval", value=value, defaultValue="hourly", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%debugIntvlInt = ESMF_UtilString2Int(value, &
              specialStringList=(/"default","yearly","hourly","daily"/), &
              specialValueList=(/0,31536000,3600,86400/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    if (is%wrap%debugIntvlInt /= 0) then
      is%wrap%lwrite_debug=.TRUE.

      call ESMF_TimeIntervalSet(is%wrap%debugIntvl, &
                                s=is%wrap%debugIntvlInt, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeIntervalSet(is%wrap%debugImpAccum, s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeIntervalSet(is%wrap%debugExpAccum, s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Determine Verbosity
    call ESMF_AttributeGet(gcomp, name="verbosity", value=value, &
                           defaultValue="default", &
                           convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%verbosity = ESMF_UtilString2Int(value, &
                 specialStringList=(/"none","min","max","debug","default"/), &
                 specialValueList=(/VERBOSITY_LV0,VERBOSITY_LV1,VERBOSITY_LV2, &
                 VERBOSITY_LV3,VERBOSITY_LV1/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Determine yaml models field filename - to be implemented
    call ESMF_AttributeGet(gcomp, name="yaml_field_file", value=value, &
                 defaultValue="fd.yaml", &
                 convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%fdYamlFile = trim(value)


    ! Write coupled grid files
    call ESMF_AttributeGet(gcomp, name="write_grid", value=value, &
               defaultValue="true", &
               convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%lwrite_grid = (trim(value)=="true")

    ! Log Memory
    call ESMF_AttributeGet(gcomp, name="log_memory", value=value, &
               defaultValue="false", &
               convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%llog_memory = (trim(value)=="true")


    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
               acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogAttributes(trim(cname),gcomp)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP1"

  !! IPDv03p - P1 model/mediator/driver advertise their import and export 
  !! Fields and set the TransferOfferGeomObject Attribute
  !! During phase1 initialization, the NWM model init method is called and 
  !! the import and export fields are advertised in a state labeled with
  !! the domain ID.

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! Local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex
    logical                     :: vmIsPresent, configIsPresent, clockIsPresent
    integer                     :: itime, ntime  
    type(state_type)            :: hydstate

#ifdef DEBUG
    character(ESMF_MAXSTR)      :: logMsg
#endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif


    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    ! copies initial internal state values into this local is
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    ! Query VM to pass to nwm model, vm was created in driver during
    ! child (model component) creation
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    !! Beheen -  at this time timeStep=3600 comes from reading nems.configure - how?? 
    !! EARTH_GRID_COMP SetRunSequence happens prior to P1

    ! Initialize NWM LSM grid, routing grid, get data needed for nuopc
    ! from initialization and save it in internal state variable
    call NWM_NUOPC_Init(is%wrap%did, vm, clock, hydstate, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    is%wrap%hydroState = hydstate      ! hydrologic state, per nwm

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogAttributes(trim(cname),gcomp)

    ! Get hgrid for domain id
    call NWM_GetHgrid(is%wrap%did,is%wrap%hgrid,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    is%wrap%NStateImp(1) = importState
    is%wrap%NStateExp(1) = exportState

    ! Read in import/export field file - TO DO
    ! call NUOPC_FieldDictionarySetup(is%wrap%fdYamlFile, rc)

    ! Add the import/export fields into global field dict.
    call NWM_FieldDictionaryAdd(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
 
    ! Advertise import and export fields
    do fIndex = 1, size(NWM_FieldList)
      if (NWM_FieldList(fIndex)%adImport) then
        call NUOPC_Advertise(is%wrap%NStateImp(1), &
          standardName=trim(NWM_FieldList(fIndex)%stdname), &
          name=trim(NWM_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (NWM_FieldList(fIndex)%adExport) then
        call NUOPC_Advertise(is%wrap%NStateExp(1), &
          standardName=trim(NWM_FieldList(fIndex)%stdname), & 
          name=trim(NWM_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogAdvertised(trim(cname))

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "InitializeP3"
  !! IPDv03 - P3 model/mediator/driver realize their "connected" 
  !! import and export Fields that have TransferActionGeomObject 
  !! equal to "provide".
  !!
  !! During initialize phase 3 import and export fields are
  !! realized, if they are connected through NUOPC. 
  !! Realized fields are created on the NWM_LSM_Grid object. 

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: NWM_LSMGrid, NWM_RTGrid
    type(ESMF_LocStream)       :: NWM_LocStream 
    type(ESMF_Field)           :: field
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex

#ifdef DEBUG
    ! number of entrance is pet PE (i.e 767 for NWM)
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    NWM_LSMGrid = NWM_LSMGridCreate(is%wrap%did,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%lwrite_grid) then
      call NWM_ESMF_GridWrite(NWM_LSMGrid, &
        trim(cname)//'_LSMGrid_D'//trim(is%wrap%hgrid)//".nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    NWM_LocStream = NWM_LocStreamCreate(is%wrap%did,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out


    do fIndex = 1, size(NWM_FieldList)
     
      !! the model checks to see if fields are connected via NUOPC_IsConnected.
      !! If the field is connected then the field is realized.  
      !! The model doesn't realize all fields because it's wasteful
      if (NWM_FieldList(fIndex)%adImport) then
        importConnected = NUOPC_IsConnected(is%wrap%NStateImp(1), &
                         fieldName=NWM_FieldList(fIndex)%stdname)
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then 
        NWM_FieldList(fIndex)%realizedImport = .TRUE.
            field = NWM_FieldCreate(NWM_FieldList(fIndex)%stdname, &
                        grid=NWM_LSMGrid, locstream=NWM_LocStream, &
                                            did=is%wrap%did,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return


      elseif(NWM_FieldList(fIndex)%adImport) then
        call ESMF_StateRemove(is%wrap%NStateImp(1), &
           (/trim(NWM_FieldList(fIndex)%stdname)/), &
                          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (NWM_FieldList(fIndex)%adExport) then
        exportConnected = NUOPC_IsConnected(is%wrap%NStateExp(1), &
                         fieldName=NWM_FieldList(fIndex)%stdname)   
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        NWM_FieldList(fIndex)%realizedExport = .TRUE.
        field = NWM_FieldCreate(stdName=NWM_FieldList(fIndex)%stdname, &
                             grid=NWM_LSMGrid,locstream=NWM_LocStream, &
                                                did=is%wrap%did,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateExp(1), field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return

      elseif(NWM_FieldList(fIndex)%adExport) then
        call ESMF_StateRemove(is%wrap%NStateExp(1), &
           (/trim(NWM_FieldList(fIndex)%stdname)/), &
                          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(NWM_FieldList(fIndex)%farrayPtr) ) NWM_FieldList(fIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

!    Model has initialized its own field memory so don't fill state.
!    call NUOPC_FillState(is%wrap%NStateImp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call NUOPC_FillState(is%wrap%NStateExp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return

    is%wrap%mode(1) = NWM_RunModeGet(is%wrap%NStateImp(1),rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogRealized(trim(cname))
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogMode(trim(cname),gcomp)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "DataInitialize"

  !! During data initialize this cap checks the timestamp of all import fields
  !! dependent on a coupled model.  Once all dependent import fields have been
  !! initialized this cap is marked initalized.
  !!
  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)                          :: cname
    type(type_InternalState)               :: is
    type(ESMF_Clock)                       :: modelClock
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    
    ! Beheen - why is this here?
    !is%wrap%timeSlice = is%wrap%timeSlice + 1

    ! Query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Initialize import and export fields
    ! No initialization. Fields remain set to initial value

    call ESMF_StateGet(is%wrap%NStateExp(1),itemCount=itemCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return ! bail out

    allocate( &
      itemNameList(itemCount), &
      itemTypeList(itemCount), &
      stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Allocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_StateGet(is%wrap%NStateExp(1),itemNameList=itemNameList, &
      itemTypeList=itemTypeList,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return

    do iIndex=1, itemCount
      if ( itemTypeList(iIndex) == ESMF_STATEITEM_FIELD) then
        call ESMF_StateGet(is%wrap%NStateExp(1),field=field, &
          itemName=itemNameList(iIndex),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
        call NUOPC_SetAttribute(field, name="Updated", value="true", rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    deallocate(itemNameList, itemTypeList, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of state item list memory failed.", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Write init files if lwrite_debug is on
    if (is%wrap%lwrite_debug) then
      call NUOPC_Write(is%wrap%NStateImp(1), &
        fileNamePrefix="field_"//trim(cname)//"_imp_D"//trim(is%wrap%hgrid)//'_', &
        overwrite=.false., timeslice=is%wrap%debugImpSlice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix="field_"//trim(cname)//"_exp_D"//trim(is%wrap%hgrid)//'_', &
        overwrite=.false., timeslice=is%wrap%debugExpSlice, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%debugImpSlice = is%wrap%debugImpSlice + 1
      is%wrap%debugExpSlice = is%wrap%debugExpSlice + 1
    endif

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !if (is%wrap%verbosity >= VERBOSITY_LV3) &
    !  call NWM_FieldListLog(label=trim(cname)) ! TO DO

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine 

  !-----------------------------------------------------------------------------
  !! During set clock the cap creates a new clock using the timestep configured
  !! in the NWM configuration file. The restart write time step is also 
  !! created and the restart write time accumulation tracker is reset to zero.
  !! NOTE:
  !! setClock timeStep=3600s is not a divisor of runDuration=30s
  !! stepCount = 5 = NTIME and stepTime = 30.00 = noah_timestep 

#undef METHOD
#define METHOD "SetClock"

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    integer                    :: dt           ! timestep in seconds
    type(ESMF_Clock)           :: modelclock
    type(ESMF_TimeInterval)    :: timestep     ! in/out
    type(ESMF_Time)            :: starttime
    type(ESMF_Time)            :: stoptime
    

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query component for its clock - check where modelClock is set! TODO
    call NUOPC_ModelGet(gcomp, modelClock=modelclock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query model clock for its timestep
    call ESMF_ClockGet(modelclock, timeStep=timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query timestep for seconds 
    !call ESMF_TimeIntervalGet(timestep,s=dt,rc=rc)
    !if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! override timestep
    !if (is%wrap%timeStepInt /= 0) then
    !  call ESMF_TimeIntervalSet(timestep, s=is%wrap%timeStepInt, rc=rc)
    !  if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      
    !  call NWM_SetTimestep(is%wrap%did,real(is%wrap%timeStepInt),rc)
    !  if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !  call ESMF_ClockSet(modelClock, timeStep=timestep, rc=rc)
    !  if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !else
    !  print *, "Beheen in else ......."
    !  call NWM_SetTimestep(is%wrap%did,real(dt),rc)
    !  if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    !endif

    ! Initialize and set the internal Clock of a GridComp,
    ! could be any external clock as well.
    call NUOPC_CompSetClock(gcomp, modelclock, timestep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Save model clock in internal state
    is%wrap%clock(1) = modelclock

    ! Reset Timer
    call ESMF_TimeIntervalSet(is%wrap%stepTimer(1), &
      s_r8=0._ESMF_KIND_R8, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (is%wrap%lwrite_debug) then
      call ESMF_TimeIntervalSet(is%wrap%debugImpAccum, &
        s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeIntervalSet(is%wrap%debugExpAccum, &
        s_r8=0._ESMF_KIND_R8, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogClock(trim(cname),gcomp)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  !! During check import the import data is checked to verify that it is at
  !! the beginning or end of the timestep. By default the current time is 
  !! checked for each field and if it doesn't match the current time of the
  !! model advance then the model fails, overriding this default check.
  !! This happens before each model advance phase.
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "CheckImport"

subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    integer                     :: nIndex
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    logical                     :: allCurrTime

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Get the curr time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Check that Fields in the importState show correct timestamp
    allCurrTime = NUOPC_IsAtTime(is%wrap%NStateImp(1), modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    if (.not.allCurrTime) then
      call ESMF_LogWrite(trim(cname)//": NUOPC INCOMPATIBILITY DETECTED: "// &
        "Import Fields not at correct time", &
        ESMF_LOGMSG_WARNING)
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  !! Calls NWM advance for the configured domain per NWM noah timestep
  !-----------------------------------------------------------------------------
#undef METHOD
#define METHOD "ModelAdvance"

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime, advEndTime
    character(len=32)           :: currTimeStr, advEndTimeStr
    type(ESMF_TimeInterval)     :: timeStep
    integer(ESMF_KIND_I8)       :: advanceCount
    
    integer :: itime, dt

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%timeSlice = is%wrap%timeSlice + 1   

    if (is%wrap%timeSlice > 999999999) then
      sStr = '999999999+'
    else
      write (sStr,"(I0)") is%wrap%timeSlice
    endif

    ! Beheen - I am not sure what is difference between modelClock and 
    ! clock(1) yet!!
    ! Query the component for its clock, importState, and exportState
    call NUOPC_ModelGet(gcomp,modelClock=modelClock, &
         importState=importState, exportState=exportState, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Query the clock for its current time and timestep
    call ESMF_ClockGet(modelClock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    advEndTime = currTime + timeStep

    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_TimeGet(advEndTime, timeString=advEndTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out


    ! Write import files
    if (is%wrap%lwrite_debug) then
      is%wrap%debugImpAccum = is%wrap%debugImpAccum + timeStep
      if (is%wrap%debugImpAccum >= is%wrap%debugIntvl) then
        call NUOPC_Write(is%wrap%NStateImp(1), &
          fileNamePrefix="field_"//TRIM(cname)//"_imp_D"//trim(is%wrap%hgrid)//'_', &
          overwrite=.false., timeslice=is%wrap%debugImpSlice, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_TimeIntervalSet(is%wrap%debugImpAccum, &
          s_r8=0._ESMF_KIND_R8, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        is%wrap%debugImpSlice = is%wrap%debugImpSlice + 1
      endif
    endif

    is%wrap%stepTimer(1) = is%wrap%stepTimer(1) + timeStep
    ! above upto here used timeStep of modelClock

    ! below from here using timeStep of clock(1)
    call ESMF_ClockGet(is%wrap%clock(1),timeStep=timeStep, &
                       advanceCount=advanceCount, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! this is same as itime = 1 hour for nwm exec method
    if ( MOD(advanceCount, 24) .eq. 0) then
       is%wrap%timeStepInt = is%wrap%timeStepInt + 1
    endif
   
    do while (is%wrap%stepTimer(1) >= timeStep)
      
      call ESMF_LogWrite( &
        'NWM: Advance timeSlice='//trim(sStr)//" HGRID="//trim(is%wrap%hgrid), &
        ESMF_LOGMSG_INFO)

      ! Call nwm exe
      call NWM_NUOPC_Run(is%wrap%did,is%wrap%mode(1), &
                         is%wrap%clock(1), is%wrap%hydroState, is%wrap%timeStepInt, &
                         is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out

      call ESMF_ClockAdvance(is%wrap%clock(1),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      is%wrap%stepTimer(1) = is%wrap%stepTimer(1) - timeStep
    enddo


    ! Write export files
    if (is%wrap%lwrite_debug) then
      is%wrap%debugExpAccum = is%wrap%debugExpAccum + timeStep
      if (is%wrap%debugExpAccum >= is%wrap%debugIntvl) then
        call NUOPC_Write(is%wrap%NStateExp(1), &
          fileNamePrefix="field_"//trim(cname)//"_exp_D"//trim(is%wrap%hgrid)//'_', &
          overwrite=.false., timeslice=is%wrap%debugExpSlice, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_TimeIntervalSet(is%wrap%debugExpAccum, &
          s_r8=0._ESMF_KIND_R8, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        is%wrap%debugExpSlice = is%wrap%debugExpSlice + 1
      endif
    endif

    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogMode(trim(cname),gcomp)
    if (is%wrap%verbosity >= VERBOSITY_LV1) &
      call LogClock(trim(cname),gcomp)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "ModelFinalize"

  subroutine ModelFinalize(gcomp,rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local Variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    integer                    :: stat
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: currTime
    character(len=32)          :: currTimeStr

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NWM_NUOPC_Fin(is%wrap%did,is%wrap%hydroState,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='NWM: Deallocation of internal state memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------
  ! Log Utilities
  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogAdvertised"

  subroutine LogAdvertised(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

    ! Count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do fIndex = 1, size(NWM_FieldList)
      if (NWM_FieldList(fIndex)%adImport) cntImp = cntImp + 1
      if (NWM_FieldList(fIndex)%adExport) cntExp = cntExp + 1
    enddo

    ! Report advertised import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(NWM_FieldList)
      if (.NOT.NWM_FieldList(fIndex)%adImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(NWM_FieldList(fIndex)%stdname), &
        ' ',TRIM(NWM_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
    enddo

    ! Report advertised export fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of advertised export fields(',cntExp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do fIndex=1, size(NWM_FieldList)
      if (.NOT.NWM_FieldList(fIndex)%adExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(NWM_FieldList(fIndex)%stdname), &
        ' ',TRIM(NWM_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogRealized"

  subroutine LogRealized(label)
    character(len=*),intent(in) :: label

    ! local variables
    integer                    :: cntImp
    integer                    :: cntExp
    integer                    :: fIndex
    character(ESMF_MAXSTR)     :: logMsg
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! Count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do fIndex = 1, size(NWM_FieldList)
      if (NWM_FieldList(fIndex)%realizedImport) cntImp = cntImp + 1
      if (NWM_FieldList(fIndex)%realizedExport) cntExp = cntExp + 1
    enddo

    ! Report realized import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(NWM_FieldList)
      if (.NOT. NWM_FieldList(fIndex)%realizedImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(NWM_FieldList(fIndex)%stdname), &
        ' ',TRIM(NWM_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo


    ! Report realized export fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of realized export fields(',cntExp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntExp = 0
    do fIndex=1, size(NWM_FieldList)
      if (.NOT. NWM_FieldList(fIndex)%realizedExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(NWM_FieldList(fIndex)%stdname), &
        ' ',TRIM(NWM_FieldList(fIndex)%stdName)
      call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
    enddo

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogAttributes"

  subroutine LogAttributes(label,gcomp)
    character(len=*), intent(in)  :: label
    type(ESMF_GridComp)           :: gcomp

    ! local variables

    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    character(len=64)          :: modeStr
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Debug Write Interval   = ",is%wrap%debugIntvlInt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Log Memory             = ",is%wrap%llog_memory
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Domain ID              = ",is%wrap%did
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Time StepInt           = ",is%wrap%timeStepInt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Verbosity              = ",is%wrap%verbosity
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,A))") trim(label)//": ", &
      "fd.yaml field File     = ",is%wrap%fdYamlFile
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Write Grid             = ",is%wrap%lwrite_grid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogMode"

  subroutine LogMode(label,gcomp)
    character(len=*), intent(in)  :: label
    type(ESMF_GridComp)           :: gcomp

    ! local variables

    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    character(len=64)          :: modeStr
    integer                    :: rc

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "LogClock"

  subroutine LogClock(label,gcomp)
    character(len=*), intent(in) :: label
    type(ESMF_GridComp)          :: gcomp

    ! local variables
    type(type_InternalState)   :: is
    character(ESMF_MAXSTR)     :: logMsg
    type(ESMF_Time)            :: currTime
    type(ESMF_TimeInterval)    :: timestep
    character(len=64)          :: currTimeStr
    character(len=64)          :: timestepStr
    integer                    :: rc

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entering "//METHOD, ESMF_LOGMSG_INFO)
#endif

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

    if (ESMF_ClockIsCreated(is%wrap%clock(1))) then
      call ESMF_ClockGet(is%wrap%clock(1), &
        currTime=currTime,timeStep=timestep,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeGet(currTime, &
                        timeString=currTimeStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      call ESMF_TimeIntervalGet(timestep, &
                                timeString=timestepStr,rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    else
      currTimeStr = "(not_created)"
      timestepStr = "(not_created)"
    endif

    write (logMsg, "(A,(A,I0,A),(A,A))") trim(label)//": ", &
      "timeSlice(",is%wrap%timeSlice,") ", &
      "Current Time = ",trim(currTimeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    write (logMsg, "(A,(A,I0,A),(A,A))") trim(label)//": ", &
      "timeSlice(",is%wrap%timeSlice,") ", &
      "Time Step    = ",trim(timestepStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

end module
