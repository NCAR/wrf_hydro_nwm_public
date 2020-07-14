!>
!! @mainpage NCAR's WRF-Hydro NUOPC Cap
!! @author Daniel Rosen (daniel.rosen@noaa.gov)
!! @author ESMF Support (esmf_support@list.woc.noaa.gov)
!! @date 03/14/2017 WRF-Hydro NUOPC Cap Added to GitHub
!! @date 03/17/2017 Documentation Added
!!
!! @tableofcontents
!!
!! @section Overview Overview
!!
!! The Weather Research and Forecasting Hydrological (WRF-Hydro) model is a 
!! hydrometerological forecasting model developed and maintained by the 
!! National Center for Atmospheric Research (NCAR).  The WRF-Hydro cap wraps 
!! the WRF-Hydro model with NUOPC compliant interfaces.  The result is a 
!! WRF-Hydro model capable of coupling with other models using National 
!! Unified Operational Prediction Capability (NUOPC).
!!
!! This page documents the technical design of the specialized NUOPC model and 
!! the WRF-Hydro gluecode.  For generic NUOPC model documentation please see 
!! the NUOPC reference manual: https://www.earthsystemcog.org/projects/nuopc/refmans.
!!
!!
!! @section NuopcSpecialization NUOPC Model Specialized Entry Points
!!
!! This cap specializes the cap configuration, initialization, advertised
!! fields, realized fields, data initialization, clock, run, and finalize.
!!
!! @subsection SetServices Set Services (Register Subroutines)
!!
!! Table summarizing the NUOPC specialized subroutines registered during
!! [SetServices] (@ref WRFHYDRO_NUOPC::SetServices).  The "Phase" column says
!! whether the subroutine is called during the initialization, run, or
!! finalize part of the coupled system run.
!!
!! Phase  |     Cap Subroutine                                | Description
!! -------|---------------------------------------------------|-------------------------------------------------------------
!! Init   | [InitializeP0] (@ref WRFHYDRO_NUOPC::InitializeP0)     | Set the Initialize Phase Definition (IPD). Configure model
!! Init   | [InitializeP1] (@ref WRFHYDRO_NUOPC::InitializeP1)     | Initialize model.  Advertize import and export fields
!! Init   | [InitializeP3] (@ref WRFHYDRO_NUOPC::InitializeP3)     | Realize import and export fields
!! Init   | [DataInitialize] (@ref WRFHYDRO_NUOPC::DataInitialize) | Initialize import and export data
!! Init   | [SetClock] (@ref WRFHYDRO_NUOPC::SetClock)             | Set model clock during initialization
!! Run    | [CheckImport] (@ref WRFHYDRO_NUOPC::CheckImport)       | Check timestamp on import data.
!! Run    | [ModelAdvance] (@ref WRFHYDRO_NUOPC::ModelAdvance)     | Advances the model by a timestep
!! Final  | [ModelFinalize] (@ref WRFHYDRO_NUOPC::ModelFinalize)   | Releases memory
!!
!!
!! @section Initialize Initialize
!!
!! Description of the initialization phases and internal model calls.
!! - [InitializeP0] (@ref WRFHYDRO_NUOPC::InitializeP0)
!! - [InitializeP1] (@ref WRFHYDRO_NUOPC::InitializeP1)
!! - [InitializeP3] (@ref WRFHYDRO_NUOPC::InitializeP3)
!! - [DataInitialize] (@ref WRFHYDRO_NUOPC::DataInitialize)
!! - [SetClock] (@ref WRFHYDRO_NUOPC::SetClock)
!!
!! @subsection InitializeP0 InitializeP0
!!
!! During initialize phase 0 the runtime configuration is read in from model
!! attributes and the initialization phase definition version is set to
!! IPDv03.
!!
!! @subsection InitializeP1 InitializeP1
!!
!! During initialize phase 1 the model is initialized and the import and
!! export fields are advertised in a state labeled with the domain ID.
!!
!! @subsection InitializeP3 InitializeP3
!!
!! During initialize phase 3 import and export fields are realized if they are 
!! connected through NUOPC. Realized fields are created on the WRF-Hydro grid. 
!!
!! @subsection DataInitialize DataInitialize
!!
!! During data initialize this cap checks the timestamp of all import fields
!! dependent on a coupled model.  Once all dependent import fields have been
!! initialized this cap is marked initalized.
!!
!! @subsection SetClock SetClock
!!
!! During set clock the cap creates a new clock using the timestep configured
!! in te WRF-Hydro configuration file. The restart write time step is also 
!! created and the restart write time accumulation tracker is reset to zero.
!!
!!
!! @section Run Run
!!
!! Description of the run phase(s) and internal model calls.
!! - [CheckImport] (@ref WRFHYDRO_NUOPC::CheckImport)
!! - [ModelAdvance] (@ref WRFHYDRO_NUOPC::ModelAdvance)
!!
!! @subsection CheckImport CheckImport
!!
!! During check import the import data is checked to verify that it is at
!! the beginning or end of the timestep.
!!
!! @subsection ModelAdvance ModelAdvance
!!
!! Calls WRF-Hydro advance for the configured domain.
!!
!!
!! @section Finalize Finalize
!!
!! Description of the finalize phase and internal model calls.
!! - [ModelFinalize] (@ref WRFHYDRO_NUOPC::ModelFinalize)
!!
!! @subsection ModelFinalize ModelFinalize
!!
!! During model finalize WRF-Hydro finalize subroutines are called and memory
!! allocated during cap initialization is released.
!!
!!
!! @section ModelConfiguration Model Configuration
!!
!! Custom model attributes are used to configure the model.
!!
!! Attribute         | Default         | Description
!! ------------------|-----------------|-------------------------------------------------------------------------------------
!! Verbosity         | VERBOSITY_LV1   | Verbosity levels are defined in WRFHYDRO_NUOPC_Macros.h
!! DomainID          | 1               |
!! RestartInterval   | NEVER           | Determine when to write NUOPC state restart files in seconds
!! ConfigFile        | hydro.namelist  | Set the WRF-Hydro configuraion file
!! dasConfigFile     | namelist.hrldas | Set the WRF-Hydro DAS configuration file
!! WriteGrid         | FALSE           | Write a NetCDF file for the WRF-Hydro domain
!! WriteImport       | FALSE           | Write a NetCDF file for the import state before model advance
!! WriteExport       | FALSE           | Write a NetCDF file for the export state after model advance
!! LogMemory         | FALSE           | Write memory statistics. (Not Implemented)
!! TestFillImport    | FALSE           | Fill the import state with ESMF_FieldFill(sincos) for testing
!! TestFillExport    | FALSE           | Fill the export state with ESMF_FieldFill(sincos) for testing
!!
!!
!! @section ModelFields Model Fields
!!
!! The following tables list the import and export fields.
!!
!! @subsection ImportFields Import Fields
!!
!! Import fields are listed in the import_list parameter.
!!
!! Standard Name  | Units  | Model Variable  | Description                                | Notes
!! ---------------|--------|-----------------|--------------------------------------------|--------------------------------------
!! dummy_field_1  | Pa     | forcing_1       | field description for first import field   | |
!! dummy_field_2  | kg     | forcing_2       | field description for second import field  | |
!! dummy_field_3  | W m-2  | forcing_3       | field description for third import field   | field notes
!!
!! @subsection ExportField Export Fields
!!
!! Export fields are listed in the export_list parameter.
!!
!! Standard Name  | Units   | Model Variable  | Description                               | Notes
!! ---------------|---------|-----------------|-------------------------------------------|---------------------------
!! dummy_field_1  | m       | output_1        | field description for first export field  | field notes
!! dummy_field_2  | kg      | output_2        | field description for second export field | |
!! dummy_field_3  | m s-1   | output_3        | field description for third export field  | field notes
!!
!!
!! @section MemoryManagement Memory Management
!!
!! Model configuration is stored in a custom internal state data type. A
!! pointer to the custom internal state data type is stored in the component.
!!
!! The cap allocates new memory for each field.  This will be updated so that
!! NUOPC fields directly access the WRF-Hydro field memory.
!!
!! @section IO Input and Output
!!
!! Cap diagnostic output is written to the ESMF PET Logs. Cap diagnostic
!! output can be increased or decreased by setting the Verbosity attribute.
!!
!! NUOPC state restart write files are written depending on the
!! RestartInterval attribute. If set to 0 then NUOPC state restart write files
!! will never be written.
!!
!! WRF-Hydro diagnostics output is written to standard out. To increase the
!! diagnostic output compile WRF-Hydro with -DHYDRO_D.
!!
!! WRF-Hydro writes several output files.  Please see the 
!! [WRF-Hydro documentation] (https://www.ral.ucar.edu/projects/wrf_hydro).
!!
!! @section Dependencies Dependencies
!!
!! Dependencies
!! - [ESMF v7.0.0+] (https://www.earthsystemcog.org/projects/esmf/) 
!! - [NetCDF v4.3.0+] (http://www.unidata.ucar.edu/software/netcdf/docs/)
!! - [NetCDF FORTRAN] (http://www.unidata.ucar.edu/software/netcdf/docs/building_netcdf_fortran.html)
!!
!! @subsection ESMF ESMF
!!
!! See the [ESMF User's Guide] 
!! (http://www.earthsystemmodeling.org/esmf_releases/public/last/ESMF_usrdoc). 
!!
!! @section BuildingAndInstalling Building and Installing
!!
!! Environment Variables
!! - ESMFMKFILE
!!
!! NUOPC Makefile Targets
!! - nuopc
!! - nuopcinstall
!! - nuopcclean
!!
!! The build system in [Makefile] (@ref Makefile) wraps the WRF-Hydro build 
!! system and adds the nuopc, nuopcinstall, and nuopcclean targets. Before 
!! building make sure to configure the internal model.
!!
!! To build and install into the current directory run:
!!    $ make nuopc
!!
!! To install into an alternative directory run:
!!    $ make nuopcinstall DESTDIR=<INSTALL_DIR> INSTDIR=<SUBDIR>
!!
!! To build with debugging information run:
!!    $ make nuopc DEBUG=on
!!
!! @section Repository
!! The WRF-Hydro NUOPC cap is maintained in a GitHub repository:
!! https://github.com/NESII/wrfhydro_cap
!!
!! @section References
!!
!! - [WRF-Hydro] (https://www.ral.ucar.edu/projects/wrf_hydro) 
!! - [ESPS] (https://www.earthsystemcog.org/projects/esps)
!! - [ESMF] (https://www.earthsystemcog.org/projects/esmf)
!! - [NUOPC] (https://www.earthsystemcog.org/projects/nuopc/)

#define FILENAME "WRFHydro_NUOPC_Cap"
#define MODNAME "WRFHydro_NUOPC"
#include "WRFHydro_NUOPC_Macros.h"

module WRFHydro_NUOPC
  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS        => SetServices, &
    model_label_DataInitialize => label_DataInitialize, &
    model_label_SetClock    => label_SetClock, &
    model_label_CheckImport => label_CheckImport, &
    model_label_Advance     => label_Advance, &
    model_label_Finalize    => label_Finalize
  use WRFHYDRO_NUOPC_Gluecode
  use WRFHydro_ESMF_Extensions

  implicit none

  private

  public SetServices

  CHARACTER(LEN=*), PARAMETER :: label_InternalState = 'InternalState'

  type type_InternalStateStruct
    integer                  :: did           = 1
    character                :: hgrid         = '0'
    integer                  :: verbosity     = VERBOSITY_LV1
    character(len=64)        :: configFile    = 'hydro.namelist'
    character(len=64)        :: dasConfigFile = 'namelist.hrldas'
    character(len=256)       :: forcingDir    = 'WRFHYDRO_FORCING'
    logical                  :: nestToNest    = .FALSE.
    logical                  :: lwrite_grid   = .TRUE.
    logical                  :: llog_memory   = .FALSE.
    logical                  :: ltestfill_imp = .FALSE.
    logical                  :: ltestfill_exp = .FALSE.
    integer                  :: nfields       = size(WRFHYDRO_FieldList)
    integer                  :: timeSlice     = 0
    integer                  :: timeStepInt = 0
    logical                  :: lwrite_debug = .FALSE.
    integer                  :: debugIntvlInt =0
    type(ESMF_TimeInterval)  :: debugIntvl
    type(ESMF_TimeInterval)  :: debugImpAccum
    type(ESMF_TimeInterval)  :: debugExpAccum
    integer                  :: debugImpSlice = 1
    integer                  :: debugExpSlice = 1
    type (ESMF_Clock)        :: clock(1)
    type (ESMF_TimeInterval) :: stepTimer(1)
    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    integer                  :: mode(1)    = WRFHYDRO_Unknown
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  ! some temporary debug variables
  character(len=ESMF_MAXSTR) :: logMsg

  !-----------------------------------------------------------------------------
  contains
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

    ! allocate memory for this internal state and set it in the component
    allocate(is%wrap, stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg='WRFHYDRO: Allocation of internal state memory failed.', &
      file=FILENAME, rcToReturn=rc)) return ! bail out
    call ESMF_UserCompSetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(gcomp, model_routine_SS, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! switching to IPD versions
    call ESMF_GridCompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call NUOPC_CompSetEntryPoint(gcomp, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_DataInitialize, &
       specRoutine=DataInitialize, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_MethodRemove(gcomp, label=model_label_CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail ou
    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_CheckImport, &
       specRoutine=CheckImport, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail ou
    call NUOPC_CompSpecialize(gcomp, speclabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
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

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! check gcomp for config
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    if (configIsPresent) then

      ! read and ingest free format component attributes
      call ESMF_GridCompGet(gcomp, config=config, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      attrFF = NUOPC_FreeFormatCreate(config, &
        label=trim(cname)//"_attributes::", relaxedflag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    endif

    ! Determine Domain ID
    call ESMF_AttributeGet(gcomp, name="did", value=value, &
      defaultValue="1", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%did = ESMF_UtilString2Int(value, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Time Step
    call ESMF_AttributeGet(gcomp, name="time_step", value=value, defaultValue="0", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    read (value,*,iostat=stat) is%wrap%timeStepInt
    if (stat /= 0) then
      call ESMF_LogSetError(ESMF_FAILURE, &
        msg=METHOD//": Cannot convert "//trim(value)//" to integer.", &
        line=__LINE__,file=__FILE__,rcToReturn=rc)
      return  ! bail out
    endif

    ! Debug Write Interval
    call ESMF_AttributeGet(gcomp, name="debug_interval", value=value, defaultValue="default", &
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

    ! Determine hydro configuration filename
    call ESMF_AttributeGet(gcomp, name="config_file", value=value, &
      defaultValue="hydro.namelist", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%configFile = value

    ! Determine DAS configuration filename
    call ESMF_AttributeGet(gcomp, name="das_config_file", value=value, &
      defaultValue="namelist.hrldas", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%dasConfigFile = value

    ! Connect Nest to Nest
    call ESMF_AttributeGet(gcomp, name="nest_to_nest", value=value, &
     defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%nestToNest = (trim(value)=="true")

    ! Forcing Directory
    call ESMF_AttributeGet(gcomp, name="forcings_directory", value=value, &
     defaultValue=is%wrap%forcingDir, &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%forcingDir = trim(value)

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

    ! Test fill import fields
    call ESMF_AttributeGet(gcomp, name="testfill_imp", value=value, &
      defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%ltestfill_imp = (trim(value)=="true")

    ! Test fill export fields
    call ESMF_AttributeGet(gcomp, name="testfill_exp", value=value, &
      defaultValue="false", &
      convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    is%wrap%ltestfill_exp = (trim(value)=="true")

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

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex

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

    ! initialize wrfhydro
    call ESMF_GridCompGet(gcomp, vm=vm, rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    call wrfhydro_nuopc_ini(is%wrap%did,vm,clock,is%wrap%forcingDir,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! get hgrid for domain id
    call WRFHYDRO_get_hgrid(is%wrap%did,is%wrap%hgrid,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    ! add namespace
    if(.NOT.is%wrap%nestToNest) then
      is%wrap%NStateImp(1) = importState
      is%wrap%NStateExp(1) = exportState
    else
      call NUOPC_AddNestedState(importState, &
        CplSet=trim(is%wrap%hgrid), &
        nestedStateName="NestedStateImp_N1", &
        nestedState=is%wrap%NStateImp(1), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_AddNestedState(exportState, &
        CplSet=trim(is%wrap%hgrid), &
        nestedStateName="NestedStateExp_N1", &
        nestedState=is%wrap%NStateExp(1), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    call WRFHYDRO_FieldDictionaryAdd(rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    !!
    !! advertise import and export fields
    !!
    do fIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fIndex)%adImport) then
        call NUOPC_Advertise(is%wrap%NStateImp(1), &
          standardName=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        call NUOPC_Advertise(is%wrap%NStateExp(1), &
          standardName=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
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

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: WRFHYDRO_Grid
    type(ESMF_Field)           :: field
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex

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

    WRFHYDRO_Grid = WRFHYDRO_GridCreate(is%wrap%did,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (is%wrap%lwrite_grid) then
      call WRFHYDRO_ESMF_GridWrite(WRFHYDRO_Grid, &
        trim(cname)//'_grid_D'//trim(is%wrap%hgrid)//".nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    do fIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fIndex)%adImport) then
        importConnected = NUOPC_IsConnected(is%wrap%NStateImp(1), &
          fieldName=WRFHYDRO_FieldList(fIndex)%stdname)
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedImport = .TRUE.
        field = WRFHYDRO_FieldCreate(stdName=WRFHYDRO_FieldList(fIndex)%stdname, &
          grid=WRFHYDRO_grid, &
          did=is%wrap%did, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fIndex)%adImport) then
        call ESMF_StateRemove(is%wrap%NStateImp(1), (/trim(WRFHYDRO_FieldList(fIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        exportConnected = NUOPC_IsConnected(is%wrap%NStateExp(1), &
          fieldName=WRFHYDRO_FieldList(fIndex)%stdname)
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedExport = .TRUE.
        field = WRFHYDRO_FieldCreate(stdName=WRFHYDRO_FieldList(fIndex)%stdname, &
          grid=WRFHYDRO_grid, &
          did=is%wrap%did, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateExp(1), field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fIndex)%adExport) then
        call ESMF_StateRemove(is%wrap%NStateExp(1),(/trim(WRFHYDRO_FieldList(fIndex)%stdname)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      ! TODO: Initialize the value in the pointer to 0 after proper restart is setup
      !if(associated(WRFHYDRO_FieldList(fIndex)%farrayPtr) ) WRFHYDRO_FieldList(fIndex)%farrayPtr = 0.0
      ! remove a not connected Field from State

    enddo

!    Model has initialized its own field memory so don't fill state.
!    call NUOPC_FillState(is%wrap%NStateImp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return
!    call NUOPC_FillState(is%wrap%NStateExp(1),0,rc=rc)
!    if (ESMF_STDERRORCHECK(rc)) return

    is%wrap%mode(1) = WRFHYDRO_RunModeGet(is%wrap%NStateImp(1),rc)
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

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%timeSlice = is%wrap%timeSlice + 1

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Initialize import and export fields
    ! No initialization. Fields remain set to initial value

    ! Fill import fields with test data
    if (is%wrap%ltestfill_imp) then
      ! Not Implemented
    endif

    ! Fill export fields with test data
    if (is%wrap%ltestfill_exp) then
      ! Not Implemented
    endif

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

!    if (is%wrap%verbosity >= VERBOSITY_LV3) &
!      call WRFHydro_FieldListLog(label=trim(cname))

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

  !-----------------------------------------------------------------------------

#undef METHOD
#define METHOD "SetClock"

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    type(type_InternalState)   :: is
    integer                    :: dt
    type(ESMF_Clock)           :: modelClock
    type(ESMF_TimeInterval)    :: timeStep

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

    ! query the clock for its timestep
    call ESMF_ClockGet(modelClock, timeStep=timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the timestep for seconds
    call ESMF_TimeIntervalGet(timestep,s=dt,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! override timestep
    if (is%wrap%timeStepInt /= 0) then 
      call ESMF_TimeIntervalSet(timestep, &
        s=is%wrap%timeStepInt, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call WRFHYDRO_set_timestep(is%wrap%did,real(is%wrap%timeStepInt),rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call ESMF_ClockSet(modelClock, timeStep=timeStep, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    else
      call WRFHYDRO_set_timestep(is%wrap%did,real(dt),rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    call NUOPC_CompSetClock(gcomp, modelClock, timeStep, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%clock(1) = modelClock

    ! Reset Timers
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

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! get the curr time out of the clock
    call ESMF_ClockGet(modelClock, currTime=modelCurrTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! check that Fields in the importState show correct timestamp

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

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": entered "//METHOD, ESMF_LOGMSG_INFO)
#endif

    rc = ESMF_SUCCESS

    ! Query component for name
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    is%wrap%timeSlice = is%wrap%timeSlice + 1
    if (is%wrap%timeSlice > 999999999) then
      sStr = '999999999+'
    else
      write (sStr,"(I0)") is%wrap%timeSlice
    endif

    ! query the component for its clock, importState, and exportState
    call NUOPC_ModelGet(gcomp, &
      modelClock=modelClock, &
      importState=importState, &
      exportState=exportState, &
      rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the clock for its current time and timestep
    call ESMF_ClockGet(modelClock, &
      currTime=currTime, timeStep=timeStep, rc=rc)
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

    call ESMF_ClockGet(is%wrap%clock(1),timeStep=timestep,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do while (is%wrap%stepTimer(1) >= timestep)
    ! call wrfhydro advance
      call ESMF_LogWrite( &
        'WRFHYDRO: Advance Slice='//trim(sStr)//" HGRID="//trim(is%wrap%hgrid), &
        ESMF_LOGMSG_INFO)
      call wrfhydro_nuopc_run(is%wrap%did,is%wrap%mode(1), &
        is%wrap%clock(1),is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
      call ESMF_ClockAdvance(is%wrap%clock(1),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%stepTimer(1) = &
        is%wrap%stepTimer(1) - timestep
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

    call wrfhydro_nuopc_fin(is%wrap%did,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of internal state memory failed.', &
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
    do fIndex = 1, size(WRFHydro_FieldList)
      if (WRFHydro_FieldList(fIndex)%adImport) cntImp = cntImp + 1
      if (WRFHydro_FieldList(fIndex)%adExport) cntExp = cntExp + 1
    enddo

    ! Report advertised import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of advertised import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%adImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
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
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%adExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
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

    ! Count advertised import and export fields
    cntImp = 0
    cntExp = 0
    do fIndex = 1, size(WRFHydro_FieldList)
      if (WRFHydro_FieldList(fIndex)%realizedImport) cntImp = cntImp + 1
      if (WRFHydro_FieldList(fIndex)%realizedExport) cntExp = cntExp + 1
    enddo

    ! Report realized import fields
    write(logMsg,'(a,a,i0,a)') TRIM(label)//': ', &
      'List of realized import fields(',cntImp,'):'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    write(logMsg,'(a,a5,a,a16,a,a)') TRIM(label)//': ', &
      'index',' ','name',' ','standardName'
    call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
    cntImp = 0
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%realizedImport) cycle
      cntImp = cntImp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
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
    do fIndex=1, size(WRFHydro_FieldList)
      if (.NOT.WRFHydro_FieldList(fIndex)%realizedExport) cycle
      cntExp = cntExp + 1
      write(logMsg,'(a,i5,a,a16,a,a)') TRIM(label)//': ', &
        cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stdname), &
        ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
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

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (.NOT.(rc.eq.ESMF_SUCCESS)) then
      call ESMF_LogWrite(trim(label)// &
        ' ESMF_UserCompGetInternalState failed.',ESMF_LOGMSG_ERROR)
      return  ! bail out
    endif

    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Nest To Nest           = ",is%wrap%nestToNest
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label)//": ", &
      "Forcing Directory      = ",trim(is%wrap%forcingDir)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Time Step Config       = ",is%wrap%timeStepInt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Write Debug Files      = ",is%wrap%lwrite_debug
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Debug Write Interval   = ",is%wrap%debugIntvlInt
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Domain ID              = ",is%wrap%did
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0))") trim(label)//": ", &
      "Verbosity              = ",is%wrap%verbosity
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label)//": ", &
      "Config File            = ",is%wrap%configFile
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,A))") trim(label)//": ", &
      "DAS Config File        = ",is%wrap%dasConfigFile
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Write Grid             = ",is%wrap%lwrite_grid
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Test Fill Import       = ",is%wrap%ltestfill_imp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Test Fill Export       = ",is%wrap%ltestfill_exp
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,L1))") trim(label)//": ", &
      "Log Memory             = ",is%wrap%llog_memory
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

    select case(is%wrap%mode(1))
      case (WRFHYDRO_Offline)
        modeStr ="WRFHYDRO_Offline"
      case (WRFHYDRO_Coupled)
        modeStr = "WRFHYDRO_Coupled"
      case (WRFHYDRO_Hybrid)
        modeStr = "WRFHYDRO_Hybrid"
      case default
        modeStr = "WRFHYDRO_Unknown"
    end select
    write (logMsg, "(A,(A,A))") trim(label)//": ", &
      "Mode = ",trim(modeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

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
      "Slice(",is%wrap%timeSlice,") ", &
      "Current Time = ",trim(currTimeStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
    write (logMsg, "(A,(A,I0,A),(A,A))") trim(label)//": ", &
      "Slice(",is%wrap%timeSlice,") ", &
      "Time Step    = ",trim(timestepStr)
    call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

#ifdef DEBUG
    call ESMF_LogWrite(MODNAME//": leaving "//METHOD, ESMF_LOGMSG_INFO)
#endif

  end subroutine

end module
