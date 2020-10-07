!>
!! @mainpage NCAR's WRF-Hydro NUOPC Cap
!! @author Daniel Rosen (daniel.rosen@noaa.gov)
!! @author ESMF Support (esmf_support@ucar.edu)
!! @date 03/14/2017 WRF-Hydro NUOPC Cap Added to GitHub
!! @date 03/17/2017 Documentation Added
!!
!! @tableofcontents
!!
!! @section Overview Overview
!!
!! The Weather Research and Forecasting Hydrological (WRF-Hydro) model is a 
!! hydrometerological forecasting model developed and maintained by the 
!! National Center for Atmospheric Research (NCAR). The WRF-Hydro cap wraps 
!! the WRF-Hydro model with NUOPC compliant interfaces. The result is a 
!! WRF-Hydro model capable of coupling with other models using National 
!! Unified Operational Prediction Capability (NUOPC).
!!
!! This page documents the technical design of the specialized NUOPC model and 
!! the WRF-Hydro gluecode. For generic NUOPC model documentation please see 
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
!! Attribute          | Default         | Description
!! -------------------|------------------|-----------------------------------------------------------------------------------
!! Verbosity          | 0                | String, converted into an integer. Bit 16: LIS cap information will be logged.
!! Diagnostic         | 0                | String, converted into an integer. Bit 16: LIS cap diagnostics will be written.
!! realize_all_export | false            | Realize all export fields including non connected fields.
!! config_file        | hydro.namelist   | Override the WRF-Hydro configuration file.
!! das_config_file    | namelist.hrldas  | Override the WRF-Hydro DAS configuration file.
!! time_step          | 0                | Override the WRF-Hydro time step. Value 0: Does not override time step.
!! forcings_directory | WRFHYDRO_FORCING | Override the WRF-Hydro forcings directory.
!! domain_id          | 1                | Set the WRF-Hydro domain identifier.
!! nest_to_nest       | false            | Turn on nest to nest coupling. Each nest will be identified with an integer.
!! import_dependency  | false            | Data initialization will loop until all import field dependencies are satisfied.
!! output_directory   | [CNAME]_OUTPUT   | Configure the WRF-Hydro Cap output directory.
!! multi_instance_hyd | false            | Run multiple instances of WRF-Hydro, each in a different directory.
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

#define FILENAME "WRFHydro_NUOPC_Cap.F90"
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
    logical                  :: realizeAllExport = .FALSE.
    character(len=64)        :: configFile       = 'hydro.namelist'
    character(len=64)        :: dasConfigFile    = 'namelist.hrldas'
    integer                  :: timeStepInt      = 0
    character(len=128)       :: forcingDir       = 'WRFHYDRO_FORCING'
    integer                  :: did              = 1
    logical                  :: nestToNest       = .FALSE.
    logical                  :: importDependency = .FALSE.
    character(len=128)       :: dirOutput        = "."
    character(len=128)       :: dirInput         = "."
    logical                  :: writeRestart     = .FALSE.
    logical                  :: readRestart      = .FALSE.
    logical                  :: multiInstance    = .FALSE.
    character                :: hgrid            = '0'
    integer                  :: nnests           = 1
    integer                  :: nfields          = size(WRFHYDRO_FieldList)
    type (ESMF_Clock)        :: clock(1)
    type (ESMF_TimeInterval) :: stepTimer(1)
    type(ESMF_State)         :: NStateImp(1)
    type(ESMF_State)         :: NStateExp(1)
    integer                  :: mode(1)    = WRFHYDRO_Unknown
  endtype

  type type_InternalState
    type(type_InternalStateStruct), pointer :: wrap
  end type

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    integer                    :: stat
    type(type_InternalState)   :: is

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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP0(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: gcomp
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP0"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(gcomp, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    call WRFHydro_AttributeGet(rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! change directory for multiple instances
    if (is%wrap%multiInstance) then
      if (btest(verbosity,16)) then
        call ESMF_LogWrite(trim(cname)//": Change working directory", &
          ESMF_LOGMSG_INFO)
      endif
      call WRFHYDRO_ESMF_ChDir(trim(cname),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! prepare diagnostics folder
    if (btest(diagnostic,16) .OR. is%wrap%writeRestart) then
      call ESMF_UtilIOMkDir(pathName=trim(is%wrap%dirOutput), &
        relaxedFlag=.true., rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine WRFHydro_AttributeGet(rc)
      integer, intent(out)  :: rc

      ! local variables
      logical                    :: configIsPresent
      type(ESMF_Config)          :: config
      type(NUOPC_FreeFormat)     :: attrFF
      character(ESMF_MAXSTR)     :: logMsg
      character(len=64)          :: modeStr

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

      ! Realize all export fields
      call ESMF_AttributeGet(gcomp, name="realize_all_export", value=value, &
        defaultValue="false", convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%realizeAllExport = (trim(value)=="true")

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

      ! Time Step
      call ESMF_AttributeGet(gcomp, name="time_step", value=value, defaultValue="0", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      read (value,*,iostat=stat) is%wrap%timeStepInt
      if (stat /= 0) then
        call ESMF_LogSetError(ESMF_FAILURE, &
          msg="Cannot convert "//trim(value)//" to integer.", &
          line=__LINE__,file=__FILE__,rcToReturn=rc)
        return  ! bail out
      endif

      ! Forcing Directory
      call ESMF_AttributeGet(gcomp, name="forcings_directory", value=value, &
        defaultValue=is%wrap%forcingDir, &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%forcingDir = trim(value)

      ! Determine Domain ID
      call ESMF_AttributeGet(gcomp, name="did", value=value, &
        defaultValue="1", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%did = ESMF_UtilString2Int(value, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      ! Connect Nest to Nest
      call ESMF_AttributeGet(gcomp, name="nest_to_nest", value=value, &
        defaultValue="false", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%nestToNest = (trim(value)=="true")

      ! Determine Import Dependency
      call ESMF_AttributeGet(gcomp, name="import_dependency", &
        value=value, defaultValue="false", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%importDependency = (trim(value)=="true")

      ! Get component output directory
      call ESMF_AttributeGet(gcomp, name="output_directory", &
        value=value, defaultValue=trim(cname)//"_OUTPUT", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%dirOutput = trim(value)

      ! Get component input directory
      call ESMF_AttributeGet(gcomp, name="input_directory", &
        value=value, defaultValue=trim(cname)//"_INPUT", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%dirInput = trim(value)

      ! Write cap restart state
      call ESMF_AttributeGet(gcomp, name="write_restart", &
        value=value, defaultValue="false", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%writeRestart = (trim(value)=="true")

      ! Read cap restart state
      call ESMF_AttributeGet(gcomp, name="read_restart", &
        value=value, defaultValue="false", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%readRestart = (trim(value)=="true")

      ! Determine Import Dependency
      call ESMF_AttributeGet(gcomp, name="multi_instance_hyd", &
        value=value, defaultValue="false", &
        convention="NUOPC", purpose="Instance", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%multiInstance = (trim(value)=="true")

      if (btest(verbosity,16)) then
        call ESMF_LogWrite(trim(cname)//": Settings",ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Verbosity              = ",verbosity
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Diagnostic             = ",diagnostic
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//": ", &
          "Realze All Exports     = ",is%wrap%realizeAllExport
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Config File            = ",trim(is%wrap%configFile)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "DAS Config File        = ",trim(is%wrap%dasConfigFile)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Time Step Config       = ",is%wrap%timeStepInt
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Forcing Directory      = ",trim(is%wrap%forcingDir)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,I0))") trim(cname)//": ", &
          "Domain ID              = ",is%wrap%did
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//": ", &
          "Nest To Nest           = ",is%wrap%nestToNest
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Import Dependency      = ",is%wrap%importDependency
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Output Directory       = ",trim(is%wrap%dirOutput)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,A))") trim(cname)//": ", &
          "Input Directory        = ",trim(is%wrap%dirInput)
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Write Restart          = ",is%wrap%writeRestart
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Read Restart           = ",is%wrap%readRestart
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
        write (logMsg, "(A,(A,L1))") trim(cname)//': ', &
          "Multiple Instances     = ",is%wrap%multiInstance
        call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      endif

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP1(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: gcomp
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="InitializeP1"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    type(ESMF_VM)               :: vm
    integer                     :: fIndex
    character(len=9)            :: nStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
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
      write (nStr,"(I0)") is%wrap%did
      call NUOPC_AddNestedState(importState, &
        CplSet=trim(is%wrap%hgrid), &
        nestedStateName="NestedStateImp_N"//trim(nStr), &
        nestedState=is%wrap%NStateImp(1), rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      call NUOPC_AddNestedState(exportState, &
        CplSet=trim(is%wrap%hgrid), &
        nestedStateName="NestedStateExp_N"//trim(nStr), &
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
          name=trim(WRFHYDRO_FieldList(fIndex)%stateName), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        call NUOPC_Advertise(is%wrap%NStateExp(1), &
          standardName=trim(WRFHYDRO_FieldList(fIndex)%stdname), &
          name=trim(WRFHYDRO_FieldList(fIndex)%stateName), &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    enddo

    if (btest(verbosity,16)) call LogAdvertised()

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogAdvertised()
      ! local variables
      integer                    :: cntImp
      integer                    :: cntExp
      integer                    :: fIndex
      character(ESMF_MAXSTR)     :: logMsg

      ! Count advertised import and export fields
      cntImp = 0
      cntExp = 0
      do fIndex = 1, size(WRFHydro_FieldList)
        if (WRFHydro_FieldList(fIndex)%adImport) cntImp = cntImp + 1
        if (WRFHydro_FieldList(fIndex)%adExport) cntExp = cntExp + 1
      enddo

      ! Report advertised import fields
      write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
        'List of advertised import fields(',cntImp,'):'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
        'index',' ','name',' ','standardName'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      cntImp = 0
      do fIndex=1, size(WRFHydro_FieldList)
        if (.NOT.WRFHydro_FieldList(fIndex)%adImport) cycle
        cntImp = cntImp + 1
        write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
          cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stateName), &
          ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
        call ESMF_LogWrite(trim(logMsg), ESMF_LOGMSG_INFO)
      enddo

      ! Report advertised export fields
      write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
        'List of advertised export fields(',cntExp,'):'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
        'index',' ','name',' ','standardName'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      cntExp = 0
      do fIndex=1, size(WRFHydro_FieldList)
        if (.NOT.WRFHydro_FieldList(fIndex)%adExport) cycle
        cntExp = cntExp + 1
        write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
          cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stateName), &
          ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
        call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
      enddo

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(gcomp, importState, exportState, clock, rc)
    type(ESMF_GridComp)         :: gcomp
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Clock)            :: clock
    integer, intent(out)        :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="InitializeP3"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    type(ESMF_Grid)            :: WRFHYDRO_Grid
    type(ESMF_Field)           :: field
    logical                    :: importConnected, exportConnected
    integer                    :: fIndex
    character(len=9)           :: nStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    write (nStr,"(I0)") is%wrap%did

    ! call gluecode to create grid
    WRFHYDRO_Grid = WRFHYDRO_GridCreate(is%wrap%did,rc=rc)
    if(ESMF_STDERRORCHECK(rc)) return ! bail out

    if (btest(verbosity,16)) then
      call WRFHYDRO_ESMF_LogGrid(WRFHYDRO_Grid, &
        trim(cname)//"_"//rname//"_D"//trim(nStr),rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    ! Write grid to NetCDF file.
    if (btest(diagnostic,16)) then
      call WRFHYDRO_ESMF_GridWrite(WRFHYDRO_Grid, &
        trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
        rname//'_grid_D'//trim(nStr)//".nc", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    do fIndex = 1, size(WRFHYDRO_FieldList)
      if (WRFHYDRO_FieldList(fIndex)%adImport) then
        importConnected = NUOPC_IsConnected(is%wrap%NStateImp(1), &
          fieldName=WRFHYDRO_FieldList(fIndex)%stateName, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      else
        importConnected = .FALSE.
      endif

      if (importConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedImport = .TRUE.
        field = WRFHYDRO_FieldCreate(stateName=WRFHYDRO_FieldList(fIndex)%stateName, &
          grid=WRFHYDRO_grid, &
          did=is%wrap%did, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateImp(1), field=field, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fIndex)%adImport) then
        call ESMF_StateRemove(is%wrap%NStateImp(1), (/trim(WRFHYDRO_FieldList(fIndex)%stateName)/), &
          relaxedflag=.true.,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      endif

      if (WRFHYDRO_FieldList(fIndex)%adExport) then
        if (is%wrap%realizeAllExport) then
          exportConnected = .TRUE.
        else
          exportConnected = NUOPC_IsConnected(is%wrap%NStateExp(1), &
            fieldName=WRFHYDRO_FieldList(fIndex)%stateName, rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        endif
      else
        exportConnected = .FALSE.
      endif

      if (exportConnected) then
        WRFHYDRO_FieldList(fIndex)%realizedExport = .TRUE.
        field = WRFHYDRO_FieldCreate(stateName=WRFHYDRO_FieldList(fIndex)%stateName, &
          grid=WRFHYDRO_grid, &
          did=is%wrap%did, &
          rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Realize(is%wrap%NStateExp(1), field=field,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return
      elseif(WRFHYDRO_FieldList(fIndex)%adExport) then
        call ESMF_StateRemove(is%wrap%NStateExp(1),(/trim(WRFHYDRO_FieldList(fIndex)%stateName)/), &
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

    if (btest(verbosity,16)) call LogRealized()
    if (btest(verbosity,16)) call LogMode()

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogRealized()
      ! local variables
      integer                    :: cntImp
      integer                    :: cntExp
      integer                    :: fIndex
      character(ESMF_MAXSTR)     :: logMsg

      ! Count advertised import and export fields
      cntImp = 0
      cntExp = 0
      do fIndex = 1, size(WRFHydro_FieldList)
        if (WRFHydro_FieldList(fIndex)%realizedImport) cntImp = cntImp + 1
        if (WRFHydro_FieldList(fIndex)%realizedExport) cntExp = cntExp + 1
      enddo

      ! Report realized import fields
      write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
        'List of realized import fields(',cntImp,'):'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
        'index',' ','name',' ','standardName'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      cntImp = 0
      do fIndex=1, size(WRFHydro_FieldList)
        if (.NOT.WRFHydro_FieldList(fIndex)%realizedImport) cycle
        cntImp = cntImp + 1
        write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
          cntImp,' ',TRIM(WRFHydro_FieldList(fIndex)%stateName), &
          ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
        call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
      enddo

      ! Report realized export fields
      write(logMsg,'(a,a,i0,a)') TRIM(cname)//': ', &
        'List of realized export fields(',cntExp,'):'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      write(logMsg,'(a,a5,a,a16,a,a)') TRIM(cname)//': ', &
        'index',' ','name',' ','standardName'
      call ESMF_LogWrite(TRIM(logMsg), ESMF_LOGMSG_INFO)
      cntExp = 0
      do fIndex=1, size(WRFHydro_FieldList)
        if (.NOT.WRFHydro_FieldList(fIndex)%realizedExport) cycle
        cntExp = cntExp + 1
        write(logMsg,'(a,i5,a,a16,a,a)') TRIM(cname)//': ', &
          cntExp,' ',TRIM(WRFHydro_FieldList(fIndex)%stateName), &
          ' ',TRIM(WRFHydro_FieldList(fIndex)%stdName)
        call ESMF_LogWrite(trim(LogMsg), ESMF_LOGMSG_INFO)
      enddo

    end subroutine

    !---------------------------------------------------------------------------

    subroutine LogMode()
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      character(len=64)          :: modeStr

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
      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Mode = ",trim(modeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)                          :: cname
    character(*), parameter                :: rname="DataInitialize"
    integer                                :: verbosity, diagnostic
    character(len=64)                      :: value
    type(type_InternalState)               :: is
    type(ESMF_Clock)                       :: modelClock
    type(ESMF_Time)                        :: currTime
    character(len=32)                      :: currTimeStr
    character(len=9)                       :: nStr
    integer                                :: iIndex
    integer                                :: itemCount
    character(len=64),allocatable          :: itemNameList(:)
    type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
    type(ESMF_Field)                       :: field
    integer                                :: stat
    logical                                :: importCurrent
    logical                                :: importUpdated

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query Component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query the Component for its clock
    call NUOPC_ModelGet(gcomp, modelClock=modelClock, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! get the current time out of the clock
    call ESMF_ClockGet(modelClock, currTime=currTime, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_TimeGet(currTime, timeString=currTimeStr, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! Initialize import and export fields
    ! No initialization. Fields remain set to initial value

    importUpdated = .TRUE.
    write (nStr,"(I0)") is%wrap%did

    if (is%wrap%importDependency) then
      importCurrent = NUOPC_IsAtTime(is%wrap%NStateImp(1), &
        time=currTime, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out

      if (importCurrent) then
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency SATISFIED!!! Nest='//trim(nStr), &
          ESMF_LOGMSG_INFO)
      else
        call ESMF_LogWrite( &
          trim(cname)//': '//rname//' Initialize-Data-Dependency NOT YET SATISFIED!!! Nest='//trim(nStr), &
          ESMF_LOGMSG_INFO)
        importUpdated = .FALSE.
      endif
    endif

    if (is%wrap%readRestart) then
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
          call ESMF_AttributeGet(field, name="StandardName", &
            value=value, convention="NUOPC", purpose="Instance", rc=rc)
          if (ESMF_STDERRORCHECK(rc)) return
          call ESMF_FieldRead(field, &
            fileName=trim(is%wrap%dirInput)//"/restart_"//trim(cname)// &
              "_exp_D"//trim(nStr)//"_"//trim(currTimeStr)//"_"// &
              trim(itemNameList(iIndex))//".nc", &
            variableName=value, iofmt=ESMF_IOFMT_NETCDF, rc=rc)
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

      call ESMF_LogWrite( &
        trim(cname)//': '//rname//' Read cap restart complete! Nest='//trim(nStr), &
        ESMF_LOGMSG_INFO)
      is%wrap%readRestart = .FALSE.

    endif ! readRestart

    ! set InitializeDataComplete Attribute to "true", indicating to the
    ! generic code that all inter-model data dependencies are satisfied
    if (importUpdated) then
      call NUOPC_CompAttributeSet(gcomp, name="InitializeDataComplete", value="true", rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      ! Write initialization files
      if (btest(diagnostic,16)) then
        call NUOPC_Write(is%wrap%NStateImp(1), &
          fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
            rname//"_imp_D"//trim(nStr)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call NUOPC_Write(is%wrap%NStateExp(1), &
          fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
            rname//"_exp_D"//trim(nStr)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      endif
    endif

!    if (btest(verbosity,16)) call WRFHydro_FieldListLog(label=cname)

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)              :: cname
    character(*), parameter    :: rname="SetClock"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: dt
    type(ESMF_Clock)           :: modelClock
    type(ESMF_TimeInterval)    :: timeStep

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
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

    if (btest(verbosity,16)) call LogClock()

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogClock()
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      type(ESMF_Time)            :: currTime
      type(ESMF_TimeInterval)    :: timestep
      character(len=64)          :: currTimeStr
      character(len=64)          :: timestepStr

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

      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Current Time = ",trim(currTimeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A))") trim(cname)//": ", &
        "Time Step    = ",trim(timestepStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

subroutine CheckImport(gcomp, rc)
    type(ESMF_GridComp) :: gcomp
    integer,intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="CheckImport"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    integer                     :: nIndex
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_Time)             :: modelCurrTime
    logical                     :: allCurrTime

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(gcomp, rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! local variables
    character(32)               :: cname
    character(*), parameter     :: rname="ModelAdvance"
    integer                     :: verbosity, diagnostic
    character(len=64)           :: value
    type(type_InternalState)    :: is
    character(len=10)           :: sStr
    type(ESMF_Clock)            :: modelClock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime, advEndTime
    character(len=32)           :: currTimeStr, advEndTimeStr
    type(ESMF_TimeInterval)     :: timeStep
    character(len=9)            :: nStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    ! query component for its internal State
    nullify(is%wrap)
    call ESMF_UserCompGetInternalState(gcomp, label_InternalState, is, rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

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

    write (nStr,"(I0)") is%wrap%did

    ! Write import files
    if (btest(diagnostic,16)) then
      call NUOPC_Write(is%wrap%NStateImp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
          rname//"_imp_D"//trim(nStr)//"_"//trim(currTimeStr)//"_", &
        overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    is%wrap%stepTimer(1) = is%wrap%stepTimer(1) + timeStep

    call ESMF_ClockGet(is%wrap%clock(1),timeStep=timestep,rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    do while (is%wrap%stepTimer(1) >= timestep)
      ! call wrfhydro advance
      if (btest(verbosity,16)) then
        call LogAdvance(nIndex=1,nStr=nStr)
      endif
      call wrfhydro_nuopc_run(is%wrap%did,is%wrap%mode(1), &
        is%wrap%clock(1),is%wrap%NStateImp(1),is%wrap%NStateExp(1),rc)
      if(ESMF_STDERRORCHECK(rc)) return ! bail out
      call ESMF_ClockAdvance(is%wrap%clock(1),rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      is%wrap%stepTimer(1) = &
        is%wrap%stepTimer(1) - timestep
    enddo

    ! Write export files
    if (btest(diagnostic,16)) then
      call NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/diag_"//trim(cname)//"_"// &
          rname//"_exp_D"//trim(nStr)//"_"//trim(advEndTimeStr)//"_", &
        overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    contains ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    subroutine LogAdvance(nIndex,nStr)
      integer                    :: nIndex
      character(len=9)           :: nStr
      ! local variables
      character(ESMF_MAXSTR)     :: logMsg
      character(len=32)          :: nModeStr
      type(ESMF_Time)            :: nestCurrTime
      type(ESMF_TimeInterval)    :: nestTimestep
      character(len=32)          :: nCurrTimeStr
      character(len=32)          :: nTimeStepStr

      call ESMF_LogWrite(trim(cname)//': '//rname//&
        ' Advancing Nest='//trim(nStr),ESMF_LOGMSG_INFO)

      select case(is%wrap%mode(nIndex))
      case (WRFHYDRO_Offline)
        nModeStr ="WRFHYDRO_Offline"
      case (WRFHYDRO_Coupled)
        nModeStr = "WRFHYDRO_Coupled"
      case (WRFHYDRO_Hybrid)
        nModeStr = "WRFHYDRO_Hybrid"
      case default
        nModeStr = "WRFHYDRO_Unknown"
      end select
      write (logMsg, "(A,(A,A,A),(A,A))") trim(cname)//': ', &
        'Nest(',trim(nStr),') ', &
        'Mode = ',trim(nModeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

      if (ESMF_ClockIsCreated(is%wrap%clock(nIndex))) then
        call ESMF_ClockGet(is%wrap%clock(nIndex), &
          currTime=nestCurrTime,timeStep=nestTimestep,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_TimeGet(nestCurrTime, &
          timeString=nCurrTimeStr,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
        call ESMF_TimeIntervalGet(nestTimestep, &
          timeString=nTimeStepStr,rc=rc)
        if (ESMF_STDERRORCHECK(rc)) return  ! bail out
      else
        nCurrTimeStr = "(not_created)"
        nTimestepStr = "(not_created)"
      endif
      write (logMsg, "(A,(A,A,A),(A,A))") trim(cname)//": ", &
        "Nest(",trim(nStr),") ", &
        "Current Time = ",trim(nCurrTimeStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)
      write (logMsg, "(A,(A,A,A),(A,A))") trim(cname)//": ", &
        "Nest(",trim(nStr),") ", &
        "Time Step    = ",trim(nTimestepStr)
      call ESMF_LogWrite(trim(logMsg),ESMF_LOGMSG_INFO)

    end subroutine

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine ModelFinalize(gcomp,rc)
    type(ESMF_GridComp)  :: gcomp
    integer, intent(out) :: rc

    ! Local Variables
    character(32)              :: cname
    character(*), parameter    :: rname="ModelFinalize"
    integer                    :: verbosity, diagnostic
    character(len=64)          :: value
    type(type_InternalState)   :: is
    integer                    :: stat
    type(ESMF_Clock)           :: modelClock
    type(ESMF_Time)            :: currTime
    character(len=32)          :: currTimeStr
    character(len=9)           :: nStr

    rc = ESMF_SUCCESS

    ! Query component for name, verbosity, and diagnostic values
!    call NUOPC_CompGet(gcomp, name=name, verbosity=verbosity, &
!      diagnostic=diagnostic, rc=rc)
    call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Diagnostic", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    diagnostic = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    call ESMF_AttributeGet(gcomp, name="Verbosity", value=value, &
      defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    verbosity = ESMF_UtilString2Int(value, &
      specialStringList=(/"min","max","bit16","maxplus"/), &
      specialValueList=(/0,65535,65536,131071/), rc=rc)
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

    write (nStr,"(I0)") is%wrap%did

    ! Write export file
    if (is%wrap%writeRestart) then
      call NUOPC_Write(is%wrap%NStateExp(1), &
        fileNamePrefix=trim(is%wrap%dirOutput)//"/restart_"//trim(cname)// &
          "_exp_D"//trim(nStr)//"_"//trim(currTimeStr)//"_", &
          overwrite=.true., status=ESMF_FILESTATUS_REPLACE, timeslice=1, rc=rc)
      if (ESMF_STDERRORCHECK(rc)) return  ! bail out
    endif

    call wrfhydro_nuopc_fin(is%wrap%did,rc)
    if (ESMF_STDERRORCHECK(rc)) return  ! bail out

    deallocate(is%wrap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg='WRFHYDRO: Deallocation of internal state memory failed.', &
      file=FILENAME,rcToReturn=rc)) return ! bail out

  end subroutine

end module
