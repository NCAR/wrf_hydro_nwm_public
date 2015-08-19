module wrfhydro_nuopc

  !-----------------------------------------------------------------------------
  ! HYDRO Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, only: &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance, &
    model_label_Finalize  => label_Finalize

  use module_mpp_land, only: global_nx, global_ny, decompose_data_real, &
                 write_io_real, my_id, mpp_land_bcast_real1, IO_id, &
                mpp_land_bcast_real, mpp_land_bcast_int1

  use module_HYDRO_drv, only: HYDRO_ini, HYDRO_exe
  use module_HYDRO_io, only: get_file_dimension
  use module_CPL_LAND, only: CPL_LAND_INIT
  use module_rt_data, only: rt_domain
  use module_namelist, only: nlst_rt
  use netcdf

  implicit none

#include <netcdf.inc>


  private

    ! Offline mode
    logical :: offline_mode

    ! HRLDAS Configuration
    character(len=ESMF_MAXPATHLEN) :: hrldasConfigFile = "namelist.hrldas"
    CHARACTER(len=80) :: indir = " "
    character(len = 256):: GEO_STATIC_FLNM

    ! Grid Variables
    real(ESMF_KIND_R8) :: min_lat, max_lat, min_lon, max_lon
    integer :: i_count, j_count
    type(ESMF_Grid)         :: WrfhydroGrid

    ! Variables normally set in WRF
    integer :: num_tiles
    ! integer :: ide, jde - used in call to nuopc_cpl_Hydro

    ! Variables normally set in WRF Domain
    integer :: num_soil_layers
    real,dimension(:),allocatable :: zs ! zoil layer depths
    integer,dimension(:,:), allocatable :: IVGTYP, isltyp
    integer :: sf_surface_physics
    integer :: num_nests
    INTEGER,allocatable :: i_start(:),i_end(:)
    INTEGER,allocatable :: j_start(:),j_end(:)

    ! added for check soil moisture and soiltype
    integer ::  checkSOIL_flag

    ! added to track the driver clock
    character(len=19) :: cpl_outdate

    ! added to consider the adaptive time step from driver.
    real    :: dtrt0
    integer ::  mm0

    public SetServices

  contains

  !-----------------------------------------------------------------------------
  ! NUOPC subroutines
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

    call NUOPC_CompSpecialize(gcomp, specLabel=model_label_Finalize, &
      specRoutine=ModelFinalize, rc=rc)
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
    type(ESMF_TimeInterval) :: timeStep
    real                    :: HYDRO_dt
    integer                 :: importCountConnected, exportCountConnected

    rc = ESMF_SUCCESS

    call ESMF_LogWrite(msg="WRFHYDRO: Start Initialization Phase 2", &
      logmsgFlag=ESMF_LOGMSG_INFO, &
      line=__LINE__, &
      file=__FILE__)

    call InitializeWrfhydroGrid(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call InitializeWrfhydroDomain(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
     return  ! bail out

    ! create a Grid object for Fields
    gridIn = WrfhydroGrid
    gridOut = WrfhydroGrid ! for now out same as in

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

    call count_connected_fields(importState, importCountConnected, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call count_connected_fields(exportState, exportCountConnected, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if ( importCountConnected == 0 .and. exportCountConnected == 0 ) then
      call ESMF_LogWrite(msg="WRFHYDRO: Running in offline mode.", &
        logmsgFlag=ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__)

      offline_mode = .true.

      call ESMF_LogWrite(msg="WRFHYDRO: Remove all fields", &
        logmsgFlag=ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__)
      call remove_fields(importState, rc)
      call remove_fields(exportState, rc)

    elseif ( importCountConnected == 5 .and. exportCountConnected == 4 ) then
      call ESMF_LogWrite(msg="WRFHYDRO: Running in coupled mode.", &
        logmsgFlag=ESMF_LOGMSG_INFO, &
        line=__LINE__, &
        file=__FILE__)
        offline_mode = .false.

      !!
      !! import fields
      !!

      field = ESMF_FieldCreate(name="soil_temperature_b", grid=gridIn, &
        arrayspec=soilArraySpec, gridToFieldMap=(/1,2/), &
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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
        ungriddedLBound=(/1/), ungriddedUBound=(/num_soil_layers/), rc=rc)
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

    else
      call ESMF_LogWrite(msg="WRFHYDRO: Partial field connection!", &
        logmsgFlag=ESMF_LOGMSG_ERROR, &
        line=__LINE__, &
        file=__FILE__)
      rc = ESMF_RC_ARG_OUTOFRANGE
      return  ! bail out
    endif

    call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call timeinterval_to_real(timeInterval=timeStep,dt=HYDRO_dt,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call nuopc_cpl_HYDRO_ini(HYDRO_dt, clock, gcomp, &
      i_start(1),i_end(1), &
      j_start(1),j_end(1), &
      rc )

    call ESMF_LogWrite(msg="WRFHYDRO: Finish Initialization Phase 2", &
      logmsgFlag=ESMF_LOGMSG_INFO, &
      line=__LINE__, &
      file=__FILE__)

  end subroutine

  !-----------------------------------------------------------------------------

    subroutine ModelAdvance(gcomp, rc)
        type(ESMF_GridComp)  :: gcomp
        integer, intent(out) :: rc

        ! local variables
        type(ESMF_Clock)              :: clock
        type(ESMF_State)              :: importState, exportState
        type(ESMF_TimeInterval)       :: timeStep
        real                          :: HYDRO_dt

        rc = ESMF_SUCCESS

        call ESMF_LogWrite(msg="WRFHYDRO: Start Advance WRF Hydro Model", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

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
            "------>Advancing WRFHYDRO from: ", rc=rc)
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

        call ESMF_ClockGet(clock, timeStep=timeStep, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call timeinterval_to_real(timeInterval=timeStep,dt=HYDRO_dt,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        ! ide, jde?
        !    call nuopc_drv_HYDRO(HYDRO_dt, gcomp, &
        !      i_start(1),min(i_end(1), ide-1), &
        !      j_start(1),min(j_end(1), jde-1), &
        !      rc )
        call nuopc_cpl_HYDRO_run(HYDRO_dt, gcomp, &
            i_start(1),i_end(1), &
            j_start(1),j_end(1), &
            rc )
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_LogWrite(msg="WRFHYDRO: Finish Advance WRF Hydro Model", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine ModelFinalize(model,rc)
        type(ESMF_GridComp)  :: model
        integer, intent(out) :: rc

        rc = ESMF_SUCCESS

        deallocate(zs)
        deallocate(IVGTYP)
        deallocate(isltyp)
        deallocate(i_start)
        deallocate(i_end)
        deallocate(j_start)
        deallocate(j_end)

        call ESMF_LogWrite(msg="WRFHYDRO: WRF Hydro Model Finalized", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

    !-----------------------------------------------------------------------------
    ! Domain Intialization
    !-----------------------------------------------------------------------------

    subroutine InitializeWrfhydroGrid(rc)
        integer, intent(out)    :: rc

        ! local
        character(len=160)          :: msg
        integer                     :: ierr
        real, allocatable           :: latitude(:,:), longitude(:,:)

        ! NOAHLSM_OFFLINE namelist variables for NoahMP

        integer  :: finemesh, finemesh_factor
        integer :: forc_typ, snow_assim
        integer :: HRLDAS_ini_typ
        integer            :: nsoil
        integer, PARAMETER    :: MAX_SOIL_LEVELS = 10   ! maximum soil levels in namelist
        REAL, DIMENSION(MAX_SOIL_LEVELS) :: soil_thick_input       ! depth to soil interfaces from namelist [m]
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

        call ESMF_LogWrite(msg="WRFHYDRO: Start Initialize WRF-Hydro Grid", &
          logmsgFlag=ESMF_LOGMSG_INFO, &
          line=__LINE__, &
          file=__FILE__)

        open(30, file=trim(hrldasConfigFile), form="FORMATTED", iostat=ierr)
        if (ierr /= 0) then
          write (msg,"(I5)") ierr
          call ESMF_LogWrite(msg="WRFHYDRO: Error opening HRLDAS config file = " // msg, &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, &
            file=__FILE__)
          rc = ESMF_RC_ARG_OUTOFRANGE
          return  ! bail out
        endif
        read(30, NOAHLSM_OFFLINE, iostat=ierr)
        if (ierr /= 0) then
          write (msg,"(I5)") ierr
          call ESMF_LogWrite(msg="WRFHYDRO: Error reading HRLDAS config file = " // msg, &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, &
            file=__FILE__)
          rc = ESMF_RC_ARG_OUTOFRANGE
          return  ! bail out
        endif
        close ( 30, iostat=ierr )
        if (ierr /= 0) then
          write (msg,"(I5)") ierr
          call ESMF_LogWrite(msg="WRFHYDRO: Error closing hydro config file = " // msg, &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, &
            file=__FILE__)
          rc = ESMF_RC_ARG_OUTOFRANGE
          return  ! bail out
        endif

        write (msg, "(A18,A40)") "WRFHYDRO: INDIR = ", indir
        call ESMF_LogWrite(msg=msg, &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        write (msg, "(A28,A40)") "WRFHYDRO: GEO_STATIC_FLNM = ", GEO_STATIC_FLNM
        call ESMF_LogWrite(msg=msg, &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        ! Default latitude, longitude and ix,jx (Front Range)
        !min_lat = 38.60428
        !max_lat = 40.94429
        !min_lon = -106.6588
        !max_lon = -103.5294
        !i_count = 268
        !j_count = 260

        call get_file_dimension(fileName=GEO_STATIC_FLNM,ix=i_count,jx=j_count)

        write (msg, "(A29,2I5)") "WRFHYDRO: i_count, j_count = ", &
          i_count, j_count
        call ESMF_LogWrite(msg=msg, &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        num_tiles = 1

        allocate(latitude(i_count,j_count))
        allocate(longitude(i_count,j_count))


        ! Get Latitude (lat)
        call get_2d_netcdf("XLAT_M", GEO_STATIC_FLNM, latitude, i_count, j_count, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        write (msg, "(A40,2F10.5)") "WRFHYDRO: XLATM(1:1), XLAT_M(ix:jx) = ", &
          latitude(1,1), latitude(i_count,j_count)
        call ESMF_LogWrite(msg=msg, &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        min_lat = latitude(1,1)
        max_lat = latitude(i_count,j_count)
        deallocate(latitude)

        ! Get Longitude (lon)
        call get_2d_netcdf("XLONG_M", GEO_STATIC_FLNM, longitude, i_count, j_count, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        write (msg, "(A40,2F10.5)") "WRFHYDRO: XLON_M(1:1), XLON_M(ix:jx) = ", &
          longitude(1,1), longitude(i_count,j_count)
        call ESMF_LogWrite(msg=msg, &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        min_lon = longitude(1,1)
        max_lon = longitude(i_count,j_count)
        deallocate(longitude)

        ! create a Grid object for Fields
        WrfhydroGrid = NUOPC_GridCreateSimpleXY( &
            min_lat, max_lat, &
            min_lon, max_lon, &
            i_count, j_count, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        call ESMF_LogWrite(msg="WRFHYDRO: Finish Initialize WRF-Hydro Grid", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine InitializeWrfhydroDomain(rc)
        integer, intent(out)           :: rc

        ! local variables
        type(ESMF_VM)           :: currentVM
        integer                 :: localPet
        integer                 :: petCount
        integer                 :: peCount
        integer                 :: iStart, iEnd
        integer                 :: jStart, jEnd
        type(ESMF_DELayout)     :: delayout
        integer, allocatable    :: dimExtent(:,:)
        integer, allocatable    :: iIndexList(:), jIndexList(:)
        type(ESMF_DistGrid)     :: distGrid

        rc = ESMF_SUCCESS

        call ESMF_LogWrite(msg="WRFHYDRO: Start Initialize WRF-Hydro Domain", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        num_nests = 0
        num_soil_layers = 4 ! Normally set in WRF domain grid%num_soil_layers
        sf_surface_physics = 0 ! Normally set in WRF domain grid%sf_surface_physics

        allocate(zs(num_soil_layers))
        allocate(IVGTYP(i_count,j_count))
        allocate(isltyp(i_count,j_count))
        allocate(i_start(num_tiles))
        allocate(i_end(num_tiles))
        allocate(j_start(num_tiles))
        allocate(j_end(num_tiles))

        zs(1) = -0.1 ! Normally set in WRF domain grid%zs(1)
        zs(2) = -0.4 ! Normally set in WRF domain grid%zs(2)
        zs(3) = -1.0 ! Normally set in WRF domain grid%zs(3)
        zs(4) = -2.0 ! Normally set in WRF domain grid%zs(4)
        IVGTYP = 0 ! Normally set in WRF domain grid%IVGTYP(its:ite,jts:jte)
        isltyp = 0 ! Normally set in WRF domain grid%isltyp(its:ite,jts:jte)

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

        ! See if the grid create has a dist grid already
        call ESMF_GridGet(WrfhydroGrid, distgrid=distGrid, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        !! Get the grid distribution for this pet
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

        i_start(1) = minVal(iIndexList)
        i_end(1)   = maxVal(iIndexList)
        j_start(1) = minVal(jIndexList)
        j_end(1)   = maxVal(jIndexList)

        deallocate(dimExtent)
        deallocate(iIndexList)
        deallocate(jIndexList)

        call ESMF_LogWrite(msg="WRFHYDRO: Finish Initialize WRF-Hydro Domain", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

    !-----------------------------------------------------------------------------
    ! Model Glue Code
    !-----------------------------------------------------------------------------

    subroutine nuopc_cpl_HYDRO_ini(HYDRO_dt,clock,gcomp,its,ite,jts,jte,rc)
        real, intent(in)                :: HYDRO_dt
        type(ESMF_Clock)                :: clock
        type(ESMF_GridComp)             :: gcomp

        integer, intent(in)             :: its, ite, jts,jte
        integer, intent(out)            :: rc

        ! local variables
        integer             :: k, ix, jx, mm, nn
        integer             :: did
        integer             :: ntime
        integer             :: i,j
        character(len=160)   :: msg

        rc = ESMF_SUCCESS

        did = 1
        ix = ite - its + 1
        jx = jte - jts + 1

        call ESMF_LogWrite(msg="WRFHYDRO: Start Initialize WRF Hydro Model", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        if(HYDRO_dt .le. 0) then
            call ESMF_LogWrite(msg="WRFHDRYO: HYDRO_dt less than 1 is not supported.", &
                logmsgFlag=ESMF_LOGMSG_ERROR, &
                line=__LINE__, &
                file=__FILE__)
            rc = ESMF_RC_ARG_OUTOFRANGE
            return  ! bail out
        endif

        ntime = 1
        nlst_rt(did)%dt = HYDRO_dt
        nlst_rt(did)%nsoil = num_soil_layers

        call mpp_land_bcast_int1 (nlst_rt(did)%nsoil)

        allocate(nlst_rt(did)%zsoil8(nlst_rt(did)%nsoil))
        if(zs(1) <  0) then
            nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = zs(1:nlst_rt(did)%nsoil)
        else
            nlst_rt(did)%zsoil8(1:nlst_rt(did)%nsoil) = -1*zs(1:nlst_rt(did)%nsoil)
        endif

        call clock_get(clock,current_timestr=cpl_outdate,rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
        nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

        write (msg, "(A45,4I5)") "WRFHYDRO: CPL_LAND_INIT its, ite, jts, jte = ", its, ite, jts, jte
        call ESMF_LogWrite(msg=trim(msg), &
          logmsgFlag=ESMF_LOGMSG_INFO, &
          line=__LINE__, &
          file=__FILE__)
        call CPL_LAND_INIT(its, ite, jts, jte)

        write (msg, "(A32,I2)") "WRFHYDRO: sf_surface_physics is ", sf_surface_physics
        call ESMF_LogWrite(msg=trim(msg), &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        if(sf_surface_physics .eq. 5) then
            ! clm4
            call HYDRO_ini(ntime,did=did,ix0=1,jx0=1)
        else
            write (msg, "(A29,2I5)") "WRFHYDRO: HYDRO_ini ix, jx = ", ix, jx
            call ESMF_LogWrite(msg=trim(msg), &
              logmsgFlag=ESMF_LOGMSG_INFO, &
              line=__LINE__, &
              file=__FILE__)
            call HYDRO_ini(ntime,did,ix0=ix,jx0=jx,vegtyp=IVGTYP(its:ite,jts:jte),soltyp=isltyp(its:ite,jts:jte))
        endif

        write (msg, "(A21,I2)") "WRFHYDRO: sys_cpl is ", nlst_rt(did)%sys_cpl
        call ESMF_LogWrite(msg=msg, &
          logmsgFlag=ESMF_LOGMSG_INFO, &
          line=__LINE__, &
          file=__FILE__)

        nlst_rt(did)%startdate(1:19) = cpl_outdate(1:19)
        nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

        nlst_rt(did)%dt = HYDRO_dt
        if(nlst_rt(did)%dtrt .ge. HYDRO_dt) then
            nlst_rt(did)%dtrt = HYDRO_dt
            mm0 = 1
        else
            mm = HYDRO_dt/nlst_rt(did)%dtrt
            if(mm*nlst_rt(did)%dtrt .lt. HYDRO_dt) nlst_rt(did)%dtrt = HYDRO_dt/mm
            mm0 = mm
        endif

        dtrt0 = nlst_rt(did)%dtrt
        RT_DOMAIN(did)%initialized = .true.

        call ESMF_LogWrite(msg="WRFHYDRO: Finish Initialize WRF Hydro Model", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

  !-----------------------------------------------------------------------------

    subroutine nuopc_cpl_HYDRO_run(HYDRO_dt,gcomp,its,ite,jts,jte,rc)
        real, intent(in)                :: HYDRO_dt
        type(ESMF_GridComp)             :: gcomp
        integer, intent(in)             :: its, ite, jts,jte
        integer, intent(out)            :: rc

        ! local variables
        integer             :: k, ix, jx, mm, nn
        integer             :: did
        character(len=160)  :: msg
        character(len=80)   :: hgrid
        type(ESMF_State)    :: importState, exportState
        type(ESMF_Clock)    :: clock

        rc = ESMF_SUCCESS

        did = 1
        ix = ite - its + 1
        jx = jte - jts + 1

        call ESMF_LogWrite(msg="WRFHYDRO: Start model advance glue code", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        if(.not. RT_DOMAIN(did)%initialized) then
            call ESMF_LogWrite(msg="WRFHDRYO: Model has not been initialized!", &
                logmsgFlag=ESMF_LOGMSG_ERROR, &
                line=__LINE__, &
                file=__FILE__)
            rc = ESMF_RC_ARG_OUTOFRANGE
            return  ! bail out
        endif

        if(HYDRO_dt .le. 0) then
            call ESMF_LogWrite(msg="WRFHDRYO: HYDRO_dt less than 1 is not supported.", &
                logmsgFlag=ESMF_LOGMSG_ERROR, &
                line=__LINE__, &
                file=__FILE__)
            rc = ESMF_RC_ARG_OUTOFRANGE
            return  ! bail out
        endif

        nlst_rt(did)%dt = HYDRO_dt

        write (msg,"(A35,2F10.2)") "WRFHDRYO: mm0, nlst_rt(did)%dtrt = ",mm0, nlst_rt(did)%dtrt
        call ESMF_LogWrite(msg=trim(msg), &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        if((mm0*nlst_rt(did)%dtrt) .ne. HYDRO_dt) then   ! NUOPC driver time step changed.
            if(dtrt0 .ge. HYDRO_dt) then
                nlst_rt(did)%dtrt = HYDRO_dt
                mm0 = 1
            else
                mm = HYDRO_dt/dtrt0
                if(mm*dtrt0 .lt. HYDRO_dt) nlst_rt(did)%dtrt = HYDRO_dt/mm
                mm0 = mm
            endif
        endif

        write (msg,"(A32,2F10.2)") "WRFHDRYO: mm, nlst_rt(did)%dt = ",mm, nlst_rt(did)%dt
        call ESMF_LogWrite(msg=trim(msg), &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        if(nlst_rt(did)%SUBRTSWCRT .eq.0  .and. &
            nlst_rt(did)%OVRTSWCRT .eq. 0 .and. &
            nlst_rt(did)%GWBASESWCRT .eq. 0) then
          call ESMF_LogWrite(msg="WRFHDRYO: SUBRTSWCRT,OVRTSWCRT,GWBASESWCRT are zero!", &
            logmsgFlag=ESMF_LOGMSG_ERROR, &
            line=__LINE__, &
            file=__FILE__)
          rc = ESMF_RC_ARG_OUTOFRANGE
          return  ! bail out
        endif

        if((.not. RT_DOMAIN(did)%initialized) .and. (nlst_rt(did)%rst_typ .eq. 1) ) then
            call ESMF_LogWrite(msg="WRFHYDRO: Restart initial data from offline file.", &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)
        else
            if (.not. offline_mode) then

              call ESMF_LogWrite(msg="WRFHYDRO: Copy import data from NUOPC", &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              ! query the Component for its importState
              call ESMF_GridCompGet(gcomp, importState=importState, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

              ! Copy the data from NUOPC fields
              call CopyImportData(importState, did, rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            else

              call ESMF_LogWrite(msg="WRFHYDRO: Read forcing data from ldasout", &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              ! query the Component for its clock
              call ESMF_GridCompGet(gcomp, clock=clock, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out

              call clock_get(clock,current_timestr=cpl_outdate,rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              nlst_rt(did)%olddate(1:19) = cpl_outdate(1:19)

              call ESMF_LogWrite(msg="WRFHYDRO: nlst_rt(did)%olddate = " // nlst_rt(did)%olddate, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              write (msg,"(A36,F10.2,2I5)") "WRFHDRYO: nlst_rt(did)%dt, ix, jx = ",nlst_rt(did)%dt,ix,jx
              call ESMF_LogWrite(msg=msg, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              write(hgrid,"(I1)") nlst_rt(did)%IGRID
              call ESMF_LogWrite(msg="WRFHYDRO: hgrid = " // hgrid, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              write(msg,"(A49,2I5)") "WRFHYDRO: rt_domain(did)%ix, rt_domain(did)%jx = ", &
                rt_domain(did)%ix, rt_domain(did)%jx
              call ESMF_LogWrite(msg=msg, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              write(msg,*) shape(rt_domain(did)%infxsrt)
              call ESMF_LogWrite(msg="WRFHYDRO: shape(rt_domain(did)%infxsrt) = " // msg, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              write(msg,*) shape(rt_domain(did)%soldrain)
              call ESMF_LogWrite(msg="WRFHYDRO: shape(rt_domain(did)%soldrain) = " // msg, &
                logmsgFlag=ESMF_LOGMSG_INFO, &
                line=__LINE__, &
                file=__FILE__)

              call ESMF_LogFlush()

              call read_forc_ldasout(cpl_outdate,hgrid, &
                indir, HYDRO_dt,rt_domain(did)%ix,rt_domain(did)%jx, &
                rt_domain(did)%infxsrt,rt_domain(did)%soldrain)

            endif
        endif

        call ESMF_LogWrite(msg="WRFHYDRO: Start HYDRO_exe", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

        ! Call the WRF-HYDRO run routine
        call HYDRO_exe(did=did)

        call ESMF_LogWrite(msg="WRFHYDRO: Finish HYDRO_exe", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)
        if(.not. offline_mode) then

          call ESMF_LogWrite(msg="WRFHYDRO: Copy export data to NUOPC", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

          ! query the Component for its exportState
          call ESMF_GridCompGet(gcomp, exportState=exportState, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          !! Copy the data to NUOPC fields
          call CopyExportData(exportState, did, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif

        ! provide groundwater soil flux to WRF for fully coupled simulations (FERSCH 09/2014)
        !if(nlst_rt(did)%GWBASESWCRT .eq. 3 ) then
          !Wei Yu: comment the following two lines. Not ready
        !yw     qsgw(its:ite,jts:jte) = gw2d(did)%qsgw
        !yw     config_flags%gwsoilcpl = nlst_rt(did)%gwsoilcpl
        !end if

        call ESMF_LogWrite(msg="WRFHYDRO: Finish model advance glue code", &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)

    end subroutine

  !-----------------------------------------------------------------------------
  ! Data copy Model to/from NUOPC
  !-----------------------------------------------------------------------------

  subroutine CopyImportData(importState, did, rc)
    type(ESMF_State), intent(in)  :: importState
    integer,          intent(in)  :: did
    integer,          intent(out) :: rc

    ! local variables

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

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine CopyExportData(exportState, did, rc)
    type(ESMF_State), intent(inout) :: exportState
    integer,          intent(in)  :: did
    integer,          intent(out)   :: rc

    ! local variables

    rc = ESMF_SUCCESS

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

  end subroutine

    !-----------------------------------------------------------------------------
    ! Utilities
    !-----------------------------------------------------------------------------

    subroutine count_connected_fields(state, connectedCount, rc)
      type(ESMF_State)     :: state
      integer, intent(out) :: connectedCount
      integer, intent(out) :: rc

      character(len=80), allocatable  :: fieldNameList(:)
      integer                         :: i, itemCount

      rc = ESMF_SUCCESS

      connectedCount = 0

      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i=1, itemCount
        if (NUOPC_StateIsFieldConnected(state, fieldName=fieldNameList(i))) then
          call ESMF_LogWrite(msg="WRFHYDRO: Field IS connected: " // fieldNameList(i), &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)
          connectedCount = connectedCount + 1
        else
          call ESMF_LogWrite(msg="WRFHYDRO: Field IS NOT connected: " // fieldNameList(i), &
            logmsgFlag=ESMF_LOGMSG_INFO, &
            line=__LINE__, &
            file=__FILE__)
        endif
      enddo
    end subroutine

    subroutine remove_fields(state, rc)
      ! TODO: this method may move into the NUOPC_ utility layer
      type(ESMF_State)                :: state
      integer, intent(out), optional  :: rc
      ! local variables
      character(len=80), allocatable  :: fieldNameList(:)
      integer                         :: i, itemCount

      if (present(rc)) rc = ESMF_SUCCESS

      call ESMF_StateGet(state, itemCount=itemCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      allocate(fieldNameList(itemCount))
      call ESMF_StateGet(state, itemNameList=fieldNameList, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      do i=1, itemCount
          ! remove field from State
          call ESMF_StateRemove(state, (/fieldNameList(i)/), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
      enddo

    end subroutine

    SUBROUTINE clock_get(clock, current_timestr, rc)
        IMPLICIT NONE

        type(ESMF_Clock)                :: clock
        integer, intent(out)            :: rc
        character (LEN=*), intent(out)  :: current_timestr

        ! Locals
        type(ESMF_Time)     :: currTime
        character (LEN=256) :: tmpstr
        integer             :: strlen

        rc = ESMF_SUCCESS

        IF ( LEN(current_timestr) < 19 ) THEN
            call ESMF_LogWrite(msg="current_timestr is too short!", &
                logmsgFlag=ESMF_LOGMSG_ERROR, &
                line=__LINE__, &
                file=__FILE__)
            rc = ESMF_RC_ARG_OUTOFRANGE
            return  ! bail out
        ENDIF

        ! Get the current time from the clock
        call ESMF_ClockGet(clock=clock,currTime=currTime, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        tmpstr = ''
        CALL ESMF_TimeGet( currTime, timeString=tmpstr, rc=rc )
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        strlen = MIN( LEN(current_timestr), LEN_TRIM(tmpstr) )
        current_timestr = ''
        current_timestr(1:strlen) = tmpstr(1:strlen)
        current_timestr(11:11) = '_'

    END SUBROUTINE clock_get

    !-----------------------------------------------------------------------------

    subroutine timeinterval_to_real(timeInterval,dt,rc)
        type(ESMF_TimeInterval),intent(in)   :: timeInterval
        real, intent(out)                     :: dt
        integer, intent(out)                 :: rc

        ! local variables
        real(ESMF_KIND_R8) :: s_r8

        rc = ESMF_SUCCESS

        call ESMF_TimeIntervalGet(timeInterval,s_r8=s_r8)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        dt = s_r8

    end subroutine

    !-----------------------------------------------------------------------------

    subroutine get_2d_netcdf(name, geo_static_flnm, array, ix, jx, rc)
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: geo_static_flnm
      integer, intent(in) :: ix, jx
      real, dimension(ix,jx), intent(out) :: array
      integer, intent(out) :: rc

      ! Local variables
      integer   :: iret, varid
      integer   :: ncid

      rc = ESMF_SUCCESS

      iret = nf_open(geo_static_flnm, NF_NOWRITE, ncid)
      if (iret /= 0) then
        call ESMF_LogWrite(msg="WRFHYDRO: NetCDF Problem opening domain file "//trim(geo_static_flnm), &
          logmsgFlag=ESMF_LOGMSG_ERROR, &
          line=__LINE__, &
          file=__FILE__)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
      endif

      iret = nf90_inq_varid(ncid,  name,  varid)
      if (iret /= 0) then
        call ESMF_LogWrite(msg="WRFHYDRO: Problem finding variable "//trim(name), &
          logmsgFlag=ESMF_LOGMSG_ERROR, &
          line=__LINE__, &
          file=__FILE__)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
      endif

      iret = nf90_get_var(ncid, varid, values=array, start=(/1,1/), count=(/ix,jx/))
      if (iret /= 0) then
        call ESMF_LogWrite(msg="WRFHYDRO: Problem retrieving variable "//trim(name), &
          logmsgFlag=ESMF_LOGMSG_ERROR, &
          line=__LINE__, &
          file=__FILE__)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
      endif

      iret = nf_close(ncid)
      if (iret /= 0) then
        call ESMF_LogWrite(msg="WRFHYDRO: NetCDF Problem closing domain file "//trim(geo_static_flnm), &
          logmsgFlag=ESMF_LOGMSG_ERROR, &
          line=__LINE__, &
          file=__FILE__)
        rc = ESMF_RC_ARG_OUTOFRANGE
        return  ! bail out
      endif

  end subroutine

end module


