! This module defines and instantiates objects
! for a rfc forecasts type reservoir. The rfc
! forecasts reservoir type inherits input and
! output types from the reservoir base module
! and calls instantiation of these into
! sub-objects. The rfc forecasts reservoir
! type also points to types for rfc forecasts
! properties and state and calls instantiation
! of these into sub-objects. There is also a
! subroutine to run rfc forecasts reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_rfc_forecasts

    use module_rfc_forecasts_properties, only: rfc_forecasts_properties_interface
    use module_rfc_forecasts_state, only: rfc_forecasts_state_interface
    use module_levelpool, only: levelpool
    use module_reservoir, only: reservoir, reservoir_input, reservoir_output
    use module_reservoir_read_rfc_time_series_data, only: time_series_data
    implicit none

    ! Extend/derive rfc forecasts type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir) :: rfc_forecasts

        ! Define pointers to sub-types / sub-objects to and
        ! held by an rfc reservoir object.
        type (rfc_forecasts_properties_interface), pointer :: properties => null()
        type (rfc_forecasts_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => rfc_forecasts_init
        procedure :: destroy => rfc_forecasts_destroy
        procedure :: run => run_rfc_forecasts_reservoir

    end type rfc_forecasts

contains

    ! RFC Forecasts Constructor
    subroutine rfc_forecasts_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number, rfc_forecasts_parameter_file, start_date, &
        time_series_path, forecast_lookback_hours)
        implicit none
        class(rfc_forecasts), intent(inout) :: this ! object being initialized
        real,    intent(inout) :: water_elevation           ! meters AMSL
        real,    intent(in)    :: lake_area      		    ! area of lake (km^2)
        real,    intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real,    intent(in)    :: weir_coeffecient          ! weir coefficient
        real,    intent(in)    :: weir_length               ! weir length (meters)
        real,    intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real,    intent(in)    :: orifice_coefficient       ! orifice coefficient
        real,    intent(in)    :: orifice_area              ! orifice area (meters^2)
        real,    intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        real,    intent(in)    :: initial_fractional_depth  ! initial fraction water depth
        integer, intent(in)    :: lake_number               ! lake number
        character(len=*),   intent(in) :: rfc_forecasts_parameter_file
        character(len=19),  intent(in) :: start_date
        character(len=256), intent(in) :: time_series_path
        integer,            intent(in) :: forecast_lookback_hours

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate properties
            allocate ( this%properties )
            if ( .not. associated(this%properties) ) then
                ! if the properties structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts properties structure"
            else
                ! initialize rfc forecasts properties
                call this%properties%init(lake_number, rfc_forecasts_parameter_file)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate rfc forecasts state structure"
            else
                ! initialize rfc forecasts state
                call this%state%init(water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, initial_fractional_depth)
            end if
            this%pointer_allocation_guard = .true.

        end if

        ! Allocate a single level pool reservoir
        allocate(levelpool :: this%state%levelpool_ptr)

        ! Initialize level pool reservoir
        call this%state%levelpool_ptr%init(water_elevation, lake_area, &
        weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
        orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        ! Call to initialize time series data object. This object is a singleton,
        ! so if another reservoir has already initialized the object, it will return
        ! immediately.
        call time_series_data%init(start_date, time_series_path, forecast_lookback_hours, this%properties%rfc_gage_id, &
        this%properties%lookback_seconds, this%properties%offset_time_seconds, this%properties%forecast_counts, &
        this%properties%time_step_seconds, this%state%forecast_discharges, this%state%forecast_found)

    end subroutine rfc_forecasts_init

    ! RFC Forecasts Destructor
    subroutine rfc_forecasts_destroy(this)
        implicit none
        class(rfc_forecasts), intent(inout) :: this ! object being destroyed
    end subroutine rfc_forecasts_destroy

    ! Subroutine for running reservoir for a rfc forecasts reservoir
    subroutine run_rfc_forecasts_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(rfc_forecasts), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        real                :: levelpool_outflow        ! cubic meters per second (cms)

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! Update state water elevation
        this%state%water_elevation = water_elevation

        ! Run levelpool reservoir
        call this%state%levelpool_ptr%run(previous_timestep_inflow, inflow, &
        lateral_inflow, this%state%water_elevation, levelpool_outflow, routing_period)

        if (this%state%forecast_found) then
            ! USE FORECAST VALUE
            this%output%outflow = -999.0 ! Temporarily setting to -999.0
        else
            this%output%outflow = levelpool_outflow
        end if

        ! Update output variable returned from this subroutine
        outflow = this%output%outflow

        ! Set current inflow to previous_timestep_inflow
        this%input%previous_timestep_inflow = inflow

        ! Update water_elevation variable returned from this subroutine
        water_elevation = this%state%water_elevation

        ! Update the current time
        this%state%current_time = this%state%current_time + int(routing_period)

    end subroutine run_rfc_forecasts_reservoir

end module module_rfc_forecasts
