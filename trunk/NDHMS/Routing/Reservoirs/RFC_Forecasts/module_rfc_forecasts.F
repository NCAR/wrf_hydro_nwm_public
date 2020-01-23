! This module defines and instantiates objects
! for a RFC Forecasts type reservoir. The RFC
! Forecasts reservoir type inherits input and
! output types from the reservoir base module
! and calls instantiation of these into
! sub-objects. The RFC Forecasts reservoir
! type also points to types for RFC Forecasts
! properties and state and calls instantiation
! of these into sub-objects. There is also a
! subroutine to run RFC Forecasts reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_rfc_forecasts

    use module_rfc_forecasts_properties, only: rfc_forecasts_properties_interface
    use module_rfc_forecasts_state, only: rfc_forecasts_state_interface
    use module_levelpool, only: levelpool
    use module_reservoir_utilities, only: create_rfc_forecasts_diagnostic_log_file, &
                                          log_rfc_forecasts_diagnostic_data
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
        weir_length, dam_length, orifice_elevation, orifice_coefficient, &
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
        real,    intent(in)    :: dam_length                ! dam length (meters)
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
        integer                        :: update_offset_seconds

#ifdef RESERVOIR_D
        ! Create diagnostic log file only for development/debugging purposes
        call create_rfc_forecasts_diagnostic_log_file(lake_number)
#endif

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
        weir_elevation, weir_coeffecient, weir_length, dam_length, orifice_elevation, &
        orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        ! Call to initialize time series data object. This object is a singleton,
        ! so if another reservoir has already initialized the object, it will return
        ! immediately.
        call time_series_data%init(start_date, time_series_path, forecast_lookback_hours, &
        this%properties%rfc_timeslice_offset_hours, this%properties%rfc_gage_id, &
        this%properties%lake_number, this%properties%lookback_seconds, this%properties%total_counts, &
        this%properties%observed_counts, this%properties%time_step_seconds, this%state%discharges, this%state%forecast_found)

        if (this%state%forecast_found) then
            update_offset_seconds = mod(this%properties%lookback_seconds, this%properties%time_step_seconds)

            this%state%time_series_update_time = this%properties%time_step_seconds - update_offset_seconds

            this%state%time_series_index = this%properties%lookback_seconds / this%properties%time_step_seconds + 1 &
            + this%properties%observed_counts - this%properties%rfc_timeslice_offset_hours
        end if

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
        integer		        :: missing_outflow_index

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! Update state water elevation
        this%state%water_elevation = water_elevation

        ! Run levelpool reservoir
        call this%state%levelpool_ptr%run(previous_timestep_inflow, inflow, &
        lateral_inflow, this%state%water_elevation, levelpool_outflow, routing_period)

        ! If a forecast is found and the current time is less than the max time an RFC Forecast is persisted
        if (this%state%forecast_found .and. this%state%current_time .le. this%properties%rfc_forecast_persist_seconds) then

            ! If current time is at or past the update time to cycle to the next value in the discharge array and the number
            ! forecast counts has not yet been reached
            if (this%state%current_time .ge. this%state%time_series_update_time .and. this%state%time_series_index &
            .lt. this%properties%total_counts) then

                ! Increment time series index
                this%state%time_series_index = this%state%time_series_index + 1

                ! Set next update time
                this%state%time_series_update_time = this%state%time_series_update_time + this%properties%time_step_seconds

            end if

            this%output%outflow = this%state%discharges(this%state%time_series_index)

            if (this%output%outflow < 0) then
                missing_outflow_index = this%state%time_series_index

                do while (this%output%outflow < 0 .and. missing_outflow_index > 1)
                    missing_outflow_index = missing_outflow_index - 1

                    this%output%outflow = this%state%discharges(missing_outflow_index)

                end do

                if (this%output%outflow < 0) then
                    this%output%outflow = levelpool_outflow
                end if

            end if

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

#ifdef RESERVOIR_D
        ! Log diagnostic data only for development/debugging purposes
        call log_rfc_forecasts_diagnostic_data(this%properties%lake_number, this%state%current_time, &
        this%properties%time_step_seconds, this%state%time_series_update_time, this%properties%lookback_seconds, &
        this%state%time_series_index, this%properties%rfc_gage_id, this%input%inflow, this%state%water_elevation, &
        levelpool_outflow, this%output%outflow)
#endif

    end subroutine run_rfc_forecasts_reservoir

end module module_rfc_forecasts
