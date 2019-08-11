! This module defines and instantiates objects
! for a hybrid persistence levelpool type
! reservoir. The hybrid reservoir type
! inherits input and output stucts from the
! reservoir base module and calls instantiation
! of these into sub-objects. The hybrid
! reservoir type also points to types for
! hybrid parameters and state and calls
! instantiation of these into sub-objects.
! A pointer to a levelpool reservoir object
! is also held in state, and this module
! instantiates that levelpool object. There
! is also a subroutine to run hybrid reservoir
! that is derived from the reservoir base
! type interface to run reservoir.

module module_persistence_levelpool_hybrid

    use module_persistence_levelpool_hybrid_parameters, only: hybrid_parameters_interface
    use module_persistence_levelpool_hybrid_state, only: hybrid_state_interface
    use module_levelpool, only: levelpool_type
    use module_reservoir_utilities, only: modify_for_projected_storage, warn_negative_inflow, &
                                          create_hybrid_diagnostic_log_file, &
                                          log_hybrid_diagnostic_data
    use module_reservoir_base
    use module_reservoir_read_timeslice_data, only: timeslice_data
    implicit none

    ! Extend/derive hybrid type from the abstract base
    ! type for reservoirs.
    type, extends(reservoir_base_type) :: persistence_levelpool_hybrid_type

        ! Define pointers to sub-types / sub-objects to and
        ! held by a level pool reservoir object.
        type (hybrid_parameters_interface), pointer :: parameters => null()
        type (hybrid_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => hybrid_type_init
        procedure :: destroy => hybrid_type_destroy
        procedure :: run => run_hybrid_reservoir

    end type persistence_levelpool_hybrid_type

#ifndef NCEP_WCOSS
    integer, parameter :: log_warning = 6
#else
    integer, parameter :: log_warning = 78
#endif

contains

    ! Hybrid Constructor
    subroutine hybrid_type_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number, persistence_parameter_file, start_date, &
        timeslice_path, observation_lookback_hours, &
        observation_update_time_interval_seconds)
        implicit none
        class(persistence_levelpool_hybrid_type), intent(inout) :: this ! object being initialized
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
        character(len=*),   intent(in) :: persistence_parameter_file
        character(len=19),  intent(in) :: start_date
        character(len=256), intent(in) :: timeslice_path
        integer,            intent(in) :: observation_lookback_hours
        integer,            intent(in) :: observation_update_time_interval_seconds
        character(len=15)              :: lake_number_string

        ! Create diagnostic log file
        ! Uncomment this call only for development/debugging purposes
        !call create_hybrid_diagnostic_log_file(lake_number)

        ! Call to initialize read timeslice data object. This object is a singleton,
        ! so if another reservoir has already initialized the object, it will return
        ! immediately.
        call timeslice_data%init(start_date, &
            timeslice_path, persistence_parameter_file, observation_lookback_hours)

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate hybrid input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate hybrid output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate parameters
            allocate ( this%parameters )
            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate hybrid parameters structure"
            else
                ! initialize hybrid parameters
                call this%parameters%init(lake_area, lake_max_water_elevation, orifice_elevation, lake_number, &
                observation_lookback_hours, observation_update_time_interval_seconds, persistence_parameter_file)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate hybrid state structure"
            else
                ! initialize hybrid state
                call this%state%init(water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, initial_fractional_depth)
            end if
            this%pointer_allocation_guard = .true.

            ! Initialize persistence weight index to be out of bounds of the persistence weights array
            this%state%persistence_weight_index = SIZE(this%parameters%persistence_weighted_coefficients) + 1

            ! Call hydro stop if the initial storage is greater than max storage or less than zero.
            if (this%state%current_storage > this%parameters%max_storage) then
                this%state%current_storage = this%parameters%max_storage
                write(lake_number_string, "(I15)") lake_number
                write(log_warning,*) "Warning, initial storage exceeds max storage for reservoir ", lake_number_string, &
                ". Setting initial storage to max storage."

            else if (this%state%current_storage < this%parameters%min_storage) then
                this%state%current_storage = this%parameters%min_storage
                write(lake_number_string, "(I15)") lake_number
                write(log_warning,*) "Warning, initial storage is less than zero for reservoir ", lake_number_string, &
                ". Setting initial storage to zero."

            end if

            ! Allocate a single level pool reservoir
            allocate(levelpool_type :: this%state%levelpool_ptr)

            ! Initialize level pool reservoir
            call this%state%levelpool_ptr%init(water_elevation, lake_area, &
            weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
            orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        end if

    end subroutine hybrid_type_init


    ! Hybrid Destructor
    subroutine hybrid_type_destroy(this)
        implicit none
        class(persistence_levelpool_hybrid_type), intent(inout) :: this ! object being destroyed
    end subroutine hybrid_type_destroy


    ! Subroutine for running reservoir for a hybrid reservoir
    subroutine run_hybrid_reservoir(this, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(persistence_levelpool_hybrid_type), intent(inout) :: this
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        real                :: delta_storage            ! timestep change in storage (cubic meters)
        real                :: local_water_elevation    ! water elevation passed to levelpool (meters AMSL)
        real                :: levelpool_outflow        ! cubic meters per second (cms)
        logical             :: max_storage_reached      ! flag for when max storage is reached
        integer             :: gage_lookback_seconds    ! seconds between current model
                                                        ! time and the time a gage
                                                        ! discharge is read

        max_storage_reached = .false.
        gage_lookback_seconds = 0

        ! Update input variables
        this%input%inflow = inflow
        this%input%lateral_inflow = lateral_inflow

        ! The initialization and management of water elevation might diverge between the global state of the model
        ! and the internal state. If water elevation is refactored to no longer be a global state, then this check
        ! is not necessary. This currently allows restarts to work without the reservoir managing its own restart.
        if (this%state%water_elevation .ne. water_elevation) then

            ! Update state variables
            this%state%water_elevation = water_elevation

            this%state%current_storage = (this%state%water_elevation - this%parameters%orifice_elevation) * &
            this%parameters%lake_area

        end if

        local_water_elevation = this%state%water_elevation

        ! If update time to read new timeslice gage discharges. In forecast modes, the update time automatically is set to 1,000,000
        ! to ensure that gage discharges are only read at the first timestep. For analysis runs, a timeslice update time is set as a
        ! namelist parameter. A typical update time for an analysis run would be 3600 seconds (1 hour), at which time new observations would
        ! be retrieved. The first reservoir for each processor to reach an update time will call the function to read a new timeslice file,
        ! and these timeslice discharges will be held in memory for the subsequent reservoirs to use at that timestep.
        if (this%state%current_time >= this%state%timeslice_update_time) then

            ! Read new timeslice gage discharges
            call timeslice_data%setup_read_timeslice(this%parameters%observation_update_time_interval_seconds, &
            this%state%current_time, this%parameters%gage_id, gage_lookback_seconds, this%state%gage_discharge)

            ! If no good quality gage discharge was found in the given lookback period, the gage discharge is returned
            ! as -1.0. The persistence weight index will be set out of bounds larger than the array. This causes the
            ! the weight update logic to set the persistence weight to 0.0. Therefore, only levelpool calculations will be used.
            if (this%state%gage_discharge < 0.0) then

                ! If at weight update time, then increment persistence weight index. Otherwise, continue using previous persistence weight index
                if (this%state%current_time >= this%state%weight_update_time) then
                    ! Increment weight index
                    this%state%persistence_weight_index = this%state%persistence_weight_index + 1

                    ! Set next weight update time
                    this%state%weight_update_time = this%state%weight_update_time + this%parameters%weight_update_time_interval
                end if

                ! If out of bounds of array, then set persistence weight to 0.0. Otherwise, set persistence weight from array's indexed value.
                if (this%state%persistence_weight_index > SIZE(this%parameters%persistence_weighted_coefficients)) then
                    this%state%persistence_current_weight = 0.0

                else
                    this%state%persistence_current_weight = this%parameters%persistence_weighted_coefficients(this%state%persistence_weight_index)
                end if

            else

                ! Set persisted outflow to gage discharge
                this%state%persisted_outflow = this%state%gage_discharge

                ! Start persistence weight index at 1
                this%state%persistence_weight_index = 1

                ! Grab first persistence weight at index 1 from array
                this%state%persistence_current_weight = this%parameters%persistence_weighted_coefficients(this%state%persistence_weight_index)

                ! Set weight update time offset by how long back a timeslice discharge was read. For instance, if the weight update interval is 24 hours, and the gage
                ! discharge was read 2 hours back, then the weight update time will be set to 22 hours after the current time.
                this%state%weight_update_time = this%state%current_time + this%parameters%weight_update_time_interval - gage_lookback_seconds

            end if

            ! Calculate levelpool weight
            this%state%levelpool_current_weight = 1.0 - this%state%persistence_current_weight

            ! Set timeslice update time
            this%state%timeslice_update_time = this%state%timeslice_update_time + this%parameters%observation_update_time_interval_seconds

        ! If update time to change persistence weights
        else if (this%state%current_time >= this%state%weight_update_time) then

            ! Increment weight index
            this%state%persistence_weight_index = this%state%persistence_weight_index + 1

            ! Boundary check to not exceed the size of the persistence weights array
            if (this%state%persistence_weight_index <= SIZE(this%parameters%persistence_weighted_coefficients)) then

                ! Grab indexed persistence weight from array
                this%state%persistence_current_weight = this%parameters%persistence_weighted_coefficients(this%state%persistence_weight_index)

            else
                ! If boundary of persistence weights array has been exceeded, then set all persistence weights to 0.0
                this%state%persistence_current_weight = 0.0

            end if

            ! Calculate levelpool weight
            this%state%levelpool_current_weight = 1.0 - this%state%persistence_current_weight

            ! Set next weight update time
            this%state%weight_update_time = this%state%weight_update_time + this%parameters%weight_update_time_interval

        end if

        ! Run levelpool reservoir
        call this%state%levelpool_ptr%run(previous_timestep_inflow, inflow, &
        lateral_inflow, local_water_elevation, levelpool_outflow, routing_period)

        ! Calculate outflow weighted between persistence and levelpool
        this%output%outflow = this%state%persistence_current_weight &
        * this%state%persisted_outflow + this%state%levelpool_current_weight &
        * levelpool_outflow

        ! Warn if there is a negative inflow
        call warn_negative_inflow(this%input%inflow, this%parameters%lake_number, this%state%current_time)

        ! Modify if exceeding storage boundary conditions
        call modify_for_projected_storage(this%input%inflow, &
        this%state%current_storage, this%parameters%min_storage, &
        this%parameters%max_storage, this%parameters%lake_number, &
        this%state%current_time, int(routing_period), this%output%outflow, max_storage_reached)

        ! If max storage has been reached, then set storage and water elevation to max
        if (max_storage_reached) then
            this%state%current_storage = this%parameters%max_storage

            this%state%water_elevation = this%state%levelpool_ptr%parameters%max_depth

        else
            ! Calculate change in storage
            delta_storage = (this%input%inflow - this%output%outflow) * routing_period

            ! Update storage from the most recent model states
            this%state%current_storage = this%state%current_storage + delta_storage

            ! Calculate new water elevation. Delta storage is used as opposed to calculating from current storage in
            ! order to minimize floating point error because current storage is a much larger magnitude than water elevation.
            this%state%water_elevation = this%state%water_elevation + delta_storage / this%parameters%lake_area

            ! In theory this shouldn't happen, but due to floating point arithmetic, this is double checked to prevent
            ! propagation of error.
            if (this%state%water_elevation > this%state%levelpool_ptr%parameters%max_depth) then
                this%state%water_elevation = this%state%levelpool_ptr%parameters%max_depth
            end if

        end if

        ! Update output variable returned from this subroutine
        outflow = this%output%outflow

        ! Set current inflow to previous_timestep_inflow
        this%input%previous_timestep_inflow = inflow

        ! Update water_elevation variable returned from this subroutine
        water_elevation = this%state%water_elevation

        ! Update the current time
        this%state%current_time = this%state%current_time + int(routing_period)

        ! Log diagnostic data
        ! Uncomment this call only for development/debugging purposes
        !call log_hybrid_diagnostic_data(this%parameters%lake_number, this%state%current_time, &
        !this%state%timeslice_update_time, this%state%weight_update_time, gage_lookback_seconds, &
        !this%parameters%gage_id, this%state%persistence_weight_index, &
        !this%state%persistence_current_weight, this%input%inflow, this%state%current_storage, &
        !this%state%water_elevation, this%state%gage_discharge, this%state%persisted_outflow, &
        !levelpool_outflow, this%output%outflow)

    end subroutine run_hybrid_reservoir

end module module_persistence_levelpool_hybrid
