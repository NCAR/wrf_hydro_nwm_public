! This module defines and instantiates objects
! for a hybrid persistence levelpool type
! reservoir. The hybrid reservoir struct
! inherits input and output stucts from the
! reservoir base module and calls instantiation
! of these into sub-objects. The hybrid
! reservoir struct also points to structs for
! hybrid parameters and state and calls
! instantiation of these into sub-objects.
! A pointer to a levelpool reservoir object
! is also held in state, and this module
! instantiates that levelpool object. There
! is also a subroutine to run hybrid release
! that is derived from the reservoir base
! struct interface to run release.

! FIXME: Change value for WCOS
#define log_warning 6

module module_persistence_levelpool_hybrid

    use module_persistence_levelpool_hybrid_parameters
    use module_persistence_levelpool_hybrid_state
    use module_levelpool
    use module_reservoir_base
    use module_read_timeslice_data
    implicit none


    ! Extend/derive hybrid struct from the abstract base
    ! struct for reservoirs.
    type, extends(reservoir_base_struct) :: persistence_levelpool_hybrid_struct

        ! Define pointers to sub-structs / sub-objects to and
        ! held by a level pool reservoir object.
        type (hybrid_parameters_interface), pointer :: parameters => null()
        type (hybrid_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => hybrid_struct_init
        procedure :: destroy => hybrid_struct_destroy
        procedure :: run_release => run_hybrid_release
        !These functions don't belong at this level of the interface.  I.e. a model holds a reservoir, and it won't ever need to call timestep_pre_conditions_warn
        ! I don't know of a good way to try to enforce mass balance checking in each reservoir in this sense.  Remove these and localize them to each
        ! implementation since they are really only used INTERNALLY to the run_relelase function....
        procedure :: timestep_pre_conditions_warn => no_op3
        procedure :: timestep_pre_conditions_modify => no_op4
        procedure :: timestep_post_conditions_warn => no_op3
        procedure :: timestep_post_conditions_modify => no_op3
        procedure :: update_pre_conditions_warn => no_op3
        procedure :: update_pre_conditions_modify => no_op3
        procedure :: update_post_conditions_warn => no_op3
        procedure :: update_post_conditions_modify => no_op3

    end type persistence_levelpool_hybrid_struct

contains

    ! Hybrid Constructor
    subroutine hybrid_struct_init(this, water_elevation,  &
        lake_area, weir_elevation, weir_coeffecient, &
        weir_length, orifice_elevation, orifice_coefficient, &
        orifice_area, lake_max_water_elevation, initial_fractional_depth, &
        lake_number, gage_id, persistence_parameter_file, start_date, &
        timeslice_path, observation_lookback_hours, &
        observation_update_time_interval_seconds, reservoir_gage_ids, &
        number_of_lakes)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: this ! object being initialized
        real, intent(inout) :: water_elevation           ! meters AMSL
        real, intent(in)    :: lake_area      		     ! area of lake (km^2)
        real, intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient          ! weir coefficient
        real, intent(in)    :: weir_length               ! weir length (meters)
        real, intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient       ! orifice coefficient
        real, intent(in)    :: orifice_area              ! orifice area (meters^2)
        real, intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        real, intent(in)    :: initial_fractional_depth
        integer, intent(in) :: lake_number               ! lake number
        integer, intent(in) :: gage_id
        !real, intent(in)    :: gage_discharge
        character(len=*),   intent(in)                  :: persistence_parameter_file
        character(len=19),  intent(in)                  :: start_date
        character(len=256), intent(in)                  :: timeslice_path
        integer,            intent(in)                  :: observation_lookback_hours
        integer,            intent(in)                  :: observation_update_time_interval_seconds
        integer,            intent(in), dimension(:)    :: reservoir_gage_ids
        integer,            intent(in)                  :: number_of_lakes


       call read_timeslice_data%init(start_date, &
            timeslice_path, reservoir_gage_ids, number_of_lakes)

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate level pool input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate level pool output structure"
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
                gage_id, observation_lookback_hours, observation_update_time_interval_seconds, persistence_parameter_file)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate state parameters structure"
            else
                ! initialize hybrid state
                call this%state%init(water_elevation, lake_area, lake_max_water_elevation, orifice_elevation, initial_fractional_depth)
            end if
            this%pointer_allocation_guard = .true.

            ! Allocate a single level pool reservoir
            allocate(levelpool_struct :: this%state%levelpool_ptr)

            ! Initialize level pool reservoir
            call this%state%levelpool_ptr%init(water_elevation, lake_area, &
            weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
            orifice_coefficient, orifice_area, lake_max_water_elevation, lake_number)

        end if

    end subroutine hybrid_struct_init


    !Level Pool Destructor
    subroutine hybrid_struct_destroy(this)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: this ! object being destroyed
    end subroutine hybrid_struct_destroy


    ! Subroutine for running release for a hybrid reservoir
    subroutine run_hybrid_release(reservoir_data, previous_timestep_inflow, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        integer             :: gage_lookback_seconds
        real                :: levelpool_water_elevation, levelpool_outflow
        real                :: persistence_water_elevation, persistence_outflow

print *, '--------------------------'
print *, '--------------------------'
print *, 'IN RELEASE'

print *, 'reservoir_data%parameters%lake_number'
print *, reservoir_data%parameters%lake_number


print *, 'reservoir_data%state%gage_discharge'
print *, reservoir_data%state%gage_discharge


        ! Update input variables
        reservoir_data%input%inflow = inflow
        reservoir_data%input%lateral_inflow = lateral_inflow

        ! The initialization and management of water elevation might diverge between the global state of the model
        ! and the internal state. If water elevation is refactored to no longer be a global state, then this check
        ! is not necessary. This currently allows restarts to work without the reservoir managing its own restart.
        if (reservoir_data%state%water_elevation .ne. water_elevation) then

            ! Update state variables
            reservoir_data%state%water_elevation = water_elevation

            reservoir_data%state%current_storage = (reservoir_data%state%water_elevation - reservoir_data%parameters%orifice_elevation) * &
            reservoir_data%parameters%lake_area * 1.0E6

        end if

        ! If update time to read new timeslice gage discharges. This condition is only met in retrospective runs.
        if (reservoir_data%state%current_time >= reservoir_data%state%timeslice_update_time) then

            print *, 'Timeslice update time'
            print *, 'reservoir_data%parameters%lake_number'
            print *, reservoir_data%parameters%lake_number

            ! Read new timeslice gage discharges
            call read_timeslice_data%setup_read_timeslice(reservoir_data%parameters%observation_update_time_interval_seconds, &
            reservoir_data%state%current_time, reservoir_data%parameters%gage_id, gage_lookback_seconds, reservoir_data%state%gage_discharge)

            ! If no good quality gage discharge was found in the given lookback period, the gage discharge is returned
            ! as -1.0. The persistence weight index will be set out of bounds larger than the array. This causes the
            ! the weight update logic to set the persistence weight to 0.0. Therefore, only levelpool calculations will be used.
            if (reservoir_data%state%gage_discharge < 0.0) then
                reservoir_data%state%persistence_weight_index = SIZE(reservoir_data%parameters%persistence_weighted_coefficients) + 1

                reservoir_data%state%persistence_current_weight = 0.0
            else
                ! Start persistence weight index at 1
                reservoir_data%state%persistence_weight_index = 1

                ! Grab first persistence weight at index 1 from array
                reservoir_data%state%persistence_current_weight = reservoir_data%parameters%persistence_weighted_coefficients(reservoir_data%state%persistence_weight_index)
            end if

print *, 'reservoir_data%state%persistence_weight_index'
print *, reservoir_data%state%persistence_weight_index

print *, 'reservoir_data%state%persistence_current_weight'
print *, reservoir_data%state%persistence_current_weight

print *, 'gage_lookback_seconds'
print *, gage_lookback_seconds

            ! Calculate levelpool weight
            reservoir_data%state%levelpool_current_weight = 1.0 - reservoir_data%state%persistence_current_weight

            ! Set weight update time offset by how long back a timeslice discharge was read. For instance, if the weight update interval is 24 hours, and the gage
            ! discharge was read 2 hours back, then the weight update time will be set to 22 hours after the current time.
            reservoir_data%state%weight_update_time = reservoir_data%state%current_time + reservoir_data%parameters%weight_update_time_interval - gage_lookback_seconds

            ! Set timeslice update time
            reservoir_data%state%timeslice_update_time = reservoir_data%state%timeslice_update_time + reservoir_data%parameters%observation_update_time_interval_seconds


print *, 'reservoir_data%state%weight_update_time'
print *, reservoir_data%state%weight_update_time

print *, 'reservoir_data%state%timeslice_update_time'
print *, reservoir_data%state%timeslice_update_time

        ! If update time to change persistence weights
        else if (reservoir_data%state%current_time >= reservoir_data%state%weight_update_time) then

            print *, 'WEIGHT UPDATE'

            reservoir_data%state%persistence_weight_index = reservoir_data%state%persistence_weight_index + 1

print *, 'reservoir_data%state%persistence_weight_index'
print *, reservoir_data%state%persistence_weight_index

            ! Boundary check to not exceed the size of the persistence weights array
            if (reservoir_data%state%persistence_weight_index <= SIZE(reservoir_data%parameters%persistence_weighted_coefficients)) then

                ! Grab indexed persistence weight from array
                reservoir_data%state%persistence_current_weight = reservoir_data%parameters%persistence_weighted_coefficients(reservoir_data%state%persistence_weight_index)

            else
                ! If boundary of persistence weights array has been exceeded, then set all persistence weights to 0.0
                reservoir_data%state%persistence_current_weight = 0.0

            end if


print *, 'reservoir_data%state%persistence_current_weight'
print *, reservoir_data%state%persistence_current_weight


            ! Calculate levelpool weight
            reservoir_data%state%levelpool_current_weight = 1.0 - reservoir_data%state%persistence_current_weight

            ! Set next weight update time
            reservoir_data%state%weight_update_time = reservoir_data%state%weight_update_time + reservoir_data%parameters%weight_update_time_interval

        end if

        ! Run levelpool release
        call reservoir_data%state%levelpool_ptr%run_release(previous_timestep_inflow, inflow, &
        lateral_inflow, reservoir_data%state%water_elevation, levelpool_outflow, routing_period)

print *, 'reservoir_data%state%persistence_weight_index'
print *, reservoir_data%state%persistence_weight_index

print *, 'reservoir_data%state%persistence_current_weight'
print *, reservoir_data%state%persistence_current_weight

print *, 'gage_lookback_seconds'
print *, gage_lookback_seconds


print *, 'levelpool_outflow'
print *, levelpool_outflow

        ! Set persistence outflow to gage discharge
        persistence_outflow = reservoir_data%state%gage_discharge

print *, 'persistence_outflow'
print *, persistence_outflow

        ! Calculate outflow weighted between persistence and levelpool
        reservoir_data%output%outflow = reservoir_data%state%persistence_current_weight * persistence_outflow &
        + reservoir_data%state%levelpool_current_weight * levelpool_outflow

print *, 'reservoir_data%output%outflow before boundary check'
print *, reservoir_data%output%outflow


        ! Warn if there is a negative inflow
        call warn_negative_inflow(reservoir_data%input%inflow, reservoir_data%parameters%lake_number, reservoir_data%state%current_time)

        ! Modify if exceeding storage boundary conditions
        call modify_for_projected_storage(reservoir_data%input%inflow, &
        reservoir_data%state%current_storage, reservoir_data%parameters%min_storage, &
        reservoir_data%parameters%max_storage, reservoir_data%parameters%lake_number, &
        reservoir_data%state%current_time, int(routing_period), reservoir_data%output%outflow)

        ! Update storage from the most recent model states
        reservoir_data%state%current_storage = reservoir_data%state%current_storage &
        + (reservoir_data%input%inflow - reservoir_data%output%outflow) * routing_period

print *, 'reservoir_data%state%current_storage'
print *, reservoir_data%state%current_storage

        ! Calculate new water elevation
        reservoir_data%state%water_elevation = (reservoir_data%state%current_storage / (reservoir_data%parameters%lake_area * 1.0E6)) &
        + reservoir_data%parameters%orifice_elevation

print *, 'reservoir_data%state%water_elevation'
print *, reservoir_data%state%water_elevation

        ! Update output variable returned from this subroutine
        outflow = reservoir_data%output%outflow

print *, 'outflow'
print *, outflow
print *, '--------------------------'
print *, '--------------------------'


        ! Set current inflow to previous_timestep_inflow
        reservoir_data%input%previous_timestep_inflow = inflow

        ! Update water_elevation variable returned from this subroutine
        water_elevation = reservoir_data%state%water_elevation

        ! Update the current time
        reservoir_data%state%current_time = reservoir_data%state%current_time + int(routing_period)


!ADD LOG FOR WEIGHT UPDATE TIMES
!!!!!!!!!

        !TEMP
        if (reservoir_data%parameters%lake_number == 10361596) then

            !print *, 'WRITE PERSISTENCE VALUES'

            open (113,file="current_inflow.txt",status="unknown", access = 'append')
             Write (113, "(F15.5)") reservoir_data%input%inflow

                        close (113)

            open (117,file="current_storage.txt",status="unknown", access = 'append')
             Write (117, "(F15.5)") reservoir_data%state%current_storage

                        close (117)

            open (119,file="current_outflow.txt",status="unknown", access = 'append')
             Write (119, "(F15.5)") reservoir_data%output%outflow

                        close (119)

        end if


    end subroutine run_hybrid_release


    subroutine no_op3(reservoir_data)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: reservoir_data
    end subroutine no_op3


    subroutine no_op4(reservoir_data, routing_period)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: reservoir_data
        real, intent(in)       :: routing_period
    end subroutine no_op4


end module module_persistence_levelpool_hybrid
