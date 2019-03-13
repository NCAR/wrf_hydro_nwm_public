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
        orifice_area, lake_max_water_elevation, lake_number, gage_id, &
        start_date, timeslice_path, reservoir_gage_ids, number_of_lakes)
        implicit none
        class(persistence_levelpool_hybrid_struct), intent(inout) :: this ! object being initialized
        real, intent(inout) :: water_elevation           ! meters AMSL
        real, intent(in)    :: lake_area      		 ! area of lake (km^2)
        real, intent(in)    :: weir_elevation            ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient          ! weir coefficient
        real, intent(in)    :: weir_length               ! weir length (meters)
        real, intent(in)    :: orifice_elevation         ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient       ! orifice coefficient
        real, intent(in)    :: orifice_area              ! orifice area (meters^2)
        real, intent(in)    :: lake_max_water_elevation  ! max water elevation (meters)
        integer, intent(in) :: lake_number               ! lake number
        integer, intent(in) :: gage_id
        !real, intent(in)    :: gage_discharge
        character(len=19),  intent(in)                  :: start_date
        character(len=256), intent(in)                  :: timeslice_path
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
                call this%parameters%init(lake_area, lake_max_water_elevation, orifice_elevation, lake_number, gage_id)
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate state parameters structure"
            else
                ! initialize hybrid state
                call this%state%init( water_elevation, lake_area )
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

        real :: gage_discharge
        real :: levelpool_water_elevation, levelpool_outflow
        real :: persistence_water_elevation, persistence_outflow

        ! If first call to release
        if (reservoir_data%state%current_time == 0) then
            reservoir_data%state%timeslice_update_time = int(routing_period)
        end if

        ! Update the current time
        reservoir_data%state%current_time = reservoir_data%state%current_time + int(routing_period)


        if (reservoir_data%state%current_time >= reservoir_data%state%timeslice_update_time) then

print *, 'reservoir_data%parameters%lake_number'
print *, reservoir_data%parameters%lake_number

            call read_timeslice_data%setup_read_timeslice(reservoir_data%state%timeslice_update_time_interval, reservoir_data%state%current_time, &
            reservoir_data%parameters%gage_id, gage_discharge)


print *, 'reservoir_data%parameters%lake_number'
print *, reservoir_data%parameters%lake_number

            print *, 'gage_discharge'
            print *, gage_discharge


            !after update
            if (gage_discharge == -1.0) then
                !Set all P weights to 0
                print *, 'p'
            end if


            reservoir_data%state%timeslice_update_time = reservoir_data%state%timeslice_update_time + reservoir_data%state%timeslice_update_time_interval

            print *, 'reservoir_data%state%timeslice_update_time'
            print *, reservoir_data%state%timeslice_update_time

            print *, 'reservoir_data%state%timeslice_update_time_interval'
            print *, reservoir_data%state%timeslice_update_time_interval


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
