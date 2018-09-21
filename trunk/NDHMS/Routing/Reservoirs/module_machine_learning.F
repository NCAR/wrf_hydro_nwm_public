! This module defines and instantiates objects
! for a machine learning type reservoir. The machine
! learning reservoir struct inherits input and
! output stucts from the reservoir base
! module and calls instantiation of these into
! sub-objects. The machine learning reservoir struct
! also points to stucts for machine learning parameters
! and state and calls instantiation of these into
! sub-objects. This module also contains the
! subroutine to run machine learning release that is
! derived from the reservoir base struct interface
! to run release. Running machine learning release will
! then call the MACHINE_LEARNING subroutine, which
! processes the given inputs, parameters, and
! state for a particular machine learning reservoir and
! return the output/outflow.
module module_machine_learning

    use module_machine_learning_parameters
    use module_machine_learning_state
    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning struct from the abstract base
    ! struct for reservoirs.
    type, extends(reservoir_base_struct) :: machine_learning_struct

        ! Define pointers to sub-structs / sub-objects to and
        ! held by a machine learning reservoir object.
        type (machine_learning_parameters_interface), pointer :: parameters => null()
        type (machine_learning_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => machine_learning_struct_init
        procedure :: destroy => machine_learning_struct_destroy
        procedure :: run_release => run_machine_learning_release
    end type machine_learning_struct

contains

    !subroutine machine_learning_struct_init(this, water_elevation,  &
    !    previous_day_water_elevation, previous_day_inflow, &
    !    previous_day_release, current_storage, previous_day_storage, &
    !    min_release, max_release, min_storage, max_storage, &
    !    lake_number)



    !Machine Learning Constructor
    subroutine machine_learning_struct_init(this, start_date, lake_number)
        implicit none
        class(machine_learning_struct), intent(inout) :: this ! object being initialized
        character(len=19), intent(in) :: start_date
        integer, intent(in) :: lake_number               ! lake number

        if (this%pointer_allocation_guard .eqv. .false. ) then
            ! try to allocate input
            allocate ( this%input )
            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate machine learning input structure"
            else
                ! initialize the input structure
                call this%input%init()
            end if

            ! try to allocate output
            allocate ( this%output )
            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate machine learning output structure"
            else
                ! initialize the output structure
                call this%output%init()
            end if

            ! try to allocate parameters
            allocate ( this%parameters )
            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate machine_learning parameters structure"
            else
                ! initialize machine_learning parameters
                call this%parameters%init( lake_number )
            end if
            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )
            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate state parameters structure"
            else
                ! initialize machine_learning state
                call this%state%init( start_date, lake_number )
            end if
            this%pointer_allocation_guard = .true.
        end if

    end subroutine machine_learning_struct_init

    !Machine Learning Destructor
    subroutine machine_learning_struct_destroy(this)
        implicit none
        class(machine_learning_struct), intent(inout) :: this ! object being destroyed
    end subroutine machine_learning_struct_destroy

    ! Subroutine for running release for a machine learning reservoir,
    ! which will then call the MACHINE_LEARNING method/subroutine for processing the
    ! inputs and returning the output.
    subroutine run_machine_learning_release(reservoir_data, inflow, &
        lateral_inflow, water_elevation, outflow, routing_period)
        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: inflow               ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow       ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation      ! meters
        real, intent(out)   :: outflow              ! cubic meters per second (cms)
        real, intent(in)    :: routing_period       ! seconds

        !update_time initially set to 86400, seconds in 24 hours
        !time_interval also set to 86400
        reservoir_data%state%current_time = reservoir_data%state%current_time + routing_period

        reservoir_data%input%inflow = inflow

        reservoir_data%state%release_count = reservoir_data%state%release_count + 1

        reservoir_data%state%accumulated_inflow = reservoir_data%state%accumulated_inflow + reservoir_data%input%inflow

        reservoir_data%state%current_storage = reservoir_data%state%current_storage &
        + reservoir_data%input%inflow - reservoir_data%output%outflow

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set the current_storage to the max_storage and change the outflow accordingly.
        if (reservoir_data%state%current_storage > reservoir_data%parameters%max_storage) then
            print *, "Current storage has exceeded the max storage for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."
            !reservoir_data%state%current_storage = reservoir_data%parameters%max_storage
            !reservoir_data%output%outflow = ((reservoir_data%state%previous_day_storage - reservoir_data%state%current_storage) &
            !+ reservoir_data%input%inflow  * ( 24 * 3600 )) / ( 24 * 3600 )

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set if < reservoir_data%parameters%min_storage then current storage would equal min_storage
        ! and outflow would change accordingly.
        ! Need further discussion about accuracy of the model with using min_storage vs. zero.
        else if (reservoir_data%state%current_storage < 0.0) then
            print *, "Current storage has reached below zero for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."
            reservoir_data%state%current_storage = 0.0
            reservoir_data%output%outflow = 0.0
            !reservoir_data%state%current_storage = reservoir_data%parameters%min_storage
            !reservoir_data%output%outflow = ((reservoir_data%state%previous_day_storage - reservoir_data%state%current_storage) &
            !+ reservoir_data%input%inflow  * ( 24 * 3600 )) / ( 24 * 3600 )

        end if

        if (reservoir_data%state%current_time >= reservoir_data%state%update_time) then
            reservoir_data%state%average_inflow_for_time_interval = reservoir_data%state%accumulated_inflow / reservoir_data%state%release_count

            ! Call ML model with (reservoir_data%state%previous_day_storage, reservoir_data%state%average_inflow_for_time_interval, and reservoir_data%output%outflow passed)

            reservoir_data%state%previous_day_storage = reservoir_data%state%current_storage

            reservoir_data%state%accumulated_inflow = reservoir_data%input%inflow

            reservoir_data%state%release_count = 0

            reservoir_data%state%update_time = reservoir_data%state%update_time + reservoir_data%state%time_interval

        end if

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set the outflow to the max_release.
        if (reservoir_data%output%outflow > reservoir_data%parameters%max_release) then
            !reservoir_data%output%outflow = reservoir_data%parameters%max_release
            print *, "Current outflow has exceeded the max outflow for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set if < reservoir_data%parameters%min_release.
        ! Need further discussion about accuracy of the model with using min_release vs. zero.
        else if (reservoir_data%output%outflow < 0.0) then
            print *, "Current outflow has reached below zero for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."
            reservoir_data%output%outflow = 0.0
            !reservoir_data%output%outflow = reservoir_data%parameters%min_release
        end if

        outflow = reservoir_data%output%outflow

    end subroutine run_machine_learning_release

end module module_machine_learning
