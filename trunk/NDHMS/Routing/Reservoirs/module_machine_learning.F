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
        outflow, routing_period)
        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: inflow               ! cubic meters per second (cms)
        real, intent(out)   :: outflow              ! cubic meters per second (cms)
        real, intent(in)    :: routing_period       ! seconds






        reservoir_data%input%inflow = inflow

        reservoir_data%state%time_count = reservoir_data%state%time_count + 1

        reservoir_data%state%average_inflow = reservoir_data%state%average_inflow + reservoir_data%input%inflow

        reservoir_data%state%accumulated_storage = reservoir_data%input%inflow -




















    !    ! Update input variables
    !    reservoir_data%input%inflow = inflow
    !    reservoir_data%input%lateral_inflow = lateral_inflow

    !    ! Update state variables
    !    reservoir_data%state%water_elevation = water_elevation

    !    call MACHINE_LEARNING(reservoir_data%parameters%lake_number,   &
    !    reservoir_data%input%previous_timestep_inflow,          &
    !    reservoir_data%input%inflow,                            &
    !    reservoir_data%output%outflow,                          &
    !    reservoir_data%input%lateral_inflow,                    &
    !    routing_period,                                         &
    !    reservoir_data%state%water_elevation,                   &
    !    reservoir_data%parameters%reservoir_area,               &
    !    reservoir_data%parameters%weir_elevation,               &
    !    reservoir_data%parameters%max_depth,                    &
    !    reservoir_data%parameters%weir_coeffecient,             &
    !    reservoir_data%parameters%weir_length,                  &
    !    reservoir_data%parameters%orifice_elevation,            &
    !    reservoir_data%parameters%orifice_coefficient,           &
    !    reservoir_data%parameters%orifice_area                  &
    !    )

    !    ! Update output variable returned from this subroutine
    !    outflow = reservoir_data%output%outflow

    !    ! Set current inflow to previous_timestep_inflow
    !    reservoir_data%input%previous_timestep_inflow = inflow

    end subroutine run_machine_learning_release

end module module_machine_learning
