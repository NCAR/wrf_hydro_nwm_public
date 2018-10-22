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

#define days_in_year 365
#define days_in_leap_year 366
#define pi 3.14159265359

module module_machine_learning

    use module_machine_learning_parameters
    use module_machine_learning_state
    use module_reservoir_base
    use module_machine_learning_model
    use module_machine_learning_tests

    implicit none

    !type :: machine_learning_model_container
    !    class (machine_learning_model_struct), pointer :: ptr
    !end type

    !class (machine_learning_model_container), allocatable, dimension(:) :: machine_learning_models
    !integer, allocatable, dimension(:) :: machine_learning_model_ids

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

    !Machine Learning Constructor
    !subroutine machine_learning_struct_init(this, start_date, model_id, number_of_unique_models, &
    !lake_number)
    subroutine machine_learning_struct_init(this, start_date, lake_number)

        implicit none
        class(machine_learning_struct), intent(inout) :: this ! object being initialized
        character(len=19), intent(in) :: start_date
        !integer, intent(in) :: model_id
        !integer, intent(in) :: number_of_unique_models
        integer, intent(in) :: lake_number               ! lake number
                                                         ! TODO: potentially change to lake_id for this and
                                                         ! LP and Pers modules. lake_number is used in LP
                                                         ! which has been submitted to a PR.
        !integer :: model_counter, model_index

        integer :: model_counter, model_index, model_id

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




            if ( .not. allocated(machine_learning_models) ) then
                number_of_unique_models = 0
                ! get_unique_number_of_models = .true.
            end if
            !else
            !    get_unique_number_of_models = .false.


            ! try to allocate parameters
            allocate ( this%parameters )

            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate machine_learning parameters structure"

            else

                ! initialize machine_learning parameters
                !call this%parameters%init( model_id, model_index, lake_number, number_of_unique_models )
                call this%parameters%init( lake_number, number_of_unique_models )
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


        ! Model Initialization
        ! This if statement is only entered on first machine learning reservoir
        if ( .not. allocated(machine_learning_models) ) then

            ! Allocates array of pointers to machine learning models.
            allocate(machine_learning_models(number_of_unique_models))

            ! Allocates array of machine learning model numbers that match
            ! the indices of the machine learning models array.
            allocate(machine_learning_model_ids(number_of_unique_models))

            machine_learning_model_ids = 0 ! set model number array values to 0

            do model_counter = 1, number_of_unique_models
                nullify(machine_learning_models(model_counter)%ptr)
            end do

        end if

        this%parameters%model_index = 0
        do model_counter = 1, number_of_unique_models


                ! If the model has already been initialized, and the passed model number
                ! matches the model number in the array, then assign that slot to
                ! model_index
                if (machine_learning_model_ids(model_counter) == this%parameters%model_id) then
                        this%parameters%model_index = model_counter
                        exit

                ! Check if any slot is zero, which means a machine learning model has
                ! not been initialized or assigned to that slot.
                else if (machine_learning_model_ids(model_counter) == 0) then

                        ! Assign the model id to the given slot.
                        machine_learning_model_ids(model_counter) = this%parameters%model_id
                        this%parameters%model_index = model_counter

                        !machine_learning_models(model_counter) !??????????

                        allocate(machine_learning_model_struct :: &
                        machine_learning_models(model_counter)%ptr)  ! should this be ml_models instead????
                        !machine_learning_model_ids(model_counter)%ptr  ! should this be ml_models instead????

                        !select type (ml => machine_learning_model_ids(model_counter)%ptr)
                        !select type (ml => machine_learning_models(model_counter)%ptr)

                        ! Calls initialization of a particular Machine Learning model.
                        !call ml%machine_learning_model_init(this%parameters%model_id)

                        !call machine_learning_models(model_counter)%ptr%machine_learning_model_init(this%parameters%model_id)
                        call machine_learning_models(model_counter)%ptr%init(this%parameters%model_id)

                        exit

                end if

        end do

    end subroutine machine_learning_struct_init


    !Machine Learning Destructor
    subroutine machine_learning_struct_destroy(this)
        implicit none

        class(machine_learning_struct), intent(inout) :: this ! object being destroyed

    end subroutine machine_learning_struct_destroy



    ! Pre-Warn Subroutine
    subroutine pre_warn(inflow, current_time, lake_number)
        implicit none

        real,    intent(in) :: inflow
        integer, intent(in) :: current_time
        integer, intent(in) :: lake_number

        ! FIXME: Change 6 to macro for WCOS
        write(6,*) "WARNING: Current inflow ", inflow, &
        " cms, has reached below zero for reservoir ", &
        lake_number, " at ", current_time, " seconds after model start time."

    end subroutine pre_warn


    ! Subroutine for running release for a machine learning reservoir,
    ! which will then call the MACHINE_LEARNING method/subroutine for processing the
    ! inputs and returning the output.
    subroutine run_machine_learning_release(reservoir_data, previous_timestep_inflow, &
        inflow, lateral_inflow, water_elevation, outflow, routing_period)

        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        integer             :: boundary_passed
        logical             :: leap_year
        logical             :: okay_run_machine_learning_model
        real                :: pre_modify_storage

        !update_time initially set to 86400, seconds in 24 hours
        !time_interval also set to 86400
        reservoir_data%state%current_time = reservoir_data%state%current_time + routing_period

        reservoir_data%state%release_count = reservoir_data%state%release_count + 1

        ! Timestep pre warn to check inflow
        call timestep_pre_conditions_warn()

        reservoir_data%input%inflow = inflow

        reservoir_data%state%accumulated_inflow = reservoir_data%state%accumulated_inflow + reservoir_data%input%inflow

        reservoir_data%state%current_storage = reservoir_data%state%current_storage &
        + reservoir_data%input%inflow - reservoir_data%output%outflow

        reservoir_data%state%totaled_storage = reservoir_data%state%totaled_storage + reservoir_data%state%current_storage

        okay_run_machine_learning_model = .true.

        ! Timestep pre modify storage
        call timestep_pre_conditions_modify()

        ! If update time, then run machine learning model to calculate new outflow value
        if (reservoir_data%state%current_time >= reservoir_data%state%update_time) then
            reservoir_data%state%average_inflow_over_past_time_intervals = reservoir_data%state%accumulated_inflow &
            / reservoir_data%state%release_count

            ! Check leap year and update day x and y
            call update_pre_conditions_modify()


            if (okay_run_machine_learning_model) then

                ! TODO: Put outputs from a subroutine/function at the end of the list consistently across all modules in NWM
                ! TODO: Make outputs from function in consistent location in the call order across all modules
                call machine_learning_models(reservoir_data%parameters%model_index)%ptr%run_model &
                    (reservoir_data%state%average_inflow_over_past_time_intervals, reservoir_data%output%outflow, &
                    reservoir_data%state%previous_day_release, reservoir_data%state%average_storage_over_past_time_intervals, &
                    reservoir_data%state%x_day_of_year, reservoir_data%state%y_day_of_year)
            end if

            !reservoir_data%state%average_storage_over_past_time_intervals = reservoir_data%state%current_storage

            reservoir_data%state%average_storage_over_past_time_intervals = reservoir_data%state%totaled_storage / reservoir_data%state%release_count

            reservoir_data%state%totaled_storage = 0.0

            reservoir_data%state%accumulated_inflow = reservoir_data%input%inflow

            reservoir_data%state%previous_day_release = reservoir_data%output%outflow

            reservoir_data%state%release_count = 0

            reservoir_data%state%update_time = reservoir_data%state%update_time + reservoir_data%state%time_interval

        end if

        !NEED TO COMPLETE THIS CALL
        ! Timestep post modify outflow
        call update_post_conditions_modify()

        outflow = reservoir_data%output%outflow

    end subroutine run_machine_learning_release



    function machine_learning_data_info(machine_learning_data) result(rv)
        implicit none
        type (machine_learning_struct) :: machine_learning_data
        integer :: rv
        logical, dimension(1) :: ptr_state
        logical, dimension(1) :: data_state

        rv = 0

        ! Check to see if the machine_learning_state data structure exists
        print *, "Checking pointer association on data%state "
        if ( associated(machine_learning_data%state) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
        else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_parameters data structure exists
        print *, "Checking pointer association on data%parameters "
        if ( associated(machine_learning_data%parameters) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_input data structure exists
        print *, "Checking pointer association on data%input "
        if ( associated(machine_learning_data%input) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_output data structure exists
        print *, "Checking pointer association on data%output "
        if ( associated(machine_learning_data%output) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "



        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_machine_learning_state(machine_learning_data%state)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_machine_learning_parameters(machine_learning_data%parameters)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_input(machine_learning_data%input)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_output(machine_learning_data%output)
        end if


        if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Tests Passed"
            print *, "========================================================================"
        end if
        rv = 1

        flush(6)
    end function machine_learning_data_info


    function machine_learning_model_data_info(machine_learning_model_data) result(rv)
        implicit none
        type (machine_learning_model_struct) :: machine_learning_model_data
        integer :: rv
        logical, dimension(1) :: ptr_state
        logical, dimension(1) :: data_state

        rv = 0

        ! Check to see if the machine_learning_model data structure exists
        print *, "Checking pointer association on model data "
        !if ( associated(machine_learning_model_data) ) then
        !    print *, "PASSED"
        !    ptr_state(1) = .true.
        !else
        !    print *, "FAILED"
        !    ptr_state(1) = .false.
        !end if
        print *, " "


        data_state(1) = test_machine_learning_model(machine_learning_model_data)


        if ( all(data_state) ) then
        !if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Model Tests Passed"
            print *, "========================================================================"
        end if
        rv = 1

        flush(6)
    end function machine_learning_model_data_info

end module module_machine_learning



logical function leap_year(current_year)
    integer, intent(in) :: current_year

    if (((mod(current_year,4)==0 .and. &
        mod(current_year,100)/=0) .or. &
        mod(current_year,400)==0)) then
        leap_year = .TRUE.

    else
        leap_year = .FALSE.

    end if

end function leap_year
