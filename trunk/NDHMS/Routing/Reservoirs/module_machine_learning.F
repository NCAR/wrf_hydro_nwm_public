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

!#define days_in_year 365
!#define days_in_leap_year 366
!#define pi 3.14159265359

module module_machine_learning

    use module_machine_learning_parameters
    use module_machine_learning_state
    use module_reservoir_base
    !use module_machine_learning_model

    implicit none

    !type :: machine_learning_model_container
    !    class (machine_learning_model_struct), pointer :: ptr
    !end type

    !class (machine_learning_model_container), allocatable, dimension(:) :: machine_learning_models
    !integer, allocatable, dimension(:) :: machine_learning_model_numbers

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
    subroutine machine_learning_struct_init(this, start_date, model_number, number_of_unique_models, &
				lake_number)

        implicit none
        class(machine_learning_struct), intent(inout) :: this ! object being initialized
        character(len=19), intent(in) :: start_date
        integer, intent(in) :: model_number
        integer, intent(in) :: number_of_unique_models
        integer, intent(in) :: lake_number               ! lake number
							 ! TODO: potentially change to lake_id for this and
							 ! LP and Pers modules. lake_number is used in LP
							 ! which has been submitted to a PR.
        integer :: model_counter, model_index


	! Model Initialization
	! This if statement is only entered on first machine learning reservoir
!        if ( .not. allocated(machine_learning_models) ) then

       	    ! Allocates array of pointers to machine learning models.
!            allocate(machine_learning_models(number_of_unique_models))

	    ! Allocates array of machine learning model numbers that match
	    ! the indices of the machine learning models array.
!            allocate(machine_learning_model_numbers(number_of_unique_models))

!	    machine_learning_model_numbers = 0	  ! set model number array values to 0

!	    do model_counter = 1, number_of_unique_models
!	       nullify(machine_learning_models(model_counter)%ptr)

!	    end do

!       end if

!		model_index = 0
!		do model_counter = 1, number_of_unique_models

			! If the model has already been initialized, and the passed model number
			! matches the model number in the array, then assign that slot to
			! model_index
!			if (machine_learning_model_numbers(model_counter) == model_number) then
!				model_index = model_counter
!				exit

			! Check if any slot is zero, which means a machine learning model has
			! not been initialized or assigned to that slot.
!			else if (machine_learning_model_numbers(model_counter) == 0) then
!				machine_learning_model_numbers(model_counter) = model_number
!				model_index = model_counter

!				machine_learning_models(model_counter)

!				allocate(machine_learning_model_struct :: &
!				machine_learning_model_numbers(model_counter)%ptr

!				select type (ml => machine_learning_model_numbers(model_counter)%ptr)

!				call ml%machine_learning_model_init(model_number)

!				exit

!			end if

!		end do


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
                call this%parameters%init( model_number, model_index, lake_number )

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
		integer				:: boundary_passed
		logical				:: leap_year

    end subroutine run_machine_learning_release



end module module_machine_learning


!logical function check_leap_year(current_year)
!	real current_year
!
!	if (((mod(current_year,4)==0 .and. &
!	   mod(current_year,100)/=0) .or. &
!	   mod(current_year,400)==0)) then
!	   check_leap_year = .TRUE.

!	else
!	   check_leap_year = .FALSE.

!end function check_leap_year
