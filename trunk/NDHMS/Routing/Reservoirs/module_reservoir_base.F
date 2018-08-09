module module_reservoir_base






    type, abstract :: reservoir_base_state_struct


    end type


    type, abstract :: reservoir_base_properties_struct


    end type




    type, abstract :: reservoir_base_struct


    end type




    !real :: i = 1.0

    type :: reservoir_base_input_struct

        !real, allocatable, dimension(:_) :: inflow    !We don't do this b/c not an array, just a single var

        real :: inflow

        contains

        procedure :: init => reservoir_base_input_struct_init
        !procedure :: destroy => reservoir_base_input_struct_destroy

    end type



    contains

    !constructor for the reservoir input struct
    subroutine reservoir_base_input_struct_init(this)      !I don't think I need to pass any parameters here??
	implicit none
    	class (reservoir_base_input_struct), intent(inout) :: this ! object being initialized

        this%inflow = 0.0


        !logical :: allocation_error = .false.

        ! allocate reservoir_inflow
        !if ( .not. allocated(this%inflow) ) then
        !    allocate(this%inflow())             !Is this correct???
        !    this%inflow = 0.0
        !else
        !    allocation_error = .true.
        !end if


    end subroutine reservoir_base_input_struct_init



    ! destructor for the reservoir input struct
    !subroutine reservoir_base_input_struct_destroy(this)
    !    implicit none
    !    class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed


    !    deallocate(this%inflow)

    !    !logical :: allocation_error = .false.

    !    ! deallocate reservoir_inflow
    !    !if (  allocated(this%inflow) ) then
    !    !    deallocate(this%inflow)
    !    !else
    !    !    allocation_error = .true.
    !    !end if


    !end subroutine reservoir_base_input_struct_destroy





end module module_reservoir_base
