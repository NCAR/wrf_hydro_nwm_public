module module_reservoir_input
    use module_reservoir_base
    implicit none


    type, extends(reservoir_base_input_struct) :: reservoir_input_struct

        real, allocatable :: inflow


        contains


        procedure :: init => reservoir_input_struct_init
        procedure :: destroy => reservoir_input_struct_destroy


    end type reservoir_input_struct


    contains


    !constructor for the reservoir input struct
    subroutine reservoir_input_struct_init(this)      !I don't think I need to pass any parameters here??
	implicit none
    	class (reservoir_input_struct), intent(inout) :: this ! object being initialized


        logical :: allocation_error = .false.


        ! allocate reservoir_inflow
        if ( .not. allocated(this%inflow) ) then
            allocate(this%inflow())             !Is this correct???
            this%inflow = 0.0
        else
            allocation_error = .true.
        end if


    end subroutine reservoir_input_struct_init




    ! destructor for the reservoir input struct
    subroutine reservoir_input_struct_destroy(this)
        implicit none
        class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed

        logical :: allocation_error = .false.

        ! deallocate reservoir_inflow
        if (  allocated(this%inflow) ) then
            deallocate(this%inflow)
        else
            allocation_error = .true.
        end if


    end subroutine reservoir_input_struct_destroy


end module module_reservoir_input
