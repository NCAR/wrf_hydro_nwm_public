module module_reservoir_base


    type, abstract :: reservoir_base_state_struct


    end type


    type, abstract :: reservoir_base_properties_struct


    end type




    type, abstract :: reservoir_base_struct


    end type



    type :: reservoir_base_input_struct


        real :: inflow

        contains

        procedure :: init => reservoir_base_input_struct_init
        procedure :: destroy => reservoir_base_input_struct_destroy

    end type



    contains

    !constructor for the reservoir input struct
    subroutine reservoir_base_input_struct_init(this)      !I don't think I need to pass any parameters here??
	implicit none
    	class (reservoir_base_input_struct), intent(inout) :: this ! object being initialized

        this%inflow = 0.0


    end subroutine reservoir_base_input_struct_init



    !destructor for the reservoir input struct
    subroutine reservoir_base_input_struct_destroy(this)
        implicit none
        class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed


    end subroutine reservoir_base_input_struct_destroy





end module module_reservoir_base
