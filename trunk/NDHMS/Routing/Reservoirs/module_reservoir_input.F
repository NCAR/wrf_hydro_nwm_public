module module_reservoir_input
    use module_reservoir_routing_base
    implicit none


    type, extends(reservoir_base_input_struct) :: reservoir_input_struct



	contains


	procedure :: init => reservoir_input_struct_init
	procedure :: destroy => reservoir_input_struct_destroy


    end type reservoir_input_struct



    !Does Don intend to get rid of his gw input and output modules because he put those subroutines in the base class??


end module module_reservoir_input
