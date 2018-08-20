module module_reservoir_base


    type, abstract :: reservoir_base_state_struct


    end type


    type, abstract :: reservoir_base_properties_struct


    end type



    type :: reservoir_base_input_struct

        real :: inflow
        real :: lateral_inflow
        real :: previous_day_inflow
        real :: inflow_previous_timestep

        contains

        procedure :: init => reservoir_base_input_struct_init
        procedure :: destroy => reservoir_base_input_struct_destroy

    end type



    type :: reservoir_base_output_struct


        real :: outflow

        contains

        procedure :: init => reservoir_base_output_struct_init
        procedure :: destroy => reservoir_base_output_struct_destroy

    end type



    type, abstract :: reservoir_base_struct

        type(reservoir_base_input_struct), pointer :: input
        type(reservoir_base_output_struct), pointer :: output


        contains

        procedure (run_release_interface), deferred :: run_release


    end type

    type :: reservoir_base_container
        class (reservoir_base_struct), pointer :: ptr
    end type



    abstract interface
        subroutine run_release_interface(reservoir_data, inflow, lateral_inflow, water_elevation, routing_period)
            import :: reservoir_base_struct
            class(reservoir_base_struct), intent(inout) :: reservoir_data
            real, intent(IN)    :: inflow
            real, intent(IN)    :: lateral_inflow
            real, intent(INOUT) :: water_elevation
            real, intent(IN)    :: routing_period      ! routing period [s]

        end subroutine
    end interface



    contains

    !constructor for the reservoir input struct
    subroutine reservoir_base_input_struct_init(this)      !I don't think I need to pass any parameters here??
        implicit none
    	class (reservoir_base_input_struct), intent(inout) :: this ! object being initialized

        this%inflow = 0.0
        this%lateral_inflow = 0.0
        this%previous_day_inflow = 0.0
        this%inflow_previous_timestep = 0.0


    end subroutine reservoir_base_input_struct_init



    ! destructor for the reservoir input struct
    subroutine reservoir_base_input_struct_destroy(this)
        implicit none
        class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed


    end subroutine reservoir_base_input_struct_destroy



    !constructor for the reservoir output struct
    subroutine reservoir_base_output_struct_init(this)      !I don't think I need to pass any parameters here??
        implicit none
    	class (reservoir_base_output_struct), intent(inout) :: this ! object being initialized

        this%outflow = 0.0


    end subroutine reservoir_base_output_struct_init



    !destructor for the reservoir output struct
    subroutine reservoir_base_output_struct_destroy(this)
        implicit none
        class (reservoir_base_output_struct), intent(inout) :: this ! object being destroyed


    end subroutine reservoir_base_output_struct_destroy




end module module_reservoir_base
