! This module is the base module for reservoirs that defines
! abstract types for a reservoir base, reservoir state, and
! reservoir properties. It also defines types for the inputs
! and outputs common to all sub-types of reservoirs. Finally,
! it defines a pointer to a reservoir object and an interface
! for running release on a reservoir.
module module_reservoir_base

    ! Defines the reservoir base state stuct that sub-types of
    ! reservoirs will derive from. Since this is abstract, an instance
    ! of reservoir state cannot be created but only derived to sub-types
    ! of reservoirs. State holds and tracks dynamic/changing variables
    ! that are only relevant to the given reservoir object and not other
    ! modules or areas of the system.
    type, abstract :: reservoir_base_state_struct

    end type

    ! Defines the reservoir base properties/parameters stuct that
    ! sub-types of reservoirs will derive from. Since this is abstract,
    ! an instance of reservoir properties cannot be created but only
    ! derived to sub-types of reservoirs. Properties holds
    ! static/unchanging variables that are set when the given reservoir
    ! object is initialized/instantiated.
    type, abstract :: reservoir_base_properties_struct

    end type

    ! Defines the input base stuct which holds the same inputs that all
    ! sub-types of reservoirs will derive from. Therefore, all types of
    ! reservoirs will hold the inputs defined below. These are
    ! dynamic/changing variables that will receive values passed into the
    ! object's methods/subroutines at runtime.
    type :: reservoir_base_input_struct

        real :: inflow                      ! cubic meters per second (cms)
        real :: lateral_inflow              ! cubic meters per second (cms)
        real :: previous_timestep_inflow    ! cubic meters per second (cms)

    contains

        procedure :: init => reservoir_base_input_struct_init
        procedure :: destroy => reservoir_base_input_struct_destroy

    end type

    ! Defines the output base stuct which holds the same output(s) that all
    ! sub-types of reservoirs will derive from. Therefore, all types of
    ! reservoirs will hold the output(s) defined below. These are
    ! dynamic/changing variables that will receive values returned from the
    ! object's methods/subroutines at runtime.
    type :: reservoir_base_output_struct

        real :: outflow                     ! cubic meters per second (cms)

    contains

        procedure :: init => reservoir_base_output_struct_init
        procedure :: destroy => reservoir_base_output_struct_destroy

    end type

    ! Defines the reservoir base stuct that sub-types of reservoirs
    ! will derive from. Since this is abstract, an instance of reservoir
    ! cannot be created but only derived to sub-types of reservoirs.
    ! Derived sub-types of reservoirs of this base struct will create a
    ! reservoir object that will hold state, properties/parameters,
    ! inputs, and outputs as sub-objects.
    type, abstract :: reservoir_base_struct

        type(reservoir_base_input_struct), pointer :: input
        type(reservoir_base_output_struct), pointer :: output

    contains

        ! Defines procedure to call release for a type of reservoir,
        ! which will then call methods/subroutines for processing the
        ! inputs and returning the outputs. Since this is deferred,
        ! it cannot be implemented for a base reservoir but only for
        ! a sub-type of reservoir.
        procedure (run_release_interface), deferred :: run_release

    end type

    ! Defines a pointer to a reservoir base struct
    type :: reservoir_base_container
        class (reservoir_base_struct), pointer :: ptr
    end type

    ! Defines the abstract implementation of run_release that all
    ! reservoir sub-types will derive from with the same given
    ! inputs and outputs.
    abstract interface
        subroutine run_release_interface(reservoir_data, previous_timestep_inflow, inflow, &
            lateral_inflow, water_elevation, outflow, routing_period)
            import reservoir_base_struct
            class(reservoir_base_struct), intent(inout) :: reservoir_data
            real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
            real, intent(in)    :: inflow                   ! cubic meters per second (cms)
            real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
            real, intent(inout) :: water_elevation          ! meters AMSL
            real, intent(out)   :: outflow                  ! cubic meters per second (cms)
            real, intent(in)    :: routing_period           ! seconds

        end subroutine run_release_interface

    end interface

contains

    !Constructor for the reservoir input struct
    subroutine reservoir_base_input_struct_init(this)
        implicit none
    	class (reservoir_base_input_struct), intent(inout) :: this ! object being initialized
        this%inflow = 0.0
        this%lateral_inflow = 0.0
        this%previous_timestep_inflow = 0.0

    end subroutine reservoir_base_input_struct_init


    ! Destructor for the reservoir input struct
    subroutine reservoir_base_input_struct_destroy(this)
        implicit none
        class (reservoir_base_input_struct), intent(inout) :: this ! object being destroyed

    end subroutine reservoir_base_input_struct_destroy


    ! Constructor for the reservoir output struct
    subroutine reservoir_base_output_struct_init(this)
        implicit none
    	class (reservoir_base_output_struct), intent(inout) :: this ! object being initialized
        this%outflow = 0.0

    end subroutine reservoir_base_output_struct_init


    ! Destructor for the reservoir output struct
    subroutine reservoir_base_output_struct_destroy(this)
        implicit none
        class (reservoir_base_output_struct), intent(inout) :: this ! object being destroyed

    end subroutine reservoir_base_output_struct_destroy

end module module_reservoir_base
