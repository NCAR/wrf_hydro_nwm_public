! This module defines and instantiates objects
! for a machine learning type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given machine learning
! reservoir object and not other modules or areas
! of the system.
module module_machine_learning_state

    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning state from the abstract base
    ! struct for reservoir state.
    type, extends(reservoir_base_state_struct) :: machine_learning_state_interface
        real :: current_storage
        real :: previous_day_storage
        real :: previous_day_release

    contains

        procedure :: init => machine_learning_state_init
        procedure :: destroy => machine_learning_state_destroy

    end type machine_learning_state_interface

contains

    !Machine Learning State Constructor
    subroutine machine_learning_state_init(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(inout) :: water_elevation     ! meters AMSL

        ! Assign the water elevation value passed in to a particular level pool reservoir
        ! state object's variable for water elevation
        this%water_elevation = water_elevation

    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy

end module module_machine_learning_state
