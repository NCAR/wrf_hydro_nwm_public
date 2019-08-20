! This module defines and instantiates objects
! for an rfc forecasts type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given rfc forecasts
! reservoir object and not other modules or areas
! of the system.

module module_rfc_forecasts_state
    use module_reservoir_base
    implicit none

    ! Extend/derive rfc forecasts state from the abstract base
    ! type for reservoir state.
    type, extends(reservoir_base_state_type) :: rfc_forecasts_state_interface

    contains

        procedure :: init => rfc_forecasts_state_init
        procedure :: destroy => rfc_forecasts_state_destroy

    end type rfc_forecasts_state_interface

contains

    ! RFC Forecasts State Constructor
    subroutine rfc_forecasts_state_init(this)
        implicit none
        class(rfc_forecasts_state_interface), intent(inout) :: this ! the type object being initialized

    end subroutine rfc_forecasts_state_init

    ! RFC Forecasts State Destructor
    subroutine rfc_forecasts_state_destroy(this)
        implicit none
        class(rfc_forecasts_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine rfc_forecasts_state_destroy

end module module_rfc_forecasts_state
