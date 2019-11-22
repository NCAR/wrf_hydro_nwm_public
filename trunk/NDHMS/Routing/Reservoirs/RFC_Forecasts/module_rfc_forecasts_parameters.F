! This module defines and instantiates objects
! for an rfc forecasts type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.

module module_rfc_forecasts_parameters
    use module_reservoir_base
    implicit none

    ! Extend/derive rfc forecasts parameters from the abstract base
    ! type for reservoir properties.
    type, extends(reservoir_base_properties_type) :: rfc_forecasts_parameters_interface

    contains

        procedure :: init => rfc_forecasts_parameters_init
        procedure :: destroy => rfc_forecasts_parameters_destroy

    end type rfc_forecasts_parameters_interface

contains

    ! RFC Forecasts Parameters Constructor
    subroutine rfc_forecasts_parameters_init(this)
        implicit none
        class(rfc_forecasts_parameters_interface), intent(inout) :: this ! the type object being initialized

    end subroutine rfc_forecasts_parameters_init

    ! RFC Forecasts Parameters Destructor
    subroutine rfc_forecasts_parameters_destroy(this)
        implicit none
        class(rfc_forecasts_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine rfc_forecasts_parameters_destroy

end module module_rfc_forecasts_parameters
