module module_levelpool_parameters

    use module_reservoir_base
    implicit none

    type, extends(reservoir_base_properties_struct) :: levelpool_parameters_interface


    contains

        procedure :: init => levelpool_parameters_init
        procedure :: destroy => levelpool_parameters_destroy


    end type levelpool_parameters_interface


contains

    subroutine levelpool_parameters_init(this, reservoir_area, &
        weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
        orifice_coffecient, orifice_area, max_depth, lake_number)

        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being initialized

        !real, intent(IN)    :: routing_period           ! routing period [s]
        real, intent(IN)    :: reservoir_area                     ! area of reservoir (km^2)
        real, intent(IN)    :: weir_elevation           ! bottom of weir elevation
        real, intent(IN)    :: weir_coeffecient         ! weir coeff.
        real, intent(IN)    :: weir_length              ! weir length (m)
        real, intent(IN)    :: orifice_elevation        ! orifice elevation
        real, intent(IN)    :: orifice_coffecient       ! orifice coeff.
        real, intent(IN)    :: orifice_area             ! orifice area (m^2)
        real, intent(IN)    :: max_depth                ! max depth of reservoir before overtop (m)
        integer, intent(IN) :: lake_number              ! lake number


    end subroutine levelpool_parameters_init


    subroutine levelpool_parameters_destroy(this)                          ! NEED THIS SUBROUTINE??
        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being destroyed


    end subroutine levelpool_parameters_destroy


end module module_levelpool_parameters

















