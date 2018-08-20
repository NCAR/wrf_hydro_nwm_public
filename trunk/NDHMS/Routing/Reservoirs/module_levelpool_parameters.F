module module_levelpool_parameters

    use module_reservoir_base
    implicit none

    type, extends(reservoir_base_properties_struct) :: levelpool_parameters_interface

        real :: reservoir_area           ! area of reservoir (km^2)
        real :: weir_elevation           ! bottom of weir elevation
        real :: weir_coeffecient         ! weir coeff.
        real :: weir_length              ! weir length (m)
        real :: orifice_elevation        ! orifice elevation
        real :: orifice_coffecient       ! orifice coeff.
        real :: orifice_area             ! orifice area (m^2)
        real :: max_depth                ! max depth of reservoir before overtop (m)
        integer :: lake_number           ! lake number



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
        real, intent(in)    :: reservoir_area           ! area of reservoir (km^2)
        real, intent(in)    :: weir_elevation           ! bottom of weir elevation
        real, intent(in)    :: weir_coeffecient         ! weir coeff.
        real, intent(in)    :: weir_length              ! weir length (m)
        real, intent(in)    :: orifice_elevation        ! orifice elevation
        real, intent(in)    :: orifice_coffecient       ! orifice coeff.
        real, intent(in)    :: orifice_area             ! orifice area (m^2)
        real, intent(in)    :: max_depth                ! max depth of reservoir before overtop (m)
        integer, intent(in) :: lake_number              ! lake number



        this%reservoir_area = reservoir_area
        this%weir_elevation = weir_elevation
        this%weir_coeffecient = weir_coeffecient
        this%weir_length = weir_length
        this%orifice_elevation = orifice_elevation
        this%orifice_coffecient = orifice_coffecient
        this%orifice_area = orifice_area
        this%max_depth = max_depth
        this%lake_number = lake_number


    end subroutine levelpool_parameters_init


    subroutine levelpool_parameters_destroy(this)                          ! NEED THIS SUBROUTINE??
        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being destroyed


    end subroutine levelpool_parameters_destroy


end module module_levelpool_parameters

















