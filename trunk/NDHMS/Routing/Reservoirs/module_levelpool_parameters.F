! This module defines and instantiates objects
! for a level pool type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_levelpool_parameters

    use module_reservoir_base
    implicit none

    ! Extend/derive level pool parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: levelpool_parameters_interface
        real :: lake_area                ! area of lake (km^2)
        real :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real :: weir_coeffecient         ! weir coefficient
        real :: weir_length              ! weir length (meters)
        real :: orifice_elevation        ! orifice elevation (meters AMSL)
        real :: orifice_coefficient      ! orifice coefficient
        real :: orifice_area             ! orifice area (meters^2)
        real :: max_depth                ! max depth of reservoir before overtop (meters)
        integer :: lake_number           ! lake number

    contains

        procedure :: init => levelpool_parameters_init
        procedure :: destroy => levelpool_parameters_destroy

    end type levelpool_parameters_interface

contains

    !Level Pool Parameters Constructor
    subroutine levelpool_parameters_init(this, lake_area, &
        weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
        orifice_coefficient, orifice_area, max_depth, lake_number)
        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: lake_area      	        ! area of lake (km^2)
        real, intent(in)    :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient         ! weir coefficient
        real, intent(in)    :: weir_length              ! weir length (meters)
        real, intent(in)    :: orifice_elevation        ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient      ! orifice coefficient
        real, intent(in)    :: orifice_area             ! orifice area (meters^2)
        real, intent(in)    :: max_depth                ! max depth of reservoir before overtop (meters)
        integer, intent(in) :: lake_number              ! lake number

        ! Assign the values passed in to a particular level pool reservoir
        ! parameter object's variables.
        this%lake_area = lake_area
        this%weir_elevation = weir_elevation
        this%weir_coeffecient = weir_coeffecient
        this%weir_length = weir_length
        this%orifice_elevation = orifice_elevation
        this%orifice_coefficient = orifice_coefficient
        this%orifice_area = orifice_area
        this%max_depth = max_depth
        this%lake_number = lake_number

    end subroutine levelpool_parameters_init

    !Level Pool Parameters Destructor
    subroutine levelpool_parameters_destroy(this)
        implicit none
        class(levelpool_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine levelpool_parameters_destroy

end module module_levelpool_parameters
