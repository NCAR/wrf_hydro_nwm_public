! This module defines and instantiates objects
! for a machine learning type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_machine_learning_parameters

    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: machine_learning_parameters_interface
        real :: reservoir_area           ! area of reservoir (km^2)
        real :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real :: weir_coeffecient         ! weir coefficient
        real :: weir_length              ! weir length (meters)
        real :: orifice_elevation        ! orifice elevation (meters AMSL)
        real :: orifice_coefficient      ! orifice coefficient
        real :: orifice_area             ! orifice area (meters^2)
        real :: max_depth                ! max depth of reservoir before overtop (meters)
        integer :: lake_number           ! lake number

    contains

        procedure :: init => machine_learning_parameters_init
        procedure :: destroy => machine_learning_parameters_destroy

    end type machine_learning_parameters_interface

contains

    !Machine Learning Parameters Constructor
    subroutine machine_learning_parameters_init(this, reservoir_area, &
        weir_elevation, weir_coeffecient, weir_length, orifice_elevation, &
        orifice_coefficient, orifice_area, max_depth, lake_number)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: reservoir_area           ! area of reservoir (km^2)
        real, intent(in)    :: weir_elevation           ! bottom of weir elevation (meters AMSL)
        real, intent(in)    :: weir_coeffecient         ! weir coefficient
        real, intent(in)    :: weir_length              ! weir length (meters)
        real, intent(in)    :: orifice_elevation        ! orifice elevation (meters AMSL)
        real, intent(in)    :: orifice_coefficient      ! orifice coefficient
        real, intent(in)    :: orifice_area             ! orifice area (meters^2)
        real, intent(in)    :: max_depth                ! max depth of reservoir before overtop (meters)
        integer, intent(in) :: lake_number              ! lake number

        ! Assign the values passed in to a particular machine learning reservoir
        ! parameter object's variables.
        this%reservoir_area = reservoir_area
        this%weir_elevation = weir_elevation
        this%weir_coeffecient = weir_coeffecient
        this%weir_length = weir_length
        this%orifice_elevation = orifice_elevation
        this%orifice_coefficient = orifice_coefficient
        this%orifice_area = orifice_area
        this%max_depth = max_depth
        this%lake_number = lake_number

    end subroutine machine_learning_parameters_init

    !Machine Learning Parameters Destructor
    subroutine machine_learning_parameters_destroy(this)
        implicit none
        class(machine_learning_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine machine_learning_parameters_destroy

end module module_machine_learning_parameters
