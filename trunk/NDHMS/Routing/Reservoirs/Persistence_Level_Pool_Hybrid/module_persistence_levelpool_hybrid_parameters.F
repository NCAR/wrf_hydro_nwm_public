! This module defines and instantiates objects
! for a hybrid type reservoir's
! parameters/properties. Parameters holds
! static/unchanging variables that are
! set when the given reservoir object is
! initialized/instantiated.
module module_persistence_levelpool_hybrid_parameters

    use module_reservoir_base
    implicit none

    ! Extend/derive hybrid parameters from the abstract base
    ! struct for reservoir properties.
    type, extends(reservoir_base_properties_struct) :: hybrid_parameters_interface
        real    :: min_storage                  ! minimum storage (cubic meters)
        real    :: max_storage                  ! maximum storage (cubic meters)
        real    :: lake_area                    ! area of reservoir (km^2)
        integer :: lake_number                  ! lake number
        real, allocatable, dimension(:) :: persistence_weighted_coefficients

    contains

        procedure :: init => hybrid_parameters_init
        procedure :: destroy => hybrid_parameters_destroy

    end type hybrid_parameters_interface

contains

    ! Hybrid Parameters Constructor
    subroutine hybrid_parameters_init(this, lake_area, lake_max_water_elevation, orifice_elevation, lake_number)
        implicit none
        class(hybrid_parameters_interface), intent(inout) :: this ! the type object being initialized
        real, intent(in)    :: lake_area                    ! area of lake (km^2)
        real, intent(in)    :: lake_max_water_elevation     ! max water elevation (meters)
        real, intent(in)    :: orifice_elevation            ! orifice elevation (meters AMSL)
        integer, intent(in) :: lake_number                  ! lake number

        this%lake_area = lake_area

        this%lake_number = lake_number

        this%min_storage = 0.0

        this%max_storage = (lake_max_water_elevation - orifice_elevation) * lake_area * 1.0E6

    end subroutine hybrid_parameters_init

    ! Hybrid Parameters Destructor
    subroutine hybrid_parameters_destroy(this)
        implicit none
        class(hybrid_parameters_interface), intent(inout) :: this ! the type object being destroyed
    end subroutine hybrid_parameters_destroy

end module module_persistence_levelpool_hybrid_parameters
