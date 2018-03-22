module module_subsurface_grid_transform
    implicit none

    type subsurface_grid_transform_interface

        ! temp array? field capacity disaggregated to routing grid, for each soil layer
        real, allocatable, dimension(:,:,:) :: smcrefrt

        ! Soil Moisture Content -- ???
        real, allocatable, dimension(:,:,:) :: smcrt

        ! Soil Moisture Content -- ??? Porosity
        real, allocatable, dimension(:,:,:) :: smcmaxrt

        ! Soil Moisture Content -- ??? Wilting Point
        real, allocatable, dimension(:,:,:) :: smcwltrt
    contains
        procedure :: init => subsurface_grid_transform_init
        procedure :: destroy => subsurface_grid_transfrom_destroy
    end type

    contains

    subroutine subsurface_grid_transform_init(this,ix,jx)
        implicit none
        class(subsurface_grid_transform_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
    end subroutine subsurface_grid_transform_init

    subroutine subsurface_grid_transfrom_destroy(this)
        implicit none
        class(subsurface_grid_transform_interface), intent(inout) :: this ! the type object being initialized
    end subroutine

end module module_subsurface_grid_transform
