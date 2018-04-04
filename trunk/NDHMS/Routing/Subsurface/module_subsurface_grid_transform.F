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

        ! check allocation status of smcrefrt
        if ( .not. allocated(this%smcrefrt) ) then
            allocate( this%smcrefrt(ix,jx) )
            !no initialization on this variable?
            !this%smcrefrt = 0.0
        end if

        ! check to see if smcrt is allocated
        if ( .not. allocated(this%smcrt) ) then
            allocate( this%smcrt(ix,jx) )
            this%smcrt = 0.0
        end if

        ! check to see if smcmaxrt is allocated
        if ( .not. allocated(this%smcmaxrt) ) then
            allocate( this%smcmaxrt(ix,jx) )
            this%smcmaxrt = 0.0
        end if

        ! check to see if smcwltrt is allocated
        if ( .not. allocated(this%smcwltrt) ) then
            allocate( this%smcwltrt(ix,jx) )
            this%smcwltrt = 0.0
        end if

    end subroutine subsurface_grid_transform_init

    subroutine subsurface_grid_transfrom_destroy(this)
        implicit none
        class(subsurface_grid_transform_interface), intent(inout) :: this ! the type object being initialized

        ! check allocation status of smcrefrt
        if ( allocated(this%smcrefrt) ) then
            deallocate( this%smcrefrt )
        end if

        ! check to see if smcrt is allocated
        if ( allocated(this%smcrt) ) then
            deallocate( this%smcrt )
        end if

        ! check to see if smcmaxrt is allocated
        if ( allocated(this%smcmaxrt) ) then
            deallocate( this%smcmaxrt )
        end if

        ! check to see if smcwltrt is allocated
        if ( allocated(this%smcwltrt) ) then
            allocate( this%smcwltrt )
        end if

    end subroutine

end module module_subsurface_grid_transform
