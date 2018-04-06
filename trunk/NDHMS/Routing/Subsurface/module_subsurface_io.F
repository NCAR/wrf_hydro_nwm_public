module module_subsurface_io
    implicit none

    type subsurface_io_interface
        ! total flow from boundary cells to outside of domain; on routing grid
        real :: qsubbdrytrt

        ! subsurface flow (m^3/s)
        real, allocatable, dimension(:,:) :: qsubrt

        ! flow from boundary cells to outside of domain on routing grid
        real, allocatable, dimension(:,:) :: qsubbdryrt
    contains
        procedure :: init => subsurface_io_init
        procedure :: destroy => subsurface_io_destroy
    end type subsurface_io_interface

    contains

    subroutine subsurface_io_init(this, ix, jx)
        implicit none
        class(subsurface_io_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size

        logical :: allocation_error = .false.

        this%qsubbdrytrt = 0.0

        if ( .not. allocated(this%qsubrt) ) then
            allocate( this%qsubrt(ix,jx) )
            this%qsubrt = 0.0
        else
            allocation_error = .true.
        end if

        if ( .not. allocated(this%qsubbdryrt) ) then
            allocate( this%qsubbdryrt(ix,jx) )
            this%qsubbdryrt = 0.0
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of subsurface io structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine subsurface_io_init

    subroutine subsurface_io_destroy(this)
        implicit none
        class(subsurface_io_interface), intent(inout) :: this ! the type object being initialized

        logical :: allocation_error = .false.

        if ( allocated(this%qsubrt) ) then
            deallocate( this%qsubrt )
        else
            allocation_error = .true.
        end if

        if ( allocated(this%qsubbdryrt) ) then
            deallocate( this%qsubbdryrt )
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of subsurface io structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine subsurface_io_destroy

end module module_subsurface_io
