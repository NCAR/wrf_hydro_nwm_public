module module_subsurface_static_data
    implicit none

    type subsurface_static_interface
    
    integer :: ixrt
    integer :: jxrt
    integer :: nsoil
    integer :: dt
    integer :: rt_options   
 
    contains
        procedure :: init => subsurface_io_init
        procedure :: destroy => subsurface_io_destroy
    end type subsurface_static_interface

    contains

    subroutine subsurface_io_init(this, ixrt, jxrt, nsoil, dt, rt_options)
        implicit none
        class(subsurface_io_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ixrt                     ! x routing grid size
        integer, intent(in) :: jxrt                     ! y routing grid size
        integer, intent(in) :: nsoil                    ! nsoil
        integer, intent(in) :: dt
        integer, intent(in) :: rt_options

        logical :: allocation_error = .false.

        this%ixrt = ixrt
        this%jxrt = jxrt
        this%nsoil = nsoil
        this%dt = dt
        this%rt_options = rt_options

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of subsurface io structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine subsurface_io_init

    subroutine subsurface_io_destroy(this)
        implicit none
        class(subsurface_io_interface), intent(inout) :: this ! the type object being initialized

        logical :: allocation_error = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of subsurface io structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine subsurface_io_destroy

end module module_subsurface_io
