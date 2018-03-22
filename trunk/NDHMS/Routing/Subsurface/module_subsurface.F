module module_subsurface

    use module_subsurface_io
    use module_subsurface_properties
    use module_subsurface_grid_transform

    implicit none

    type subsurface_struct

        type ( subsurface_io_interface), pointer :: io
        type ( subsurface_properties_interface), pointer :: properties
        type ( subsurface_grid_transform_interface), pointer :: grid_transfrom

        ! unused pointer are in an undefined state
        ! this means the result of calling associated(<pointer>)
        ! on a pointer that has not been set is unknown
        ! therefore associated can not be used as a guard
        ! in inital pointer allocation
        logical, private :: pointer_allocation_guard = .false.

        contains

    end type subsurface_struct

    contains

    subroutine subsurface_struct_init(this,ix,jx)
        implicit none
        class(subsurface_struct), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size

    if (this%pointer_allocation_guard .eqv. .false. ) then
        this%pointer_allocation_guard = .true.
        ! allocate the io interface
        allocate( this%io )
        if ( .not. associated( this%io) ) then
            write(0,*) "Failure to allocate subsurface io interface"
        else
            call this%io%init(ix,jx)
        end if

        ! allocate the properties interface
        allocate( this%properties )
        if ( .not. associated( this%properties) ) then
            write(0,*) "Failure to allocate subsurface io interface"
        else
            call this%properties%init(ix,jx)
        end if

        ! allocate the grid_transfrom interface
        allocate( this%grid_transfrom )
        if ( .not. associated( this%grid_transfrom) ) then
            write(0,*) "Failure to allocate grid transform interface"
        else
            call this%grid_transfrom%init(ix,jx)
        end if

    end if

    end subroutine subsurface_struct_init

    subroutine subsurface_struct_destroy(this)
        implicit none
        class(subsurface_struct), intent(inout) :: this ! the type object being initialized

        if ( associated( this%io ) ) then
            call this%io%destroy
            deallocate( this%io )
        end if

        if (associated( this%properties ) )then
            call this%properties%destroy
            deallocate( this%properties )
        end if

        if (associated( this%grid_transfrom ) ) then
            call this%grid_transfrom%destroy
            deallocate( this%grid_transfrom )
        end if

        this%pointer_allocation_guard = .false.

    end subroutine subsurface_struct_destroy

end module module_subsurface
