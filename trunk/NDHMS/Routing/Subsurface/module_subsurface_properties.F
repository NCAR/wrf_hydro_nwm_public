module module_subsurface_properties
    use overland_data
    implicit none

    type subsurface_properties_interface
        ! disaggregated lateral hydraulic conductivity, on the routing grid, with adjustment factor applied (Noah_distr_routing.F: 2641*) LKSAT*LKSATFAC*[0..1]
        real, allocatable, dimension(:,:) :: lksatrt

        ! water table depth (meters) (module_HYDRO_io.F: 1962*)
        real, allocatable, dimension(:,:) :: zwattablrt

        ! soil depth on routing grid
        real, allocatable, dimension(:,:) :: soldeprt

        ! need info
        real, dimension(100) :: zsoil

    contains

        procedure :: init => subsurface_properties_init
        procedure :: destroy => subsurface_properties_destroy
    end type subsurface_properties_interface

contains

    subroutine subsurface_properties_init(this,ix,jx,overland_data)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size
        class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%lksatrt) ) then
            allocate( this%lksatrt(ix,jx) )
            this%lksatrt = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%zwattablrt) ) then
            allocate( this%zwattablrt(ix,jx) )
            this%zwattablrt = 0.0
        else
            allocation_error = .true.
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%soldeprt) ) then
            allocate( this%soldeprt(ix,jx) )
            this%soldeprt = 0.0
        else
            allocation_error = .true.
        end if

        this%zsoil = 0.0

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of subsurface properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine subsurface_properties_init

    subroutine subsurface_properties_destroy(this)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! only deallocated if already allocated
        if ( allocated(this%lksatrt) ) then
            deallocate( this%lksatrt)
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%zwattablrt) ) then
            deallocate( this%zwattablrt)
        else
            allocation_error = .true.
        end if

        ! only deallocated if already allocated
        if ( allocated(this%soldeprt) ) then
            deallocate( this%soldeprt)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of subsurface properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine subsurface_properties_destroy

end module module_subsurface_properties
