module module_subsurface_properties
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

    subroutine subsurface_properties_init(this,ix,jx)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix                     ! x grid size
        integer, intent(in) :: jx                     ! y grid size

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%lksatrt) ) then
            allocate( this%lksatrt(ix,jx) )
            this%lksatrt = 0.0
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%zwattablrt) ) then
            allocate( this%zwattablrt(ix,jx) )
            this%zwattablrt = 0.0
        end if

        ! allocate the array only if not already allocated
        if ( .not. allocated(this%soldeprt) ) then
            allocate( this%soldeprt(ix,jx) )
            this%soldeprt = 0.0
        end if

        this%zsoil = 0.0

    end subroutine subsurface_properties_init

    subroutine subsurface_properties_destroy(this)
        implicit none
        class(subsurface_properties_interface), intent(inout) :: this ! the type object being destroyed

        ! only deallocated if already allocated
        if ( allocated(this%lksatrt) ) then
            deallocate( this%lksatrt)
        end if

        ! only deallocated if already allocated
        if ( allocated(this%zwattablrt) ) then
            deallocate( this%zwattablrt)
        end if

        ! only deallocated if already allocated
        if ( allocated(this%soldeprt) ) then
            deallocate( this%soldeprt)
        end if
    end subroutine subsurface_properties_destroy

end module module_subsurface_properties
