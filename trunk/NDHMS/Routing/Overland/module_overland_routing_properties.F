! module overland_routing_properties_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the physical property variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson

module overland_routing_properties

    ! holds proprties of the the routing grid needed by overland routing algs
    type overland_routing_properties_struct
        integer :: ixrt         ! number of cells in x direction on the high res grid
        integer :: jxrt         ! number of cells in y direction on the high res grid
        real, allocatable, dimension(:,:) :: soxrt        ! terrian slope X
        real, allocatable, dimension(:,:) :: soyrt        ! terrian slope Y
        real, allocatable, dimension(:,:) :: ovroughrt    ! surface roughness
        real, allocatable, dimension(:,:) :: retdeprt     ! retention depthdrodirection slope)
        real, allocatable, dimension(:,:,:) :: so8rt ! terrian slope (maybe 8 direction slope)
        integer, allocatable, dimension(:,:,:) :: so8rt_d ! slope direction (nieghbor index?)?
        real, allocatable, dimension(:,:,:) :: dist ! NEED VARIABLE INFO (distance relative to what)
    contains
        procedure :: init => overland_properties_init
        procedure :: destroy => overland_properties_destory
    end type overland_routing_properties_struct

    contains

! this procedure allocates memory for an overland_routing_properties structure that has not been allocated
! if the structure has been allocated an error will be logged

subroutine overland_properties_init(this,ix,jx)
    class(overland_routing_properties_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    logical :: allocation_error = .false.

    ! record the grid dimensions
    ! TODO find a better place for this to be stored
    this%ixrt = ix
    this%jxrt = jx

    ! allocate x slope
    if ( .not. allocated(this%soxrt) ) then
        allocate( this%soxrt(ix,jx) )
        this%soxrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate y slope
    if ( .not. allocated(this%soyrt) ) then
        allocate( this%soyrt(ix,jx) )
        this%soyrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate 8 directional slope
    if ( .not. allocated(this%so8rt) ) then
        allocate( this%so8rt(ix,jx,8) )
        this%so8rt = -999
    else
        allocation_error = .true.
    end if

    ! allocate slope index
    if ( .not. allocated(this%so8rt_d) ) then
        allocate( this%so8rt_d(ix,jx,3) )
        this%so8rt_d = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate surface roughness
    if ( .not. allocated(this%ovroughrt) ) then
        allocate( this%ovroughrt(ix,jx) )
        this%ovroughrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate retention depth
    if ( .not. allocated(this%retdeprt) ) then
        allocate( this%retdeprt(ix,jx) )
        this%retdeprt = 0.001   ! units (mm)
    else
        allocation_error = .true.
    end if

    ! allocate dist
    if ( .not. allocated(this%dist) ) then
        allocate( this%dist(ix,jx,9) )
        this%dist = -999
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland properties structure&
        &that where allready allocated. The allocated members where not changed"      
end subroutine overland_properties_init

! this procedure deallocates and overland_routing_properties structure that was initalized with
! overland_properties_init

subroutine overland_properties_destory(this)
    class(overland_routing_properties_struct), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate x slope
    if ( allocated(this%soxrt) ) then
        deallocate( this%soxrt )
    else
        allocation_error = .true.
    end if

    ! allocate y slope
    if ( allocated(this%soyrt) ) then
        deallocate( this%soyrt )
    else
        allocation_error = .true.
    end if

    ! allocate 8 directional slope
    if ( allocated(this%so8rt) ) then
        deallocate( this%so8rt )
    else
        allocation_error = .true.
    end if

    ! allocate slope index
    if ( allocated(this%so8rt_d) ) then
        deallocate( this%so8rt_d )
    else
        allocation_error = .true.
    end if

    ! allocate surface roughness
    if ( allocated(this%ovroughrt) ) then
        deallocate( this%ovroughrt )
    else
        allocation_error = .true.
    end if

    ! allocate retention depth
    if ( allocated(this%retdeprt) ) then
        deallocate( this%retdeprt )
    else
        allocation_error = .true.
    end if

    ! allocate dist
    if (  allocated(this%dist) ) then
        deallocate( this%dist )
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland properties structure&
        &that where not allocated. The unallocated members where not changed"      
end subroutine overland_properties_destory


end module overland_routing_properties
