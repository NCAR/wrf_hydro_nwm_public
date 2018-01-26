! module overland_control_data.F
! Purpose: This module contains the overland_control_struct class. This types holds
! the control variables used in the overland routing code
! National Water Center
! Responsibility: Donald W Johnson donald.w.johnson@noaa.gov
! Authors: Donald W Johnson

module overland_control
    !type that holds the variables that are inputs to or outputs of the routing code
    ! along with book keeping variables

    !TODO change output integers to variables that are set dependant on the running system
    ! example WCOSS

    type overland_control_struct
        ! outputs
        real, allocatable, dimension(:,:) :: sfcheadrt ! surface water head

        ! inputs
        real, allocatable, dimension(:,:) :: infxsubrt ! infiltration excess subgrid weighted from INFXSRT

        ! misc are bookkeeping
        real :: qbdrytrt        ! total of qbdrvrt
        real, allocatable, dimension(:,:) :: dhrt           ! NEED VARIABLE INFO
        real, allocatable, dimension(:,:) :: qbdryrt        ! accumlated vlaue of the boundary flux, + into domain - out of domain
    contains
        procedure :: init => overland_control_init
        procedure :: destroy => overland_control_destroy
    end type overland_control_struct

    contains

! this procedure allocates memory for an overland_control structure that has not been allocated
! if the structure has been allocated an error will be logged

subroutine overland_control_init(this,ix,jx)
    class(overland_control_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    logical :: allocation_error = .false.

   this%qbdrytrt = 0.0

    ! allocate surface head
    if ( .not. allocated(this%sfcheadrt) ) then
        allocate( this%sfcheadrt(ix,jx) )
        this%sfcheadrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate inflitration excess
    if ( .not. allocated(this%infxsubrt) ) then
        allocate( this%infxsubrt(ix,jx) )
        this%infxsubrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate dhrt
    if ( .not. allocated(this%dhrt) ) then
         allocate( this%dhrt(ix,jx) )
         this%dhrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate qbdryrt
    if ( .not. allocated(this%qbdryrt) ) then
         allocate( this%qbdryrt(ix,jx) )          ! allocate qbdryrt
         this%qbdryrt = 0.0
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland control structure&
        &that where allready allocated. The allocated members where not changed"

end subroutine overland_control_init

! this procedure deallocates and overland_control structure that was initalized with
! overland_control_init

subroutine overland_control_destroy(this)
    class(overland_control_struct), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate surface head
    if ( allocated(this%sfcheadrt) ) then
        deallocate( this%sfcheadrt )
    else
        allocation_error = .true.
    end if

    ! deallocate inflitration excess
    if ( allocated(this%infxsubrt) ) then
        deallocate( this%infxsubrt)
    else
        allocation_error = .true.
    end if

    ! deallocate dhrt
    if ( allocated(this%dhrt) ) then
         deallocate( this%dhrt )
    else
        allocation_error = .true.
    end if

    ! deallocate qbdryrt
    if ( allocated(this%qbdryrt) ) then
         deallocate( this%qbdryrt )          ! allocate qbdryrt
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland control structure&
        &that where not allready allocated. The unallocated members where not changed"    
end subroutine overland_control_destroy
end module overland_control
