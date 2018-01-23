module overland_data

    !type that holds the variables that are inputs to or outputs of the routing code
    ! along with book keepinh variables

    !TODO change output integers to variables that are set dependant on the running system
    ! example WCOSS

    type overland_control
        ! outputs
        real, allocatable, dimension(:,:) :: sfcheadrt ! surface water head

        ! inputd
        real, allocatable, dimension(:,:) :: infxsubrt ! infiltration excess subgrid weighted from INFXSRT

        ! misc are bookkeeping
        real :: qbdrytrt        ! total of qbdrvrt
        real, allocatable, dimension(:,:) :: dhrt           ! NEED VARIABLE INFO
        real, allocatable, dimension(:,:) :: qbdryrt        ! accumlated vlaue of the boundary flux, + into domain - out of domain
    contains
        procedure :: init => overland_control_init
        procedure :: destroy => overland_control_destroy
    end type overland_control

    ! type that hold inputs and outputs for stream and channels as well as
    ! variables used to interface with channels and lakes
    type overland_stream_and_lakes
        real :: qstrmvoltrt     ! total of qstrmvolrt
        real :: lake_inflotrt   ! lake inflow from surface head

        real, allocatable, dimension(:,:) :: ch_netrt      ! keeps trake of the 0-1 channel network
        integer, allocatable, dimension(:,:) :: lake_mskrt ! mask for identifing lake elements in channel network

        real, allocatable, dimension(:,:) :: qstrmvolrt     ! accumulated channel inflow
        real, allocatable, dimension(:,:) :: lake_inflort   ! NEED VARIABLE INFO
    contains
        procedure :: init => overland_stream_and_lakes_init
        procedure :: destroy => overland_stream_and_lakes_destroy
    end type overland_stream_and_lakes

    ! holds proprties of the the routing grid needed by overland routing algs
    type overland_rt_properties
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
        procedure :: destroy => overland_properties_destroy
    end type overland_rt_properties

    ! holds variables used for mass balance
    type overland_mass_balance
        ! mass balance
        real(kind=8) :: dsmctot     ! NEED VARIABLE INFO
        real(kind=8) :: smctot1     ! NEED VARIABLE INFO
        real(kind=8) :: smctot2     ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsrt  ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsr1  ! NEED VARIABLE INFO
    contains
        procedure :: init => overland_mass_balance_init
        procedure :: destroy => overland_mass_balance_destroy
    end type overland_mass_balance

    ! hold all variables used in overland routing
    type overland_struct

        type (overland_control), pointer :: control                    ! i/o and control variables
        type (overland_stream_and_lakes), pointer :: streams_and_lakes ! channel and lake related
        type (overland_rt_properties), pointer :: properties           ! properties used in routing code
        type (overland_mass_balance), pointer :: mass_balance          ! mass balance variables

    end type overland_struct

contains

! this procedure allocates memory for an overland_control structure that has not been allocated
! if the structure has been allocated an error will be logged

subroutine overland_control_init(this,ix,jx)
    type(overland_control), intent(inout) :: this ! the type object being initalized
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
        that where allready allocated. The allocated members where not changed"

end subroutine overland_control_init

! this procedure deallocates and overland_control structure that was initalized with
! overland_control_init

subroutine overland_control_destroy(this)
    type(overland_control), intent(inout) :: this ! the type object being destroyed

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
        that where not allready allocated. The unallocated members where not changed"    
end subroutine overland_control_delete

! this structure allocates and initalizes the members of an overland_streams_and_lakes strucutre
! if members have allready been initalized they will not be altered and an error will be logged

subroutine overland_streams_and_lakes_init(this,ix,jx)
    type(overland_streams_and_lakes), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    logical :: allocation_error = .false.

    this%qstrmvoltrt = 0.0
    this%lake_inflotrt = 0.0

    ! allocate the stream network
    if ( .not. allocated(this%ch_netrt) ) then
        allocate( this%ch_netrt(ix,jx) )
        this%ch_netrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate the lake mask
    if ( .not. allocated(this%lake_mskrt) ) then
        allocate( this%lake_mskrt(ix,jx) )
        this%lake_mskrt = -9999
    else
        allocation_error = .true.
    end if

    ! allocate qstrmvolrt
    if ( .not. allocated(this%qstrmvolrt) ) then
        allocate( this%qstrmvolrt(ix,jx) )
        this%qstrmvolrt = 0.0
    else
        allocation_error = .true.
    end if

    ! allocate lake_inflort
    if ( .not. allocated(this%lake_inflort) ) then
        allocate( this%lake_inflort(ix,jx) )
        this%lake_inflort = 0.0
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to allocate data in members of overland lakes and streams structure&
        that where allready allocated. The allocated members where not changed"    
end subroutine overland_streams_and_lakes_init


subroutine overland_streams_and_lakes_destroy(this)
    type(overland_lakes_and_streams), intent(inout) :: this ! the type object being destroyed

    logical :: allocation_error = .false.

    ! deallocate channel network
    if ( allocated(this%ch_netrt) ) then
        deallocate( this%ch_netrt )
    else
        allocation_error = .true.
    end if

    ! deallocate the lake mask
    if ( allocated(this%lake_mskrt) ) then
        deallocate( this%lake_mskrt)
    else
        allocation_error = .true.
    end if

    ! deallocate qstrmvolrt
    if ( allocated(this%qstrmvolrt) ) then
         deallocate( this%qstrmvolrt )
    else
        allocation_error = .true.
    end if

    ! deallocate qbdryrt
    if ( allocated(this%lake_inflort) ) then
         deallocate( this%lake_inflort )          ! allocate qbdryrt
    else
        allocation_error = .true.
    end if

    if ( allocation_error ) &
        write(0,*) "attempt to deallocate data in members of overland streams and lakes structure&
        that where not allready allocated. The unallocated members where not changed"   
end subroutine overland_streams_and_lakes_destroy

subroutine overland_properties_init(this,ix,jx)
    type(overland_properties), intent(inout) :: this ! the type object being initalized
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
        that where allready allocated. The allocated members where not changed"      
end subroutine overland_properties_init

subroutine overland_properties_destory(this)
    type(overland_properties), intent(inout) :: this ! the type object being destroyed

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
        that where not allocated. The unallocated members where not changed"      
end subroutine overland_properties_destory

! initalize the mass balance variables
subroutine overland_mass_balance_init(this)
    type(overland_mass_balance), intent(inout) :: this ! the type object being initalized
    this%dsmctot = 0.0
    this%smctot1 = 0.0
    this%smctot2 = 0.0
    this%suminfxsrt = 0.0
    this%suminfxsr1 = 0.0
end subroutine overland_mass_balance_init

! none of the mass balance variables are dynamicly allocated if such are added
! they should be deallocated here
subroutine overland_mass_balance_destroy(this)
    type(overland_mass_balance), intent(inout) :: this ! the type object being destroyed
end subroutine overland_mass_balance_destroy

! this procedure allocates

subroutine overland_struct_init(this,ix,jx)
    type(overland_struct), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size    allocate( thist%control )

    ! allocate the control structure then initalize
    allocate( this%control )
    if ( .not. allocated( this%control) ) then
        write(0,*) "Failure to allocate overland control structure"
    else
        call this%control%init
    end if

    ! allocate the streams and lakes structure
    allocate( this%streams_and_lakes )
    if ( .not. allocated( this%streams_and_lakes) ) then
        write(0,*) "Failure to allocate overland lakes and streams structure"
    else
        call this%streams_and_lakes
    end if

    ! allocate the properties structure
    allocate( this%properties )
    if ( .not. allocated( this%properties) ) then
        write(0,*) "Failure to allocate overland properties structure"
    else
        call this%properties
    end if

    ! allocate the mass balance structure
    allocate( this%mass_balance)
    if ( .not. allocated( this%mass_balance) ) then
        write(0,*) "Failure to allocate overland mass balance structure"
    else
        call this%mass_balance
    end if

end subroutine overland_struct_init

subroutine overland_struct_destroy(this)
    ! call the destructors
    type (overland_struct), intent(inout) :: this

    if ( allocated(this%control ) ) then
        call this%control%destroy
        deallocate( this%control )
    end if

    if ( allocated(this%streams_and_lakes) )then
        call this%stream_and_lakes%destroy
        deallocate( this%stream_and_lakes )
    end if

    if ( allocated(this%properties) ) then
        call this%properties%destroy
        deallocate( this%properties )
    end if

    if ( allocated(thist%mass_balance) ) then
        call this%mass_balance%destroy
        deallocate( this%mass_balance )
    end if


end subroutine overland_struct_destroy

end module overland_data
