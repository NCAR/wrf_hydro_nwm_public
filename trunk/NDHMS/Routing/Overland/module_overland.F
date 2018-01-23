module overland_data

    !type that holds the variables that are inputs to or outputs of the routing code
    ! along with book keepinh variables

    type overland_control
        ! outputs
        real, allocatable, dimension(:,:) :: sfcheadrt ! surface water head

        ! inputd
        real, allocatable, dimension(:,:) :: infxsubrt ! infiltration excess subgrid weighted from INFXSRT

        ! misc are bookkeeping
        real :: qbdrytrt        ! total of qbdrvrt
        real, allocatable, dimension(:,:) :: dhrt           ! NEED VARIABLE INFO
        real, allocatable, dimension(:,:) :: qbdryrt        ! accumlated vlaue of the boundary flux, + into domain - out of domain

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
    end type overland_rt_properties

    ! holds variables used for mass balance
    type overland_mass_balance
        ! mass balance
        real(kind=8) :: dsmctot     ! NEED VARIABLE INFO
        real(kind=8) :: smctot1     ! NEED VARIABLE INFO
        real(kind=8) :: smctot2     ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsrt  ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsr1  ! NEED VARIABLE INFO

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
! if the structure has been allocated and not deallocated there is no effect

subroutine overland_control_init(this,ix,jx)
    type(overland_control), intent(inout) :: this ! the type object being initalized
    integer, intent(in) :: ix                     ! x grid size
    integer, intent(in) :: jx                     ! y grid size

    ! allocate surface head
    if ( .not. allocated(this%sfcheadrt) ) then
        allocate( this%sfcheadrt(ix,jx) )
        this%sfcheadrt = 0.0
    end if

    ! allocate inflitration excess
    if ( .not. allocated(this%infxsubrt) ) then
        allocate( this%infxsubrt(ix,jx) )
        this%infxsubrt = 0.0
    end if

    ! allocate dhrt
    if ( .not. allocated(this%dhrt) ) then
         allocate( this%dhrt(ix,jx) )
         this%dhrt = 0.0
    end if

    ! allocate qbdryrt
    if ( .not. allocated(this%qbdryrt) ) then
         allocate( this%qbdryrt(ix,jx) )          ! allocate qbdryrt
         this%qbdryrt = 0.0
    end if

end subroutine overland_control_init

end module overland_data
