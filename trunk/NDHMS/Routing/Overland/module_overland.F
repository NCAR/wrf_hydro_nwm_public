Module overland_data
    type overland_struct

        real :: qstrmvoltrt     ! total of qstrmvolrt
        real :: qbdrytrt        ! total of qbdrvrt
        real :: lake_inflotrt   ! lake inflow from surface head

        integer :: ixrt         ! number of cells in x direction on the high res grid
        integer :: jxrt         ! number of cells in y direction on the high res grid

        real, allocatable, dimension(:,:) :: ch_netrt      ! keeps trake of the 0-1 channel network
        integer, allocatable, dimension(:,:) :: lake_mskrt ! mask for identifing lake elements in channel network

        real, allocatable, dimension(:,:) :: soxrt        ! terrian slope X
        real, allocatable, dimension(:,:) :: soyrt        ! terrian slope Y
        real, allocatable, dimension(:,:) :: ovroughrt    ! surface roughness
        real, allocatable, dimension(:,:) :: retdeprt     ! retention depth
        real, allocatable, dimension(:,:,:) :: so8rt      ! terrian slope (maybe 8 direction slope)
        integer, allocatable, dimension(:,:,:) :: so8rt_d ! slope direction (nieghbor index?)?

        real, allocatable, dimension(:,:) :: sfcheadrt ! surface water head
        real, allocatable, dimension(:,:) :: infxsubrt ! infiltration excess subgrid weighted from INFXSRT

        real, allocatable, dimension(:,:) :: dhrt           ! NEED VARIABLE INFO
        real, allocatable, dimension(:,:) :: qstrmvolrt     ! accumulated channel inflow
        real, allocatable, dimension(:,:) :: qbdryrt        ! accumlated vlaue of the boundary flux, + into domain - out of domain
        real, allocatable, dimension(:,:) :: lake_inflort   ! NEED VARIABLE INFO

        real, allocatable, dimension(:,:,:) :: dist ! NEED VARIABLE INFO (distance relative to what)

        ! mass balance
        real(kind=8) :: dsmctot     ! NEED VARIABLE INFO
        real(kind=8) :: smctot1     ! NEED VARIABLE INFO
        real(kind=8) :: smctot2     ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsrt  ! NEED VARIABLE INFO
        real(kind=8) :: suminfxsr1  ! NEED VARIABLE INFO

    end type overland_struct
end Module overland_data
