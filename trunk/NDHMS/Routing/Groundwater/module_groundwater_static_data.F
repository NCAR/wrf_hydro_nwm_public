!> @brief static data module for groundwater
!!
!! This module implements the data structures used to interact with the static data
!! used in groundwater module implementations
module module_groundwater_static_data
    use module_groundwater_base
    implicit none

    !> Type used to hold static data values needed by groundwater modules

    type, extends(groundwater_base_static_struct) :: groundwater_static_struct

        integer :: ix_lsm  !< LSM X dimension size
        integer :: jx_lsm  !< LSM Y dimension size
        integer :: ix_rt   !< Routing grid X dimension size
        integer :: jx_rt   !< Routing grid Y dimension size
        real :: dt         !< Model timestep
        integer :: gwbaseswcrt !< groundwater configuration: 0 = off, 1 = exp bucket, 2 = passthrough
        integer :: ovrtswcrt !< overland flag: Control is overland grid interacts with groundwater: 1 = on, 0 = off


    contains

        procedure :: init => groundwater_static_struct_init
        procedure :: destroy => groundwater_static_struct_destroy
    end type groundwater_static_struct

contains

    !> Constructor for the groundwater static struct.
    !! @param this  The nhd groundwater static object being constructed
    !! @param ix  The x dimension of the lsm grid
    !! @param jx  The y dimension of the lsm grid
    !! @param ixrt  The x dimension of the routing grid
    !! @param jxrt  The y dimensions of the routing grid
    !! @param dt  The size of the time step
    !! @param gwbaseswcrt  Groundwater configuration option: 0 = disabled, 1 = exp bucket, 2 = passthrough.
    !! @param ovrtswcrt  Overland routing contributes to groundwater: 0 = off, 1 = on

    subroutine groundwater_static_struct_init(this, ix, jx, ixrt, jxrt, dt, gwbaseswcrt, ovrtswcrt)
        implicit none
        class (groundwater_static_struct), intent(inout):: this
        integer, intent(in) :: ix
        integer, intent(in) :: jx
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        real, intent(in) :: dt
        integer, intent(in) :: gwbaseswcrt
        integer, intent(in) :: ovrtswcrt

        this%ix_lsm = ix
        this%jx_lsm = jx
        this%ix_rt = ixrt
        this%jx_rt = jxrt
        this%dt = dt
        this%gwbaseswcrt = gwbaseswcrt
        this%ovrtswcrt = ovrtswcrt

    end subroutine groundwater_static_struct_init

    !> Destructor for the groundwater static struct.
    !! @param this  The nhd groundwater static object being destroyed

    subroutine groundwater_static_struct_destroy(this)
        implicit none
        class (groundwater_static_struct), intent(inout) :: this ! object being destroyed
    end subroutine groundwater_static_struct_destroy
end module module_groundwater_static_data
