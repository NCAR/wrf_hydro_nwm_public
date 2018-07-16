module module_groundwater_static_data
    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_static_struct) :: groundwater_static_struct

        integer :: ix_lsm
        integer :: jx_lsm
        integer :: ix_rt
        integer :: jx_rt
        real :: dt
        integer :: gwbaseswcrt
        integer :: ovrtswcrt


        contains

        procedure :: init => groundwater_static_struct_init
        procedure :: destroy => groundwater_static_struct_destroy
    end type groundwater_static_struct

    contains

    !constructor for the ground water struct
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

    ! destructor for the ground water struct
    subroutine groundwater_static_struct_destroy(this)
        implicit none
        class (groundwater_static_struct), intent(inout) :: this ! object being destroyed
    end subroutine groundwater_static_struct_destroy
end module module_groundwater_static_data
