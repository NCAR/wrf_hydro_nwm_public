module module_levelpool_state

    use module_reservoir_base
    implicit none

    type, extends(reservoir_base_state_struct) :: levelpool_state_interface


    contains

        procedure :: init => levelpool_state_init
        procedure :: destroy => levelpool_state_destroy


    end type levelpool_state_interface


contains

    subroutine levelpool_state_init(this, water_elevation)

        implicit none
        class(levelpool_state_interface), intent(inout) :: this ! the type object being initialized

        real, intent(inout)    :: water_elevation



    end subroutine levelpool_state_init


    subroutine levelpool_state_destroy(this)                          ! NEED THIS SUBROUTINE??
        implicit none
        class(levelpool_state_interface), intent(inout) :: this ! the type object being destroyed


    end subroutine levelpool_state_destroy


end module module_levelpool_state
