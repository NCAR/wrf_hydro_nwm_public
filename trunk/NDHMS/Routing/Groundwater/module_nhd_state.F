module module_nhd_state

    use module_groundwater_base
    implicit none

    type, extends(groundwater_base_state_struct) :: nhd_state_interface
        real, allocatable, dimension(:) :: z_gwsubbas

    contains

        procedure :: init => nhd_state_init
        procedure :: destroy => nhd_state_destroy
    end type nhd_state_interface

contains

    ! allocate groundwater state variables
    subroutine nhd_state_init(this, numbasins)
        implicit none
        class(nhd_state_interface), intent(inout) :: this ! the type object being destroyed
        integer, intent(in) :: numbasins

        logical :: allocation_error = .false.

        ! allocate distance_to_neighbor lsm, NOT on the routing grid
        if ( .not. allocated(this%z_gwsubbas) ) then
            allocate(this%z_gwsubbas(numbasins))
            this%z_gwsubbas = 0.0
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of nhd groundwater state structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine nhd_state_init

    ! deallocate groundwater state variables
    subroutine nhd_state_destroy(this)
        implicit none
        class(nhd_state_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! allocate distance_to_neighbor lsm, NOT on the routing grid
        if ( allocated(this%z_gwsubbas) ) then
            deallocate(this%z_gwsubbas)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of nhd groundwater state structure&
            &that where not already allocated. The unallocated members where not changed"

    end subroutine nhd_state_destroy
end module module_nhd_state
