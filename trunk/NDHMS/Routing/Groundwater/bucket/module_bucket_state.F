!> @brief NHD state module for groundwater
!!
!! This module implements the data structures used to interact with the state variables
!! in the nhd implementation of an exponential bucket groundwater model

module module_bucket_state

    use module_groundwater_base
    implicit none

    !> type used to contain information used to interact with state variables for the modeled basins in the
    !! nhd implementation of an exponential bucket groundwater bucket model

    type, extends(groundwater_base_state_struct) :: bucket_state_interface

        !> The current water elevation in each groundwater basin modeled on the local process
        real, allocatable, dimension(:) :: z_gwsubbas

    contains

        procedure :: init => bucket_state_init
        procedure :: destroy => buket_state_destroy
    end type bucket_state_interface

contains

    !> Constructor for nhd state struct
    !! @params this The nhd state struct being allocated
    !! @params numbasins The number of local basins being modeled by this process

    subroutine bucket_state_init(this, numbasins)
        implicit none
        class(bucket_state_interface), intent(inout) :: this ! the type object being destroyed
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

    end subroutine bucket_state_init

    !> Destructor for nhd state struct
    !! @params this The nhd state struct being destroyed
    subroutine buket_state_destroy(this)
        implicit none
        class(bucket_state_interface), intent(inout) :: this ! the type object being destroyed

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

    end subroutine buket_state_destroy
end module module_bucket_state
