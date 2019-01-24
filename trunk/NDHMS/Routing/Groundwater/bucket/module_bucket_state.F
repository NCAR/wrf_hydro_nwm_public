!> @brief NHD state module for groundwater
!!
!! This module implements the data structures used to interact with the state variables
!! in the nhd implementation of an exponential bucket groundwater model

module module_bucket_state
  
  use module_groundwater_base
  implicit none

  interface bucket_state_interface
     procedure bucket_state_init
  end interface bucket_state_interface

  !> type used to contain information used to interact with state variables for the modeled basins in the
  !! nhd implementation of an exponential bucket groundwater bucket model

  type, extends(groundwater_base_state_struct) :: bucket_state_interface
     
     !> The current water elevation in each groundwater basin modeled on the local process
     real, allocatable, dimension(:) :: z_gwsubbas
     
   contains
     final :: bucket_state_destroy
     !procedure :: init => bucket_state_init
     !procedure :: destroy => buket_state_destroy
  end type bucket_state_interface
  
contains

  !> Constructor for nhd state struct
  !! @params this The nhd state struct being allocated
  !! @params numbasins The number of local basins being modeled by this process
  
  function bucket_state_init(numbasins) result(new_state)
    implicit none
    integer, intent(in) :: numbasins
    type(bucket_state_interface) :: new_state
    
    logical :: allocation_error = .false.
    
    ! allocate distance_to_neighbor lsm, NOT on the routing grid
    if ( .not. allocated(new_state%z_gwsubbas) ) then
       allocate(new_state%z_gwsubbas(numbasins))
       new_state%z_gwsubbas = 0.0
    else
       allocation_error = .true.
    end if
    
    if ( allocation_error ) &
         write(0,*) "attempt to allocate data in members of nhd groundwater state structure&
         &that where already allocated. The allocated members where not changed"
    
  end function bucket_state_init

    !> Destructor for nhd state struct
    !! @params this The nhd state struct being destroyed
  subroutine bucket_state_destroy(this)
    implicit none
    type(bucket_state_interface), intent(inout) :: this ! the type object being destroyed
    
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
    
  end subroutine bucket_state_destroy
end module module_bucket_state
