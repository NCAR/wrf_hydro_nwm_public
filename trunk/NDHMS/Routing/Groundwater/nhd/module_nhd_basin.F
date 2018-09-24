!> @brief NHD basin module for groundwater
!!
!! This module implements the data structures used to interact with the basin data
!! in the exponential bucket groundwater model for nhd stream network

module module_nhd_basin

    use module_groundwater_base
    !use module_UDMAP
    implicit none

    !> type used to contain infomation used to interact with modeled basins in the
    !! nhd groundwater bucket model

    type, extends(groundwater_base_basin_struct) :: nhd_basin_interface

        !> the local number of basins
        integer :: num_basins

        !> the area of each basin
        real, allocatable, dimension(:) :: basin_area

        !> bucket mask for use with nhd plus stream network
        integer, allocatable, dimension(:) :: nhd_bucket_mask

        !> number of nhd links !FIX ME!
        integer :: lnlinksl

        !> flag indicating if inflow comes from the overland and or subsurface routing grid or is sent directly
        integer :: channel_bucket_only

        !> the aggregation factor from the routing to LSM domains
        !integer :: agg_factor

        !> guard variable for to know if fortran pointers have been allocated
        logical, private :: pointers_need_set = .true.

    contains

        procedure :: init => nhd_basin_init
        procedure :: destroy => nhd_basin_destroy
    end type nhd_basin_interface

contains

    !> Constructor for the nhd basin type
    !! @param this the nhd basin object being initalized
    !! @param ixrt the x size of the routing grid
    !! @param jxrt the y size of the routing grid
    !! @param numbasins the number of local basins
    !! @param num_local_links the number of local nhd links
    !! @param cb_only flag indicating if physics will run in channel bucket only mode
    !! @param aggfactrt the aggregation factor used to move from routing to LSM domain

    subroutine nhd_basin_init(this,ixrt, jxrt, numbasins, num_local_links, cb_only, aggfactrt )
        implicit none
        class(nhd_basin_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ixrt
        integer, intent(in) :: jxrt
        integer, intent(in) :: numbasins                  ! number of basins
        integer, intent(in) :: num_local_links
        integer, intent(in) :: cb_only                    ! flag corresponding with namelist channelBucket_only flag
        integer, intent(in) :: aggfactrt
        !class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        this%num_basins = numbasins

        ! allocate basin area
        if ( .not. allocated(this%basin_area) ) then
            allocate(this%basin_area(numbasins))
        else
            allocation_error = .true.
        end if

        !allocate nhd_bucket_mask
        if ( .not. allocated(this%nhd_bucket_mask) ) then
            allocate (this%nhd_bucket_mask(numbasins))
        else
            allocation_error = .true.
        end if

        ! set the flag for aggfactrt
        !this%agg_factor = aggfactrt

        ! set the number of local nhdplus links
        this%lnlinksl = num_local_links

        ! set the flag for channel bucket only
        this%channel_bucket_only = cb_only

        this%pointers_need_set = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of nhd groundwater basin structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine nhd_basin_init

    !> Destructor for the nhd basin type
    !! @param this The nhd basin object being destroyed

    subroutine nhd_basin_destroy(this)
        implicit none
        class(nhd_basin_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! deallocate basin area
        if ( allocated(this%basin_area) ) then
            deallocate(this%basin_area)
        else
            allocation_error = .true.
        end if

        ! deallocate nhd_bucket_mask
        if ( allocated(this%nhd_bucket_mask) ) then
            deallocate(this%nhd_bucket_mask)
        else
            allocation_error = .true.
        end if


        this%pointers_need_set = .true.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine nhd_basin_destroy

end module module_nhd_basin
