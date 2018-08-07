module module_nhd_basin
    use overland_data
    use module_groundwater_base
    !use module_UDMAP
    implicit none

    type, extends(groundwater_base_basin_struct) :: nhd_basin_interface

        ! the local number of basins
        integer :: num_basins

        ! the area of each basin
        real, allocatable, dimension(:) :: basin_area

        ! bucket mask for use with nhd plus stream network
        integer, allocatable, dimension(:) :: nhd_bucket_mask

        integer :: lnlinksl

        integer :: channel_bucket_only

        integer :: agg_factor

        real, allocatable, dimension(:,:) :: cell_area

        real, allocatable, dimension(:,:) :: cell_area_lsm

        ! guard variable for broken fortran pointers
        logical :: pointers_need_set = .true.

    contains

        procedure :: init => nhd_basin_init
        procedure :: destroy => nhd_basin_destroy
    end type nhd_basin_interface

contains

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
        this%agg_factor = aggfactrt

        ! set the number of local nhdplus links
        this%lnlinksl = num_local_links

        ! set the flag for channel bucket only
        this%channel_bucket_only = cb_only

        this%pointers_need_set = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine nhd_basin_init

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

        if ( allocated(this%cell_area) ) then
            deallocate(this%cell_area)
        else
            allocation_error = .true.
        end if

        if ( allocated(this%cell_area_lsm) ) then
            deallocate(this%cell_area_lsm)
        else
            allocation_error = .true.
        end if

        this%pointers_need_set = .true.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine nhd_basin_destroy

end module module_nhd_basin
