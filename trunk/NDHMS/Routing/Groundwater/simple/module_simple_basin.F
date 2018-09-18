module module_simple_basin
    use module_groundwater_base
    !use module_UDMAP
    implicit none

    !> type to contain information about the basins used in the simple groundwater scheme ground

    type, extends(groundwater_base_basin_struct) :: simple_basin_interface

        !> The local number of basins
        integer :: num_basins

        !> global number of basins
        integer :: global_num_basins

        !> basin area
        real, allocatable, dimension(:) :: basin_area

        !> basin index for tile
        integer, allocatable, dimension(:) :: basin_index

        !> Groundwater basin mask grid
        integer, allocatable, dimension(:,:) :: stream_mask

        !> Groundwater basin mask grid tile mapping index
        integer, allocatable, dimension(:,:) :: stream_mask_index

        !> Groundwater subbasin mask grid
        integer, allocatable, dimension(:,:) :: sub_basin_mask

        !> Count of grid pixels in each basin
        real, allocatable, dimension(:) :: basin_stream_pixels

    contains

        procedure :: init => simple_basin_init
        procedure :: destroy => simple_basin_destroy
    end type simple_basin_interface

contains

    !> Constructor for simple groundwater basin struct

    subroutine simple_basin_init(this,ix, jx, ixrt, jxrt, numbasins, gnumbasins )
        implicit none
        class(simple_basin_interface), intent(inout) :: this ! the type object being initialized
        integer, intent(in) :: ix           !< x dimension of the LSM Grid
        integer, intent(in) :: jx           !< y dimension of the LSM Grid
        integer, intent(in) :: ixrt         !< x dimension of the Routing Grid
        integer, intent(in) :: jxrt         !< x dimension of the LSM Grid
        integer, intent(in) :: numbasins    !< Local number of basins
        integer, intent(in) :: gnumbasins   !< global number of basins

        !class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        this%num_basins = numbasins
        this%global_num_basins = gnumbasins

        ! allocate basin area
        if ( .not. allocated(this%basin_area) ) then
            allocate(this%basin_area(numbasins))
        else
            allocation_error = .true.
        end if

        ! allocate sub_basin_mask
        if ( .not. allocated(this%sub_basin_mask) ) then
            allocate(this%sub_basin_mask(ix,jx))
            this%sub_basin_mask = 0
        else
            allocation_error = .true.
        end if

        ! allocate stream_mask
        if ( .not. allocated(this%stream_mask) ) then
            allocate(this%stream_mask(ixrt,jxrt))
            this%stream_mask = 0
        else
            allocation_error = .true.
        end if

        ! allocate stream_mask_index
        if ( .not. allocated(this%stream_mask_index) ) then
            allocate(this%stream_mask_index(ixrt,jxrt))
        else
            allocation_error = .true.
        end if

        ! allocate basin_stream_pixels
        if ( .not. allocated(this%basin_stream_pixels) ) then
            allocate(this%basin_stream_pixels(numbasins))
            this%basin_stream_pixels = 0.
        else
            allocation_error = .true.
        end if

        ! -- WARNING -- basin_index is not allocated here
        !it will be allocated by SIMP_GW_IND() which can not be called until after the
        !call too MPP_READ_SIMP_GW() or READ_SIMP_GW()

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of simple groundwater basin structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine simple_basin_init

    subroutine simple_basin_destroy(this)
        implicit none
        class(simple_basin_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.

        ! deallocate basin area
        if ( allocated(this%basin_area) ) then
            deallocate(this%basin_area)
        else
            allocation_error = .true.
        end if

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine simple_basin_destroy

      subroutine SIMP_GW_IND(ix,jx,GWSUBBASMSK,numbasns,gnumbasns,basnsInd)
! create an index of basin mask so that it is faster for parallel computation.
     implicit none
     integer, intent(in) ::  ix,jx
     integer, intent(in),dimension(ix,jx) ::  GWSUBBASMSK
     integer, intent(out):: gnumbasns
     integer, intent(inout):: numbasns
     integer, intent(inout),allocatable,dimension(:):: basnsInd

     integer,dimension(numbasns):: tmpbuf

     integer :: i,j,k


     gnumbasns = numbasns
     numbasns = 0
     tmpbuf = -999.

     do j = 1,jx
        do i = 1, ix
           if(GWSUBBASMSK(i,j) .gt.0) then
                tmpbuf(GWSUBBASMSK(i,j)) = GWSUBBASMSK(i,j)
           endif
        end do
     end do
     do k = 1, gnumbasns
         if(tmpbuf(k) .gt. 0) numbasns = numbasns + 1
     end do

     allocate(basnsInd(numbasns))

     i = 1
     do k = 1, gnumbasns
         if(tmpbuf(k) .gt. 0) then
             basnsInd(i) = tmpbuf(k)
             i = i + 1
         endif
     end do
#ifdef HYDRO_D
     write(6,*) "check numbasns, gnumbasns : ", numbasns, gnumbasns
#endif

     return
  end subroutine SIMP_GW_IND

end module module_simple_basin
