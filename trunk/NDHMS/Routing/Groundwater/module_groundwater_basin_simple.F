module module_groundwater_basin_simple
    use overland_data
    implicit none

    type groundwater_simple_basin_interface

        ! the global number of basins
        integer :: global_num_basins

        ! the local number of basins
        integer :: local_num_basins

        ! the area of each basin
        real, allocatable, dimension(:) :: basin_area

        ! basin index for tile
        integer, allocatable, dimension(:) :: basin_index

        ! basin mask grid
        integer, allocatable, dimension(:,:) :: stream_mask

        ! basin mask grid tile mapping index
        integer, allocatable, dimension(:,:) :: stream_mask_index

        ! basin mask grid (duplicate description needs clarification)
        integer, allocatable, dimension(:,:) :: sub_basin_mask

        ! (*guess) number of stream pixels in each basin, need to know if this is for the routing grid or lsm grid
        real, allocatable, dimension(:) :: basin_stream_pixels

        ! guard variable for broken fortran pointers
        logical :: pointers_need_set = .true.

    contains

        procedure :: init => groundwater_basins_init
        procedure :: destroy => groundwater_basins_destroy
    end type groundwater_simple_basin_interface

contains

    subroutine groundwater_basins_init(this,ix_lsm,jx_lsm,numbasins,gnumbasins, overland_data)
        implicit none
        class(groundwater_simple_basin_interface), intent(inout) :: this ! the type object being initalized
        integer, intent(in) :: ix_lsm                     ! x grid size
        integer, intent(in) :: jx_lsm                     ! y grid size
        integer, intent(in) :: numbasins                  ! number of basins
        integer, intent(in) :: global_numbasins                  ! number of basins
        class(overland_struct), intent(inout) :: overland_data

        logical :: allocation_error = .false.

        this%local_num_basins = numbasins
        this%global_num_basins = gnumbasins

        ! allocate basin area
        if ( .not. allocated(this%basin_area) ) then
            allocate(this%basin_area(numbasins))
        else
            allocation_error = .true.
        end if

        ! allocate the sub basin mask
        if ( .not. allocated(this%subbasin_mask) ) then
            allocate(this%subbasin_mask(ix_lsm,jx_lsm))
            this%subbasin_mask = 0
        else
            allocation_error = .true.
        end if

        ! allocate the basin index
        if ( .not. allocated(this%basin_index) ) then
            ! basin index is created with SIMP_GW_IND

            call SIMP_GW_IND(ix_lsm,jx_lsm,this%sub_basin_mask,this%local_num_basins,this%global_num_basins,this%basin_index)
        else
            allocation_error = .true.
        end if
        ! allocate basin index
        ! TODO Move basin_index initialization into this class from procedure SIMP_GW_IND

        this%pointers_need_set = .false.

        if ( allocation_error ) &
            write(0,*) "attempt to allocate data in members of groundwater properties structure&
            &that where already allocated. The allocated members where not changed"

    end subroutine groundwater_basins_init

    subroutine groundwater_basins_destroy(this)
        implicit none
        class(groundwater_simple_basin_interface), intent(inout) :: this ! the type object being destroyed

        logical :: allocation_error = .false.


        this%pointers_need_set = .true.

        if ( allocation_error ) &
            write(0,*) "attempt to deallocate data in members of groundwater properties structure&
            &that where not allocated. The unallocated members where not changed"

    end subroutine groundwater_basins_destroy

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

end module module_groundwater_basin_simple
