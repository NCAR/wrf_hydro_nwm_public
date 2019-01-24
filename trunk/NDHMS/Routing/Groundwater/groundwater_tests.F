program groundwater_unit_tests
    use nhd_groundwater_tests
    use module_nhd_data

    implicit none

    logical :: rv = .false.

    rv = test_nhd_data(100,100,400,400,1024)

    contains

    !-----------------------------------------------------------------------------!
    !                              test_nhd_data()                                !
    ! this function verifies that the constructor for the nhd data type correctly !
    ! initializes all data members                                                !
    !-----------------------------------------------------------------------------!

    function test_nhd_data(ix,jx,ixrt,jxrt,numbasins) result(rv)
        implicit none
        integer, intent(in) :: ix         ! x dimension of the landsurface grid
        integer, intent(in) :: jx         ! y dimension of the landsurface grid
        integer, intent(in) :: ixrt       ! x dimension of the routing grid
        integer, intent(in) :: jxrt       ! y dimension of the routing grid
        integer, intent(in) :: numbasins  ! number of basins
        logical rv                        ! test result

        type (nhd_groundwater_struct) :: nhd_gw_data
        real, allocatable, dimension(:,:) :: cell_area
        integer :: call_status = 0

        print *, "Allocating cell area"
        allocate(cell_area(ixrt,jxrt))

        print *, "calling init for nhd_groundwater_struct"
        call nhd_gw_data%init(ix,jx,ixrt,jxrt,3600.,5,5,numbasins,numbasins,0,4,cell_area)

        print *, "testing data in nhd_groundwater_struct"
        call_status = nhd_data_info(nhd_gw_data)

        print *, "Deallocating cell area"
        deallocate(cell_area)

    end function test_nhd_data


end program
