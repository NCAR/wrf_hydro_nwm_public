program reservoir_unit_tests
    use module_levelpool_tests
    use module_levelpool

    implicit none

    logical :: rv = .false.
    logical :: rv2 = .false.

    rv = test_levelpool()

    rv2 = test_levelpool_overflow_max_height()

    if (rv .and. rv2) then
        print *, "========================================================================"
        print *, 'All Reservoir Tests Passed'
        print *, "========================================================================"

    else
        print *, "========================================================================"
        print *, 'Not All Reservoir Tests Passed'
        print *, "========================================================================"
    end if


    contains

    !------------------------------------------------------------------------------!
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_levelpool() result(rv)
        implicit none

        logical rv                        ! test result

        type (levelpool_struct) :: levelpool_reservoir_data
        real :: water_elevation = 2.
        integer :: call_status = 0

        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        call_status = levelpool_data_info(levelpool_reservoir_data)

    end function test_levelpool


    ! This tests the release function of the level pool module under the specific condition
    ! where the water elevation reaches the max height.
    function test_levelpool_overflow_max_height() result(rv)

        implicit none
        logical rv                       ! test result
        type (levelpool_struct) :: levelpool_reservoir_data
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth
        integer :: lake_number
        real :: inflow, prev_time_inflow, outflow, water_elevation
        real, dimension(108) :: inflow_array
        integer :: timestep_count

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0

        lake_area = 1.509490013122558594e+01
        weir_elevation = 9.626000022888183238e+00
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        orifice_elevation = 7.733333269755045869e+00
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 9.960000038146972656e+00
        lake_number = 16944276

        inflow_array = (/91.27196, 91.7394, 92.15904, 92.1518, 91.84663, &
        91.38554, 90.86131, 90.32736, 89.81273, 89.3325, 88.89427, 88.5025, 88.16228, &
        87.41539, 86.80043, 86.03979, 85.3849, 85.33451, 86.84274, 91.6084, 101.81398, &
        118.85916, 143.99232, 177.7355, 219.2348, 267.22351, 319.90402, 374.54324, 428.86066, &
        480.92096, 529.23584, 572.77673, 610.93237, 643.4389, 670.28516, 691.67767, 707.96088, &
        719.57312, 726.96997, 730.63269, 731.03186, 728.61438, 723.79578, 716.9549, 708.43268, &
        698.53247, 687.52112, 675.63123, 663.06421, 649.99976, 636.57898, 622.92926, 609.1745, &
        595.40369, 581.68799, 568.08588, 554.64484, 541.4032, 528.39185, 515.63513, 503.14838, &
        490.95123, 479.05109, 467.45493, 456.16663, 445.18753, 434.51706, 424.15311,414.0921, &
        404.32956, 394.86014, 385.67789, 376.77621, 368.14966, 359.78958, 351.68875, 343.83972, &
        336.23505, 328.86719, 321.7287, 314.81219, 308.11047, 301.61646, 295.32312, 289.22369, &
        283.31207, 277.5813, 272.02521, 266.63776, 261.41315, 256.34564, 251.42978, 246.66023, &
        242.03192, 237.53989, 233.17944, 228.94595, 224.83511, 220.84265, 216.96449, 213.19672, &
        209.53554, 205.97734, 202.51857, 199.1559, 195.88605, 192.70595, 189.61255 /)

        call levelpool_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, lake_number)

        water_elevation = 9.73733330

        print *, "outflow"

        do timestep_count = 1, 108
            inflow = inflow_array(timestep_count)
            call levelpool_reservoir_data%run_release(inflow, inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow
            print *, outflow
        end do

        if (outflow == 189.612549) then
            rv = .true.
        end if

    end function test_levelpool_overflow_max_height


end program
