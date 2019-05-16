! This program unit tests the various reservoir implementations along with edge cases.
! It is important to run these unit tests whenever making any changes to code in this
! module to ensure nothing is broken. If nothing is broken, the user will see
! "All Reservoir Tests Passed". To compile and run these tests, go to the Reservoir
! directory in the terminal and type "make test". Then type "./reservoir_tests".

program reservoir_unit_tests
    use module_levelpool_tests
    use module_persistence_levelpool_hybrid_tests

    implicit none

    logical :: rv1 = .false.
    logical :: rv2 = .false.
    logical :: rv3 = .false.

    rv1 = .false.
    rv2 = .false.
    rv3 = .false.

    rv1 = test_levelpool()

    rv2 = test_levelpool_overflow_max_height()

    rv3 = test_persistence_levelpool_hybrid()

    if (rv1 .and. rv2 .and. rv3) then
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
        logical :: call_status

        type (levelpool_struct) :: levelpool_reservoir_data
        real :: water_elevation = 2.

        call_status = .false.

        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        call_status = levelpool_data_info(levelpool_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

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

        rv = .false.
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
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Failed'
            print *, 'Final outflow should be 189.612549'
            print *, "========================================================================"
        end if

    end function test_levelpool_overflow_max_height


    ! This tests the Persistence Levelpool Hybrid Module release function. It also reads the persistence
    ! parameters from the provided persistence parameter file and a gage discharge from the provided
    ! Timeslice file.
    function test_persistence_levelpool_hybrid() result(rv)

        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        real, dimension(120) :: inflow_array
        integer :: timestep_count

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv1 = .false.
        rv2 = .false.

        inflow_array = (/189.22899, 189.27005, 189.31049, 189.35042, 189.38965, 189.42819, 189.46588, 189.50273, &
        189.53859, 189.57346, 189.60719, 189.63979, 189.6711, 189.7011, 189.72968, &
        189.75679, 189.7823, 189.80617, 189.82822, 189.84842, 189.86653, 189.88255, 189.89622, &
        189.90752, 189.91612, 189.922, 189.92482, 189.92447, 189.92067, 189.91319, 189.90175, &
        189.88611, 189.86592, 189.84088, 189.81064, 189.77487, 189.73317, 189.6852, 189.63051, &
        189.56873, 189.49939, 189.42207, 189.33635, 189.24176, 189.13782, 189.02408, &
        188.90009, 188.76535, 188.61945, 188.46188, 188.29224, 188.11006, 187.91493, 187.70644, &
        187.48419, 187.24779, 186.9969, 186.73119, 186.45035, 186.15407, 185.84213, 185.51424, &
        185.17023, 184.80989, 184.43312, 184.03975, 183.62973, 183.20296, 182.75943, 182.29909, &
        181.82205, 181.32828, 180.81792, 80.29099, 179.74774, 179.1882, 178.61267, 178.02129, &
        177.41437, 176.79207, 176.15475, 175.50269, 174.83627, 174.15576, 173.46162, &
        172.75417, 172.03389, 171.3011, 170.55634, 169.79997, 169.03255, 168.25441, 167.46616, &
        166.66815, 165.86099, 165.04509, 164.22101, 163.38913, 162.55011, 161.70428, 160.85229, &
        159.99452, 159.13156, 158.26382, 157.39188, 156.51611, 155.63715, 154.75531, 153.8712, 152.98517, &
        152.09779, 151.2094, 150.32057, 149.43166, 148.54315, 147.6554, 146.76892, 145.88405, 145.00128, 144.12091/)


        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 166758723

        print *, "calling init for persistence_levelpool_hybrid_struct"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, initial_fractional_depth, lake_number, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", "./Testing_Files/", 12, 1000000000)

        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)

        print *, "calling release for persistence_levelpool_hybrid_struct"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do


        if (outflow == 1.81549609) then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Release Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Release Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, "========================================================================"
        end if

        if (rv1 .and. rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid

end program
