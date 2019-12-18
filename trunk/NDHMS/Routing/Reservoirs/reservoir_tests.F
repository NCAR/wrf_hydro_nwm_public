! This program unit tests the various reservoir implementations along with edge cases.
! It is important to run these unit tests whenever making any changes to code in this
! module to ensure nothing is broken. If nothing is broken, the user will see
! "All Reservoir Tests Passed". To compile and run these tests, go to the Reservoir
! directory in the terminal and type "make" and then "make test". Then type "./reservoir_tests".

program reservoir_unit_tests
    use module_levelpool_tests
    use module_persistence_levelpool_hybrid_tests
    use module_rfc_forecasts_tests

    implicit none

    logical :: rv1 = .false.
    logical :: rv2 = .false.
    logical :: rv3 = .false.
    logical :: rv4 = .false.
    logical :: rv5 = .false.
    logical :: rv6 = .false.
    logical :: rv7 = .false.
    logical :: rv8 = .false.
    logical :: rv9 = .false.
    logical :: rv10 = .false.


    real, dimension(120) :: inflow_array

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


    rv1 = test_levelpool()

    rv2 = test_levelpool_overflow_max_height()

    rv3 = test_persistence_levelpool_hybrid_usgs()

    rv4 = test_persistence_levelpool_hybrid_usace()

    rv5 = test_rfc_forecasts_object()

    rv6 = test_rfc_forecasts_time_series_object()

    rv7 = test_rfc_forecasts_levelpool_fallback()

    rv8 = test_rfc_forecasts_time_series_output_with_lookback_and_offset()

    rv9 = test_rfc_forecasts_time_series_output_with_missing_data()

    rv10 = test_rfc_forecasts_time_series_output_all_missing_data()


    if (rv1 .and. rv2 .and. rv3 .and. rv4 .and. rv5 .and. rv6 .and. rv7 .and. rv8 .and. rv9 .and. rv10) then
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

        type (levelpool) :: levelpool_reservoir_data
        real :: water_elevation = 2.

        call_status = .false.

        print *, "calling init for levelpool"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 11., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool"
        call_status = levelpool_data_info(levelpool_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_levelpool


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function for USGS reservoirs.
    ! It also reads the USGS persistence parameters from the provided reservoir parameter file and
    ! a gage discharge from the provided USGS Timeslice file.
    function test_persistence_levelpool_hybrid_usgs() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number, reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv1 = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 402142
        reservoir_type = 2

        print *, "calling init for persistence_levelpool_hybrid USGS type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, initial_fractional_depth, lake_number, reservoir_type, "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2010-10-01_07:00:00", cwd_full, 12, 1000000000)

        print *, "testing data in persistence_levelpool_hybrid"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)

        print *, "calling reservoir run for persistence_levelpool_hybrid USGS type reservoir"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 13.73367 - epsilon(13.73367) .and. outflow .le. 13.73367 + epsilon(13.73367) ) then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USGS Reservoir Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USGS reservoir Test Failed'
            print *, 'Outflow should be 13.7336712'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv1 .and. rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usgs


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function for U.S. Army Corps of Engineers (USACE)
    ! type reservoirs. It also reads the USACE persistence parameters from the provided reservoir parameter file
    ! and a gage discharge from the provided USACE Timeslice file.
    function test_persistence_levelpool_hybrid_usace() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number, reservoir_type
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coeffecient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 4672717
        reservoir_type = 3

        print *, "calling init for persistence_levelpool_hybrid USACE type reservoir"

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, initial_fractional_depth, lake_number, reservoir_type, "../../../../tests/local/reservoir_testing_files/reservoir_index_short_range.nc", &
        "2016-10-10_02:00:00", cwd_full, 12, 1000000000)

        print *, "calling reservoir run for persistence_levelpool_hybrid for USACE type reservoir"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 16.0820713 - epsilon(16.0820713) .and. outflow .le. 16.0820713 + epsilon(16.0820713) ) then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run USACE Reservoir Test Failed'
            print *, 'Outflow should be 16.08207'
            print *, "========================================================================"
            print *, outflow
        end if

        if (rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid_usace


    ! This tests the reservoir function of the level pool module under the specific condition
    ! where the water elevation reaches the max height.
    function test_levelpool_overflow_max_height() result(rv)

        implicit none
        logical rv                       ! test result
        type (levelpool) :: levelpool_reservoir_data
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation
        real :: orifice_coefficient, orifice_area, max_depth
        integer :: lake_number
        real :: inflow, prev_time_inflow, outflow, water_elevation
        integer :: timestep_count

        rv = .false.
        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0

        lake_area = 1.509490013122558594e+01
        weir_elevation = 9.626000022888183238e+00
        weir_coefficient = 0.4
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 7.733333269755045869e+00
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 9.960000038146972656e+00
        lake_number = 16944276

        call levelpool_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coefficient, weir_length, dam_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, lake_number)

        water_elevation = 9.73733330

        print *, "outflow"

        do timestep_count = 1, 108
            inflow = inflow_array(timestep_count)
            call levelpool_reservoir_data%run(inflow, inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow
            print *, outflow
        end do

        if (outflow .ge. 17.0451450 - epsilon(17.0451450) .and. outflow .le. 17.0451450 + epsilon(17.0451450)) then
            rv = .true.
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Levelpool Overflow Max Height Test Failed'
            print *, 'Final outflow should be 17.0451450'
            print *, "========================================================================"
        end if

    end function test_levelpool_overflow_max_height


    function test_rfc_forecasts_object() result(rv)
        implicit none

        logical rv                        ! test result
        logical :: call_status

        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        type (time_series_data_type) :: time_series_data
        character*256 :: cwd_full

        real :: water_elevation = 2.

        cwd_full = "./Testing_Files/"

        call_status = .false.

        print *, "calling init for rfc_forecasts"
        call rfc_forecasts_reservoir_data%init(water_elevation, 4., 6., 8., 10., 11., 12., 14., 16., 18., 0.9, 3745478, &
        "Testing_Files/reservoir_index_usgs_rfc_usace.nc", "2019-08-18_07:00:00", cwd_full, 24)

        print *, "testing data in rfc_forecasts"

        call_status = rfc_forecasts_data_info(rfc_forecasts_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_rfc_forecasts_object


    function test_rfc_forecasts_time_series_object() result(rv)
        implicit none

        logical rv                        ! test result
        logical :: call_status
        type (time_series_data_type) :: time_series_data, time_series_data_second
        character*256 :: cwd_full
        integer :: lookback_seconds, forecast_counts, time_step_seconds
        real, allocatable, dimension(:) :: forecast_discharges
        logical :: forecast_found

        call_status = .false.

        cwd_full = "./Testing_Files/"

        forecast_found = .false.

        call time_series_data%init("2019-08-18_07:00:00", cwd_full, 24, "CCHC1", lookback_seconds, forecast_counts, time_step_seconds, forecast_discharges, forecast_found)

        print *, "Checking reading of time series file"
        if ( forecast_discharges(51) .ge. 0.368119 - epsilon(0.368119) .and. forecast_discharges(51) .le. 0.368119 + epsilon(0.368119) ) then
            print *, "========================================================================"
            print *, "All RFC Forecast Reservoir Time Series Object Tests Passed"
            print *, "========================================================================"
            rv = .true.
        else
            print *, "========================================================================"
            print *, "Not All RFC Forecast Reservoir Time Series Object Tests Passed"
            print *, "========================================================================"
        end if

    end function test_rfc_forecasts_time_series_object


    function test_rfc_forecasts_levelpool_fallback() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.
        rv2 = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 3745478

        cwd_full = "../../../../tests/local/reservoir_testing_files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, 3745478, &
        "Testing_Files/reservoir_index_usgs_rfc_usace.nc", "2019-05-01_22:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) ) then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Levelpool Fallback Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Levelpool Fallback Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_levelpool_fallback


    function test_rfc_forecasts_time_series_output_with_lookback_and_offset() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 3745478

        cwd_full = "./Testing_Files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, 17609317, &
        "Testing_Files/reservoir_index_usgs_rfc_usace.nc", "2019-08-18_09:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 6.368119 - epsilon(6.368119) .and. outflow .le. 6.368119 + epsilon(6.368119) ) then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Test Failed'
            print *, 'Outflow should be 6.368119'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_time_series_output_with_lookback_and_offset


    function test_rfc_forecasts_time_series_output_with_missing_data() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 3745478

        cwd_full = "./Testing_Files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, 17609317, &
        "Testing_Files/reservoir_index_usgs_rfc_usace.nc", "2019-09-18_09:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 1.368119 - epsilon(1.368119) .and. outflow .le. 1.368119 + epsilon(1.368119) ) then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Missing Data Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output Missing Data Test Failed'
            print *, 'Outflow should be 6.368119'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_time_series_output_with_missing_data


    function test_rfc_forecasts_time_series_output_all_missing_data() result(rv)
        implicit none
        logical rv, rv1, rv2                        ! test result
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coefficient
        real :: weir_length, dam_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        integer :: timestep_count
        character*256 :: cwd_full

        prev_time_inflow = 0.0
        timestep_count = 0
        water_elevation = 0.0
        rv = .false.

        lake_area = 2.096320037841796875e+02
        weir_elevation = 1.332074047851562455e+03
        weir_coefficient = 4.000000000000000222e-01
        weir_length = 1.000000000000000000e+01
        dam_length = 10.0
        orifice_elevation = 1.314473347981770758e+03
        orifice_coefficient = 1.000000000000000056e-01
        orifice_area = 1.0
        max_depth = 1.335180053710937500e+03
        initial_fractional_depth = 8.999999761581420898e-01
        lake_number = 3745478

        cwd_full = "./Testing_Files/"

        call rfc_forecasts_reservoir_data%init(water_elevation, lake_area, weir_elevation, weir_coefficient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, 0.9, 17609317, &
        "Testing_Files/reservoir_index_usgs_rfc_usace.nc", "2019-10-18_09:00:00", cwd_full, 24)

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call rfc_forecasts_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do

        if (outflow .ge. 1.81549609 - epsilon(1.81549609) .and. outflow .le. 1.81549609 + epsilon(1.81549609) ) then
            rv = .true.
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output All Missing Data Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'RFC Forecasts Time Series Output All Missing Data Test Failed'
            print *, 'Outflow should be 1.81549609'
            print *, "========================================================================"

        end if

    end function test_rfc_forecasts_time_series_output_all_missing_data


end program
