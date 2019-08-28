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
    logical :: rv3 = .false.
    logical :: rv4 = .false.
    logical :: rv5 = .false.

    rv1 = test_levelpool()

    rv3 = test_persistence_levelpool_hybrid()

    rv4 = test_rfc_forecasts_object()

    rv5 = test_rfc_forecasts_time_series_object()

    if (rv1 .and. rv3 .and. rv4 .and. rv5) then
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
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool"
        call_status = levelpool_data_info(levelpool_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_levelpool


    ! This tests the Persistence Levelpool Hybrid Module run reservoir function. It also reads the persistence
    ! parameters from the provided persistence parameter file and a gage discharge from the provided
    ! Timeslice file.
    function test_persistence_levelpool_hybrid() result(rv)

        implicit none
        logical rv, rv1, rv2                        ! test result
        type (persistence_levelpool_hybrid) :: persistence_levelpool_hybrid_reservoir_data
        real :: outflow, inflow
        real :: water_elevation
        real :: prev_time_inflow
        real :: lake_area, weir_elevation, weir_coeffecient
        real :: weir_length, orifice_elevation, orifice_coefficient
        real :: orifice_area, max_depth, initial_fractional_depth
        integer :: lake_number
        real, dimension(120) :: inflow_array
        integer :: timestep_count
        character*256 :: cwd_full

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

        print *, "calling init for persistence_levelpool_hybrid"

        cwd_full = "./Testing_Files/"

        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, lake_area, weir_elevation, &
        weir_coeffecient, weir_length, orifice_elevation, orifice_coefficient, orifice_area, max_depth, initial_fractional_depth, lake_number, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", cwd_full, 12, 1000000000)

        print *, "testing data in persistence_levelpool_hybrid"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)

        print *, "calling reservoir run for persistence_levelpool_hybrid"

        water_elevation = 1331.18005

        do timestep_count = 1, 120

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run(inflow, &
            inflow, 0.0, water_elevation, outflow, 900.0)

            prev_time_inflow = inflow

            print *, outflow
        end do


        if (outflow == 7.14947987) then
            rv2 = .true.
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run Reservoir Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Persistence Levelpool Hybrid Run reservoir Test Failed'
            print *, 'Outflow should be 7.14947987'
            print *, "========================================================================"
        end if

        if (rv1 .and. rv2) then
            rv = .true.
        end if

    end function test_persistence_levelpool_hybrid


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
        call rfc_forecasts_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 0.9, 3745478, &
        "Testing_Files/hybrid_persistence_levelpool_reservoir_index_usgs_rfc.nc", "2019-05-18_22:00:00", cwd_full, 24)

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

        call_status = .false.

        cwd_full = "./Testing_Files/"

        call time_series_data%init("2019-05-18_22:00:00", cwd_full, 24, "DIEA4")

        call_status = time_series_data_info(time_series_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_rfc_forecasts_time_series_object

end program
