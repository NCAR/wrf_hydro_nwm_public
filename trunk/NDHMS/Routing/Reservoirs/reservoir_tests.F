program reservoir_unit_tests
    use module_levelpool_tests
    use module_persistence_levelpool_hybrid_tests

    implicit none

    logical :: rv1, rv2, rv3, rv4, rv2_LP, rv3_LP, rv4_LP          ! test result

    rv1 = .false.
    rv2 = .false.
    rv3 = .false.

    rv1 = test_levelpool()

    !rv2 = test_persistence_levelpool_hybrid2()

    !rv2_LP = test_levelpool2()


    !rv3 = test_persistence_levelpool_hybrid3()

    !rv3_LP = test_levelpool3()

    !rv4 = test_persistence_levelpool_hybrid4()

    !rv4_LP = test_levelpool4()


!    if (rv1 .and. rv2) then
    if (rv1) then
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
        !integer :: call_status = 0


        call_status = .false.

        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        call_status = levelpool_data_info(levelpool_reservoir_data)

        if (call_status) then
            rv = .true.
        end if

    end function test_levelpool



    !------------------------------------------------------------------------------!
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_persistence_levelpool_hybrid2() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow

        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0

timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.

        !allocate(inflow_array(108))


!inflow_array = (/91.7394, 92.15904, 92.1518,91.84663,91.38554,90.86131,90.32736,89.81273,89.3325,88.89427,88.5025,88.16228,87.41539,86.80043,86.03979,85.3849,85.33451,86.84274,91.6084,101.81398,118.85916,143.99232,177.7355,219.2348,267.22351,319.90402,374.54324,428.86066,480.92096,529.23584,572.77673,610.93237,643.4389,&
!670.28516,691.67767,707.96088,719.57312,726.96997,730.63269,731.03186,728.61438,723.79578,716.9549,708.43268,698.53247,687.52112,675.63123,663.06421,649.99976,636.57898,622.92926,609.1745,595.40369,581.68799,568.08588,554.64484,541.4032,528.39185,515.63513,503.14838,490.95123,479.05109,467.45493,456.16663,445.18753,434.51706,424.15311,414.0921,404.32956,394.86014,385.67789,376.77621,368.14966,359.78958,351.68875,343.83972,336.23505,328.86719,321.7287,314.81219,308.11047,301.61646,295.32312,289.22369,283.31207,277.5813,272.02521,266.63776,261.41315,256.34564,251.42978,246.66023,242.03192,237.53989,233.17944,228.94595,224.83511,220.84265,216.96449,213.19672,209.53554,205.97734,202.51857,199.1559,195.88605,192.70595,189.61255 /)


inflow_array = (/91.27196,&
91.7394,&
92.15904,&
92.1518,&
91.84663,&
91.38554,&
90.86131,&
90.32736,&
89.81273,&
89.3325,&
88.89427,&
88.5025,&
88.16228,&
87.41539,&
86.80043,&
86.03979,&
85.3849,&
85.33451,&
86.84274,&
91.6084,&
101.81398,&
118.85916,&
143.99232,&
177.7355,&
219.2348,&
267.22351,&
319.90402,&
374.54324,&
428.86066,&
480.92096,&
529.23584,&
572.77673,&
610.93237,&
643.4389,&
670.28516,&
691.67767,&
707.96088,&
719.57312,&
726.96997,&
730.63269,&
731.03186,&
728.61438,&
723.79578,&
716.9549,&
708.43268,&
698.53247,&
687.52112,&
675.63123,&
663.06421,&
649.99976,&
636.57898,&
622.92926,&
609.1745,&
595.40369,&
581.68799,&
568.08588,&
554.64484,&
541.4032,&
528.39185,&
515.63513,&
503.14838,&
490.95123,&
479.05109,&
467.45493,&
456.16663,&
445.18753,&
434.51706,&
424.15311,&
414.0921,&
404.32956,&
394.86014,&
385.67789,&
376.77621,&
368.14966,&
359.78958,&
351.68875,&
343.83972,&
336.23505,&
328.86719,&
321.7287,&
314.81219,&
308.11047,&
301.61646,&
295.32312,&
289.22369,&
283.31207,&
277.5813,&
272.02521,&
266.63776,&
261.41315,&
256.34564,&
251.42978,&
246.66023,&
242.03192,&
237.53989,&
233.17944,&
228.94595,&
224.83511,&
220.84265,&
216.96449,&
213.19672,&
209.53554,&
205.97734,&
202.51857,&
199.1559,&
195.88605,&
192.70595,&
189.61255 /)







!print *, inflow_array


        print *, "calling init for persistence_levelpool_hybrid_struct"
        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.509490013122558594e+01, 9.626000022888183238e+00 , &
        4.000000000000000222e-01, 1.000000000000000000e+01, 7.733333269755045869e+00, 1.000000000000000056e-01, 1.0, 9.960000038146972656e+00, 8.999999761581420898e-01, 166758723, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", "./Testing_Files/", 12, 1000000000)
        !"2010-10-01_07:00:00", "./Testing_Files/", 12, 86400)




        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", "", 12, 86400)


        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 9.73733330


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0)

            prev_time_inflow = inflow

            print *, 'outflow'
            print *, outflow

        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        if (rv1) then
            rv = .true.
        end if


    end function test_persistence_levelpool_hybrid2






    function test_levelpool2() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (levelpool_struct) :: levelpool_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow

        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0

timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.


inflow_array = (/91.27196,&
91.7394,&
92.15904,&
92.1518,&
91.84663,&
91.38554,&
90.86131,&
90.32736,&
89.81273,&
89.3325,&
88.89427,&
88.5025,&
88.16228,&
87.41539,&
86.80043,&
86.03979,&
85.3849,&
85.33451,&
86.84274,&
91.6084,&
101.81398,&
118.85916,&
143.99232,&
177.7355,&
219.2348,&
267.22351,&
319.90402,&
374.54324,&
428.86066,&
480.92096,&
529.23584,&
572.77673,&
610.93237,&
643.4389,&
670.28516,&
691.67767,&
707.96088,&
719.57312,&
726.96997,&
730.63269,&
731.03186,&
728.61438,&
723.79578,&
716.9549,&
708.43268,&
698.53247,&
687.52112,&
675.63123,&
663.06421,&
649.99976,&
636.57898,&
622.92926,&
609.1745,&
595.40369,&
581.68799,&
568.08588,&
554.64484,&
541.4032,&
528.39185,&
515.63513,&
503.14838,&
490.95123,&
479.05109,&
467.45493,&
456.16663,&
445.18753,&
434.51706,&
424.15311,&
414.0921,&
404.32956,&
394.86014,&
385.67789,&
376.77621,&
368.14966,&
359.78958,&
351.68875,&
343.83972,&
336.23505,&
328.86719,&
321.7287,&
314.81219,&
308.11047,&
301.61646,&
295.32312,&
289.22369,&
283.31207,&
277.5813,&
272.02521,&
266.63776,&
261.41315,&
256.34564,&
251.42978,&
246.66023,&
242.03192,&
237.53989,&
233.17944,&
228.94595,&
224.83511,&
220.84265,&
216.96449,&
213.19672,&
209.53554,&
205.97734,&
202.51857,&
199.1559,&
195.88605,&
192.70595,&
189.61255 /)



print *, inflow_array

        !print *, "calling init for persistence_levelpool_hybrid_struct"
        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.509490013122558594e+01, 9.626000022888183238e+00 , &
        !4.000000000000000222e-01, 1.000000000000000000e+01, 7.733333269755045869e+00, 1.000000000000000056e-01, 1.0, 9.960000038146972656e+00, 8.999999761581420898e-01, 166758723, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)

        call levelpool_reservoir_data%init(water_elevation, 1.509490013122558594e+01, 9.626000022888183238e+00 , &
        4.000000000000000222e-01, 1.000000000000000000e+01, 7.733333269755045869e+00, 1.000000000000000056e-01, 1.0, 9.960000038146972656e+00, 16944276)


        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)


        !print *, "testing data in persistence_levelpool_hybrid_struct"
        !rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        !print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 9.73733330


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call levelpool_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow

        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        !if (rv1) then
        !    rv = .true.
        !end if


    end function test_levelpool2














    function test_persistence_levelpool_hybrid3() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow


        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0


timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.


        !allocate(inflow_array(108))


!inflow_array = (/91.7394, 92.15904, 92.1518,91.84663,91.38554,90.86131,90.32736,89.81273,89.3325,88.89427,88.5025,88.16228,87.41539,86.80043,86.03979,85.3849,85.33451,86.84274,91.6084,101.81398,118.85916,143.99232,177.7355,219.2348,267.22351,319.90402,374.54324,428.86066,480.92096,529.23584,572.77673,610.93237,643.4389,&
!670.28516,691.67767,707.96088,719.57312,726.96997,730.63269,731.03186,728.61438,723.79578,716.9549,708.43268,698.53247,687.52112,675.63123,663.06421,649.99976,636.57898,622.92926,609.1745,595.40369,581.68799,568.08588,554.64484,541.4032,528.39185,515.63513,503.14838,490.95123,479.05109,467.45493,456.16663,445.18753,434.51706,424.15311,414.0921,404.32956,394.86014,385.67789,376.77621,368.14966,359.78958,351.68875,343.83972,336.23505,328.86719,321.7287,314.81219,308.11047,301.61646,295.32312,289.22369,283.31207,277.5813,272.02521,266.63776,261.41315,256.34564,251.42978,246.66023,242.03192,237.53989,233.17944,228.94595,224.83511,220.84265,216.96449,213.19672,209.53554,205.97734,202.51857,199.1559,195.88605,192.70595,189.61255 /)


inflow_array = (/16.41417,&
16.34501,&
16.27684,&
16.20959,&
16.14314,&
16.07742,&
16.01236,&
15.94794,&
15.88411,&
15.82085,&
15.75815,&
15.69599,&
15.63438,&
15.57332,&
15.51281,&
15.45285,&
15.39345,&
15.33467,&
15.27646,&
15.21883,&
15.16178,&
15.10531,&
15.04943,&
14.99415,&
14.93946,&
14.88537,&
14.83189,&
14.779,&
14.72671,&
14.67501,&
14.62391,&
14.57339,&
14.52347,&
14.47413,&
14.42537,&
14.37718,&
14.32957,&
14.28253,&
14.23605,&
14.19012,&
14.14475,&
14.09992,&
14.05562,&
14.01186,&
13.96862,&
13.92589,&
13.88368,&
13.84198,&
13.80077,&
13.76007,&
13.71986,&
13.68012,&
13.64086,&
13.60207,&
13.56374,&
13.52586,&
13.48843,&
13.45144,&
13.41489,&
13.37877,&
13.34307,&
13.30781,&
13.27295,&
13.23851,&
13.20446,&
13.17081,&
13.13755,&
13.10467,&
13.07217,&
13.04005,&
13.00829,&
12.9769,&
12.94586,&
12.91519,&
12.88487,&
12.85489,&
12.82525,&
12.79594,&
12.76696,&
12.73831,&
12.70997,&
12.68195,&
12.65424,&
12.62684,&
12.59974,&
12.57295,&
12.54646,&
12.52025,&
12.49433,&
12.46869,&
12.44333,&
12.41825,&
12.39343,&
12.36889,&
12.3446,&
12.32058,&
12.29681,&
12.27331,&
12.25006,&
12.22705,&
12.20428,&
12.18176,&
12.15947,&
12.13741,&
12.11559,&
12.09399,&
12.07262,&
12.05147 /)





!print *, inflow_array


        print *, "calling init for persistence_levelpool_hybrid_struct"
        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.306920051574707031e+00, 2.106899932861328182e+02 , &
        4.000000000000000222e-01, 1.000000000000000000e+01, 2.101233266194661553e+02, 1.000000000000000056e-01, 1.0, 2.107899932861328125e+02, 8.999999761581420898e-01, 10361596, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", ".", 12, 86400)




        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)


        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 210.723328


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow


        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        if (rv1) then
            rv = .true.
        end if


    end function test_persistence_levelpool_hybrid3




    function test_levelpool3() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (levelpool_struct) :: levelpool_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow

        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0

timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.


inflow_array = (/16.41417,&
16.34501,&
16.27684,&
16.20959,&
16.14314,&
16.07742,&
16.01236,&
15.94794,&
15.88411,&
15.82085,&
15.75815,&
15.69599,&
15.63438,&
15.57332,&
15.51281,&
15.45285,&
15.39345,&
15.33467,&
15.27646,&
15.21883,&
15.16178,&
15.10531,&
15.04943,&
14.99415,&
14.93946,&
14.88537,&
14.83189,&
14.779,&
14.72671,&
14.67501,&
14.62391,&
14.57339,&
14.52347,&
14.47413,&
14.42537,&
14.37718,&
14.32957,&
14.28253,&
14.23605,&
14.19012,&
14.14475,&
14.09992,&
14.05562,&
14.01186,&
13.96862,&
13.92589,&
13.88368,&
13.84198,&
13.80077,&
13.76007,&
13.71986,&
13.68012,&
13.64086,&
13.60207,&
13.56374,&
13.52586,&
13.48843,&
13.45144,&
13.41489,&
13.37877,&
13.34307,&
13.30781,&
13.27295,&
13.23851,&
13.20446,&
13.17081,&
13.13755,&
13.10467,&
13.07217,&
13.04005,&
13.00829,&
12.9769,&
12.94586,&
12.91519,&
12.88487,&
12.85489,&
12.82525,&
12.79594,&
12.76696,&
12.73831,&
12.70997,&
12.68195,&
12.65424,&
12.62684,&
12.59974,&
12.57295,&
12.54646,&
12.52025,&
12.49433,&
12.46869,&
12.44333,&
12.41825,&
12.39343,&
12.36889,&
12.3446,&
12.32058,&
12.29681,&
12.27331,&
12.25006,&
12.22705,&
12.20428,&
12.18176,&
12.15947,&
12.13741,&
12.11559,&
12.09399,&
12.07262,&
12.05147 /)



!print *, inflow_array

        !print *, "calling init for persistence_levelpool_hybrid_struct"
        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.509490013122558594e+01, 9.626000022888183238e+00 , &
        !4.000000000000000222e-01, 1.000000000000000000e+01, 7.733333269755045869e+00, 1.000000000000000056e-01, 1.0, 9.960000038146972656e+00, 8.999999761581420898e-01, 166758723, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)

        call levelpool_reservoir_data%init(water_elevation, 1.306920051574707031e+00, 2.106899932861328182e+02 , &
        4.000000000000000222e-01, 1.000000000000000000e+01, 2.101233266194661553e+02, 1.000000000000000056e-01, 1.0, 2.107899932861328125e+02, 14785127)




        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)


        !print *, "testing data in persistence_levelpool_hybrid_struct"
        !rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        !print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 210.723328


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call levelpool_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow

        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        !if (rv1) then
        !    rv = .true.
        !end if


    end function test_levelpool3

















!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





    function test_persistence_levelpool_hybrid4() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow


        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0


timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.


        !allocate(inflow_array(108))


!inflow_array = (/91.7394, 92.15904, 92.1518,91.84663,91.38554,90.86131,90.32736,89.81273,89.3325,88.89427,88.5025,88.16228,87.41539,86.80043,86.03979,85.3849,85.33451,86.84274,91.6084,101.81398,118.85916,143.99232,177.7355,219.2348,267.22351,319.90402,374.54324,428.86066,480.92096,529.23584,572.77673,610.93237,643.4389,&
!670.28516,691.67767,707.96088,719.57312,726.96997,730.63269,731.03186,728.61438,723.79578,716.9549,708.43268,698.53247,687.52112,675.63123,663.06421,649.99976,636.57898,622.92926,609.1745,595.40369,581.68799,568.08588,554.64484,541.4032,528.39185,515.63513,503.14838,490.95123,479.05109,467.45493,456.16663,445.18753,434.51706,424.15311,414.0921,404.32956,394.86014,385.67789,376.77621,368.14966,359.78958,351.68875,343.83972,336.23505,328.86719,321.7287,314.81219,308.11047,301.61646,295.32312,289.22369,283.31207,277.5813,272.02521,266.63776,261.41315,256.34564,251.42978,246.66023,242.03192,237.53989,233.17944,228.94595,224.83511,220.84265,216.96449,213.19672,209.53554,205.97734,202.51857,199.1559,195.88605,192.70595,189.61255 /)


inflow_array = (/154.6527557,&
154.6150208,&
154.5515594,&
154.4784851,&
154.401947,&
154.3236237,&
154.2500153,&
154.1869202,&
154.1379547,&
154.1045227,&
154.0855103,&
154.0786591,&
154.0811462,&
154.091156,&
154.1060638,&
154.124649,&
154.1455841,&
154.1681061,&
154.1914825,&
154.2156067,&
154.2402496,&
154.265274,&
154.290329,&
154.3154297,&
154.3404236,&
154.3651428,&
154.3896179,&
154.4137573,&
154.4375305,&
154.4608765,&
154.4837799,&
154.5061493,&
154.5280914,&
154.5495911,&
154.5706787,&
154.5909729,&
154.6102448,&
154.6286469,&
154.6462097,&
154.6630859,&
154.6792908,&
154.6948242,&
154.7098236,&
154.72435,&
154.7385406,&
154.7523041,&
154.7657166,&
154.7785797,&
154.7910004,&
154.8029327,&
154.8143463,&
154.8252411,&
154.8355865,&
154.845459,&
154.8547211,&
154.8634796,&
154.8718109,&
154.8796082,&
154.8869781,&
154.8937378,&
154.8998413,&
154.9054108,&
154.9104919,&
154.9155273,&
154.9200439,&
154.9244385,&
154.928833,&
154.9334259,&
154.9382172,&
154.9434357,&
154.9489899,&
154.9550781,&
154.9614105,&
154.9675446,&
154.9744415,&
154.9818573,&
154.9893341,&
154.996933,&
155.0048218,&
155.013092,&
155.0214996,&
155.029953,&
155.0383301,&
155.0466156,&
155.0545349,&
155.0620575,&
155.0691223,&
155.075882,&
155.0821075,&
155.0880585,&
155.093811,&
155.0994568,&
155.10495,&
155.1104279,&
155.1572266,&
155.4083099,&
155.5628357,&
155.5335083,&
155.5141449,&
155.5797882,&
155.7202911,&
155.786911,&
155.7349854,&
155.6275024,&
155.5153656,&
155.4232025,&
155.3576202,&
155.3176422 /)





!print *, inflow_array


        print *, "calling init for persistence_levelpool_hybrid_struct"
        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.306920051574707031e+00, 2.106899932861328182e+02 , &
        !4.000000000000000222e-01, 1.000000000000000000e+01, 2.101233266194661553e+02, 1.000000000000000056e-01, 1.0, 2.107899932861328125e+02, 8.999999761581420898e-01, 10361596, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)



        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 94.41929626, 763.92398682, &
        4.000000000000000222e-01, 1.000000000000000000e+01, 699.45998128, 1.000000000000000056e-01, 1.0, 775.29998779, 8.999999761581420898e-01, 10361596, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", ".", 12, 86400)



        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)


        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 767.716003


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call persistence_levelpool_hybrid_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 300.0)

            prev_time_inflow = inflow


        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        if (rv1) then
            rv = .true.
        end if


    end function test_persistence_levelpool_hybrid4




    function test_levelpool4() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (levelpool_struct) :: levelpool_reservoir_data

        real :: outflow, inflow

        real :: water_elevation

        real :: prev_time_inflow

        !real, allocatable, dimension(:) :: inflow_array

        !real, dimension(108) :: inflow_array

real, dimension(108) :: inflow_array

        integer :: timestep_count

prev_time_inflow = 0.0

timestep_count = 0

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.


inflow_array = (/154.6527557,&
154.6150208,&
154.5515594,&
154.4784851,&
154.401947,&
154.3236237,&
154.2500153,&
154.1869202,&
154.1379547,&
154.1045227,&
154.0855103,&
154.0786591,&
154.0811462,&
154.091156,&
154.1060638,&
154.124649,&
154.1455841,&
154.1681061,&
154.1914825,&
154.2156067,&
154.2402496,&
154.265274,&
154.290329,&
154.3154297,&
154.3404236,&
154.3651428,&
154.3896179,&
154.4137573,&
154.4375305,&
154.4608765,&
154.4837799,&
154.5061493,&
154.5280914,&
154.5495911,&
154.5706787,&
154.5909729,&
154.6102448,&
154.6286469,&
154.6462097,&
154.6630859,&
154.6792908,&
154.6948242,&
154.7098236,&
154.72435,&
154.7385406,&
154.7523041,&
154.7657166,&
154.7785797,&
154.7910004,&
154.8029327,&
154.8143463,&
154.8252411,&
154.8355865,&
154.845459,&
154.8547211,&
154.8634796,&
154.8718109,&
154.8796082,&
154.8869781,&
154.8937378,&
154.8998413,&
154.9054108,&
154.9104919,&
154.9155273,&
154.9200439,&
154.9244385,&
154.928833,&
154.9334259,&
154.9382172,&
154.9434357,&
154.9489899,&
154.9550781,&
154.9614105,&
154.9675446,&
154.9744415,&
154.9818573,&
154.9893341,&
154.996933,&
155.0048218,&
155.013092,&
155.0214996,&
155.029953,&
155.0383301,&
155.0466156,&
155.0545349,&
155.0620575,&
155.0691223,&
155.075882,&
155.0821075,&
155.0880585,&
155.093811,&
155.0994568,&
155.10495,&
155.1104279,&
155.1572266,&
155.4083099,&
155.5628357,&
155.5335083,&
155.5141449,&
155.5797882,&
155.7202911,&
155.786911,&
155.7349854,&
155.6275024,&
155.5153656,&
155.4232025,&
155.3576202,&
155.3176422 /)




!print *, inflow_array

        !print *, "calling init for persistence_levelpool_hybrid_struct"
        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 1.509490013122558594e+01, 9.626000022888183238e+00 , &
        !4.000000000000000222e-01, 1.000000000000000000e+01, 7.733333269755045869e+00, 1.000000000000000056e-01, 1.0, 9.960000038146972656e+00, 8.999999761581420898e-01, 166758723, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)

        call levelpool_reservoir_data%init(water_elevation, 94.41929626, 763.92398682, &
        4.000000000000000222e-01, 1.000000000000000000e+01, 699.45998128, 1.000000000000000056e-01, 1.0, 775.29998779, 22886855)




        !call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        !0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 16944276, "Testing_Files/hybrid_persistence_levelpool_ACF.nc", &
        !"2010-10-01_07:00:00", ".", 12, 86400)


        !print *, "testing data in persistence_levelpool_hybrid_struct"
        !rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        !print *, "calling release for persistence_levelpool_hybrid_struct"


        water_elevation = 260.5342102


        do timestep_count = 1, 108

            inflow = inflow_array(timestep_count)
            call levelpool_reservoir_data%run_release(inflow, &
            inflow, 0.0, water_elevation, outflow, 3600.0)

            prev_time_inflow = inflow

        end do

        !print *, "outflow"
        !print *, outflow

        !if (outflow == 120.170197) then
        !    rv3 = .true.
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Passed'
        !    print *, "========================================================================"
        !else
        !    print *, "========================================================================"
        !    print *, 'Persistence Levelpool Hybrid Release Test Failed'
        !    print *, 'Outflow should be 120.170197'
        !    print *, "========================================================================"
        !end if


        !if (rv1 .and. rv2 .and. rv3) then
        !    rv = .true.
        !end if

        !if (rv1) then
        !    rv = .true.
        !end if


    end function test_levelpool4


end program
