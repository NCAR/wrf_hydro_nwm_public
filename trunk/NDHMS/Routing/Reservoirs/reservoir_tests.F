program reservoir_unit_tests
    use module_levelpool_tests
    use module_levelpool
    use module_machine_learning_tests
    use module_machine_learning

    implicit none

    logical rv1, rv2, rv3, rv4          ! test result

    rv1 = .false.
    rv2 = .false.
    rv3 = .false.
    rv4 = .false.


    rv1 = test_levelpool()

    rv2 = test_machine_learning()

    !rv3 = test_machine_learning_dates()

    rv4 = test_machine_learning_boundary_conditions()

    if (rv1 .and. rv2 .and. rv3) then
        print *, "========================================================================"
        print *, 'All tests passed'
        print *, "========================================================================"

    else
        print *, "========================================================================"
        print *, 'Not all tests passed'
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

        rv = .false.

        print *, "calling init for levelpool_struct"
        call levelpool_reservoir_data%init(water_elevation, 4., 6., 8., 10., 12., 14., 16., 18., 20)

        print *, "testing data in levelpool_struct"
        rv = levelpool_data_info(levelpool_reservoir_data)

    end function test_levelpool



    !------------------------------------------------------------------------------!
    !                              test_machine_learning()                                !
    ! this function verifies that the constructor for the machine_learning type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_machine_learning() result(rv)
        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (machine_learning_struct) :: machine_learning_reservoir_data

        type (machine_learning_model_struct) :: machine_learning_model_data

        real :: outflow, water_elevation

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.

        print *, "calling init for machine_learning_struct"
        call machine_learning_reservoir_data%init(water_elevation, 0.0, "2017-09-29_17:24:22" , 1, 166758723)


        print *, "calling init for machine_learning_model_struct"
        call machine_learning_model_data%init(2)


        print *, "testing data in machine_learning_struct"
        rv1 = machine_learning_data_info(machine_learning_reservoir_data)

        print *, "testing data in machine_learning_model_struct"
        rv2 = machine_learning_model_data_info(machine_learning_model_data)

        print *, "calling release for machine_learning_struct"

        call machine_learning_reservoir_data%run_release(0.0, &
        192.46272, 0.0, water_elevation, outflow, 86400.0)

        print *, "outflow"
        print *, outflow

        if (outflow == 120.377434) then
            rv3 = .true.
            print *, "========================================================================"
            print *, 'Machine Learning Release Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Machine Learning Release Test Failed'
            print *, 'Outflow should be 120.377434'
            print *, "========================================================================"
        end if


        if (rv1 .and. rv2 .and. rv3) then
            rv = .true.
        end if

    end function test_machine_learning



    !------------------------------------------------------------------------------!
    !                       test_test_machine_learning_dates()                                !
    ! This tests multi-year day tracking including switching to a new year
    ! and accounting for a leap year. This runs 15 minute timesteps from
    ! Nov 29, 2015 to Mar 4, 2018.                                              !
    !------------------------------------------------------------------------------!
    function test_machine_learning_dates() result(rv)
        implicit none

        logical rv                        ! test result

        type (machine_learning_struct) :: machine_learning_reservoir_data

        type (machine_learning_model_struct) :: machine_learning_model_data

        real :: outflow, water_elevation

        integer :: timestep_count

        rv = .false.

        water_elevation = 0.0

        timestep_count = 0

        outflow = 120.0

        print *, "calling init for machine_learning_struct"
        call machine_learning_reservoir_data%init(water_elevation, 0.0, "2015-11-29_05:24:22" , 1, 166758723)

        print *, "calling init for machine_learning_model_struct"
        call machine_learning_model_data%init(2)

        print *, '----------------------------------------------------'
        print *, "Paused for a few seconds for multi-year test..."
        do timestep_count = 1, 79296
            call machine_learning_reservoir_data%run_release(0.0, &
            outflow, 0.0, water_elevation, outflow, 900.0)
        end do

        if (machine_learning_reservoir_data%state%current_day_of_year == 63 .and. &
        machine_learning_reservoir_data%state%current_year == 2018 ) then
            rv = .true.
            print *, "========================================================================"
            print *, 'Machine Learning Dates Test Passed'
            print *, "========================================================================"
        else
            print *, "========================================================================"
            print *, 'Machine Learning Dates Test Failed'
            print *, 'Current Day of Year', machine_learning_reservoir_data%state%time_interval
            print *, 'Current Year', machine_learning_reservoir_data%state%current_day_of_year
            print *, "========================================================================"
        endif

    end function test_machine_learning_dates


!make arrays of inflows w/ negatives and other out of range. Set routing period to 300 seconds.
! Assert equal to array of outflows. Test new year, and leap year after february


    !TEST FOR NEGATVIE INFLOW AND THINGS THAT OUTPUT THE PRE/POST COND WARNS/MODS FOR TIMESTEP/UPDATE


    !------------------------------------------------------------------------------!
    !                   test_machine_learning_boundary_conditions()                                !
    ! this function verifies that the constructor for the machine_learning type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_machine_learning_boundary_conditions() result(rv)
        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (machine_learning_struct) :: machine_learning_reservoir_data

        type (machine_learning_model_struct) :: machine_learning_model_data

        real :: inflow, outflow, water_elevation

        integer :: timestep_count

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.

        print *, "calling init for machine_learning_struct"
        call machine_learning_reservoir_data%init(water_elevation, 0.0, "2015-11-29_05:24:22" , 1, 166758723)


        print *, "calling init for machine_learning_model_struct"
        call machine_learning_model_data%init(2)


        print *, "testing data in machine_learning_struct"
        rv1 = machine_learning_data_info(machine_learning_reservoir_data)

        print *, "testing data in machine_learning_model_struct"
        rv2 = machine_learning_model_data_info(machine_learning_model_data)

        print *, "calling release for machine_learning_struct"

        !do timestep_count = 1, 2879
        !    call machine_learning_reservoir_data%run_release(0.0, &
        !    outflow, 0.0, water_elevation, outflow, 900.0)
        !end do

        print *, 'storage', machine_learning_reservoir_data%state%end_time_storage

        call machine_learning_reservoir_data%run_release(0.0, &
        -100000.0, 0.0, water_elevation, outflow, 900.0)

        print *, 'storage', machine_learning_reservoir_data%state%end_time_storage


        print *, 'inflow', machine_learning_reservoir_data%input%inflow

        print *, 'outflow: ', outflow


        call machine_learning_reservoir_data%run_release(0.0, &
        !-300.0, 0.0, water_elevation, outflow, 900.0)
        -111300.0, 0.0, water_elevation, outflow, 900.0)

        print *, 'storage', machine_learning_reservoir_data%state%end_time_storage


        print *, 'inflow', machine_learning_reservoir_data%input%inflow

        print *, 'outflow: ', outflow




        if (machine_learning_reservoir_data%input%inflow == 0.00 .and. outflow == 99.3558960) then
            rv1 = .true.
            print *, 'Machine Learning Negative Inflow on Update Test Passed'
        else
            print *, 'Machine Learning Negative Inflow on Update Test Failed'
            print *, 'Inflow should be returned to 0.00, and Outflow should be 99.3558960'
        endif





    end function test_machine_learning_boundary_conditions









end program
