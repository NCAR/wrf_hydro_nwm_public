program reservoir_unit_tests
    use module_levelpool_tests
    !use module_levelpool
    use module_persistence_levelpool_hybrid_tests
    !use module_persistence_levelpool_hybrid

    implicit none

    logical :: rv1, rv2          ! test result

    rv1 = .false.
    rv2 = .false.

    rv1 = test_levelpool()

    rv2 = test_persistence_levelpool_hybrid()

    if (rv1 .and. rv2) then
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



    !------------------------------------------------------------------------------!
    !                              test_levelpool()                                !
    ! this function verifies that the constructor for the levelpool type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_persistence_levelpool_hybrid() result(rv)

        implicit none

        logical rv, rv1, rv2, rv3                        ! test result

        type (persistence_levelpool_hybrid_struct) :: persistence_levelpool_hybrid_reservoir_data

        real :: outflow, water_elevation

        water_elevation = 0.0

        rv = .false.
        rv1 = .false.
        rv2 = .false.
        rv3 = .false.

        print *, "calling init for persistence_levelpool_hybrid_struct"
        call persistence_levelpool_hybrid_reservoir_data%init(water_elevation, 123.829002380371, 63.6739971160889 , &
        0.4, 10.0, 50.3799997965495, 0.1, 1.0, 66.0199966430664, 0.9, 166758723, "hybrid_persistence_levelpool_ACF.nc", &
        "2010-10-01_07:00:00", ".", 12, 86400)


        print *, "testing data in persistence_levelpool_hybrid_struct"
        rv1 = persistence_levelpool_hybrid_data_info(persistence_levelpool_hybrid_reservoir_data)


        !print *, "calling release for persistence_levelpool_hybrid_struct"

        !call persistence_levelpool_hybrid_reservoir_data%run_release(0.0, &
        !192.46272, 0.0, water_elevation, outflow, 300.0)

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


    end function test_persistence_levelpool_hybrid




end program
