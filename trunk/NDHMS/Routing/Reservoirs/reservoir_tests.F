program reservoir_unit_tests
    use module_levelpool_tests
    use module_levelpool
    use module_machine_learning_tests
    use module_machine_learning

    implicit none

    logical :: rv = .false.

    rv = test_levelpool()


    rv = .false.

    rv = test_machine_learning()

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
    !                              test_machine_learning()                                !
    ! this function verifies that the constructor for the machine_learning type correctly !
    ! initializes all data members                                                 !
    !------------------------------------------------------------------------------!

    function test_machine_learning() result(rv)
        implicit none

        logical rv                        ! test result

        type (machine_learning_struct) :: machine_learning_reservoir_data
        integer :: call_status = 0

        print *, "calling init for machine_learning_struct"
        call machine_learning_reservoir_data%init("2017-08-25_17:24:22" , 25)

        print *, "testing data in machine_learning_struct"
        call_status = machine_learning_data_info(machine_learning_reservoir_data)

    end function test_machine_learning


end program




