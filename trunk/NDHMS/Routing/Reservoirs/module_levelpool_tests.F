module module_levelpool_tests
    use module_levelpool
    use module_levelpool_state
    use module_levelpool_parameters
    use module_reservoir_base
contains
    ! test to see that each member of the input structure is correctly allocated and readable
    !function test_input(o) result(rv)
    !    type (reservoir_base_input_struct), intent(in) :: o
    !    logical rv

    !    rv = .true.

    !    print *, "========================================================================"
    !    print *, "Checking the values of the input structure"
    !    print *, " "

    !end function test_input





    ! test to see the each member of the state structure is correctly allocated and readable
    !function test_state(o) result(rv)
    !    type (levelpool_state_interface), intent(in) :: o
    !    logical rv

    !    rv = .true.

    !    print *, "========================================================================"
    !    print *, "Checking the values of the aggregation structure"
    !    print *, " "

    !    print *, "Checking allocation of state%z_gwsubbas"
    !    if ( allocated(o%z_gwsubbas) ) then
    !        print *, "PASSED"

    !        print *, "Checking read on state%z_gwsubbas"
    !        print *, o%z_gwsubbas(1:16)
    !    else
    !        print *, "FAILED"
    !        rv = .false.
    !    end if

    !end function test_state



    ! test to see the each member of the level pool parameters is correctly allocated and readable
    function test_levelpool_parameters(o) result(rv)
        type (levelpool_parameters_interface), intent(in) :: o
        logical rv

        rv = .true.

    !    print *, "========================================================================"
    !    print *, "Checking the values of the level pool parameters data structure"
    !    print *, " "

    !    print *, "Checking read on reservoir_area"
    !    print *, o%reservoir_area
    !    print *, " "

    !    print *, "Checking read on weir_elevation"
    !    print *, o%weir_elevation
    !    print *, " "

    !    print *, "Checking read on weir_coeffecient"
    !    print *, o%weir_coeffecient
    !    print *, " "

    !    print *, "Checking read on weir_length"
    !    print *, o%weir_length
    !    print *, " "

    !    print *, "Checking read on orifice_elevation"
    !    print *, o%orifice_elevation
    !    print *, " "

    !    print *, "Checking read on orifice_coefficient"
    !    print *, o%orifice_coefficient
    !    print *, " "

    !    print *, "Checking read on orifice_area"
    !    print *, o%orifice_area
    !    print *, " "

    !    print *, "Checking read on max_depth"
    !    print *, o%max_depth
    !    print *, " "

    !    print *, "Checking read on lake_number"
    !    print *, o%lake_number
    !    print *, " "

    end function test_levelpool_parameters

end module

