module module_levelpool_tests
    use module_levelpool_state
    use module_levelpool_parameters
    use module_reservoir_base

contains

    ! test to see that each member of the input structure is correctly allocated and readable
    function test_input(o) result(rv)
        type (reservoir_base_input_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the input structure"
        print *, " "

        print *, "Checking read on inflow"
        print *, o%inflow
        print *, " "

        print *, "Checking read on lateral_inflow"
        print *, o%lateral_inflow
        print *, " "

        print *, "Checking read on previous_timestep_inflow"
        print *, o%previous_timestep_inflow
        print *, " "

    end function test_input

    ! test to see that each member of the output structure is correctly allocated and readable
    function test_output(o) result(rv)
        type (reservoir_base_output_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        print *, " "

    end function test_output

    ! test to see that each member of the state structure is correctly allocated and readable
    function test_levelpool_state(o) result(rv)
        type (levelpool_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the level pool state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%water_elevation
        print *, " "

    end function test_levelpool_state


    ! test to see that each member of the parameters structure is correctly allocated and readable
    function test_levelpool_parameters(o) result(rv)
        type (levelpool_parameters_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the level pool parameters data structure"
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%lake_area
        print *, " "

        print *, "Checking read on weir_elevation"
        print *, o%weir_elevation
        print *, " "

        print *, "Checking read on weir_coeffecient"
        print *, o%weir_coeffecient
        print *, " "

        print *, "Checking read on weir_length"
        print *, o%weir_length
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%orifice_elevation
        print *, " "

        print *, "Checking read on orifice_coefficient"
        print *, o%orifice_coefficient
        print *, " "

        print *, "Checking read on orifice_area"
        print *, o%orifice_area
        print *, " "

        print *, "Checking read on max_depth"
        print *, o%max_depth
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        print *, " "

    end function test_levelpool_parameters

end module
