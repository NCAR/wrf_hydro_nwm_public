module module_levelpool_tests
    use module_levelpool


contains

    function levelpool_data_info(levelpool_data) result(data_info_result)
    implicit none
    type (levelpool_struct) :: levelpool_data
    logical  :: data_info_result
    logical, dimension(1) :: ptr_state
    logical, dimension(1) :: data_state

    data_info_result = .false.

    ! Check to see if the levelpool_state data structure exists
    print *, "Checking pointer association on data%state "
    if ( associated(levelpool_data%state) ) then
        print *, "PASSED"
        ptr_state(1) = .true.
    else
        print *, "FAILED"
        ptr_state(1) = .false.
    end if

    ! Check to see if the levelpool_parameters data structure exists
    print *, "Checking pointer association on data%parameters "
    if ( associated(levelpool_data%parameters) ) then
        print *, "PASSED"
        ptr_state(2) = .true.
    else
        print *, "FAILED"
        ptr_state(2) = .false.
    end if

    ! Check to see if the levelpool_input data structure exists
    print *, "Checking pointer association on data%input "
    if ( associated(levelpool_data%input) ) then
        print *, "PASSED"
        ptr_state(3) = .true.
    else
        print *, "FAILED"
        ptr_state(3) = .false.
    end if

    ! Check to see if the levelpool_output data structure exists
    print *, "Checking pointer association on data%output "
    if ( associated(levelpool_data%output) ) then
        print *, "PASSED"
        ptr_state(4) = .true.
    else
        print *, "FAILED"
        ptr_state(4) = .false.
    end if

    ! Now check the data members of each substructure
    if ( ptr_state(1) ) then
        data_state(1) = test_levelpool_state(levelpool_data%state)
    end if

    if ( ptr_state(2) ) then
        data_state(2) = test_levelpool_parameters(levelpool_data%parameters)
    end if

    if ( ptr_state(3) ) then
        data_state(3) = test_input(levelpool_data%input)
    end if

    if ( ptr_state(4) ) then
        data_state(4) = test_output(levelpool_data%output)
    end if


    if ( all(ptr_state) .and. all(data_state) ) then
        data_info_result = .true.
        print *, "========================================================================"
        print *, "All Level Pool Tests Passed"
        print *, "========================================================================"

    else
        data_info_result = .false.
        print *, "========================================================================"
        print *, "Not All Level Pool Tests Passed"
        print *, "========================================================================"
    end if

    end function levelpool_data_info


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
