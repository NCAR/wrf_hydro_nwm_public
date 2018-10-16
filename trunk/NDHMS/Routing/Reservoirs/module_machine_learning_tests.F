module module_machine_learning_tests
    use module_machine_learning_state
    use module_machine_learning_parameters
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
    function test_machine_learning_state(o) result(rv)
        type (machine_learning_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the machine learning state data structure"
        print *, " "

        print *, "Checking read on current_storage"
        print *, o%current_storage
        print *, " "

        print *, "Checking read on average_storage_over_past_time_intervals"
        print *, o%average_storage_over_past_time_intervals
        print *, " "

        print *, "Checking read on totaled_storage"
        print *, o%totaled_storage
        print *, " "

        print *, "Checking read on previous_day_release"
        print *, o%previous_day_release
        print *, " "

        print *, "Checking read on average_inflow_over_past_time_intervals"
        print *, o%average_inflow_over_past_time_intervals
        print *, " "

        print *, "Checking read on accumulated_inflow"
        print *, o%accumulated_inflow
        print *, " "

        print *, "Checking read on routing_period"
        print *, o%routing_period
        print *, " "

        print *, "Checking read on time_interval"
        print *, o%time_interval
        print *, " "

        print *, "Checking read on update_time"
        print *, o%update_time
        print *, " "

        print *, "Checking read on release_count"
        print *, o%release_count
        print *, " "

        print *, "Checking read on current_time"
        print *, o%current_time
        print *, " "

        print *, "Checking read on start_date"
        print *, o%start_date
        print *, " "

        print *, "Checking read on start_day_of_year"
        print *, o%start_day_of_year
        print *, " "

        print *, "Checking read on current_day_of_year"
        print *, o%current_day_of_year
        print *, " "

        print *, "Checking read on start_month"
        print *, o%start_month
        print *, " "

        print *, "Checking read on current_month"
        print *, o%current_month
        print *, " "

        print *, "Checking read on start_year"
        print *, o%start_year
        print *, " "

        print *, "Checking read on current_year"
        print *, o%current_year
        print *, " "

        print *, "Checking read on x_day_of_year"
        print *, o%x_day_of_year
        print *, " "

        print *, "Checking read on y_day_of_year"
        print *, o%y_day_of_year
        print *, " "

    end function test_machine_learning_state



    ! test to see that each member of the parameters structure is correctly allocated and readable
    function test_machine_learning_parameters(o) result(rv)
        type (machine_learning_parameters_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the machine learning parameters data structure"
        print *, " "

        print *, "Checking read on min_storage"
        print *, o%min_storage
        print *, " "

        print *, "Checking read on max_storage"
        print *, o%max_storage
        print *, " "

        print *, "Checking read on min_release"
        print *, o%min_release
        print *, " "

        print *, "Checking read on max_release"
        print *, o%max_release
        print *, " "

        print *, "Checking read on model_id"
        print *, o%model_id
        print *, " "

        print *, "Checking read on model_index"
        print *, o%model_index
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        print *, " "

    end function test_machine_learning_parameters

end module
