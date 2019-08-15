module module_rfc_forecasts_tests
    use module_rfc_forecasts

contains

    !Testing
    function rfc_forecasts_data_info(rfc_forecasts_reservoir_data) result(rv)
        implicit none
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        logical :: rv
        logical, dimension(1) :: ptr_state
        logical, dimension(1) :: data_state

        rv = .false.

        ! Check to see if the rfc_forecasts_state data structure exists
        print *, "Checking pointer association on data%state for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%state) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
       else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_properties data structure exists
        print *, "Checking pointer association on data%properties for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%properties) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_input data structure exists
        print *, "Checking pointer association on data%input for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%input) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the rfc_forecasts_output data structure exists
        print *, "Checking pointer association on data%output for rfc forecasts reservoirs"
        if ( associated(rfc_forecasts_reservoir_data%output) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "


        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_rfc_forecasts_state(rfc_forecasts_reservoir_data%state)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_rfc_forecasts_properties(rfc_forecasts_reservoir_data%properties)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_input_rfc_forecasts(rfc_forecasts_reservoir_data%input)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_output_rfc_forecasts(rfc_forecasts_reservoir_data%output)
        end if


        if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All RFC Forecast Reservoir Object Tests Passed"
            print *, "========================================================================"
            rv = .true.

        else
            print *, "========================================================================"
            print *, "Not All RFC Forecast Reservoir Object Tests Passed"
            print *, "========================================================================"

        end if

        flush(6)
    end function rfc_forecasts_data_info


    ! test to see that each member of the input structure is correctly allocated and readable
    function test_input_rfc_forecasts(o) result(rv)
        type (reservoir_base_input_type), intent(in) :: o
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

    end function test_input_rfc_forecasts

    ! test to see that each member of the output structure is correctly allocated and readable
    function test_output_rfc_forecasts(o) result(rv)
        type (reservoir_base_output_type), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the output structure"
        print *, " "

        print *, "Checking read on outflow"
        print *, o%outflow
        print *, " "

    end function test_output_rfc_forecasts

    ! test to see that each member of the state structure is correctly allocated and readable
    function test_rfc_forecasts_state(o) result(rv)
        type (rfc_forecasts_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc_forecasts state data structure"
        print *, " "

        print *, "Checking read on water_elevation"
        print *, o%water_elevation
        print *, " "

        print *, "Checking read on current_storage"
        print *, o%current_storage
        print *, " "

        print *, "Checking read on gage_discharge"
        print *, o%gage_discharge
        print *, " "

        print *, "Checking read on persisted_outflow"
        print *, o%persisted_outflow
        print *, " "

        print *, "Checking read on weight_update_time"
        print *, o%weight_update_time
        print *, " "

        print *, "Checking read on timeslice_update_time"
        print *, o%timeslice_update_time
        print *, " "

        print *, "Checking read on current_time"
        print *, o%current_time
        print *, " "

        print *, "Checking read on persistence_weight_index"
        print *, o%persistence_weight_index
        print *, " "

        print *, "Checking read on levelpool_current_weight"
        print *, o%levelpool_current_weight
        print *, " "

        print *, "Checking read on persistence_current_weight"
        print *, o%persistence_current_weight
        print *, " "

        print *, "Checking read on levelpool_ptr"
        !print *, o%levelpool_ptr
        print *, " "

    end function test_rfc_forecasts_state



    ! test to see that each member of the parameters structure is correctly allocated and readable
    function test_rfc_forecasts_parameters(o) result(rv)
        type (rfc_forecasts_parameters_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the rfc_forecasts parameters data structure"
        print *, " "

        print *, "Checking read on min_storage"
        print *, o%min_storage
        print *, " "

        print *, "Checking read on max_storage"
        print *, o%max_storage
        print *, " "

        print *, "Checking read on lake_area"
        print *, o%lake_area
        print *, " "

        print *, "Checking read on orifice_elevation"
        print *, o%orifice_elevation
        print *, " "

        print *, "Checking read on lake_number"
        print *, o%lake_number
        print *, " "

        print *, "Checking read on gage_id"
        print *, o%gage_id
        print *, " "

        print *, "Checking read on observation_lookback_hours"
        print *, o%observation_lookback_hours
        print *, " "

        print *, "Checking read on observation_update_time_interval_seconds"
        print *, o%observation_update_time_interval_seconds
        print *, " "

        print *, "Checking read on weight_update_time_interval"
        print *, o%weight_update_time_interval
        print *, " "

        print *, "Checking read on persistence_weighted_coefficients"
        print *, o%persistence_weighted_coefficients
        print *, " "

    end function test_rfc_forecasts_parameters

end module
