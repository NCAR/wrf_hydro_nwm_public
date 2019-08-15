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
        !if ( ptr_state(1) ) then
        !    data_state(1) = test_rfc_forecasts_state(rfc_forecasts_reservoir_data%state)
        !end if

        !if ( ptr_state(2) ) then
        !    data_state(2) = test_rfc_forecasts_properties(rfc_forecasts_reservoir_data%properties)

        !end if

        !if ( ptr_state(3) ) then
        !    data_state(3) = test_input_rfc_forecasts(rfc_forecasts_reservoir_data%input)
        !end if

        !if ( ptr_state(4) ) then
        !    data_state(4) = test_output_rfc_forecasts(rfc_forecasts_reservoir_data%output)
        !end if


        !if ( all(ptr_state) .and. all(data_state) ) then
        if ( all(ptr_state) ) then
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

end module
