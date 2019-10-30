module module_rfc_forecasts_tests
    use module_rfc_forecasts

contains

    !Testing
    function rfc_forecasts_data_info(rfc_forecasts_reservoir_data) result(rv)
        implicit none
        type (rfc_forecasts) :: rfc_forecasts_reservoir_data
        logical :: rv
        logical, dimension(4) :: ptr_state

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

    end function rfc_forecasts_data_info

end module
