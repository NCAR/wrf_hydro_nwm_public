module nhd_groundwater_tests
    use module_bucket_state
    use module_nhd_basin
    use module_bucket_properties
    use module_groundwater_static_data
    use module_groundwater_base
contains
    ! test to see the each member of the input structure is correctly allocated and readable
    function test_input(o) result(rv)
        type (groundwater_base_input_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the input structure"
        print *, " "

        print *, "Checking allocation of input%infxsrt"
        if ( allocated(o%infxsrt) ) then
            print *, "PASSED"

            print *, "Checking read on input%infxsrt"
            print *, o%infxsrt(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of input%soldrain"
        if ( allocated(o%soldrain) ) then
            print *, "PASSED"

            print *, "Checking read on input%soldrain"
            print *, o%soldrain(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of input%qin_gwsubbas"
        if ( allocated(o%qin_gwsubbas) ) then
            print *, "PASSED"

            print *, "Checking read on input%qin_gwsubbas"
            print *, o%qin_gwsubbas(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if
    end function test_input

    ! test to see the each member of the input structure is correctly allocated and readable
    function test_output(o) result(rv)
        type (groundwater_base_output_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the output structure"
        print *, " "

        print *, "Checking allocation of output%qinflowbase"
        if ( allocated(o%qinflowbase) ) then
            print *, "PASSED"

            print *, "Checking read on output%qinflowbase"
            print *, o%qinflowbase(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of output%qout_gwsubbas"
        if ( allocated(o%qout_gwsubbas) ) then
            print *, "PASSED"

            print *, "Checking read on output%qout_gwsubbas"
            print *, o%qout_gwsubbas(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if
    end function test_output

    ! test to see the each member of the input structure is correctly allocated and readable
    function test_aggregation(o) result(rv)
        type (groundwater_base_aggregation_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the aggregation structure"
        print *, " "

        print *, "Checking allocation of aggregation%infxswgt"
        if ( allocated(o%infxswgt) ) then
            print *, "PASSED"

            print *, "Checking read on aggregation%infxswgt"
            print *, o%infxswgt(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking read on agg_factor"
        print *, o%agg_factor
        print *, " "

    end function test_aggregation

    ! test to see the each member of the state structure is correctly allocated and readable
    function test_state(o) result(rv)
        class (bucket_state_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the aggregation structure"
        print *, " "

        print *, "Checking allocation of state%z_gwsubbas"
        if ( allocated(o%z_gwsubbas) ) then
            print *, "PASSED"

            print *, "Checking read on state%z_gwsubbas"
            print *, o%z_gwsubbas(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

    end function test_state

    ! test to see the each member of the basin structure is correctly allocated and readable
    function test_nhd_basin(o) result(rv)
        type (nhd_basin_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the basin structure"
        print *, " "

        !print *, "Checking allocation of basin%cell_area"
        !if ( allocated(o%cell_area) ) then
        !    print *, "PASSED"

        !    print *, "Checking read on basin%cell_area"
        !    print *, o%cell_area(1:4,1:4)
        !else
        !    print *, "FAILED"
        !    rv = .false.
        !end if

        !print *, "Checking allocation of basin%cell_area_lsm"
        !if ( allocated(o%cell_area_lsm) ) then
        !    print *, "PASSED"

        !    print *, "Checking read on basin%cell_area_lsm"
        !    print *, o%cell_area_lsm(1:4,1:4)
        !else
        !    print *, "FAILED"
        !    rv = .false.
        !end if

        print *, "Checking allocation of basin%basin_area"
        if ( allocated(o%basin_area) ) then
            print *, "PASSED"

            print *, "Checking read on basin%basin_area"
            print *, o%basin_area(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of basin%nhd_bucket_mask"
        if ( allocated(o%nhd_bucket_mask) ) then
            print *, "PASSED"

            print *, "Checking read on basin%nhd_bucket_mask"
            print *, o%nhd_bucket_mask(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking read on basin channel_bucket_only"
        print *, o%channel_bucket_only
        print *, " "

        print *, "Checking read on basin lnlinksl"
        print *, o%lnlinksl
        print *, " "

        print *, "Checking read on basin num_basins"
        print *, o%num_basins
        print *, " "

    end function test_nhd_basin


    ! test to see the each member of the properties structure is correctly allocated and readable
    function test_nhd_properties(o) result(rv)
        class (bucket_properties_interface), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the properties structure"
        print *, " "

        print *, "Checking allocation of properties%cell_area"
        if ( allocated(o%cell_area) ) then
            print *, "PASSED"

            print *, "Checking read on properties%cell_area"
            print *, o%cell_area(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of properties%cell_area_lsm"
        if ( allocated(o%cell_area_lsm) ) then
            print *, "PASSED"

            print *, "Checking read on properties%cell_area_lsm"
            print *, o%cell_area_lsm(1:4,1:4)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of properties%gw_buck_coeff"
        if ( allocated(o%gw_buck_coeff) ) then
            print *, "PASSED"

            print *, "Checking read on properties%gw_buck_coeff"
            print *, o%gw_buck_coeff(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of properties%gw_buck_exp"
        if ( allocated(o%gw_buck_exp) ) then
            print *, "PASSED"

            print *, "Checking read on properties%gw_buck_exp"
            print *, o%gw_buck_exp(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

        print *, "Checking allocation of properties%z_max"
        if ( allocated(o%z_max) ) then
            print *, "PASSED"

            print *, "Checking read on properties%z_max"
            print *, o%z_max(1:16)
        else
            print *, "FAILED"
            rv = .false.
        end if

    end function test_nhd_properties

    ! test to see the each member of the basin structure is correctly allocated and readable
    function test_nhd_static_data(o) result(rv)
        type (groundwater_static_struct), intent(in) :: o
        logical rv

        rv = .true.

        print *, "========================================================================"
        print *, "Checking the values of the static data structure"
        print *, " "

        print *, "Checking read on basin dt"
        print *, o%dt
        print *, " "

        print *, "Checking read on basin gwbaseswcrt"
        print *, o%gwbaseswcrt
        print *, " "

        print *, "Checking read on basin ix_lsm"
        print *, o%ix_lsm
        print *, " "

        print *, "Checking read on basin jx_lsm"
        print *, o%jx_lsm
        print *, " "

        print *, "Checking read on basin ix_rt"
        print *, o%ix_rt
        print *, " "

        print *, "Checking read on basin jx_rt"
        print *, o%jx_rt
        print *, " "

        print *, "Checking read on basin ovrtswcrt"
        print *, o%ovrtswcrt
        print *, " "

    end function test_nhd_static_data

end module
