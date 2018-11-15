module module_reservoir_io

    use module_reservoir_base
    use module_io_utilities
    use netcdf

    !use module_machine_learning

    implicit none




contains


    subroutine reservoir_write_to_restart_file2(reservoirs, q_types, inflows, outflows, water_elevations, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    !subroutine reservoir_write_to_restart_file2(reservoirs, q_type, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
        implicit none
        class (reservoir_base_container), dimension(:), intent(in):: reservoirs
        integer, intent(inout) :: iret, ncid, varid
        integer, intent(in) :: NF_FLOAT, nlakes
        integer, intent(in) :: lake_index(nlakes)
        integer, dimension(:), intent(in) :: dimid_lakes
        real, dimension(:), intent(in) :: q_types, inflows, outflows, water_elevations

        integer :: e
        integer :: lake_index_for_q_type

            !dd = nf90_def_var(aa,"machine_learning_current_time_storage",ee,1,cc) ! builds okay

            !iret = nf_def_var(ncid,"qlakei",NF_FLOAT,1,(/dimid_lakes/),varid)


        ! use below
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes(1)/), varid)


        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Works but need to verify dimid_lakes
        iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, dimid_lakes, varid)


        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes/), varid)


        !!!!!!!!!!!!!!!!!!!!!!!!!!
        ! Builds w/ e set as an integer
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, e)

        ! Builds now b/c varid set to inout instead of just in
        !iret = nf90_def_var(1, "machine_learning_current_time_storage", 2, 1, varid)
        !!!!!!!!!!!!!!!!!!!!!


        !call w_rst_crt_nc1_lake(ncid, nlakes, reservoir_data%state%current_time_storage, "machine_learning_current_time_storage", &
        !       lake_index)






    end subroutine reservoir_write_to_restart_file2















end module module_reservoir_io
