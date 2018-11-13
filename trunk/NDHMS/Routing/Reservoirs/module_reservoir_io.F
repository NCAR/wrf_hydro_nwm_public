module module_reservoir_io

    use module_reservoir_base

    implicit none




contains



    subroutine reservoir_write_to_restart_file2(reservoirs, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    !subroutine machine_learning_write_to_restart_file(reservoir_data, iret, ncid, dimid_lakes, varid, nlakes, lake_index)
        implicit none
        !class(machine_learning_struct), intent(inout) :: reservoir_data
        class (reservoir_base_container), dimension(:), intent(in):: reservoirs
        integer, intent(inout) :: iret, ncid
        integer, intent(in) :: NF_FLOAT, varid, nlakes, lake_index
        integer, dimension(:), intent(in) :: dimid_lakes


        ! use below
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes(1)/), varid)


        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, dimid_lakes, varid)



        !call w_rst_crt_nc1_lake(ncid, nlakes, reservoir_data%state%current_time_storage, "machine_learning_current_time_storage", &
        !       lake_index)




    end subroutine reservoir_write_to_restart_file2















end module module_reservoir_io
