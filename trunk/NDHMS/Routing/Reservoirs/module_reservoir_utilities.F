module module_reservoir_utilities
    use netcdf

    implicit none

contains

    ! Determine if leap year
    logical function leap_year(current_year)
        integer, intent(in) :: current_year

        if (((mod(current_year,4)==0 .and. &
            mod(current_year,100)/=0) .or. &
            mod(current_year,400)==0)) then
            leap_year = .TRUE.

        else
            leap_year = .FALSE.
        end if

    end function leap_year

    ! Get the start month, current year, and current day of year
    subroutine get_current_times(start_date, start_month, current_year, current_day_of_year)
        implicit none
        character(len=19), intent(in)   :: start_date
        integer, intent(out)            :: start_month, current_year, current_day_of_year
        integer :: start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index

        read (start_date(6:7), *) start_month
        read (start_date(9:10), *) start_day_of_month
        read (start_date(1:4), *) start_year

        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        start_day_of_year = 0

        ! If leap year and after February
        if (leap_year(start_year) .and. start_month>2) then
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do

            ! add one day for leap year after February
            start_day_of_year = start_day_of_year + start_day_of_month + 1

        else
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do

            ! non leap year or leap year before March
            start_day_of_year = start_day_of_year + start_day_of_month
        end if

        current_year = start_year
        current_day_of_year = start_day_of_year

    end subroutine get_current_times

    ! Read lake id from machine learning parameters NetCDF and determine the lake_id_index, which
    ! is the index of the particular reservoir's values in all of the parameter arrays.
    subroutine read_machine_learning_netcdf_lake_id(ncid, lake_number, netcdf_array_name, lake_id_index)
        integer, intent(in) :: ncid, lake_number
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: lake_id_index
        integer, allocatable, dimension(:) :: temp_lake_id_array
        integer :: var_id, number_of_lakes, status, lake_index
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        allocate(temp_lake_id_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_lake_id_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        do lake_index = 1, number_of_lakes
            if (temp_lake_id_array(lake_index) == lake_number) then
                lake_id_index = lake_index
            end if
        end do

        if(allocated(temp_lake_id_array)) deallocate(temp_lake_id_array)

    end subroutine read_machine_learning_netcdf_lake_id

    ! Read the machine learning model id array from the machine learning parameters NetCDF
    subroutine read_machine_learning_netcdf_model_ids(ncid, netcdf_array_name, var_id, number_of_lakes)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_lakes
        integer, allocatable, dimension(:) :: temp_integer_array
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_netcdf_model_ids

    ! Read integer one dimension parameter arrays from the machine learning parameters NetCDF
    subroutine read_machine_learning_netcdf_integer_1D_parameters(ncid, lake_id_index, netcdf_array_name, returned_integer_parameter)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: returned_integer_parameter
        integer, allocatable, dimension(:) :: temp_integer_array
        integer :: var_id, number_of_lakes, status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        allocate(temp_integer_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_integer_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        returned_integer_parameter = temp_integer_array(lake_id_index)

        if(allocated(temp_integer_array)) deallocate(temp_integer_array)

    end subroutine read_machine_learning_netcdf_integer_1D_parameters

    ! Read real one dimension parameter arrays from the machine learning parameters NetCDF
    subroutine read_machine_learning_netcdf_real_1D_parameters(ncid, lake_id_index, netcdf_array_name, returned_real_parameter)
        integer, intent(in) :: ncid, lake_id_index
        character(len=*), intent(in) :: netcdf_array_name
        real, intent(out) :: returned_real_parameter
        real, allocatable, dimension(:) :: temp_real_array
        integer :: var_id, number_of_lakes, status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        allocate(temp_real_array(number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_real_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        returned_real_parameter = temp_real_array(lake_id_index)

        if(allocated(temp_real_array)) deallocate(temp_real_array)

    end subroutine read_machine_learning_netcdf_real_1D_parameters

    ! Read real two dimension parameter arrays from the machine learning parameters NetCDF
    subroutine read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, netcdf_array_name, returned_real_parameter)
        integer, intent(in) :: ncid, lake_id_index, start_month
        character(len=*), intent(in) :: netcdf_array_name
        real, intent(out) :: returned_real_parameter
        real, allocatable, dimension(:,:) :: temp_real_2D_array
        integer :: var_id, number_of_lakes, monthly_averages_rows, status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_lakes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        allocate(temp_real_2D_array(monthly_averages_rows, number_of_lakes))

        status = nf90_get_var(ncid, var_id, temp_real_2D_array)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        returned_real_parameter = temp_real_2D_array(lake_id_index, start_month)

        if(allocated(temp_real_2D_array)) deallocate(temp_real_2D_array)

    end subroutine read_machine_learning_netcdf_real_2D_parameters

    ! Read integer one dimension parameter arrays from the machine learning model parameters NetCDF
    subroutine read_machine_learning_model_netcdf_integer_1D_parameters(ncid, netcdf_array_name, var_id, number_of_inputs)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_inputs
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_inputs)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_integer_1D_parameters

    ! Read real one dimension parameter arrays from the machine learning model parameters NetCDF
    subroutine read_machine_learning_model_netcdf_real_1D_parameters(ncid, netcdf_array_name, var_id, number_of_inputs)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_inputs
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_inputs)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_real_1D_parameters

    ! Read real two dimension parameter arrays from the machine learning model parameters NetCDF
    subroutine read_machine_learning_model_netcdf_real_2D_parameters(ncid, netcdf_array_name, var_id, number_of_layers, number_of_nodes)
        integer, intent(in) :: ncid
        character(len=*), intent(in) :: netcdf_array_name
        integer, intent(out) :: var_id, number_of_layers, number_of_nodes
        integer :: status
        integer, dimension(nf90_max_var_dims) :: dim_ids

        status = nf90_inq_varid(ncid, netcdf_array_name, var_id)
        if (status /= nf90_noerr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_variable(ncid, var_id, dimids = dim_ids)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(1), len = number_of_layers)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

        status = nf90_inquire_dimension(ncid, dim_ids(2), len = number_of_nodes)
        if(status /= nf90_NoErr) call handle_err(status, netcdf_array_name)

    end subroutine read_machine_learning_model_netcdf_real_2D_parameters

    ! Handle NetCDF error
    subroutine handle_err(status, variable_error)
        implicit none
        integer, intent (in) :: status
        character(len=32), intent (in) :: variable_error
        if(status /= nf90_noerr) then
            print *, trim(nf90_strerror(status)), ': ', variable_error
            stop "Stopped"
        end if

    end subroutine handle_err

end module module_reservoir_utilities
