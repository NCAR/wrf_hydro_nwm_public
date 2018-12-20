! This module defines and instantiates objects
! for a machine learning type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given machine learning
! reservoir object and not other modules or areas
! of the system.

! Time between running machine learning model to update the outflow
#define seconds_in_day 86400
#define pi 3.14159265359

module module_machine_learning_state

    use netcdf
    use module_reservoir_utilities
    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning state from the abstract base
    ! struct for reservoir state.
    type, extends(reservoir_base_state_struct) :: machine_learning_state_interface
        real    :: water_elevation
        real*4  :: current_time_storage
        real    :: end_time_storage
        real    :: previous_time_release
        integer :: time_interval
        integer :: update_time
        integer :: new_day_time
        integer :: current_time
        integer :: current_day_of_year
        integer :: current_year
        real    :: x_day_of_year
        real    :: y_day_of_year

    contains

        procedure :: init => machine_learning_state_init
        procedure :: destroy => machine_learning_state_destroy

    end type machine_learning_state_interface

contains

            subroutine machine_learning_state_init(this, start_date, restart_flag, lake_number, water_elevation)

    !Machine Learning State Constructor
    !subroutine machine_learning_state_init(this, machine_learning_parameter_file, start_date, &
    !  restart_flag, lake_number, water_elevation)
        use netcdf
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being initialized
        !character(len=256), intent(in) :: machine_learning_parameter_file
        character(len=19), intent(in)   :: start_date
        integer, intent(in)             :: restart_flag             ! signals if NWM is on a restart or not
                                                                    ! 1 is cold start and 99 is restart
        integer, intent(in)             :: lake_number              ! lake number
        real, intent(inout)             :: water_elevation

        integer :: start_month
        integer :: ncid, lake_id_index
        integer :: status                               ! Status of reading NetCDF
        real    :: release_monthly_average, storage_monthly_average


        this%water_elevation = water_elevation

        print *, 'ml state before open parm'

        !Reservoir Parm NC file
        status = nf90_open(path = "ML_RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !status = nf90_open(path = machine_learning_parameter_file, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status, "ML_RESERVOIR_PARM.nc")

        print *, 'ml state after open parm'


        call read_machine_learning_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)

        call read_machine_learning_netcdf_integer_parameters(ncid, lake_id_index, "time_interval", this%time_interval)

        call get_current_times(start_date, start_month, this%current_year, this%current_day_of_year)

        this%current_time = 0
        this%update_time = 0
        this%new_day_time = seconds_in_day
        this%x_day_of_year = 0.0
        this%y_day_of_year = 0.0

print *, '1.7'
        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "release_monthly_averages", release_monthly_average)
        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "storage_monthly_averages", storage_monthly_average)


        this%previous_time_release = release_monthly_average
        this%current_time_storage = storage_monthly_average
        this%end_time_storage = storage_monthly_average

print *, '1.8'

    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy


    subroutine get_current_times(start_date, start_month, current_year, current_day_of_year)
        implicit none
        character(len=19), intent(in)   :: start_date
        integer, intent(out) :: start_month, current_year, current_day_of_year
        integer :: start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index

        read (start_date(6:7), *) start_month
        read (start_date(9:10), *) start_day_of_month
        read (start_date(1:4), *) start_year

        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        start_day_of_year = 0

        if (((mod(start_year,4)==0 .and. mod(start_year,100)/=0) .or. mod(start_year,400)==0) .and. start_month>2) then
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do
            start_day_of_year = start_day_of_year + start_day_of_month + 1    ! add one day for leap year after February

        else
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do

            start_day_of_year = start_day_of_year + start_day_of_month     ! non leap year or leap year before March
        end if

        current_year = start_year
        current_day_of_year = start_day_of_year

    end subroutine get_current_times


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

        print *, 'lake_id_index'
        print *, lake_id_index


        if(allocated(temp_lake_id_array)) deallocate(temp_lake_id_array)


    end subroutine read_machine_learning_netcdf_lake_id


    subroutine read_machine_learning_netcdf_integer_parameters(ncid, lake_id_index, netcdf_array_name, returned_integer_parameter)
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

        print *, 'number_of_lakes'
        print *, number_of_lakes

        print *, 'temp_integer_array'
        print *, temp_integer_array

        returned_integer_parameter = temp_integer_array(lake_id_index)

        print *, 'returned_integer_parameter'
        print *, returned_integer_parameter

        if(allocated(temp_integer_array)) deallocate(temp_integer_array)

    end subroutine read_machine_learning_netcdf_integer_parameters


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

        print *, 'number_of_lakes'
        print *, number_of_lakes

        print *, 'temp_real_2D_array'
        print *, temp_real_2D_array

        returned_real_parameter = temp_real_2D_array(lake_id_index, start_month)

        print *, 'returned_real_parameter'
        print *, returned_real_parameter


        if(allocated(temp_real_2D_array)) deallocate(temp_real_2D_array)

    end subroutine read_machine_learning_netcdf_real_2D_parameters


end module module_machine_learning_state
