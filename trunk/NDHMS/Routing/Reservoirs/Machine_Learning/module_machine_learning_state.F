! This module defines and instantiates objects
! for a machine learning type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given machine learning
! reservoir object and not other modules or areas
! of the system.

#define days_in_year 365
#define days_in_leap_year 366
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
        real*4  :: previous_time_storage
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
        procedure :: get_x_and_y_day_of_year => get_x_and_y_day_of_year

    end type machine_learning_state_interface

contains

    ! Machine Learning State Constructor
    subroutine machine_learning_state_init(this, start_date, restart_flag, lake_number, &
    machine_learning_parameter_file, water_elevation)
        use netcdf
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being initialized
        character(len=19), intent(in)   :: start_date
        integer, intent(in)             :: restart_flag             ! signals if NWM is on a restart or not
                                                                    ! 1 is cold start and 99 is restart
        integer, intent(in)             :: lake_number              ! lake number
        character(len=*), intent(in)  :: machine_learning_parameter_file
        real, intent(inout)             :: water_elevation
        real                            :: release_monthly_average, storage_monthly_average
        integer                         :: ncid, lake_id_index, start_month
        integer                         :: status                    ! Status of reading NetCDF

        ! Assign the values passed in to a particular machine learning reservoir
        ! state object's variables.
        this%water_elevation = water_elevation

        ! Initialize time variables
        this%current_time = 0
        this%update_time = 0
        this%new_day_time = seconds_in_day

        ! Open Machine Learning Reservoir Parameter NetCDF file
        status = nf90_open(path = machine_learning_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open machine learning reservoir parameter file")

        ! Read relevant parameters from machine learning parameter NetCDF
        call read_machine_learning_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)
        call read_machine_learning_netcdf_integer_1D_parameters(ncid, lake_id_index, "time_interval", this%time_interval)
        call get_current_times(start_date, start_month, this%current_year, this%current_day_of_year)
        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "release_monthly_averages", release_monthly_average)
        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "storage_monthly_averages", storage_monthly_average)
        call get_x_and_y_day_of_year(this, start_month)

        this%previous_time_release = release_monthly_average
        this%previous_time_storage = storage_monthly_average
        this%end_time_storage = storage_monthly_average

    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy

    ! Calculates the x and y day of year
    subroutine get_x_and_y_day_of_year(this, start_month)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed
        integer, intent(in):: start_month

        if (((mod(this%current_year,4)==0 .and. mod(this%current_year,100)/=0) .or. mod(this%current_year,400)==0) .and. start_month>2) then
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%current_day_of_year / days_in_leap_year) ! leap year after February
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%current_day_of_year / days_in_leap_year)
        else
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%current_day_of_year / days_in_year) ! non leap year or leap year before March
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%current_day_of_year / days_in_year)
        end if

    end subroutine get_x_and_y_day_of_year


end module module_machine_learning_state
