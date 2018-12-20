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

        real    :: release_monthly_average, storage_monthly_average
        integer :: ncid, lake_id_index, start_month
        integer :: status                               ! Status of reading NetCDF

        this%water_elevation = water_elevation

        print *, 'ml state before open parm'

        !Reservoir Parm NC file
        status = nf90_open(path = "ML_RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !status = nf90_open(path = machine_learning_parameter_file, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        if (status /= nf90_noerr) call handle_err(status, "ML_RESERVOIR_PARM.nc")

        print *, 'ml state after open parm'

        call read_machine_learning_netcdf_lake_id(ncid, lake_number, "lake_id", lake_id_index)
        call read_machine_learning_netcdf_integer_1D_parameters(ncid, lake_id_index, "time_interval", this%time_interval)
        call get_current_times(start_date, start_month, this%current_year, this%current_day_of_year)

        this%current_time = 0
        this%update_time = 0
        this%new_day_time = seconds_in_day
        this%x_day_of_year = 0.0
        this%y_day_of_year = 0.0

        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "release_monthly_averages", release_monthly_average)
        call read_machine_learning_netcdf_real_2D_parameters(ncid, lake_id_index, start_month, "storage_monthly_averages", storage_monthly_average)

        this%previous_time_release = release_monthly_average
        this%current_time_storage = storage_monthly_average
        this%end_time_storage = storage_monthly_average

    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy

end module module_machine_learning_state
