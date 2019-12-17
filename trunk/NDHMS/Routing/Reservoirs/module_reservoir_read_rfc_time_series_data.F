module module_reservoir_read_rfc_time_series_data
    use module_reservoir_utilities, only: read_time_series_netcdf_integer_variables, &
                                          read_timeslice_netcdf_real_1D_variables, &
                                          geth_newdate, &
                                          handle_err
    use netcdf
    implicit none

    type :: time_series_data_type

        character(len=19)               :: start_date
        character(len=19)               :: current_date
        character(len=256)              :: time_series_path
        integer                         :: forecast_lookback_hours
        integer                         :: timeslice_offset_hours
        integer                         :: current_lookback_seconds
        character(len=5)                :: rfc_gage_id
        integer                         :: forecast_counts
        integer                         :: time_step_seconds
        real, allocatable, dimension(:) :: forecast_discharges
        logical, allocatable, dimension(:) :: synthetic_values
        logical                         :: forecast_found

    contains

        procedure :: init => time_series_data_init
        procedure :: destroy => time_series_data_destroy
        procedure :: setup_read_time_series => setup_read_time_series
        procedure :: read_time_series_file => read_time_series_file

    end type time_series_data_type

    type (time_series_data_type) :: time_series_data

    integer, parameter :: forecast_file_resolution_minutes = 60

contains

   ! Time Series Data Type Constructor
    subroutine time_series_data_init(this, start_date, time_series_path, forecast_lookback_hours, &
        timeslice_offset_hours, rfc_gage_id, lookback_seconds, forecast_counts, time_step_seconds, &
        forecast_discharges, forecast_found)
        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being initialized
        character(len=19),  intent(in)                  :: start_date
        character(len=256), intent(in)                  :: time_series_path
        integer,            intent(in)                  :: forecast_lookback_hours
        integer,            intent(in)                  :: timeslice_offset_hours
        character(len=5),   intent(in)                  :: rfc_gage_id
        integer,            intent(out)                 :: lookback_seconds
        integer,            intent(out)                 :: forecast_counts
        integer,            intent(out)                 :: time_step_seconds
        real, allocatable, dimension(:), intent(inout)  :: forecast_discharges
        logical,            intent(inout)               :: forecast_found

        this%forecast_found = .false.
        this%start_date = start_date
        this%current_date = start_date
        this%time_series_path = time_series_path
        this%forecast_lookback_hours = forecast_lookback_hours
        this%timeslice_offset_hours = timeslice_offset_hours
        this%rfc_gage_id = rfc_gage_id

        this%current_lookback_seconds = 0
        this%forecast_counts = 0
        this%time_step_seconds = 0

        call this%setup_read_time_series()

        lookback_seconds = this%current_lookback_seconds
        forecast_counts = this%forecast_counts
        time_step_seconds = this%time_step_seconds

        if (this%forecast_found) then

            allocate(forecast_discharges(forecast_counts))

            allocate(synthetic_values(forecast_counts))

            forecast_discharges = this%forecast_discharges

            synthetic_values = this%synthetic_values

            !!!RETHINK WHERE TO PUT THIS CHECK????
            ! If all of the values are synthetic or any of the values equal -999.0 then
            ! do not use the time series
            if ( all(synthetic_values) .or. any(forecast_discharges) == -999.0 ) then
                this%forecast_found = .false.
            end if

            ! Deallocate forecast arrays
            if(allocated(this%forecast_discharges)) deallocate(this%forecast_discharges)
            if(allocated(this%synthetic_values)) deallocate(this%synthetic_values)


        end if

        forecast_found = this%forecast_found

    end subroutine time_series_data_init


    ! Time Series Data Type Destructor
    subroutine time_series_data_destroy(this)

        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being destroyed

    end subroutine time_series_data_destroy


    ! Set up list of time series files to read
    subroutine setup_read_time_series(this)
        implicit none

        class(time_series_data_type), intent(inout) :: this
        character(len=256)   :: time_series_file_name
        character(len=19)    :: old_date, new_date
        character(len=13)    :: old_date_trimmed
        character(len=2)     :: forecast_file_resolution_string
        integer :: total_time_series_file_periods, time_series_file_index
        integer :: forecast_file_minute, forecast_file_resolution_seconds, timeslice_offset_seconds
        logical :: file_exists

        this%current_lookback_seconds = 0

        total_time_series_file_periods = this%forecast_lookback_hours * (60 / forecast_file_resolution_minutes)

        timeslice_offset_seconds = this%timeslice_offset_hours * 3600

        old_date = this%current_date

        call geth_newdate(new_date, old_date, timeslice_offset_seconds)
        old_date = new_date

        write(forecast_file_resolution_string, "(I2)") forecast_file_resolution_minutes

        ! Negative for going back in time
        forecast_file_resolution_seconds = forecast_file_resolution_minutes * 60 * -1

        ! Loop through the total time series periods to look for and read time series files
        do time_series_file_index = 1, total_time_series_file_periods

            old_date_trimmed = trim(old_date(:13))

            time_series_file_name = trim(this%time_series_path) // old_date_trimmed // "." // &
                            forecast_file_resolution_string // 'min.' // this%rfc_gage_id // '.RFCTimeSlice.ncdf'

            ! Check if file exists
            inquire(FILE = time_series_file_name, EXIST = file_exists)
            if (file_exists) then
                ! Call subroutine to read a particular time series file
                call this%read_time_series_file(time_series_file_name)
                exit
            end if

            ! Call subroutine to get the date from one forecast file resolution back in time
            call geth_newdate(new_date, old_date, forecast_file_resolution_seconds)
            old_date = new_date

            ! Lookback seconds to account for how long back a time series file was read
            this%current_lookback_seconds = this%current_lookback_seconds - forecast_file_resolution_seconds

        end do

    end subroutine setup_read_time_series

    ! Read given time series file to get gage discharges
    subroutine read_time_series_file(this, time_series_file)
        implicit none
        class(time_series_data_type), intent(inout) :: this
        character(len=256), intent(in) :: time_series_file
        integer*8, allocatable, dimension(:) :: gage_ids_integer_array
        integer :: reservoir_rfc_gage_index, time_series_rfc_gage_index
        integer :: ncid, status

!!!!!READ IN T0 AND T-48?????


        ! Open Time Series NetCDF file
        status = nf90_open(path = trim(time_series_file), mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open time series file")

        call read_time_series_netcdf_integer_variables(ncid, 'forecastCounts', this%forecast_counts)

        call read_time_series_netcdf_integer_variables(ncid, 'timeSteps', this%time_step_seconds)

        allocate (this%forecast_discharges(this%forecast_counts))

        call read_timeslice_netcdf_real_1D_variables(ncid, 'discharges', this%forecast_discharges)

        call read_timeslice_netcdf_logical_1D_variables(ncid, 'synthetic_values', this%synthetic_values)

        this%forecast_found = .true.

    end subroutine read_time_series_file

end module module_reservoir_read_rfc_time_series_data
