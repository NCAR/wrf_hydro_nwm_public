module module_reservoir_read_rfc_time_series_data
    use module_reservoir_utilities, only: geth_newdate, &
                                          handle_err
    use netcdf
    implicit none

    ! The time_series_data is a singleton in that the object is instantiated only once per
    ! processor by the first reservoir on that processor that needs it. The object is then used
    ! by every other reservoir on that processor that needs it.
    type :: time_series_data_type

        character(len=19)               :: start_date
        character(len=19)               :: current_date
        character(len=256)              :: time_series_path
        integer                         :: forecast_lookback_hours
        integer                         :: current_lookback_seconds
        character(len=5)                :: rfc_gage_id
        real, allocatable, dimension(:) :: forecast_discharges
        logical                         :: initialized = .FALSE.

    contains

        procedure :: init => time_series_data_init
        procedure :: destroy => time_series_data_destroy

    end type time_series_data_type

    type (time_series_data_type) :: time_series_data

    integer, parameter :: forecast_file_resolution_minutes = 60

contains

   ! Time Series Data Type Constructor
    subroutine time_series_data_init(this, start_date, time_series_path, forecast_lookback_hours, rfc_gage_id)
        implicit none
        class(time_series_data_type), intent(inout) :: this ! object being initialized
        character(len=19),  intent(in)              :: start_date
        character(len=256), intent(in)              :: time_series_path
        integer,            intent(in)              :: forecast_lookback_hours
        character(len=5),   intent(in)              :: rfc_gage_id

        ! Return from subroutine if this singleton is already initialized
        if (this%initialized) return
        this%initialized = .true.

        this%start_date = start_date
        this%current_date = start_date
        this%time_series_path = time_series_path
        this%forecast_lookback_hours = forecast_lookback_hours
        this%rfc_gage_id = rfc_gage_id

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
        character(len=2)     :: forecast_file_resolution_string
        integer :: total_time_series_file_periods, time_series_file_index
        integer :: forecast_file_minute, forecast_file_resolution_seconds
        logical :: file_exists

        this%current_lookback_seconds = 0

        total_time_series_file_periods = this%forecast_lookback_hours * (60 / forecast_file_resolution_minutes)

        old_date = this%current_date

        read (old_date(15:16), *) forecast_file_minute

        ! Match minutes to proper forecast file resolution interval
        forecast_file_minute = forecast_file_minute / forecast_file_resolution_minutes * forecast_file_resolution_minutes

        ! Formatting for writing minutes
        if (forecast_file_minute < 10) then
            old_date(15:16) = "00"
        else
            write(old_date(15:16), "(I2)") forecast_file_minute
        end if

        ! Zero out seconds
        old_date(18:19) = "00"

        write(forecast_file_resolution_string, "(I2)") forecast_file_resolution_minutes

        ! Negative for going back in time
        forecast_file_resolution_seconds = forecast_file_resolution_minutes * 60 * -1

        ! Loop through the total time series periods to look for and read time series files
        do time_series_file_index = 1, total_time_series_file_periods

            ! Use below for time series files with colons in the time
            old_date = old_date(:13) // ':' // old_date(15:16) // ':' // old_date(18:)

            ! Construct time series filename with path
            time_series_file_name = trim(this%time_series_path) // old_date // "." // &
                            forecast_file_resolution_string // 'min.' // this%rfc_gage_id // '.RFCTimeSlice.ncdf'

            ! Check if file exists
            inquire(FILE = time_series_file_name, EXIST = file_exists)
            if (file_exists) then
                ! Call subroutine to read a particular time series file
                !call this%read_time_series_file(time_series_file_name)
                exit
            end if

            ! Call subroutine to get the date from one forecast file resolution back in time
            call geth_newdate(new_date, old_date, forecast_file_resolution_seconds)
            old_date = new_date

            ! Lookback seconds to account for how long back a time series file was read
            this%current_lookback_seconds = this%current_lookback_seconds - forecast_file_resolution_seconds

        end do


    end subroutine setup_read_time_series








end module module_reservoir_read_rfc_time_series_data
