! This module reads USGS timeslice files to get gage discharge
! values that will be used by reservoirs. An observation lookback
! period is passed in to determine how far back in time from the
! current model time the module will look for timeslice files.
! The observation resolution determines the time increments the
! module will look back. For instance, a standard lookback period
! would be 18 hours with an observation resolution of 15 minutes,
! where a model current time of 8:00 PM would search for timeslice
! files at every 15 minute increment between 2:00 AM and 8:00 PM
! that day. The module will first search for the most recent
! timeslice files and grab the discharge for a particular
! lake/reservoir if the gage quality standard is met at that time.
! If a gage discharge is missing or if the gage quality standard
! is not met for any particular lake/reservoir in the given
! timeslice file, the module will continue to look back at every
! observation resolution increment until either all
! lakes/reservoirs have a good quality discharge or the end of
! the lookback period is reached. The total lookback seconds
! from current model time that the discharge is read will also
! be returned.

#define days_in_year 365
#define days_in_leap_year 366
#define observation_resolution_minutes 15
#define gage_quality_threshold 0.9

module module_read_timeslice_data

    use module_reservoir_utilities
    use netcdf
    implicit none

    type :: read_timeslice_data_struct

        character(len=19)                             :: start_date
        character(len=19)                             :: current_date
        character(len=256)                            :: timeslice_path
        integer                                       :: number_of_reservoir_gages
        integer                                       :: observation_lookback_hours
        integer                                       :: current_lookback_seconds
        integer                                       :: last_update_time_seconds
        integer,            allocatable, dimension(:) :: gage_lookback_seconds
        integer,            allocatable, dimension(:) :: reservoir_gage_ids
        !character(len=15),            allocatable, dimension(:) :: reservoir_gage_ids
        real,               allocatable, dimension(:) :: reservoir_gage_discharges
        character(len=256), allocatable, dimension(:) :: timeslice_file_names
        character(len=15),  allocatable, dimension(:) :: gage_id
        real,               allocatable, dimension(:) :: gage_discharge
        real,               allocatable, dimension(:) :: gage_quality
        logical                                       :: initialized = .FALSE.

    contains

        procedure :: init => read_timeslice_data_struct_init
        procedure :: destroy => read_timeslice_data_struct_destroy
        procedure :: setup_read_timeslice => setup_read_timeslice
        procedure :: read_timeslice_file => read_timeslice_file

    end type read_timeslice_data_struct

    type (read_timeslice_data_struct) :: read_timeslice_data

contains

    ! Timeslice Data Struct Constructor
    subroutine read_timeslice_data_struct_init(this, start_date, timeslice_path, &
        persistence_parameter_file, observation_lookback_hours)
        implicit none
        class(read_timeslice_data_struct), intent(inout) :: this ! object being initialized
        character(len=19),  intent(in)                   :: start_date
        character(len=256), intent(in)                   :: timeslice_path
        character(len=*),   intent(in)                   :: persistence_parameter_file
        integer,            intent(in)                   :: observation_lookback_hours
        integer                                          :: ncid, var_id
        integer                                          :: status  ! status of reading NetCDF
        integer                                          :: timeslice_gage_index
        character(len=15), allocatable, dimension(:)     :: gage_ids_string_array
        character(len=15)                                :: gage_id_string, gage_id_string_trimmed
        integer                                          :: gage_id_integer
        integer                                          :: char_index, number_counter
        character(len=1)                                 :: temp_char

        ! Return from subroutine if this singleton is already initialized
        if (this%initialized) return
        this%initialized = .true.

        status = nf90_open(path = persistence_parameter_file, mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open persistence reservoir parameter file")

        call read_persistence_netcdf_gage_ids_all(ncid, "gage_id", var_id, this%number_of_reservoir_gages)

        allocate (this%reservoir_gage_ids(this%number_of_reservoir_gages))
        allocate (gage_ids_string_array(this%number_of_reservoir_gages))

        status = nf90_get_var(ncid, var_id, gage_ids_string_array)
        if (status /= nf90_noerr) call handle_err(status, "gage_id")

        status = nf90_close(ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not close persistence reservoir parameter file")

        ! Convert gage ids to integers
        do timeslice_gage_index = 1, this%number_of_reservoir_gages

            gage_id_string = gage_ids_string_array(timeslice_gage_index)

            gage_id_string_trimmed = ADJUSTL(trim(gage_id_string))

            number_counter = 0

            do char_index = 1, 15
                temp_char = gage_id_string_trimmed(char_index:char_index)

                if (temp_char .eq. '0' .or. temp_char .eq. '1' .or. temp_char .eq. '2' &
                .or. temp_char .eq. '3' .or. temp_char .eq. '4' .or. temp_char .eq. '5' &
                .or. temp_char .eq. '6' .or. temp_char .eq. '7' .or. temp_char .eq. '8' &
                .or. temp_char .eq. '9') then
                    number_counter = number_counter + 1
                end if

            end do

            read(gage_id_string_trimmed(1:number_counter), *) gage_id_integer

            this%reservoir_gage_ids(timeslice_gage_index) = gage_id_integer

        end do

        if(allocated(gage_ids_string_array)) deallocate(gage_ids_string_array)

        this%start_date = start_date
        this%current_date = start_date
        this%timeslice_path = timeslice_path
        this%observation_lookback_hours = observation_lookback_hours

        this%last_update_time_seconds = -999

        ! Allocate lookback and dishcharge arrays
        allocate (this%gage_lookback_seconds(this%number_of_reservoir_gages))
        allocate (this%reservoir_gage_discharges(this%number_of_reservoir_gages))

    end subroutine read_timeslice_data_struct_init


    ! Timeslice Data Struct Destructor
    subroutine read_timeslice_data_struct_destroy(this)

        implicit none
        class(read_timeslice_data_struct), intent(inout) :: this ! object being destroyed

    end subroutine read_timeslice_data_struct_destroy


    ! Set up list of timeslice files to read
    ! The timeslices are read only once on each processor at each timeslice update time
    subroutine setup_read_timeslice(this, update_interval_seconds, current_reservoir_time_seconds, &
        reservoir_gage_id, gage_lookback_seconds, reservoir_gage_discharge)
        implicit none

        class(read_timeslice_data_struct), intent(inout) :: this
        integer, intent(in)  :: update_interval_seconds
        integer, intent(in)  :: current_reservoir_time_seconds
        integer, intent(in)  :: reservoir_gage_id
        integer, intent(out) :: gage_lookback_seconds
        real, intent(out)    :: reservoir_gage_discharge
        character(len=256)   :: timeslice_file_name
        character(len=19)    :: old_date, new_date
        character(len=2)     :: observation_resolution_string
        integer :: total_timeslice_time_periods, timeslice_file_index, reservoir_subset_index, lake_index
        integer :: observation_minute, observation_resolution_seconds
        logical :: file_exists

        ! This check ensures that the timeslice is read only once on each processor at each timeslice update time
        if (current_reservoir_time_seconds .ne. this%last_update_time_seconds) then

            ! Checks if not the first timestep, then update date.
            if (this%last_update_time_seconds .ne. -999) then
                call geth_newdate(this%current_date, this%current_date, update_interval_seconds)
            end if

            this%last_update_time_seconds = current_reservoir_time_seconds

            ! Set gage discharges of subset lakes to default -1.0
            this%reservoir_gage_discharges = -1.0

            this%gage_lookback_seconds = 0
            this%current_lookback_seconds = 0

            total_timeslice_time_periods = this%observation_lookback_hours * (60 / observation_resolution_minutes)

            ! Allocate array of all timeslice files to potentially read
            if ( .not. allocated(this%timeslice_file_names) ) then
                allocate(this%timeslice_file_names(total_timeslice_time_periods))
            end if

            old_date = this%current_date

            read (old_date(15:16), *) observation_minute

            ! Match minutes to proper observation resolution interval
            observation_minute = observation_minute / observation_resolution_minutes * observation_resolution_minutes

            ! Formatting for writing minutes
            if (observation_minute < 10) then
                old_date(15:16) = "00"
            else
                write(old_date(15:16), "(I2)") observation_minute
            end if

            ! Zero out seconds
            old_date(18:19) = "00"

            write(observation_resolution_string, "(I2)") observation_resolution_minutes

            ! Negative for going back in time
            observation_resolution_seconds = observation_resolution_minutes * 60 * -1

            ! Loop through the total timeslice periods to look for and read timeslice files
            do timeslice_file_index = 1, total_timeslice_time_periods

                ! Use below for timeslice files with colons in the time
                old_date = old_date(:13) // ':' // old_date(15:16) // ':' // old_date(18:)

                ! If any gages that still have discharges equal to -1.0, read the previous
                ! time timeslice file available.
                if ( any(this%reservoir_gage_discharges == -1.0)) then

                    ! Construct timeslice filename with path
                    timeslice_file_name = trim(this%timeslice_path) // old_date // "." // &
                                    observation_resolution_string // 'min.usgsTimeSlice.ncdf'

                    ! Check if file exists
                    inquire(FILE = timeslice_file_name, EXIST = file_exists)
                    if (file_exists) then
                        ! Call subroutine to read a particular timeslice file
                        call this%read_timeslice_file(timeslice_file_name)
                    end if

                    ! Call subroutine to get the date from one observation resolution back in time
                    call geth_newdate(new_date, old_date, observation_resolution_seconds)
                    old_date = new_date

                    ! Lookback seconds to account for how long back a timeslice file was read
                    this%current_lookback_seconds = this%current_lookback_seconds - observation_resolution_seconds

                else
                    exit
                end if
            end do

        end if

        ! Loop through gages to return gage lookback seconds and gage discharge for a given reservoir
        do reservoir_subset_index = 1, this%number_of_reservoir_gages
            if (this%reservoir_gage_ids(reservoir_subset_index) == reservoir_gage_id) then
                gage_lookback_seconds = this%gage_lookback_seconds(reservoir_subset_index)
                reservoir_gage_discharge = this%reservoir_gage_discharges(reservoir_subset_index)
            end if
        end do

    end subroutine setup_read_timeslice


    ! Read given timeslice file to get gage discharges
    subroutine read_timeslice_file(this, timeslice_file)
        implicit none
        class(read_timeslice_data_struct), intent(inout) :: this
        character(len=256), intent(in) :: timeslice_file
        integer*8, allocatable, dimension(:) :: gage_ids_integer_array
        integer :: reservoir_gage_index, timeslice_gage_index, number_of_gages
        integer :: ncid, status

        ! Open Timeslice NetCDF file
        status = nf90_open(path = trim(timeslice_file), mode = nf90_nowrite, ncid = ncid)
        if (status /= nf90_noerr) call handle_err(status, "Could not open timeslice file")

        ! Get dimension of gage arrays
        call get_timeslice_array_dimension(ncid, 'discharge', number_of_gages)

        ! Allocate gage info arrays
        allocate(this%gage_id(number_of_gages))
        allocate(this%gage_discharge(number_of_gages))
        allocate(this%gage_quality(number_of_gages))
        allocate(gage_ids_integer_array(number_of_gages))

        ! Get gage ids
        call read_timeslice_gage_ids(ncid, 'stationId', this%gage_id)

        ! Get gage discharges
        call read_timeslice_netcdf_real_1D_variables(ncid, 'discharge', this%gage_discharge)

        ! Get gage qualities
        call read_timeslice_netcdf_real_1D_variables(ncid, 'discharge_quality', this%gage_quality)

        ! Convert gage ids to integers
        do timeslice_gage_index = 1, number_of_gages
            read (this%gage_id(timeslice_gage_index), *) gage_ids_integer_array(timeslice_gage_index)
        end do

        ! Normalize gage qualities to 1
        this%gage_quality = this%gage_quality/100

        ! First, quality check on the quality flag
        where(this%gage_quality .lt. 0 .or. this%gage_quality .gt. 1) this%gage_quality=0

        ! Currently using line below that would set quality to 0 if there is a negative
        ! discharge but there may actually be a deficit in flow, i.e. no flow available
        ! at all that the transducer has given us a negative pressure. We do not account
        ! for that case here but may need to consider this later.
        where(this%gage_discharge .lt. 0.000) this%gage_quality=0

        ! Currently not using line below instead of above line that sets quality to 0 for negative discharge
        !where(this%gage_discharge .le. 0.000) this%gage_discharge = 0.0

        !! peak flow on MS river *2
        !! http://nwis.waterdata.usgs.gov/nwis/peak?site_no=07374000&agency_cd=USGS&format=html
        !! baton rouge 1945: 1,473,000cfs=41,711cfs, multiply it roughly by 2
        where(this%gage_discharge .ge. 90000.0) this%gage_quality=0

        ! Loop through reservoir gage array to look for gages that still have discharges set to -1.0.
        ! Match gage ids from the timeslice file gage id arrays to gage ids in the reservoir gage subset
        ! array. If the matched gage is good quality, then set that reservoir gage subset discharge to
        ! the discharge read from the timeslice file along with the total lookback seconds from current
        ! model time that the gage discharge is read from a timeslice file.
        do reservoir_gage_index = 1, this%number_of_reservoir_gages
            if (this%reservoir_gage_discharges(reservoir_gage_index) == -1.0) then

                do timeslice_gage_index = 1, number_of_gages
                    if (gage_ids_integer_array(timeslice_gage_index) == this%reservoir_gage_ids(reservoir_gage_index)) then
                        if (this%gage_quality(timeslice_gage_index) .gt. gage_quality_threshold) then
                            this%reservoir_gage_discharges(reservoir_gage_index) = this%gage_discharge(timeslice_gage_index)

                            this%gage_lookback_seconds(reservoir_gage_index) = this%current_lookback_seconds

                        end if
                        exit
                    end if
                end do
            end if
        end do

    ! Close timeslice NetCDF file
    status = nf90_close(ncid)
    if (status /= nf90_noerr) call handle_err(status, "Could not close timeslice file")

    ! Deallocate gage arrays
    if(allocated(this%gage_id)) deallocate(this%gage_id)
    if(allocated(this%gage_discharge)) deallocate(this%gage_discharge)
    if(allocated(this%gage_quality)) deallocate(this%gage_quality)
    if(allocated(gage_ids_integer_array)) deallocate(gage_ids_integer_array)

    end subroutine read_timeslice_file


end module module_read_timeslice_data
