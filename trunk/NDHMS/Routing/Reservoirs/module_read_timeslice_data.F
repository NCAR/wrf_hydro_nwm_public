! This module reads USGS timeslice files to get gage discharge
! values that will be used by reservoirs. It takes in the total
! number of lakes and an array of which lakes/reservoirs are
! specified to use gage discharges and then creates a subset
! array of those lakes/reservoirs. A lookback period is specified
! to determine how far back in time from the model start time the
! module will look for timeslice files. The observation resolution
! determines the time increments the module will look back. For
! instance, a standard lookback period would be 18 hours with an
! observation resolution of 15 minutes, where a model starting at
! 8:00 PM would search for timeslice files at every 15 minute
! increment between 2:00 AM and 8:00 PM that day. The module will
! first search for the most recent timeslice files and grab the
! discharge for a particular lake/reservoir if the gage quality
! standard is met at that time. If a gage discharge is missing or
! if the gage quality standard is not met for any particular
! lake/reservoir in the given timeslice file, the module will
! continue to look back at every observation resolution increment
! until either all lakes/reservoirs have a good quality discharge
! or the end of the lookback period is reached.

#define days_in_year 365
#define days_in_leap_year 366
#define timeslice_files_lookback_hours 18
#define observation_resolution_minutes 15
#define gage_quality_threshold 0.5

module module_read_timeslice_data

    use module_reservoir_utilities
    use netcdf
    implicit none

    type :: read_timeslice_data_struct

        character(len=19)   :: start_date
        character(len=19)   :: current_date
        character(len=256)  :: timeslice_path
        integer :: number_of_reservoir_gages
        integer :: current_lookback_seconds
        integer :: last_update_time_seconds
        integer, allocatable, dimension(:) :: gage_lookback_seconds
        integer, allocatable, dimension(:) :: reservoir_gage_ids_subset
        integer, allocatable, dimension(:) :: reservoir_lake_index_subset_mapping
        real, allocatable, dimension(:)    :: reservoir_gage_discharges
        real, allocatable, dimension(:)    :: reservoir_gage_discharges_all
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
        reservoir_gage_ids_all, number_of_lakes)
        implicit none
        class(read_timeslice_data_struct), intent(inout) :: this ! object being initialized
        character(len=19),  intent(in)                  :: start_date
        character(len=256), intent(in)                  :: timeslice_path
        integer,            intent(in), dimension(:)    :: reservoir_gage_ids_all
        integer,            intent(in)                  :: number_of_lakes
        integer                                         :: lake_index, gage_id_subset_index
        integer                                         :: number_of_reservoir_gages

        if (this%initialized) return
        this%initialized = .true.


        this%start_date = start_date
        this%current_date = start_date
        this%timeslice_path = timeslice_path

        !MAKE NOTE ABOUT THIS HAPPENING ON ONLY ONE CORE AT A TIME
        this%last_update_time_seconds = -999

        ! Gage discharge array for all lakes including lakes without gages
        allocate(this%reservoir_gage_discharges_all(number_of_lakes))

        ! Set gage discharges of all lakes to default -999.0
        this%reservoir_gage_discharges_all = -999.0

        this%number_of_reservoir_gages = 0

        gage_id_subset_index = 1

        ! Loop through all the lakes determine the total number of reservoir gages to look for
        do lake_index = 1, number_of_lakes
            if (reservoir_gage_ids_all(lake_index) > 0) then
                this%number_of_reservoir_gages = this%number_of_reservoir_gages + 1
            end if
        end do

        ! Allocate subset arrays of only lakes with gages
        allocate (this%gage_lookback_seconds(this%number_of_reservoir_gages))
        allocate (this%reservoir_gage_ids_subset(this%number_of_reservoir_gages))
        allocate (this%reservoir_gage_discharges(this%number_of_reservoir_gages))
        allocate (this%reservoir_lake_index_subset_mapping(this%number_of_reservoir_gages))


        ! Set gage discharges of subset lakes to default -1.0
        !this%reservoir_gage_discharges = -1.0

        ! Get gage ids to search for in timeslice files
        do lake_index = 1, number_of_lakes

            if (reservoir_gage_ids_all(lake_index) > 0) then
                this%reservoir_gage_ids_subset(gage_id_subset_index) = reservoir_gage_ids_all(lake_index)
                this%reservoir_lake_index_subset_mapping(gage_id_subset_index) = lake_index
                gage_id_subset_index = gage_id_subset_index + 1
            end if

        end do

        print *, '!!!!!!!!!!!!!!!!!!!!!!!!'
        print *, 'at timeslice init'
        print *, '!!!!!!!!!!!!!!!!!!!!!!!!'

    end subroutine read_timeslice_data_struct_init


    ! Timeslice Data Struct Destructor
    subroutine read_timeslice_data_struct_destroy(this)

        implicit none
        class(read_timeslice_data_struct), intent(inout) :: this ! object being destroyed

    end subroutine read_timeslice_data_struct_destroy


    ! Set up list of timeslice files to read
    subroutine setup_read_timeslice(this, update_interval_seconds, current_reservoir_time_seconds, reservoir_gage_id, gage_lookback_seconds, reservoir_gage_discharge)
        implicit none

        class(read_timeslice_data_struct), intent(inout) :: this
        integer, intent(in)  :: update_interval_seconds
        integer, intent(in)  :: current_reservoir_time_seconds
        !integer, intent(in) :: lake_number               ! lake number
        integer, intent(in)  :: reservoir_gage_id
        integer, intent(out) :: gage_lookback_seconds
        real, intent(out)    :: reservoir_gage_discharge
        character(len=256)   :: timeslice_file_name
        character(len=19)    :: old_date, new_date
        character(len=2)     :: observation_resolution_string
        integer :: total_timeslice_time_periods, timeslice_file_index, reservoir_subset_index, lake_index
        integer :: observation_minute, observation_resolution_seconds
        logical :: file_exists

print *, 'current_reservoir_time_seconds'
print *, current_reservoir_time_seconds

print *, 'this%last_update_time_seconds'
print *, this%last_update_time_seconds


        if (current_reservoir_time_seconds .ne. this%last_update_time_seconds) then

        print *, '!!!!!!!!!!!!!!!!!!!!!!!!'
        print *, 'at timeslice setup read'
        print *, '!!!!!!!!!!!!!!!!!!!!!!!!'

            ! CAN GET NEW DATE HERE IN RETRO MODE WITH DAILY UPDATE INTERVAL
            ! BUT CAN'T USE THE SAME CODE FOR FORECAST BECAUSE UPATE INTERVAL IS 1E8
            ! IF FIRST RUN
            !!!!

            if (this%last_update_time_seconds .ne. -999) then
                call geth_newdate(this%current_date, this%current_date, update_interval_seconds)
            end if

        print *, 'this%current_date'
        print *, this%current_date

            this%last_update_time_seconds = current_reservoir_time_seconds

            !NEW TO THIS subroutine
            ! Set gage discharges of subset lakes to default -1.0
            this%reservoir_gage_discharges = -1.0

            this%gage_lookback_seconds = 0
            this%current_lookback_seconds = 0


            total_timeslice_time_periods = timeslice_files_lookback_hours * (60 / observation_resolution_minutes)

            ! Allocate array of all timeslice files to potentially read
            if ( .not. allocated(this%timeslice_file_names) ) then
                allocate(this%timeslice_file_names(total_timeslice_time_periods))
            end if

!!!!!
            ! DOUBLE CHECK THIS IF TIME IS OR ROUTING PERIOD
            !if (current_reservoir_time_seconds > 0) then
            !    call geth_newdate(this%current_date, this%current_date, update_interval_seconds)
            !end if
!!!!

            old_date = this%current_date

            read (old_date(15:16), *) observation_minute

            observation_minute = observation_minute / observation_resolution_minutes * observation_resolution_minutes

            if (observation_minute < 10) then
                old_date(15:16) = "00"
            else
                write(old_date(15:16), "(I2)") observation_minute
            end if

            old_date(18:19) = "00"

            write(observation_resolution_string, "(I2)") observation_resolution_minutes

            observation_resolution_seconds = observation_resolution_minutes * 60 * -1

    !!!!
    !TEMP LOGGING
    open (112,file="current_all_gage_discharges.txt",status="unknown")
    !!!!!!
            do timeslice_file_index = 1, total_timeslice_time_periods

                ! Format the date for timeslice files
                old_date = old_date(:13) // '_' // old_date(15:16) // '_' // old_date(18:)

                ! Use below for Timeslice files with colons
                !old_date = old_date(:13) // ':' // old_date(15:16) // ':' // old_date(18:)

                ! If any gages that still have discharges equal to -1.0, read the previous
                ! time timeslice file available.
                if ( any(this%reservoir_gage_discharges == -1.0)) then

                    ! Construct timeslice filename with path
                    timeslice_file_name = trim(this%timeslice_path) // old_date // "." // &
                                    observation_resolution_string // 'min.usgsTimeSlice.ncdf'

                    print *, 'timeslice_file_name'
                    print *, timeslice_file_name

                    inquire(FILE = timeslice_file_name, EXIST = file_exists)
                    if (file_exists) then
                        ! Call subroutine to read a particular timeslice file
                        call this%read_timeslice_file(timeslice_file_name)
                    end if

                    ! Call subroutine to get the date from one observation resolution back in time
                    call geth_newdate(new_date, old_date, observation_resolution_seconds)
                    old_date = new_date

                    this%current_lookback_seconds = this%current_lookback_seconds - observation_resolution_seconds


                else
                    exit
                end if
            end do

    !!!!!!!!
    !TEMP LOGGING
    close (112)
    !!!!!!!


            ! Map reservoir discharges subset back to array of all reservoir discharges
            !do reservoir_subset_index = 1, this%number_of_reservoir_gages
            !    lake_index = this%reservoir_lake_index_subset_mapping(reservoir_subset_index)
            !    this%reservoir_gage_discharges_all(lake_index) = this%reservoir_gage_discharges(reservoir_subset_index)
            !end do

        end if


        do reservoir_subset_index = 1, this%number_of_reservoir_gages
            if (this%reservoir_gage_ids_subset(reservoir_subset_index) == reservoir_gage_id) then
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

        print *, '!!!!!!!!!!!!!!!!'
        print *, 'in timeslice file read'
        print *, '!!!!!!!!!!!!!!!!'


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

        ! Currently not using line below that would set quality to 0 if there is a negative
        ! discharge because there may actually be a deficit in flow, i.e. no flow available
        ! at all that the transducer has given us a negative pressure.
        !where(this%gage_discharge .le. 0.000) this%gage_quality=0

        ! Currently using line below instead of above line that sets quality to 0 for negative discharge
        where(this%gage_discharge .le. 0.000) this%gage_discharge = 0.0

        !! peak flow on MS river *2
        !! http://nwis.waterdata.usgs.gov/nwis/peak?site_no=07374000&agency_cd=USGS&format=html
        !! baton rouge 1945: 1,473,000cfs=41,711cfs, multiply it roughly by 2
        where(this%gage_discharge .ge. 90000.0) this%gage_quality=0

        print *, this%number_of_reservoir_gages
        print *, this%reservoir_gage_ids_subset
        print *, this%reservoir_gage_discharges

        print *, 'number_of_gages'
        print *, number_of_gages

        print *, 'gage_ids_integer_array'
        print *, gage_ids_integer_array

        print *, 'this%gage_discharge'
        print *, this%gage_discharge

        print *, 'this%gage_quality'
        print *, this%gage_quality

        ! Loop through reservoir gage subset to look for gages that still have discharges set to -1.0.
        ! Match gage ids from the timeslice file gage id arrays to gage ids in the reservoir gage subset
        ! array. If the matched gage is good quality, then set that reservoir gage subset discharge to
        ! the discharge read from the timeslice file.
        do reservoir_gage_index = 1, this%number_of_reservoir_gages
            if (this%reservoir_gage_discharges(reservoir_gage_index) == -1.0) then

                do timeslice_gage_index = 1, number_of_gages
                    if (gage_ids_integer_array(timeslice_gage_index) == this%reservoir_gage_ids_subset(reservoir_gage_index)) then
                        if (this%gage_quality(timeslice_gage_index) .gt. gage_quality_threshold) then
                            this%reservoir_gage_discharges(reservoir_gage_index) = this%gage_discharge(timeslice_gage_index)

                            this%gage_lookback_seconds(reservoir_gage_index) = this%current_lookback_seconds

                            !open (112,file="gage_discharges.txt",status="unknown")

                            write (112, "(4A25)") "------------"

                            Write (112, "(I20)") gage_ids_integer_array(timeslice_gage_index)


                            Write (112, "(F15.5)") this%gage_discharge(timeslice_gage_index)


                            write (112, "(4A100)") timeslice_file

                            Write (112, "(I20)") this%current_lookback_seconds

                            write (112, "(4A25)") "------------"

                            !close (112)


                        end if
                        exit
                    end if
                end do
            end if
        end do


        ! BELOW SOLEY TEMP FOR LOGGING PURPOSES
        !do reservoir_gage_index = 1, this%number_of_reservoir_gages
        !    if (this%reservoir_gage_discharges(reservoir_gage_index) == -1.0) then


        !    end if
        !end do


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
