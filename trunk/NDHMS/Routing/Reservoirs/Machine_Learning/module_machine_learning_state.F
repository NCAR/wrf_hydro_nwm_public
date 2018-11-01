! This module defines and instantiates objects
! for a machine learning type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given machine learning
! reservoir object and not other modules or areas
! of the system.

! Time between running machine learning model to update the outflow
#define update_time_interval 86400
#define pi 3.14159265359

module module_machine_learning_state

    use netcdf
    !use module_HYDRO_io, only : RESTART_OUT_nc
    !use module_namelist, only: nlst_rt
    use module_reservoir_utilities
    !use module_machine_learning_utilities
    use module_reservoir_base
    implicit none

    ! Extend/derive machine learning state from the abstract base
    ! struct for reservoir state.
    type, extends(reservoir_base_state_struct) :: machine_learning_state_interface
        !real :: current_storage
        real :: current_time_storage
        real :: end_time_storage
        !real :: accumulated_storage
        real :: average_storage_over_past_time_intervals
		real :: totaled_storage
		!real :: average_storage
        real :: previous_time_release
        real :: average_inflow_over_past_time_intervals
        real :: accumulated_inflow
        integer :: routing_period
        integer :: time_interval
        integer :: update_time    ! if reals how can we set exactly equals == ?
        !integer :: time_count
        integer :: release_count
        integer :: current_time        ! should be 'real' instead???
        character(len=19) :: start_date  ! keep as state var??
        integer :: start_day_of_year
        integer :: current_day_of_year
        integer :: start_month
        integer :: current_month
        integer :: start_year
        integer :: current_year
        real :: x_day_of_year
        real :: y_day_of_year



    contains

        procedure :: init => machine_learning_state_init
        procedure :: destroy => machine_learning_state_destroy
        !procedure :: update_time


    end type machine_learning_state_interface

contains

    !TAKE OUT ELEVATION STUFF????


    !Machine Learning State Constructor
    subroutine machine_learning_state_init(this, start_date, restart_flag, lake_number)
        use netcdf
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being initialized

        character(len=19), intent(in) :: start_date

        integer, intent(in) :: restart_flag              ! signals if NWM is on a restart or not
                                                         ! 1 is cold start and 99 is restart

        integer, intent(in) :: lake_number              ! lake number

        integer :: start_month, start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index

        !real :: pi



        integer :: lake_index, lake_id_index

        integer, allocatable, dimension (:) :: lake_id

        integer :: Nlakes
        integer :: ncid, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId, minimum_storageVarId, maximum_storageVarId, lake_idVarId, lake_id_single_VarId
        integer :: minimum_releaseVarId, maximum_releaseVarId
        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs

        integer :: status       ! Status of reading NetCDF


        integer :: monthly_averages_rows !


        real, allocatable, dimension (:)    ::long_term_release ! this represent a long-term average of the release to be used for previous day release at the very first day of calc!



        real, allocatable, dimension (:,:)  :: ID_monthly_averages
        real, allocatable, dimension (:,:)  :: release_monthly_averages
        real, allocatable, dimension (:,:)  :: elevation_monthly_averages
        real, allocatable, dimension (:,:)  :: inflow_monthly_averages
        real, allocatable, dimension (:,:)  :: storage_monthly_averages


        real :: release_monthly_average, storage_monthly_average

        integer :: ID_monthly_averagesVarId, release_monthly_averagesVarId, elevation_monthly_averagesVarId
        integer :: inflow_monthly_averagesVarId, storage_monthly_averagesVarId

        print *, "6"


        this%start_date = start_date


        !Reservoir Parm NC file
        status = nf90_open(path = "ML_RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !print *, "status", status
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_inq_varid(ncid, "lake_id", lake_idVarId)


        print *, "7"



        ! Monthly averages
        status = nf90_inq_varid(ncid, "ID_monthly_averages", ID_monthly_averagesVarId)


        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "release_monthly_averages", release_monthly_averagesVarId)

        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "elevation_monthly_averages", elevation_monthly_averagesVarId)

        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inq_varid(ncid, "inflow_monthly_averages", inflow_monthly_averagesVarId)

        if(status /= nf90_NoErr) call handle_err(status)


        status = nf90_inq_varid(ncid, "storage_monthly_averages", storage_monthly_averagesVarId)

        if(status /= nf90_NoErr) call handle_err(status)


        print *, "8"







        status = nf90_inquire_variable(ncid, lake_idVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(lake_id(Nlakes))



        print *, "9"



        ! Monthly averages

        status = nf90_inquire_variable(ncid, ID_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(ID_monthly_averages(monthly_averages_rows, Nlakes))


        status = nf90_inquire_variable(ncid, release_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(release_monthly_averages(monthly_averages_rows, Nlakes))


        print *, "10"


        status = nf90_inquire_variable(ncid, elevation_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(elevation_monthly_averages(monthly_averages_rows, Nlakes))


        print *, "11"



        status = nf90_inquire_variable(ncid, inflow_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(inflow_monthly_averages(monthly_averages_rows, Nlakes))


        status = nf90_inquire_variable(ncid, storage_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(storage_monthly_averages(monthly_averages_rows, Nlakes))




        print *, "12"




        status = nf90_get_var(ncid, lake_idVarId, lake_id)


        if (status /= nf90_noerr) call handle_err(status)


        print *, "13"


        !Monthly averages

        status = nf90_get_var(ncid, ID_monthly_averagesVarId, ID_monthly_averages)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, release_monthly_averagesVarId, release_monthly_averages)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, elevation_monthly_averagesVarId, elevation_monthly_averages)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, inflow_monthly_averagesVarId, inflow_monthly_averages)

        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_get_var(ncid, storage_monthly_averagesVarId, storage_monthly_averages)

        if (status /= nf90_noerr) call handle_err(status)




        print *, "14"



        lake_id_index = 0

        do lake_index = 1, Nlakes
            if (lake_id(lake_index) == lake_number) then
                lake_id_index = lake_index
            end if

        end do

        print *, "15"


        ! Check grab monthly avg from start date and set previous day storage and release to that.


        !integer :: start_month, start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index


        read (this%start_date(6:7), *) start_month
        read (this%start_date(9:10), *) start_day_of_month
        read (this%start_date(1:4), *) start_year


        print *, "16"

        print *, release_monthly_averages

        print *, "release_monthly_average"

        print *, release_monthly_average

        print *, lake_id_index

        print *, start_month

        print *, lake_id



        release_monthly_average = release_monthly_averages(lake_id_index, start_month)

        print *, "16.05"



        storage_monthly_average = storage_monthly_averages(lake_id_index, start_month)

        print *, "16.1"



        ! If cold start
        if (restart_flag == 1) then


            this%previous_time_release = release_monthly_average

            this%average_storage_over_past_time_intervals = storage_monthly_average

            !this%accumulated_storage = storage_monthly_average

            !this%current_storage = storage_monthly_average
            this%current_time_storage = storage_monthly_average

            print *, "16.2"

            this%end_time_storage = storage_monthly_average

            this%totaled_storage = 0.0

            this%time_interval = update_time_interval   ! seconds in 24 hours

            this%current_time = 0

            this%update_time = 0

            this%routing_period = 0

            this%average_inflow_over_past_time_intervals = 0.0

            this%accumulated_inflow = 0.0

        ! Restart
        !else


        end if


        months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

        start_day_of_year = 0

        print *, "16.5"


        if (((mod(start_year,4)==0 .and. mod(start_year,100)/=0) .or. mod(start_year,400)==0) .and. start_month>2) then
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do
            start_day_of_year = start_day_of_year + start_day_of_month + 1    ! add one day for leap year after February


        print *, "16.6"


        else
            do month_loop_index = 1, start_month-1
                start_day_of_year = start_day_of_year + months(month_loop_index)
            end do

            start_day_of_year = start_day_of_year + start_day_of_month     ! non leap year or leap year before March
        end if


        print *, "17"


        this%start_year = start_year

        this%current_year = start_year

        this%start_month = start_month

        this%current_month = start_month

        this%start_day_of_year = start_day_of_year

        this%current_day_of_year = start_day_of_year

        !pi = 3.14159265359


        if (((mod(this%start_year,4)==0 .and. mod(this%start_year,100)/=0) .or. mod(this%start_year,400)==0) .and. this%start_month>2) then
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%start_day_of_year / 366) ! leap year after February
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%start_day_of_year / 366)
        else
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%start_day_of_year / 365) ! non leap year or leap year before March
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%start_day_of_year / 365)
        end if


        print *, "18"





    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy




    !subroutine update_time(this, time_interval)
    !    implicit none
    !    class(machine_learning_state_interface), intent(inout) :: this ! the type object


    !    real, intent(in) :: time_interval

        !integer, intent(in) :: lake_number              ! lake number

    !    real :: storage_accum

    !    real :: average_inflow

    !    integer :: count


    !end subroutine update_time



end module module_machine_learning_state



! The functions that have been used for reading NetCDF file!
!function nf90_inq_varid(ncid, name, varid)
!    integer, intent(in) :: ncid
!    character (len = *), intent( in) :: name
!    real, intent(out) :: varid
!    integer :: nf90_inq_varid
!end function nf90_inq_varid


!function nf90_get_var(ncid, varid, values)
!    integer, intent( in) :: ncid, varid
!    real, intent(out) :: values
!    integer :: nf90_get_var
!end function nf90_get_var


!function nf90_strerror(ncerr)
!    integer, intent( in) :: ncerr
!    character(len = 80) :: nf90_strerror
!end function nf90_strerror

!subroutine handle_err(status)
!    implicit none
!    integer, intent ( in) :: status
!    character :: nf90_strerror
!    integer  :: nf90_noerr
!    if(status /= nf90_noerr) then
        !print *, trim(nf90_strerror(status))
        !print *, nf90_strerror(status) !FIXME this line deactivated, but we need to check on it!
!        stop "Stopped"
!    end if
!end subroutine handle_err

!function nf90_open(path, mode, ncid, chunksize)
!    character (len = *), intent(in   ) :: path
!    integer,             intent(in   ) :: mode
!    integer,             intent(  out) :: ncid
!    integer, optional,   intent(inout) :: chunksize
!    integer                            :: nf90_open
!end function nf90_open

!function nf90_inquire_variable(ncid, varid, name, xtype, ndims, dimids, nAtts)
!    integer,                         intent( in) :: ncid, varid
!    character (len = *),   optional, intent(out) :: name
!    integer, dimension(*), optional, intent(out) :: dimids
!    integer                                      :: nf90_inquire_variable
!end function nf90_inquire_variable


!function nf90_inquire_dimension(ncid, dimid, name, len)
!    integer,                       intent( in) :: ncid, dimid
!    character (len = *), optional, intent(out) :: name
!    integer,             optional, intent(out) :: len
!    integer                                    :: nf90_inquire_dimension
!end function nf90_inquire_dimension

