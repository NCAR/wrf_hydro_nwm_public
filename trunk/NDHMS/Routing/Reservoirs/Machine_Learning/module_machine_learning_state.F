! This module defines and instantiates objects
! for a machine learning type reservoir's state.
! State holds and tracks dynamic/changing variables
! that are only relevant to the given machine learning
! reservoir object and not other modules or areas
! of the system.

! Time between running machine learning model to update the outflow
#define update_time_interval 86400
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
        real  :: water_elevation
        real*4 :: current_time_storage
        real :: end_time_storage
        real :: average_storage_over_past_time_intervals
		real :: totaled_storage
        real :: previous_time_release
        real :: average_inflow_over_past_time_intervals
        real :: accumulated_inflow
        integer :: time_interval
        integer :: update_time
        integer :: new_day_time
        integer :: release_count
        integer :: current_time
        character(len=19) :: start_date
        integer :: start_day_of_year
        integer :: current_day_of_year
        integer :: start_month
        integer :: current_month
        integer :: start_year
        integer :: current_year
        real :: x_day_of_year
        real :: y_day_of_year
        logical :: previous_release_set_from_timeslice



    contains

        procedure :: init => machine_learning_state_init
        procedure :: destroy => machine_learning_state_destroy

    end type machine_learning_state_interface

contains

!!!!!!
!CAN WE JUST DO THE INQUIRE DIMENSION ONCE??????
!!!!!!



    !Machine Learning State Constructor
    subroutine machine_learning_state_init(this, water_elevation, start_date, &
        machine_learing_parameter_file, restart_flag, lake_number, gage_ids, &
        gage_qualities, gage_discharges)
        use netcdf
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being initialized
        real, intent(inout) :: water_elevation
        character(len=19), intent(in) :: start_date
        character(len=256), intent(in) :: machine_learing_parameter_file
        integer, intent(in) :: restart_flag              ! signals if NWM is on a restart or not
                                                         ! 1 is cold start and 99 is restart
        integer, intent(in) :: lake_number              ! lake number
        character(len=15), intent(in), dimension(:) :: gage_ids
        real, intent(in), dimension(:) :: gage_qualities, gage_discharges


        integer :: time_interval
        integer :: start_month, start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index
        integer :: lake_index, lake_id_index
        integer, allocatable, dimension (:) :: lake_id

        integer :: Nlakes
        integer :: ncid, biasVarId, minimum_of_featuresVarId, maximum_of_featuresVarId, weightsVarId, input_namesVarId
        integer :: activation_functionsVarId, number_of_nodesVarId, minimum_storageVarId, maximum_storageVarId, lake_idVarId
        integer :: time_intervalVarId, gage_ids_of_ML_lakes1_var_id, gage_ids_of_ML_lakes2_var_id
        character, external :: nf90_strerror1
        integer, dimension(nf90_max_var_dims) :: dimIDs

        integer :: status       ! Status of reading NetCDF


        integer :: monthly_averages_rows

        !REMOVE???????
        real, allocatable, dimension (:)    ::long_term_release ! this represent a long-term average of the release to be used for previous day release at the very first day of calc!

        real, allocatable, dimension (:,:)  :: ID_monthly_averages
        real, allocatable, dimension (:,:)  :: release_monthly_averages
        real, allocatable, dimension (:,:)  :: elevation_monthly_averages
        real, allocatable, dimension (:,:)  :: inflow_monthly_averages
        real, allocatable, dimension (:,:)  :: storage_monthly_averages
        integer, allocatable, dimension (:) :: time_intervals

        integer, allocatable, dimension (:)  :: gage_ids_of_ML_lakes1
        integer, allocatable, dimension (:)  :: gage_ids_of_ML_lakes2

        integer :: timeslice_gage_id_integer, gage2_id_integer, timeslice_gage1_id_index, timeslice_gage2_id_index

        real :: gage1_discharge, gage2_discharge

        real :: release_monthly_average, storage_monthly_average

        integer :: ID_monthly_averagesVarId, release_monthly_averagesVarId, elevation_monthly_averagesVarId
        integer :: inflow_monthly_averagesVarId, storage_monthly_averagesVarId

        this%water_elevation = water_elevation

        this%start_date = start_date

        this%previous_release_set_from_timeslice = .false.


        !Reservoir Parm NC file
        !status = nf90_open(path = "ML_RESERVOIR_PARM.nc", mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        status = nf90_open(path = machine_learing_parameter_file, mode = nf90_nowrite, ncid = ncid)                 ! open existing netCDF dataset
        !print *, "status", status
        if (status /= nf90_noerr) call handle_err(status)



        status = nf90_inq_varid(ncid, "lake_id", lake_idVarId)



        if (status /= nf90_noerr) call handle_err(status)



        status = nf90_inq_varid(ncid, "time_interval", time_intervalVarId)

        if (status /= nf90_noerr) call handle_err(status)



        status = nf90_inq_varid(ncid, "gage_id1", gage_ids_of_ML_lakes1_var_id)

        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_inq_varid(ncid, "gage_id2", gage_ids_of_ML_lakes2_var_id)

        if (status /= nf90_noerr) call handle_err(status)



        status = nf90_inquire_variable(ncid, gage_ids_of_ML_lakes1_var_id, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(gage_ids_of_ML_lakes1(Nlakes))


        status = nf90_get_var(ncid, gage_ids_of_ML_lakes1_var_id, gage_ids_of_ML_lakes1)


        if (status /= nf90_noerr) call handle_err(status)

        print *, 'gage_ids_of_ML_lakes1'
        print *, gage_ids_of_ML_lakes1





        status = nf90_inquire_variable(ncid, gage_ids_of_ML_lakes2_var_id, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(gage_ids_of_ML_lakes2(Nlakes))


        status = nf90_get_var(ncid, gage_ids_of_ML_lakes2_var_id, gage_ids_of_ML_lakes2)


        if (status /= nf90_noerr) call handle_err(status)



        print *, 'gage_ids_of_ML_lakes2'
        print *, gage_ids_of_ML_lakes2



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


        status = nf90_inquire_variable(ncid, lake_idVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(lake_id(Nlakes))


        status = nf90_inquire_variable(ncid, time_intervalVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)

        allocate(time_intervals(Nlakes))



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


        status = nf90_inquire_variable(ncid, elevation_monthly_averagesVarId, dimids = dimIDs)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(1), len = monthly_averages_rows)
        if(status /= nf90_NoErr) call handle_err(status)

        status = nf90_inquire_dimension(ncid, dimIDs(2), len = Nlakes)
        if(status /= nf90_NoErr) call handle_err(status)


        allocate(elevation_monthly_averages(monthly_averages_rows, Nlakes))


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


        status = nf90_get_var(ncid, lake_idVarId, lake_id)


        if (status /= nf90_noerr) call handle_err(status)


        status = nf90_get_var(ncid, time_intervalVarId, time_intervals)


        if (status /= nf90_noerr) call handle_err(status)



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



        lake_id_index = 0

        do lake_index = 1, Nlakes
            if (lake_id(lake_index) == lake_number) then
                lake_id_index = lake_index
            end if

        end do


        time_interval = time_intervals(lake_id_index)


        ! Check grab monthly avg from start date and set previous day storage and release to that.


        !integer :: start_month, start_day_of_month, start_year, start_day_of_year, months(12), month_loop_index


        read (this%start_date(6:7), *) start_month
        read (this%start_date(9:10), *) start_day_of_month
        read (this%start_date(1:4), *) start_year



        release_monthly_average = release_monthly_averages(lake_id_index, start_month)


print *, '--------------------------------------------------------------------------------------'
print *, '--------------------------------------------------------------------------------------'
print *, '--------------------------------------------------------------------------------------'

        storage_monthly_average = storage_monthly_averages(lake_id_index, start_month)


        print *, 'lake_id_index'

        print *, lake_id_index


        print *, 'gage_ids_of_ML_lakes1(lake_id_index)'


        print *, gage_ids_of_ML_lakes1(lake_id_index)





        !integer :: gage1_id_integer, gage2_id_integer, timeslice_gage1_id_index, timeslice_gage2_id_index


!IF ML RESERVOIR IS SET TO READ FROM TIMESLCE THEN USE THAT FOR PREV TIME RELEASE INSTEAD OF HISTORICAL

        ! If cold start
        !if (restart_flag == 1) then

                    !BEST TO USE -999????????????
            if (gage_ids_of_ML_lakes1(lake_id_index) /= -999) then

                print *, 'not null'

                print *, 'size(gage_ids)'

                print *, size(gage_ids)

                do timeslice_gage1_id_index = 1, size(gage_ids)

                    print *, 'timeslice_gage1_id_index'

                    print *, timeslice_gage1_id_index

                    read(gage_ids(timeslice_gage1_id_index), *) timeslice_gage_id_integer

                    print *, 'timeslice_gage_id_integer'
                    print *,timeslice_gage_id_integer

                    if (gage_ids_of_ML_lakes1(lake_id_index) == timeslice_gage_id_integer) then

                        !if (gage_qualities(timeslice_gage1_id_index) == 100) then

                        !    gage1_discharge = gage_discharges(timeslice_gage_id_index)

                        !else


                        gage1_discharge = gage_discharges(timeslice_gage1_id_index)

                            print *, 'gage1_discharge'
                            print *, gage1_discharge


                    end if

                end do


                !if (gage_ids_of_ML_lakes2(lake_id_index) /= -999) then
                !    print *, 'second gage id loop'

                !    do timeslice_gage2_id_index = 1, size(gage_ids)

                !        print *, 'timeslice_gage2_id_index'
                !        print *, timeslice_gage2_id_index


                !        read(gage_ids(timeslice_gage2_id_index), *) timeslice_gage_id_integer

                !        if (gage_ids_of_ML_lakes2(lake_id_index) == timeslice_gage_id_integer) then

                !            gage2_discharge = gage_discharges(timeslice_gage2_id_index)


                !            print *, 'gage2_discharge'
                !            print *, gage2_discharge

                !        end if

                !    end do

                !    if (gage_qualities(timeslice_gage1_id_index) == 100 .and. gage_qualities(timeslice_gage2_id_index) == 100) then

                !        this%previous_time_release = gage2_discharge - gage1_discharge

                !        print *, 'this%previous_time_release = gage2_discharge - gage1_discharge'


                !    else
                !        this%previous_time_release = release_monthly_average

                !        print *, 'this%previous_time_release = release_monthly_average'

                !    end if



                !else


                    if (gage_qualities(timeslice_gage1_id_index) == 100) then

                        this%previous_time_release = gage1_discharge

                        this%previous_release_set_from_timeslice = .true.


                    else
                        this%previous_time_release = release_monthly_average

                    end if


                !end if



                print *, '(gage_qualities(timeslice_gage1_id_index))'
                print *, (gage_qualities(timeslice_gage1_id_index))



            else

                print *, 'null'


                this%previous_time_release = release_monthly_average

            end if


            print *, 'this%previous_time_release'

            print *, this%previous_time_release



            this%average_storage_over_past_time_intervals = storage_monthly_average

            this%current_time_storage = storage_monthly_average

            this%end_time_storage = storage_monthly_average

            this%totaled_storage = 0.0


            ! Gets time interval from netcdf
            this%time_interval = time_interval



            this%current_time = 0

            this%update_time = 0

            this%new_day_time = seconds_in_day

            this%average_inflow_over_past_time_intervals = 0.0

            this%accumulated_inflow = 0.0

        ! Restart
        !else


        !end if


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


        this%start_year = start_year

        this%current_year = start_year

        this%start_month = start_month

        this%current_month = start_month

        this%start_day_of_year = start_day_of_year

        this%current_day_of_year = start_day_of_year


        if (((mod(this%start_year,4)==0 .and. mod(this%start_year,100)/=0) .or. mod(this%start_year,400)==0) .and. this%start_month>2) then
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%start_day_of_year / 366) ! leap year after February
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%start_day_of_year / 366)
        else
            this%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * this%start_day_of_year / 365) ! non leap year or leap year before March
            this%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * this%start_day_of_year / 365)
        end if



    end subroutine machine_learning_state_init

    !Machine Learning State Destructor
    subroutine machine_learning_state_destroy(this)
        implicit none
        class(machine_learning_state_interface), intent(inout) :: this ! the type object being destroyed

    end subroutine machine_learning_state_destroy


end module module_machine_learning_state
