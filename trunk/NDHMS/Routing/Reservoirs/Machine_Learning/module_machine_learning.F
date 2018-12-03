! This module defines and instantiates objects
! for a machine learning type reservoir. The machine
! learning reservoir struct inherits input and
! output stucts from the reservoir base
! module and calls instantiation of these into
! sub-objects. The machine learning reservoir struct
! also points to stucts for machine learning parameters
! and state and calls instantiation of these into
! sub-objects. This module also contains the
! subroutine to run machine learning release that is
! derived from the reservoir base struct interface
! to run release. Running machine learning release will
! then call the MACHINE_LEARNING subroutine, which
! processes the given inputs, parameters, and
! state for a particular machine learning reservoir and
! return the output/outflow.

#define days_in_year 365
#define days_in_leap_year 366
#define seconds_in_day 86400
#define pi 3.14159265359

module module_machine_learning

    use netcdf
    use module_machine_learning_parameters
    use module_machine_learning_state
    use module_reservoir_base
    use module_machine_learning_model
    !use module_machine_learning_utilities
    use module_reservoir_utilities
    use module_machine_learning_tests
    use module_io_utilities


    implicit none


    ! Extend/derive machine learning struct from the abstract base
    ! struct for reservoirs.
    type, extends(reservoir_base_struct) :: machine_learning_struct

        ! Define pointers to sub-structs / sub-objects to and
        ! held by a machine learning reservoir object.
        type (machine_learning_parameters_interface), pointer :: parameters => null()
        type (machine_learning_state_interface), pointer :: state => null()

        logical :: pointer_allocation_guard = .false.

    contains

        procedure :: init => machine_learning_struct_init
        procedure :: destroy => machine_learning_struct_destroy
        procedure :: run_release => run_machine_learning_release
        procedure :: write_to_restart_file => machine_learning_write_to_restart_file
        procedure :: read_from_restart_file => machine_learning_read_from_restart_file
        procedure :: get_variables => machine_learning_get_variables


        procedure :: timestep_pre_conditions_warn => machine_learning_timestep_pre_conditions_warn
        procedure :: timestep_pre_conditions_modify => machine_learning_timestep_pre_conditions_modify
        procedure :: timestep_post_conditions_warn => machine_learning_timestep_post_conditions_warn
        procedure :: timestep_post_conditions_modify => machine_learning_timestep_post_conditions_modify
        procedure :: update_pre_conditions_warn => machine_learning_update_pre_conditions_warn
        procedure :: update_pre_conditions_modify => machine_learning_update_pre_conditions_modify
        procedure :: update_post_conditions_warn => machine_learning_update_post_conditions_warn
        procedure :: update_post_conditions_modify => machine_learning_update_post_conditions_modify
        procedure :: update_preprocessing => update_preprocessing
        procedure :: timestep_preprocessing => timestep_preprocessing


    end type machine_learning_struct

contains

    !Machine Learning Constructor
    subroutine machine_learning_struct_init(this, start_date, restart_flag, lake_number)

        implicit none
        class(machine_learning_struct), intent(inout) :: this ! object being initialized
        character(len=19), intent(in) :: start_date
        integer, intent(in) :: restart_flag              ! signals if NWM is on a restart or not
                                                         ! 1 is cold start and 99 is restart
        integer, intent(in) :: lake_number               ! lake number
                                                         ! TODO: potentially change to lake_id for this and
                                                         ! LP and Pers modules. lake_number is used in LP
                                                         ! which has been submitted to a PR.
        integer :: model_counter, model_index, model_id

        if (this%pointer_allocation_guard .eqv. .false. ) then

            ! try to allocate input
            allocate ( this%input )

            if ( .not. associated(this%input) ) then
                ! if the input structure could not be created write an error
                write(0,*) "Failure to allocate machine learning input structure"

            else
                ! initialize the input structure
                call this%input%init()

            end if

            ! try to allocate output
            allocate ( this%output )

            if ( .not. associated(this%output) ) then
                ! if the output structure could not be created write an error
                write(0,*) "Failure to allocate machine learning output structure"

            else
                ! initialize the output structure
                call this%output%init()

            end if

            if ( .not. allocated(machine_learning_models) ) then
                number_of_unique_models = 0
            end if

            ! try to allocate parameters
            allocate ( this%parameters )

            if ( .not. associated(this%parameters) ) then
                ! if the parameters structure could not be created write an error
                write(0,*) "Failure to allocate machine_learning parameters structure"

            else

                ! initialize machine_learning parameters
                call this%parameters%init( lake_number, number_of_unique_models )
            end if

            this%pointer_allocation_guard = .true.

            ! try to allocate state
            allocate ( this%state )

            if ( .not. associated(this%state) ) then
                ! if the state structure could not be created write an error
                write(0,*) "Failure to allocate state parameters structure"

            else

                ! initialize machine_learning state
                call this%state%init( start_date, restart_flag, lake_number )

            end if

            this%pointer_allocation_guard = .true.

        end if

        ! Model Initialization
        ! This if statement is only entered on first machine learning reservoir
        if ( .not. allocated(machine_learning_models) ) then

            ! Allocates array of pointers to machine learning models.
            allocate(machine_learning_models(number_of_unique_models))

            ! Allocates array of machine learning model numbers that match
            ! the indices of the machine learning models array.
            allocate(machine_learning_model_ids(number_of_unique_models))

            machine_learning_model_ids = 0 ! set model number array values to 0

            do model_counter = 1, number_of_unique_models
                nullify(machine_learning_models(model_counter)%ptr)
            end do

        end if

        this%parameters%model_index = 0
        do model_counter = 1, number_of_unique_models

                ! If the model has already been initialized, and the passed model number
                ! matches the model number in the array, then assign that slot to
                ! model_index
                if (machine_learning_model_ids(model_counter) == this%parameters%model_id) then
                        this%parameters%model_index = model_counter
                        exit

                ! Check if any slot is zero, which means a machine learning model has
                ! not been initialized or assigned to that slot.
                else if (machine_learning_model_ids(model_counter) == 0) then

                        ! Assign the model id to the given slot.
                        machine_learning_model_ids(model_counter) = this%parameters%model_id
                        this%parameters%model_index = model_counter

                        allocate(machine_learning_model_struct :: machine_learning_models(model_counter)%ptr)

                        ! Calls initialization of a particular Machine Learning model.
                        call machine_learning_models(model_counter)%ptr%init(this%parameters%model_id)

                        exit

                end if
        end do

    end subroutine machine_learning_struct_init

    !Machine Learning Destructor
    subroutine machine_learning_struct_destroy(this)
        implicit none

        class(machine_learning_struct), intent(inout) :: this ! object being destroyed

    end subroutine machine_learning_struct_destroy

    ! Subroutine for running release for a machine learning reservoir,
    ! which will then call the machine learning model for processing the
    ! inputs and returning the output.
    subroutine run_machine_learning_release(reservoir_data, previous_timestep_inflow, &
        inflow, lateral_inflow, water_elevation, outflow, routing_period)

        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data
        real, intent(in)    :: previous_timestep_inflow ! cubic meters per second (cms)
        real, intent(in)    :: inflow                   ! cubic meters per second (cms)
        real, intent(in)    :: lateral_inflow           ! cubic meters per second (cms)
        real, intent(inout) :: water_elevation          ! meters
        real, intent(out)   :: outflow                  ! cubic meters per second (cms)
        real, intent(in)    :: routing_period           ! seconds
        logical             :: leap_year

        real :: current_time_real_print

        ! If first call to release
        if (reservoir_data%state%current_time == 0) then

            reservoir_data%state%update_time = int(routing_period)

        end if

        reservoir_data%state%current_time = reservoir_data%state%current_time + int(routing_period)

        ! Check for new day and update day x and y
        call reservoir_data%timestep_preprocessing()


        ! Update end time storage from current inflow and previous timestep outflow
        reservoir_data%state%end_time_storage = reservoir_data%state%current_time_storage &
        + reservoir_data%input%inflow * routing_period - reservoir_data%output%outflow * routing_period

        reservoir_data%input%inflow = inflow

        call reservoir_data%timestep_pre_conditions_warn()

        print *, "ML Timestep for Lake: ", reservoir_data%parameters%lake_number
        print *, "Current time is: ", reservoir_data%state%current_time
        print *, "Current inflow is: ", reservoir_data%input%inflow

        call reservoir_data%timestep_pre_conditions_modify(routing_period)

        ! If update time, then run machine learning model to calculate new outflow value
        if (reservoir_data%state%current_time >= reservoir_data%state%update_time) then

            ! Check leap year and update day x and y
            !call reservoir_data%update_preprocessing()

            call reservoir_data%update_pre_conditions_modify()

            print *, "ML Update for Lake: ", reservoir_data%parameters%lake_number
            print *, "Current time is: ", reservoir_data%state%current_time

            ! TODO: Put outputs from a subroutine/function at the end of the list consistently across all modules in NWM
            call machine_learning_models(reservoir_data%parameters%model_index)%ptr%run_model &
                (reservoir_data%input%inflow, reservoir_data%state%previous_time_release, &
                reservoir_data%state%current_time_storage, reservoir_data%state%x_day_of_year, &
                reservoir_data%state%y_day_of_year, reservoir_data%output%outflow)


        !temp
        if (reservoir_data%parameters%lake_number == 166758723) then
            print *, 'Writing the results into some text files!'

            !open (111,file="lakeids.txt",status="unknown")
            !write (111, "(4A15)") "Lake_IDs"
            ! Write (111, "(F25.10)") reservoir_data%parameters%lake_number

            current_time_real_print = real(reservoir_data%state%current_time)

            open (112,file="current_time.txt",status="unknown")
            write (112, "(4A15)") "Current_Time"
             !Write (112, "(I5)") reservoir_data%state%current_time
             Write (112, "(F15.5)") current_time_real_print


            open (113,file="current_inflow.txt",status="unknown")
            write (113, "(4A15)") "Current_Inflow"
             Write (113, "(F15.5)") reservoir_data%input%inflow


            open (115,file="x_doy.txt",status="unknown")
            write (115, "(4A15)") "X_DOY"
             Write (115, "(F15.5)") reservoir_data%state%x_day_of_year


            open (116,file="y_doy.txt",status="unknown")
            write (116, "(4A15)") "Y_DOY"
             Write (116, "(F15.5)") reservoir_data%state%y_day_of_year


            open (117,file="current_time_storage.txt",status="unknown")
            write (117, "(4A15)") "Current_Time_Storage"
             Write (117, "(F15.5)") reservoir_data%state%current_time_storage


            print *, "Current_Time_Storage"
            print *, reservoir_data%state%current_time_storage

            open (118,file="previous_time_release.txt",status="unknown")
            write (118, "(4A15)") "Previous_Time_Release"
             Write (118, "(F15.5)") reservoir_data%state%previous_time_release


            open (119,file="outflow.txt",status="unknown")
            write (119, "(4A15)") "Outflow"
             Write (119, "(F15.5)") reservoir_data%output%outflow

        end if

            call reservoir_data%update_post_conditions_modify()

            ! This is run to update the end_time_storage with the newly calculated release
            reservoir_data%state%end_time_storage = reservoir_data%state%current_time_storage &
            + reservoir_data%input%inflow * routing_period - reservoir_data%output%outflow * routing_period

            reservoir_data%state%previous_time_release = reservoir_data%output%outflow

            reservoir_data%state%update_time = reservoir_data%state%update_time + reservoir_data%state%time_interval

        end if

        reservoir_data%state%current_time_storage = reservoir_data%state%end_time_storage

        outflow = reservoir_data%output%outflow

        print *, "Outflow is: ", outflow

        print *, 'Release current storage'
        print *, reservoir_data%state%current_time_storage

    end subroutine run_machine_learning_release


    subroutine machine_learning_timestep_pre_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        if (reservoir_data%input%inflow < 0.0) then

            ! FIXME: Change 6 to macro for WCOS
            write(6,*) "WARNING: Current inflow ", reservoir_data%input%inflow, &
            " cms, has reached below zero for reservoir ", &
            reservoir_data%parameters%lake_number, " at ", reservoir_data%state%current_time, " seconds after model start time."

        end if

        ! Also have option to set the end_time_storage to the max_storage and change the outflow accordingly.
        if (reservoir_data%state%end_time_storage > reservoir_data%parameters%max_storage) then

            write(6,*) "WARNING: Calculated storage for this timestep, ", reservoir_data%state%end_time_storage, &
            " cubic meters, will exceed the maximum storage boundary of: ", reservoir_data%parameters%max_storage, &
            " cubic meters, for reservoir ", reservoir_data%parameters%lake_number, &
            " at ", reservoir_data%state%current_time, " seconds after model start time."
            write(6,*) "Current inflow is ", reservoir_data%input%inflow, " cms."

        ! Also have option to set the end_time_storage to the min_storage and change the outflow accordingly.
        else if (reservoir_data%state%end_time_storage < reservoir_data%parameters%min_storage .and. &
                 reservoir_data%state%end_time_storage > 0.0) then

            !SAY 'REQUIREMENT' BELOW????

            write(6,*) "WARNING: Calculated storage for this timestep, ", reservoir_data%state%end_time_storage, &
            " cubic meters, will fall below the minimum storage requirement of: ", reservoir_data%parameters%min_storage, &
            " cubic meters, for reservoir ", reservoir_data%parameters%lake_number, &
            " at ", reservoir_data%state%current_time, " seconds after model start time."
            write(6,*) "Current inflow is ", reservoir_data%input%inflow, " cms."

        end if

    end subroutine machine_learning_timestep_pre_conditions_warn


    subroutine machine_learning_timestep_pre_conditions_modify(reservoir_data, routing_period)
        class(machine_learning_struct), intent(inout) :: reservoir_data
        real, intent(in)       :: routing_period

        ! Note to user:
        ! In order to conserve mass, when a reservoir's storage would become 0 or less, the current release
        ! must be adjusted to avoid releasing water that isn't in the reservoir. To determine conditions
        ! when this could happen, this function calculates the storage balance for the duration of the current
        ! timestep (dt) based on current storage condition (at the beginning of the time period), inflow for
        ! the timestep, and the latest release value valid for the timestep.

        ! If the current conditions would cause the storage to become less than or equal to 0.0 cubic meters,
        ! then the outflow/release will be altered to provide the available storage over the timestep
        ! (given as R = (S + I*dt)/dt ). This will cause storage at the end of the timestep to be 0. From this
        ! point, the outflow/release is boudnded by the mimimum inflow provided until the next update time when
        ! the machine learning model will be run again.

        ! Therefore, the user's choice of time_interval has a significant impact on this simulated outflow/release
        ! behavior in this edge case.  Release cannot increase again until the next update time occurs. This does,
        ! however, allow the reservoir to fill back up once inflow increases to exceed current outflow/release
        ! since storage would then become > 0.

        ! If one wants to allow the release to continue to rise as inflow rises, you can set R = I*dt once storage
        ! has been depleted. This passes on inflow to outflow until an update time occurs, in which case the ML
        ! model and its post conditions are responsible for calculating an appropriate release.

        ! The logical 'and' below is not necessary because if otherwise this check updates outflow and
        ! end_time_storage before an update time, those variables would be overwritten after a new release
        ! is calculated from the machine learning model. The logical 'and' is left here for emphasis on
        ! the importance it has on timesteps that are not also update times.
        if (reservoir_data%state%end_time_storage <= 0.0 .AND. reservoir_data%state%current_time &
           < reservoir_data%state%update_time) then

            ! FIXME: Change 6 to macro for WCOS
            write(6,*) "WARNING: End time storage, ", reservoir_data%state%end_time_storage, &
            " cubic meters, has reached at or below 0.0 cubic meters for reservoir ", reservoir_data%parameters%lake_number, &
            " at ", reservoir_data%state%current_time, " seconds after model start time."
            write(6,*) "Current inflow is ", reservoir_data%input%inflow, " cms."

            reservoir_data%output%outflow = (reservoir_data%state%current_time_storage &
            + reservoir_data%input%inflow * routing_period) / routing_period

            reservoir_data%state%end_time_storage = 0.0

            if (reservoir_data%output%outflow < 0.0) then
                reservoir_data%output%outflow = 0.0
            end if

        end if

    end subroutine machine_learning_timestep_pre_conditions_modify



    subroutine machine_learning_timestep_post_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data


    end subroutine machine_learning_timestep_post_conditions_warn



    subroutine machine_learning_timestep_post_conditions_modify(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data


    end subroutine machine_learning_timestep_post_conditions_modify




    subroutine machine_learning_update_pre_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data


    end subroutine machine_learning_update_pre_conditions_warn



    subroutine machine_learning_update_pre_conditions_modify(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        if (reservoir_data%input%inflow < 0.0) then
            reservoir_data%input%inflow = 0.0

        end if

    end subroutine machine_learning_update_pre_conditions_modify


    subroutine machine_learning_update_post_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        ! Also have option to set the outflow to the max_release.
        if (reservoir_data%output%outflow > reservoir_data%parameters%max_release) then

            write(6,*) "Current outflow has exceeded the max outflow for reservoir ", reservoir_data%parameters%lake_number
            write(6,*) "at ", reservoir_data%state%current_time, " seconds after model start time."

        end if

    end subroutine machine_learning_update_post_conditions_warn


    subroutine machine_learning_update_post_conditions_modify(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        real :: projected_next_time_interval_storage

        if (reservoir_data%output%outflow < 0.0) then
            write(6,*) "Machine Learning calculations return a negative outflow for reservoir ", reservoir_data%parameters%lake_number
            write(6,*) "at ", reservoir_data%state%current_time, " seconds after model start time."
            reservoir_data%output%outflow = 0.0

        end if

        ! The projected_next_time_interval_storage is only calculated as a boundary condition to update the release
        ! and this storage is calculated assuming a constant flow rate over the time interval. If the time interval
        ! is too large relative to the channel routing period, this may not be an appropriate calculation.

        projected_next_time_interval_storage = reservoir_data%state%current_time_storage &
        + (reservoir_data%input%inflow - reservoir_data%output%outflow) * reservoir_data%state%time_interval

        if (projected_next_time_interval_storage > reservoir_data%parameters%max_storage) then

            reservoir_data%output%outflow = (reservoir_data%state%current_time_storage - reservoir_data%parameters%max_storage) &
            / reservoir_data%state%time_interval + reservoir_data%output%outflow

            write(6,*) "Modified release to maintain prevent maximum storage exceedance for reservoir ", reservoir_data%parameters%lake_number
            write(6,*) "at ", reservoir_data%state%current_time, " seconds after model start time."

        else if (projected_next_time_interval_storage < reservoir_data%parameters%min_storage .AND. &
                projected_next_time_interval_storage > 0.0) then

            reservoir_data%output%outflow = (reservoir_data%state%current_time_storage - reservoir_data%parameters%min_storage) &
            / reservoir_data%state%time_interval + reservoir_data%output%outflow

            write(6,*) "Modified release to maintain minimum storage for reservoir ", reservoir_data%parameters%lake_number
            write(6,*) "at ", reservoir_data%state%current_time, " seconds after model start time."

        else if (projected_next_time_interval_storage <= 0.0) then
            reservoir_data%output%outflow = reservoir_data%state%current_time_storage / reservoir_data%state%time_interval
            write(6,*)  "Modified release to conserve mass for reservoir ", reservoir_data%parameters%lake_number
            write(6,*) "at ", reservoir_data%state%current_time, " seconds after model start time."

        end if

        if (reservoir_data%output%outflow < 0.0) then
            reservoir_data%output%outflow = 0.0
        end if

    end subroutine machine_learning_update_post_conditions_modify


    subroutine timestep_preprocessing(reservoir_data)

        class(machine_learning_struct), intent(inout) :: reservoir_data

        if (reservoir_data%state%current_time >= reservoir_data%state%new_day_time) then
            reservoir_data%state%new_day_time = reservoir_data%state%new_day_time + seconds_in_day


            if (leap_year(reservoir_data%state%current_year)) then

               if (reservoir_data%state%current_day_of_year == days_in_leap_year) then    ! if last day of leap year
                    reservoir_data%state%current_year = reservoir_data%state%current_year + 1
                    reservoir_data%state%current_day_of_year = 1

               else
                    reservoir_data%state%current_day_of_year = reservoir_data%state%current_day_of_year + 1

               end if

               ! MAKE FUNCTION ALSO?????
               ! update x and y based on leap year
               reservoir_data%state%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * reservoir_data%state%current_day_of_year / days_in_leap_year)
               reservoir_data%state%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * reservoir_data%state%current_day_of_year / days_in_leap_year)

            else

               if (reservoir_data%state%current_day_of_year == days_in_year) then    ! if last day of non leap year
                    reservoir_data%state%current_year = reservoir_data%state%current_year + 1
                    reservoir_data%state%current_day_of_year = 1
               else

                    reservoir_data%state%current_day_of_year = reservoir_data%state%current_day_of_year + 1
               end if

               ! update x and y based on non leap year
               reservoir_data%state%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * reservoir_data%state%current_day_of_year / days_in_year)
               reservoir_data%state%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * reservoir_data%state%current_day_of_year / days_in_year)
            end if

        end if

    end subroutine timestep_preprocessing



    ! make function or subroutine???????????
    subroutine update_preprocessing(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        if (leap_year(reservoir_data%state%current_year)) then

           if (reservoir_data%state%current_day_of_year == days_in_leap_year) then    ! if last day of leap year
                reservoir_data%state%current_year = reservoir_data%state%current_year + 1
                reservoir_data%state%current_day_of_year = 1

           else
                reservoir_data%state%current_day_of_year = reservoir_data%state%current_day_of_year + 1

           end if

           ! MAKE FUNCTION ALSO?????
           ! update x and y based on leap year
           reservoir_data%state%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * reservoir_data%state%current_day_of_year / days_in_leap_year)
           reservoir_data%state%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * reservoir_data%state%current_day_of_year / days_in_leap_year)

        else

           if (reservoir_data%state%current_day_of_year == days_in_year) then    ! if last day of non leap year
                reservoir_data%state%current_year = reservoir_data%state%current_year + 1
                reservoir_data%state%current_day_of_year = 1
           else

                reservoir_data%state%current_day_of_year = reservoir_data%state%current_day_of_year + 1
           end if

           ! update x and y based on non leap year
           reservoir_data%state%x_day_of_year = 0.5 + 0.5 * COS(2 * pi * reservoir_data%state%current_day_of_year / days_in_year)
           reservoir_data%state%y_day_of_year = 0.5 + 0.5 * SIN(2 * pi * reservoir_data%state%current_day_of_year / days_in_year)
        end if


    end subroutine update_preprocessing





    subroutine machine_learning_get_variables(reservoir_data, storage)
        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data

        real, intent(out) :: storage

        storage = reservoir_data%state%current_time_storage



        print *, 'In ML get var subroutine current storage'
        print *, reservoir_data%state%current_time_storage

    end subroutine machine_learning_get_variables







    subroutine machine_learning_write_to_restart_file(reservoir_data, iret, ncid, NF_FLOAT, dimid_lakes, varid, nlakes, lake_index)
    !subroutine machine_learning_write_to_restart_file(reservoir_data, iret, ncid, dimid_lakes, varid, nlakes, lake_index)
        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data
        integer, intent(inout) :: iret, ncid
        integer, intent(in) :: NF_FLOAT, varid, nlakes, lake_index
        integer, dimension(:), intent(in) :: dimid_lakes

        ! Works
        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, (/dimid_lakes(1)/), varid)


        !iret = nf90_def_var(ncid, "machine_learning_current_time_storage", NF_FLOAT, dimid_lakes, varid)


    end subroutine machine_learning_write_to_restart_file


    subroutine machine_learning_read_from_restart_file(reservoir_data)
        implicit none
        class(machine_learning_struct), intent(inout) :: reservoir_data


    end subroutine machine_learning_read_from_restart_file










    function machine_learning_data_info(machine_learning_data) result(rv)
        implicit none
        type (machine_learning_struct) :: machine_learning_data
        integer :: rv
        logical, dimension(1) :: ptr_state
        logical, dimension(1) :: data_state

        rv = 0

        ! Check to see if the machine_learning_state data structure exists
        print *, "Checking pointer association on data%state "
        if ( associated(machine_learning_data%state) ) then
            print *, "PASSED"
            ptr_state(1) = .true.
        else
            print *, "FAILED"
            ptr_state(1) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_parameters data structure exists
        print *, "Checking pointer association on data%parameters "
        if ( associated(machine_learning_data%parameters) ) then
            print *, "PASSED"
            ptr_state(2) = .true.
        else
            print *, "FAILED"
            ptr_state(2) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_input data structure exists
        print *, "Checking pointer association on data%input "
        if ( associated(machine_learning_data%input) ) then
            print *, "PASSED"
            ptr_state(3) = .true.
        else
            print *, "FAILED"
            ptr_state(3) = .false.
        end if
        print *, " "

        ! Check to see if the machine_learning_output data structure exists
        print *, "Checking pointer association on data%output "
        if ( associated(machine_learning_data%output) ) then
            print *, "PASSED"
            ptr_state(4) = .true.
        else
            print *, "FAILED"
            ptr_state(4) = .false.
        end if
        print *, " "



        ! Now check the data members of each substructure
        if ( ptr_state(1) ) then
            data_state(1) = test_machine_learning_state(machine_learning_data%state)
        end if

        if ( ptr_state(2) ) then
            data_state(2) = test_machine_learning_parameters(machine_learning_data%parameters)
        end if

        if ( ptr_state(3) ) then
            data_state(3) = test_input(machine_learning_data%input)
        end if

        if ( ptr_state(4) ) then
            data_state(4) = test_output(machine_learning_data%output)
        end if


        if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Tests Passed"
            print *, "========================================================================"
        end if
        rv = 1

        flush(6)
    end function machine_learning_data_info


    function machine_learning_model_data_info(machine_learning_model_data) result(rv)
        implicit none
        type (machine_learning_model_struct) :: machine_learning_model_data
        integer :: rv
        logical, dimension(1) :: ptr_state
        logical, dimension(1) :: data_state

        rv = 0

        ! Check to see if the machine_learning_model data structure exists
        print *, "Checking pointer association on model data "
        !if ( associated(machine_learning_model_data) ) then
        !    print *, "PASSED"
        !    ptr_state(1) = .true.
        !else
        !    print *, "FAILED"
        !    ptr_state(1) = .false.
        !end if
        print *, " "


        data_state(1) = test_machine_learning_model(machine_learning_model_data)


        if ( all(data_state) ) then
        !if ( all(ptr_state) .and. all(data_state) ) then
            print *, "========================================================================"
            print *, "All Model Tests Passed"
            print *, "========================================================================"
        end if
        rv = 1

        flush(6)
    end function machine_learning_model_data_info

end module module_machine_learning
