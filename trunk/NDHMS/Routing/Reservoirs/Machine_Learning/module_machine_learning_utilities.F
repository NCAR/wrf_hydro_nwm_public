!module module_machine_learning_utilities
!use module_machine_learning
use module_reservoir_utilities

!    implicit none

!GO BACK AND LISTEN TO WHAT NELS SAID ABOUT WHAT EXACATLY SHOULD BE PASSED IN TO THESE
!AS IN FLAGS OR OTHER VARS.


contains







    subroutine machine_learning_timestep_pre_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data

        if (reservoir_data%input%inflow < 0.0) then
            call pre_warn(inflow, reservoir_data%state%current_time, reservoir_data%parameters%lake_number)

            ! FIXME: Change 6 to macro for WCOS
            write(6,*) "WARNING: Current inflow ", reservoir_data%input%inflow, &
            " cms, has reached below zero for reservoir ", &
            reservoir_data%parameters%lake_number, " at ", reservoir_data%state%current_time, " seconds after model start time."

        end if


    end subroutine machine_learning_timestep_pre_conditions_warn



    subroutine machine_learning_timestep_pre_conditions_modify(reservoir_data, run_machine_learning_model)
        class(machine_learning_struct), intent(inout) :: reservoir_data
        logical, intent(inout) :: run_machine_learning_model

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set the current_storage to the max_storage and change the outflow accordingly.
        if (reservoir_data%state%current_storage > reservoir_data%parameters%max_storage) then

            write(6,*) "WARNING: Current storage, ", reservoir_data%state%current_storage, &
            " cubic meters, has exceeded the max storage: ", reservoir_data%parameters%max_storage, &
            " cubic meters, for reservoir ", reservoir_data%parameters%lake_number, &
            " at ", reservoir_data%state%current_time, " seconds after model start time."
            write(6,*) "Current inflow is ", reservoir_data%input%inflow, " cms."


        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set if < reservoir_data%parameters%min_storage then current storage would equal min_storage
        ! and outflow would change accordingly.
        ! Need further discussion about accuracy of the model with using min_storage vs. zero.
        !Need macro or some other way to set min storage here to 0.0 or min_storage???
        else if (reservoir_data%state%current_storage < 0.0) then

            ! FIXME: Change 6 to macro for WCOS
            write(6,*) "WARNING: Current storage, ", storage, &
            !" cubic meters, has reached below the minimum storage: ", min_storage, &
            !" cubic meters,
            " cubic meters, has reached below 0.0 for reservoir ", lake_number, &
            " at ", reservoir_data%state%current_time, " seconds after model start time." &
            write(6,*) "Current inflow is ", reservoir_data%input%inflow, " cms."

            reservoir_data%state%current_storage = 0.0

            reservoir_data%output%outflow = 0.0
            run_machine_learning_model = .false.

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


    end subroutine machine_learning_update_pre_conditions_modify



    subroutine machine_learning_update_post_conditions_warn(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data


    end subroutine machine_learning_update_post_conditions_warn



    subroutine machine_learning_update_post_conditions_modify(reservoir_data)
        class(machine_learning_struct), intent(inout) :: reservoir_data


        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set the outflow to the max_release.
        if (reservoir_data%output%outflow > reservoir_data%parameters%max_release) then

            !reservoir_data%output%outflow = reservoir_data%parameters%max_release

            print *, "Current outflow has exceeded the max outflow for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."

        ! FIXME: Remove below print statements and write same info to a log file.
        ! Also have option to set if < reservoir_data%parameters%min_release.
        ! Need further discussion about accuracy of the model with using min_release vs. zero.

        else if (reservoir_data%output%outflow < 0.0) then
            print *, "Current outflow has reached below zero for reservoir ", reservoir_data%parameters%lake_number
            print *, "at ", reservoir_data%state%current_time, " seconds after model start time."
            reservoir_data%output%outflow = 0.0
            !reservoir_data%output%outflow = reservoir_data%parameters%min_release

        end if


    end subroutine machine_learning_update_post_conditions_modify


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



!end module module_machine_learning_utilities








