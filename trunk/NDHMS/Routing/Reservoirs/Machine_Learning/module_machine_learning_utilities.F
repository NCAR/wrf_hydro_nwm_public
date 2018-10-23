module module_machine_learning_utilities

    implicit none




contains

        subroutine machine_learning_timestep_pre_conditions_warn(reservoir_data)

            if (inflow < 0.0) then
                call pre_warn(inflow, reservoir_data%state%current_time, reservoir_data%parameters%lake_number)
            end if

        end subroutine machine_learning_timestep_pre_conditions_warn



        subroutine machine_learning_timestep_pre_conditions_modify(reservoir_data)

            ! FIXME: Remove below print statements and write same info to a log file.
            ! Also have option to set the current_storage to the max_storage and change the outflow accordingly.

            if (reservoir_data%state%current_storage > reservoir_data%parameters%max_storage) then

                    boundary_passed = 1 ! set boundary_passed to 1 for exceeding upper boundary

                    reservoir_data%state%current_storage = pre_modify_storage(reservoir_data%state%current_storage, reservoir_data%parameters%min_storage, &
                    reservoir_data%parameters%max_storage, reservoir_data%input%inflow, reservoir_data%state%current_time, &
                    reservoir_data%parameters%lake_number, boundary_passed)

                    !reservoir_data%state%current_storage = reservoir_data%parameters%max_storage

                    !reservoir_data%output%outflow = ((reservoir_data%state%average_storage_over_past_time_intervals - reservoir_data%state%current_storage) &

                    !+ reservoir_data%input%inflow  * ( 24 * 3600 )) / ( 24 * 3600 )

            ! FIXME: Remove below print statements and write same info to a log file.
            ! Also have option to set if < reservoir_data%parameters%min_storage then current storage would equal min_storage
            ! and outflow would change accordingly.
            ! Need further discussion about accuracy of the model with using min_storage vs. zero.
            !Need macro or some other way to set min storage here to 0.0 or min_storage???
            else if (reservoir_data%state%current_storage < 0.0) then

                boundary_passed = 0    ! set boundary_passed to 0 for exceeding lower boundary

                reservoir_data%state%current_storage = pre_modify_storage(reservoir_data%state%current_storage, reservoir_data%parameters%min_storage, &
                reservoir_data%parameters%max_storage, reservoir_data%input%inflow, reservoir_data%state%current_time, &
                reservoir_data%parameters%lake_number, boundary_passed)

                reservoir_data%output%outflow = 0.0
                okay_run_machine_learning_model = .false.

                !reservoir_data%state%current_storage = reservoir_data%parameters%min_storage

                !reservoir_data%output%outflow = ((reservoir_data%state%average_storage_over_past_time_intervals - reservoir_data%state%current_storage) &

                !+ reservoir_data%input%inflow  * ( 24 * 3600 )) / ( 24 * 3600 )

            end if

                real function pre_modify_storage(storage, min_storage, max_storage, inflow, current_time, lake_number, boundary_passed)
                    implicit none
                    real,    intent(in)    :: storage
                    real,    intent(in)    :: min_storage
                    real,    intent(in)    :: max_storage
                    real,    intent(in)    :: inflow
                    integer, intent(in)    :: current_time
                    integer, intent(in)    :: boundary_passed
                    integer, intent(in)    :: lake_number

                    ! FIXME: Change 6 to macro for WCOS
                    if (boundary_passed == 1) then
                        write(6,*) "WARNING: Current storage, ", storage, &
                        " cubic meters, has exceeded the max storage: ", max_storage, &
                        " cubic meters, for reservoir ", lake_number

                        pre_modify_storage = max_storage

                    ! FIXME: Change 6 to macro for WCOS
                    else if (boundary_passed == 0) then
                        write(6,*) "WARNING: Current storage, ", storage, &
                        " cubic meters, has reached below the minimum storage: ", min_storage, &
                        " cubic meters, for reservoir ", lake_number

                        pre_modify_storage = min_storage
                    end if

                    ! FIXME: Change 6 to macro for WCOS
                    write(6,*) " at ", current_time, " seconds after model start time."
                    write(6,*) "Current inflow is ", inflow, " cms."

                end function pre_modify_storage


        end subroutine machine_learning_timestep_pre_conditions_modify



        subroutine machine_learning_timestep_post_conditions_warn(reservoir_data)


        end subroutine machine_learning_timestep_post_conditions_warn



        subroutine machine_learning_timestep_post_conditions_modify(reservoir_data)


        end subroutine machine_learning_timestep_post_conditions_modify




        subroutine machine_learning_update_pre_conditions_warn(reservoir_data)


        end subroutine machine_learning_update_pre_conditions_warn



        subroutine machine_learning_update_pre_conditions_modify(reservoir_data)


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



        end subroutine machine_learning_update_pre_conditions_modify



        subroutine machine_learning_update_post_conditions_warn(reservoir_data)


        end subroutine machine_learning_update_post_conditions_warn



        subroutine machine_learning_update_post_conditions_modify(reservoir_data)


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




    ! MAKE BELOW SUBROUTINE SEPARATE FROM THE DERIVED RESERVOIR BASE CLASS????

    subroutine handle_err(status)
        implicit none
        integer, intent ( in) :: status
        character :: nf90_strerror
        integer  :: nf90_noerr
        if(status /= nf90_noerr) then
            !print *, trim(nf90_strerror(status))
            !print *, nf90_strerror(status) !FIXME this line deactivated, but we need to check on it!
            stop "Stopped"
        end if

    end subroutine handle_err


end module module_machine_learning_utilities

