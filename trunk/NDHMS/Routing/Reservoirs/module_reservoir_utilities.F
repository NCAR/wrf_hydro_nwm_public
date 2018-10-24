module module_reservoir_utilities

    implicit none

contains
    logical function leap_year(current_year)
        integer, intent(in) :: current_year

        if (((mod(current_year,4)==0 .and. &
            mod(current_year,100)/=0) .or. &
            mod(current_year,400)==0)) then
            leap_year = .TRUE.

        else
            leap_year = .FALSE.

        end if

    end function leap_year


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


end module module_reservoir_utilities
