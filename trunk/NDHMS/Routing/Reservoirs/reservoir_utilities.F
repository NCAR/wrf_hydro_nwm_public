module reservoir_utilities


contains
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


end module reservoir_utilities
