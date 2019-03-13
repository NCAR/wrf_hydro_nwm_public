module module_reservoir_utilities
    use netcdf

    implicit none

contains

    subroutine check(status, function_name, file_name)
        integer, intent(in) :: status
        character(len=*), intent(in) :: function_name
        character(len=*), optional, intent(in) :: file_name
        character(len=512) :: error_message

        if(status /= nf90_noerr) then
            if (present(file_name)) then
	    	error_message = trim(nf90_strerror(status))//' for reservoir restart file '//file_name// ' from function '//function_name

	    else	
	    	error_message = trim(nf90_strerror(status))//' for reservoir restart file from function '//function_name
	    end if

	    call hydro_stop(error_message)

        end if
    end subroutine check

end module module_reservoir_utilities
