    ! stop the job due to the fatal error.
    subroutine HYDRO_stop(msg)

        character(len=*) :: msg
        integer :: ierr
        ierr = 1

        write(*,*) "FATAL ERROR: ", trim(msg)
        write(78,*) "FATAL ERROR: ", trim(msg)
        call flush(78)
        close(78)

        return
    end  subroutine HYDRO_stop
