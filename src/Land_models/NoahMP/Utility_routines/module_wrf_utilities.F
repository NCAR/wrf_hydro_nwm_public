!WRF:DRIVER_LAYER:UTIL
!

SUBROUTINE wrf_message( str )
  IMPLICIT NONE

  CHARACTER*(*) str
  print *,trim(str)

END SUBROUTINE wrf_message

SUBROUTINE wrf_error_fatal( str )
  IMPLICIT NONE
  CHARACTER*(*) str
#ifdef NCEP_WCOSS
  write(78,*) 'FATAL ERROR: ',trim(str)
  call flush(78)
  close(78)
#else
  write(6,*) 'FATAL ERROR: ',trim(str)
  call flush(6)
#endif

  CALL wrf_abort
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_abort
  use module_cpl_land
  integer ierr
  call MPI_Abort(HYDRO_COMM_WORLD,1,ierr)
  call MPI_Finalize(ierr)
  STOP 'wrf_abort'
END SUBROUTINE wrf_abort

SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  CALL wrf_message( str )
  RETURN
END SUBROUTINE wrf_debug

subroutine wrf_dm_bcast_real(rval, ival)
  implicit none
  real,    intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_real

subroutine wrf_dm_bcast_integer(rval, ival)
  implicit none
  integer, intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_integer

subroutine wrf_dm_bcast_string(rval, ival)
  implicit none
  character(len=*), intent(in) :: rval
  integer,          intent(in) :: ival
end subroutine wrf_dm_bcast_string
