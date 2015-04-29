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
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
  call flush(6)
  CALL wrf_abort
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_abort
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
