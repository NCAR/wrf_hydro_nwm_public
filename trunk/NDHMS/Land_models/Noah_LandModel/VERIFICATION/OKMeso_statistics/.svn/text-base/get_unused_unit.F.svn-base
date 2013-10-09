integer function get_unused_unit() result(iunit)
  implicit none
  integer :: i
  logical :: used

  do i = 11, 255
     inquire(unit=i, opened=used)
     if (.not. used) then
        iunit = i
        return
     endif
  enddo

  print*, "GET_UNUSED_UNIT:  "
  print*, "      Problem getting unused unit number."
  call abort()

end function get_unused_unit
