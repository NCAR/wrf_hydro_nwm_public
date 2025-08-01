module call_python_bind_c
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer, c_int, &
       c_double, c_char, c_null_char
  implicit none

  interface
     subroutine call_python(weights, n) bind(c, name="call_python")
       use, intrinsic :: iso_c_binding, only: c_double, c_int
       real(c_double), intent(inout) :: weights(*)
       integer(c_int), intent(in) :: n
     end subroutine call_python
  end interface

contains
  subroutine foo(weights, n)
    real(c_double), intent(inout) :: weights(:)
    integer(c_int), intent(in) :: n
    call call_python(weights, n)
  end subroutine foo
end module call_python_bind_c
