program call_python_test_driver
  use call_python_bind_c

  integer(c_int), parameter :: n = 4
  real(c_double) :: weights(4)

  weights = 0

  call foo(weights, n)
  call foo(weights, n)

  print *, "weights at end:", weights
  print *, "FIN"
end program call_python_test_driver
