program call_python_test_driver
  use call_python_bind_c

  integer(c_int), parameter :: n = 4
  real(c_double) :: weights(4)

  weights = 0

  call get_py_weights(weights, n)
  call get_py_weights(weights, n)

  print *, "weights at end:", weights
  print *, "FIN"
end program call_python_test_driver
