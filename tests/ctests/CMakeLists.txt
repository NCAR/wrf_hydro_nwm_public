# --- generic ctests ---
add_executable(fortran_ctest_should_fail
  should_fail.f90)
add_test(NAME fortran_ctest_should_fail
  COMMAND fortran_ctest_should_fail)
set_tests_properties(fortran_ctest_should_fail
  PROPERTIES WILL_FAIL TRUE)

add_executable(fortran_ctest_should_not_fail
  should_not_fail.f90)
add_test(NAME fortran_ctest_should_not_fail
  COMMAND fortran_ctest_should_not_fail)
