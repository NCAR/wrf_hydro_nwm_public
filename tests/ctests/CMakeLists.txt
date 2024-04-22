# add Makefile targets to setup test runs
# Croton Test Options: {Gridded, Gridded_no_lakes, NWM, Reach, ReachLakes}
set(croton_testcases "gridded" "gridded_no_lakes" "nwm" "nwm_ana"
  "nwm_long_range" "reach" "reach_lakes")

# set default croton setup and run target
set(croton_default_testcase "gridded")
add_custom_target(croton
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh ${croton_default_testcase} ${CMAKE_BINARY_DIR})
add_custom_target(run-croton
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/run_cmake_testcase.sh ${croton_default_testcase} ${CMAKE_BINARY_DIR} 1)
add_custom_target(run-croton-parallel
  COMMAND bash
  ${CMAKE_CURRENT_SOURCE_DIR}/run_cmake_testcase.sh ${croton_default_testcase} ${CMAKE_BINARY_DIR} 2)

# generate commands for every croton testcase option
foreach(croton_testcase ${croton_testcases})
  string(TOLOWER ${croton_testcase} testcase)
  add_custom_target(croton-${testcase}
    COMMAND bash
    ${CMAKE_CURRENT_SOURCE_DIR}/setup_cmake_testcase.sh ${croton_testcase} ${CMAKE_BINARY_DIR})
  add_custom_target(run-croton-${testcase}
    DEPENDS croton-${testcase}
    COMMAND bash
    ${CMAKE_CURRENT_SOURCE_DIR}/run_cmake_testcase.sh ${croton_testcase} ${CMAKE_BINARY_DIR} 1)
  add_custom_target(run-croton-${testcase}-parallel
    DEPENDS croton-${testcase}
    COMMAND bash
    ${CMAKE_CURRENT_SOURCE_DIR}/run_cmake_testcase.sh ${croton_testcase} ${CMAKE_BINARY_DIR} 2)
endforeach()

# generic ctests
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
