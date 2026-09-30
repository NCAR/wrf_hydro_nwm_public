# --- template ---
# # checking initialization:
# add_executable(wrf_hydro_bmi_foo
#   wrf_hydro_bmi_foo.f90)
# target_link_libraries(wrf_hydro_bmi_foo
#   bmi_wrf_hydro_nwm)
# add_test(NAME wrf_hydro_bmi_foo
#   COMMAND wrf_hydro_bmi_foo
#   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
# set_tests_properties(wrf_hydro_bmi_foo
#   PROPERTIES DEPENDS setup_wrf_hydro_testcase)


# --- bmi ctests ---
add_test(NAME wrf_hydro_bmi_test
  COMMAND wrf_hydro_bmi
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
set_tests_properties(wrf_hydro_bmi_test
  PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# checking initialization: model_name, current_time, end_time
add_executable(wrf_hydro_bmi_init
  wrf_hydro_bmi_init.f90)
target_link_libraries(wrf_hydro_bmi_init
  bmi_wrf_hydro_nwm)
add_test(NAME wrf_hydro_bmi_init
  COMMAND wrf_hydro_bmi_init
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
set_tests_properties(wrf_hydro_bmi_init
  PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# # checking initialization:
# add_executable(wrf_hydro_bmi_get_var_grid
#   wrf_hydro_bmi_get_var_grid.f90)
# target_link_libraries(wrf_hydro_bmi_get_var_grid
#   bmi_wrf_hydro_nwm)
# add_test(NAME wrf_hydro_bmi_get_var_grid
#   COMMAND wrf_hydro_bmi_get_var_grid
#   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
# set_tests_properties(wrf_hydro_bmi_get_var_grid
#   PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# # checking input/output:
# add_executable(wrf_hydro_bmi_get_input_output
#   wrf_hydro_bmi_get_input_output.f90)
# target_link_libraries(wrf_hydro_bmi_get_input_output
#   bmi_wrf_hydro_nwm)
# add_test(NAME wrf_hydro_bmi_get_input_output
#   COMMAND wrf_hydro_bmi_get_input_output
#   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
# set_tests_properties(wrf_hydro_bmi_get_input_output
#   PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# checking get_value:
# add_executable(wrf_hydro_bmi_get_values
#   wrf_hydro_bmi_get_values.f90)
# target_link_libraries(wrf_hydro_bmi_get_values
#   bmi_wrf_hydro_nwm)
# add_test(NAME wrf_hydro_bmi_get_values
#   COMMAND wrf_hydro_bmi_get_values
#   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
# set_tests_properties(wrf_hydro_bmi_get_values
#   PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# checking set_value:
# add_executable(wrf_hydro_bmi_set_values
#   wrf_hydro_bmi_set_values.f90)
# tarset_link_libraries(wrf_hydro_bmi_set_values
#   bmi_wrf_hydro_nwm)
# add_test(NAME wrf_hydro_bmi_set_values
#   COMMAND wrf_hydro_bmi_set_values
#   WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
# set_tests_properties(wrf_hydro_bmi_set_values
#   PROPERTIES DEPENDS setup_wrf_hydro_testcase)

# checking bmi parallel:
add_executable(wrf_hydro_bmi_parallel_initialize
  wrf_hydro_bmi_parallel_initialize.f90)
target_link_libraries(wrf_hydro_bmi_parallel_initialize
  bmi_wrf_hydro_nwm)
add_test(NAME wrf_hydro_bmi_parallel_initialize
  COMMAND wrf_hydro_bmi_parallel_initialize
  # COMMAND mpiexec -np 1 wrf_hydro_bmi_parallel_initialize
  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Run)
set_tests_properties(wrf_hydro_bmi_parallel_initialize
  PROPERTIES DEPENDS setup_wrf_hydro_testcase)
