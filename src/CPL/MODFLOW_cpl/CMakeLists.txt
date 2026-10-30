# Add MODFLOW libraries
# add external MODFLOW
add_library(modflow SHARED IMPORTED)
set(modflow_lib_dir "$ENV{MODFLOW_BMI}")
set(modflow_lib "${modflow_lib_dir}/libmf6.so")
set_target_properties(modflow PROPERTIES IMPORTED_LOCATION "${modflow_lib}")

# add local MODFLOW BMI wrapper layer
add_library(bmi_modflow
  bmi_modflow_interface.f90
  bmi_modflow.f90)
target_link_libraries(bmi_modflow
  bmi_interface
  modflow)

# setup WRF-Hydro BMI Fortran drivers
add_executable(wrfhydro_modflow
  wrf_hydro_nwm_modflow6_serial.f90)
target_link_libraries(wrfhydro_modflow
  bmi_wrf_hydro_nwm
  bmi_modflow
  modflow
  ${CMAKE_THREAD_LIBS_INIT})

add_executable(wrfhydro_modflow_parallel
  wrf_hydro_nwm_modflow6_parallel.f90)
target_link_libraries(wrfhydro_modflow_parallel
  bmi_wrf_hydro_nwm
  bmi_modflow
  modflow
  ${CMAKE_THREAD_LIBS_INIT})

target_include_directories(bmi_modflow
  PUBLIC ${modflow_lib_dir}/libmf6.so.p/
  ${modflow_lib_dir}/../src/libmf6core.a.p/
  )
