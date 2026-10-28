add_library(bmi_interface bmi.f90)

add_library(bmi_wrf_hydro_nwm
  bmi_wrf_hydro_nwm_interface.f90
  bmi_wrf_hydro_nwm.f90)

target_link_libraries(bmi_wrf_hydro_nwm
  bmi_interface
  noahmp_data
  wrfhydro_lib
  module_NoahMP_hrldas)

# create bind c library for use with Python
add_library(bmi_wrf_hydro_nwm_bind_c SHARED
  bmi_wrf_hydro_nwm_bind_c_interface.f90
  bmi_wrf_hydro_nwm_bind_c.f90)

target_link_libraries(bmi_wrf_hydro_nwm_bind_c
  bmi_interface
  noahmp_data
  wrfhydro_lib
  bmi_wrf_hydro_nwm
  module_NoahMP_hrldas
  )

# setup WRF-Hydro BMI Fortran driver
add_executable(wrf_hydro_bmi
  wrf_hydro_nwm_bmi_driver.f90)

target_link_libraries(wrf_hydro_bmi
  bmi_wrf_hydro_nwm)

# setup WRF-Hydro BMI Python driver
file(CREATE_LINK
  "${CMAKE_CURRENT_SOURCE_DIR}/wrf_hydro_nwm_bmi_driver.py"
  "${CMAKE_BINARY_DIR}/Run/wrf_hydro_bmi.py"
  SYMBOLIC)

