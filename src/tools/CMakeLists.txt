add_library(wrf_hydro_tools
  wrf_hydro_tools.F90)

target_link_libraries(wrf_hydro_tools
  module_NoahMP_hrldas)

add_executable(print_domain_decomposition
  print_domain_decomposition.F90)

target_link_libraries(print_domain_decomposition
  wrf_hydro_tools)

# bind c
add_library(wrf_hydro_tools_bind_c SHARED
  wrf_hydro_tools_bind_c.f90)

target_link_libraries(wrf_hydro_tools_bind_c
  module_NoahMP_hrldas
  wrf_hydro_tools)

add_custom_command(TARGET print_domain_decomposition
  POST_BUILD
  COMMAND cp ${CMAKE_CURRENT_BINARY_DIR}/print_domain_decomposition ${CMAKE_BINARY_DIR}/Run/print_domain_decomposition)

file(CREATE_LINK
  "${CMAKE_CURRENT_SOURCE_DIR}/wrf_hydro_decomposition.py"
  "${CMAKE_BINARY_DIR}/Run/wrf_hydro_decomposition.py"
  SYMBOLIC)
