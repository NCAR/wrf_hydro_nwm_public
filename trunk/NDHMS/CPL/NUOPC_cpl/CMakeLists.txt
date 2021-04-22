cmake_minimum_required (VERSION 2.8)

### Library Prerequisites
if (NOT TARGET esmf)
  find_package(ESMF MODULE REQUIRED)
endif (NOT TARGET esmf)

### Library Files
list(APPEND wrfhydro_nuopc_files
  WRFHydro_NUOPC_Cap.F90
  WRFHydro_NUOPC_Gluecode.F90
  WRFHydro_ESMF_Extensions.F90
)

### Library Dependencies
list(APPEND wrfhydro_nuopc_deps
  hydro_routing
  hydro_mpp
  hydro_driver
  hydro_orchestrator
)

list(APPEND wrfhydro_nuopc_linklibs
  hydro_utils
  hydro_mpp
  hydro_debug_utils
  hydro_routing_overland
  hydro_routing_subsurface
  hydro_data_rec
  hydro_routing
  hydro_routing_reservoirs_levelpool
  hydro_routing_reservoirs_hybrid
  hydro_routing_reservoirs_rfc
  hydro_routing_reservoirs
  hydro_driver
  hydro_orchestrator
  hydro_netcdf_layer
)
if (WRF_HYDRO_NUDGING STREQUAL "1")
  list(APPEND wrfhydro_nuopc_linklibs
    hydro_nudging
  )
endif (WRF_HYDRO_NUDGING STREQUAL "1")


### New Library: wrfhydro_nuopc
add_library(wrfhydro_nuopc STATIC ${wrfhydro_nuopc_files})
add_dependencies(wrfhydro_nuopc ${wrfhydro_nuopc_deps})
target_link_libraries(wrfhydro_nuopc PUBLIC ${wrfhydro_nuopc_linklibs})
target_link_libraries(wrfhydro_nuopc PUBLIC esmf)
target_link_libraries(wrfhydro_nuopc PUBLIC ${NETCDF_LIBRARIES})

### Library Installation
install(TARGETS wrfhydro_nuopc ${wrfhydro_nuopc_linklibs}
  EXPORT  wrfhydro-config
  LIBRARY DESTINATION lib
)
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION ${CMAKE_INSTALL_PREFIX}/WRFHYDRO
)
install(EXPORT wrfhydro-config
  DESTINATION lib/cmake
)

