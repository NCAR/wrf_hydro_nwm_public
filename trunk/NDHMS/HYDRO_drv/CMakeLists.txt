cmake_minimum_required (VERSION 2.8)

# build the version static library
add_library(nwm_driver STATIC
	module_HYDRO_drv.F
)

target_link_libraries(nwm_driver PUBLIC nwm_mpp)
target_link_libraries(nwm_driver PUBLIC nwm_data_rec)

if (WRF_HYDRO_NUDGING STREQUAL "1")
	target_link_libraries(nwm_driver PUBLIC nwm_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")
