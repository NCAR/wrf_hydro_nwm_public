cmake_minimum_required (VERSION 2.8)

# build the version static library
add_library(nwm_nudging STATIC
	module_date_utils_nudging.F
	module_nudging_io.F	
	module_nudging_utils.F
	module_stream_nudging.F
)

target_link_libraries(nwm_nudging PUBLIC nwm_mpp)
target_link_libraries(nwm_nudging PUBLIC nwm_data_rec)
