cmake_minimum_required (VERSION 2.8)

# build the version static library
add_library(hydro_nudging STATIC
	module_date_utils_nudging.F
	module_nudging_io.F	
	module_nudging_utils.F
	module_stream_nudging.F
)

target_link_libraries(hydro_nudging PUBLIC hydro_mpp)
target_link_libraries(hydro_nudging PUBLIC hydro_data_rec)
target_link_libraries(hydro_nudging PUBLIC hydro_orchestrator)
