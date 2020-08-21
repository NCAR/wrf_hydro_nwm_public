cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs STATIC
	module_reservoir.F
	module_reservoir_read_timeslice_data.F
	module_reservoir_read_rfc_time_series_data.F
	module_reservoir_utilities.F
)

add_subdirectory("Level_Pool")
add_subdirectory("Persistence_Level_Pool_Hybrid")
add_subdirectory("RFC_Forecasts")

add_dependencies(hydro_routing_reservoirs hydro_utils)
