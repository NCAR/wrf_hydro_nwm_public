cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs STATIC
	module_reservoir.F
	module_reservoir_read_timeslice_data.F       
	module_reservoir_utilities.F
)

add_subdirectory("Level_Pool")
add_subdirectory("Persistence_Level_Pool_Hybrid")


