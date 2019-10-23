cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs_levelpool STATIC
	module_levelpool.F
	module_levelpool_state.F       
	module_levelpool_properties.F
)

add_dependencies(hydro_routing_reservoirs_levelpool hydro_routing_reservoirs)

