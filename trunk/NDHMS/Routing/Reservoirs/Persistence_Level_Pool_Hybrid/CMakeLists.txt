cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs_hybrid STATIC
	module_persistence_levelpool_hybrid.F
	module_persistence_levelpool_hybrid_state.F       
	module_persistence_levelpool_hybrid_properties.F
)

add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs)



