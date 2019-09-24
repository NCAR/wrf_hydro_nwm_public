cmake_minimum_required (VERSION 2.8)

add_library(nwm_routing_reservoirs_hybrid STATIC
	module_persistence_levelpool_hybrid.F
	module_persistence_levelpool_hybrid_state.F       
	module_persistence_levelpool_hybrid_properties.F
)

add_dependencies(nwm_routing_reservoirs_hybrid nwm_routing_reservoirs)



