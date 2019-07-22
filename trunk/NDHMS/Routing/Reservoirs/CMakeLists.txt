cmake_minimum_required (VERSION 2.8)

add_library(nwm_routing_reservoirs STATIC
	module_reservoir_base.F
	module_levelpool_parameters.F       
	module_levelpool_state.F
	module_levelpool_tests.F
	module_levelpool.F          
)


