cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_reservoirs_rfc STATIC
	module_rfc_forecasts.F
	module_rfc_forecasts_state.F       
	module_rfc_forecasts_properties.F
)

add_dependencies(hydro_routing_reservoirs_rfc hydro_routing_reservoirs)
add_dependencies(hydro_routing_reservoirs_rfc hydro_routing_reservoirs_levelpool)
