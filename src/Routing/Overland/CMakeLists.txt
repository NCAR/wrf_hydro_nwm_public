cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing_overland STATIC
	module_overland_control.F  
	module_overland_mass_balance.F        
	module_overland_streams_and_lakes.F           
	module_overland_routing_properties.F
	module_overland.F
)
