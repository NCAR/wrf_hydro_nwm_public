cmake_minimum_required (VERSION 2.8)

add_library(nwm_routing_subsurface STATIC
	module_subsurface_input.F  
	module_subsurface_output.F       
	module_subsurface_static_data.F           
	module_subsurface_grid_transform.F
	module_subsurface_properties.F
	module_subsurface_state.F
	module_subsurface.F 
)

target_link_libraries(nwm_routing_subsurface PUBLIC nwm_routing_overland)

