cmake_minimum_required (VERSION 2.8)

add_library(hydro_data_rec STATIC
	module_gw_gw2d_data.F
	module_RT_data.F
	module_namelist.F	
)
