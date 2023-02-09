cmake_minimum_required (VERSION 3.0)

add_library(hydro_data_rec STATIC
	module_gw_gw2d_data.F
	module_rt_inc.F
	module_namelist_inc.F
	module_RT_data.F
	module_namelist.F
)
