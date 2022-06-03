cmake_minimum_required (VERSION 2.8)

add_library(hydro_mpp STATIC
	CPL_WRF.F
	module_mpp_GWBUCKET.F
	module_mpp_ReachLS.F
	mpp_land.F          
	hashtable.F
)
