cmake_minimum_required (VERSION 2.8)

add_library(noahmp_phys STATIC
	module_sf_noahmpdrv.F
	module_sf_noahmp_glacier.F
	module_sf_noahmp_groundwater.F
	module_sf_noahmplsm.F
)
