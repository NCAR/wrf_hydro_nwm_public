cmake_minimum_required (VERSION 2.8)

add_library(nwm_noahmp_cpl STATIC
	module_hrldas_HYDRO.F
	hrldas_drv_HYDRO.F       
)

target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_mpp)
target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_utils)
target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_overland)
#target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_subsurface)
#target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_groundwater)
#target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_groundwater_bucket)
#target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_groundwater_nhd)
#target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing_groundwater_simple)
target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_data_rec)
target_link_libraries(nwm_noahmp_cpl PUBLIC nwm_routing)
