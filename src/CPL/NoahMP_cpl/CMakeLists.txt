add_library(hydro_noahmp_cpl STATIC
        module_hrldas_HYDRO.F
        hrldas_drv_HYDRO.F
)

target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_mpp)
target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_utils)
target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_overland)
target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_subsurface)
#target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_groundwater)
#target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_groundwater_bucket)
#target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_groundwater_nhd)
#target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing_groundwater_simple)
target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_data_rec)
target_link_libraries(hydro_noahmp_cpl PUBLIC hydro_routing)
