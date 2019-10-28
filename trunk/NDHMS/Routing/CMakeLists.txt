cmake_minimum_required (VERSION 2.8)

add_library(hydro_routing STATIC
	module_UDMAP.F
	module_channel_routing.F
	module_date_utilities_rt.F
	module_GW_baseflow.F
	module_gw_gw2d.F
	module_HYDRO_io.F
	module_HYDRO_utils.F
	module_lsm_forcing.F
	module_noah_chan_param_init_rt.F
	module_NWM_io_dict.F
	module_NWM_io.F
	module_reservoir_routing.F
	module_RT.F
	Noah_distr_routing.F
	Noah_distr_routing_overland.F
	Noah_distr_routing_subsurface.F        
)

target_link_libraries(hydro_routing PUBLIC hydro_mpp)
target_link_libraries(hydro_routing PUBLIC hydro_utils)
target_link_libraries(hydro_routing PUBLIC hydro_orchestrator)
target_link_libraries(hydro_routing PUBLIC hydro_routing_overland)
target_link_libraries(hydro_routing PUBLIC hydro_routing_subsurface)
#target_link_libraries(hydro_routing PUBLIC hydro_routing_groundwater)
#target_link_libraries(hydro_routing PUBLIC hydro_routing_groundwater_bucket)
#target_link_libraries(hydro_routing PUBLIC hydro_routing_groundwater_nhd)
#target_link_libraries(hydro_routing PUBLIC hydro_routing_groundwater_simple)
target_link_libraries(hydro_routing PUBLIC hydro_routing_reservoirs)
target_link_libraries(hydro_routing PUBLIC hydro_routing_reservoirs_levelpool)
target_link_libraries(hydro_routing PUBLIC hydro_routing_reservoirs_hybrid)
target_link_libraries(hydro_routing PUBLIC hydro_data_rec)
target_link_libraries(hydro_routing PUBLIC hydro_routing_reservoirs_rfc)

