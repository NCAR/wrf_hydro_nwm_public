add_library(noahmp_util STATIC
	kwm_string_utilities.F
	module_date_utilities.F
	module_model_constants.F
	module_wrf_utilities.F
)

target_link_libraries(noahmp_util PUBLIC hydro_mpp)
