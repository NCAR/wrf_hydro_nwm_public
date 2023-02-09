cmake_minimum_required (VERSION 3.0)

add_library(noahmp_util STATIC
	kwm_string_utilities.F
	module_date_utilities.F
	module_model_constants.F
	module_wrf_utilities.F
)
