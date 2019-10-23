cmake_minimum_required (VERSION 2.8)

add_library(noah_util STATIC
	kwm_string_utilities.F
	module_date_utilities.F
	module_model_constants.F
	module_Noahlsm_utility.F
	module_sfcdif_wrf.F 
)
