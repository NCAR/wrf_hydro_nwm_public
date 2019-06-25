cmake_minimum_required (VERSION 2.8)

# build the version static library
add_library(nwm_debug_utils STATIC
	debug_dump_variable.F
)
