cmake_minimum_required (VERSION 3.0)

# build the version static library
add_library(hydro_debug_utils STATIC
	debug_dump_variable.F
)
