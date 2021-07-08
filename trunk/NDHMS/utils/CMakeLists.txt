cmake_minimum_required (VERSION 2.8)

# read version numbers for wrf_hydro_version and nwm_version from 
# ../.version and ../.nwm_version
file (STRINGS "../.version" WRF_HYDRO_VERSION)
if (NWM_META)
	file (STRINGS "../.nwm_version"  NWM_VERSION)
else (NWM_META)
	set(NWM_VERSION "none")
endif (NWM_META)

# add the preprocessor definitions for NWM_VERSION and WRF_HYDRO_VERSION
# needed to compile module_version.F
add_definitions (-DNWM_VERSION="${NWM_VERSION}")
add_definitions (-DWRF_HYDRO_VERSION="${WRF_HYDRO_VERSION}")

# build the version static library
add_library(hydro_utils STATIC
	module_version.F
	module_hydro_stop.F	
)
