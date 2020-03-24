# The set of languages for which implicit dependencies are needed:
SET(CMAKE_DEPENDS_LANGUAGES
  "Fortran"
  )
# The set of files for implicit dependencies of each language:
SET(CMAKE_DEPENDS_CHECK_Fortran
  "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/utils/module_hydro_stop.F" "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/utils/CMakeFiles/hydro_utils.dir/module_hydro_stop.F.o"
  "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/utils/module_version.F" "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/utils/CMakeFiles/hydro_utils.dir/module_version.F.o"
  )
SET(CMAKE_Fortran_COMPILER_ID "Intel")

# Preprocessor definitions for this target.
SET(CMAKE_TARGET_DEFINITIONS
  "HYDRO_D"
  "MPP_LAND"
  "NWM_VERSION=\"none\""
  "SPATIAL_SOIL"
  "WRFIO_NCD_LARGE_FILE_SUPPORT"
  "WRF_HYDRO"
  "WRF_HYDRO_NUDGING"
  "WRF_HYDRO_VERSION=\"v5.1.0-beta2\""
  )

# Targets to which this target links.
SET(CMAKE_TARGET_LINKED_INFO_FILES
  )

# Fortran module output directory.
SET(CMAKE_Fortran_TARGET_MODULE_DIR "/scratch2/COASTAL/coastal/save/NAMED_STORMS/NEMS_APP/NWM/trunk/NDHMS/cmake-build-standalone/mods")

# The include file search paths:
SET(CMAKE_C_TARGET_INCLUDE_PATH
  "mods"
  "/apps/netcdf/4.6.1/intel/16.1.150/include"
  "../Data_Rec"
  )
SET(CMAKE_CXX_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
SET(CMAKE_Fortran_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
SET(CMAKE_ASM_TARGET_INCLUDE_PATH ${CMAKE_C_TARGET_INCLUDE_PATH})
