cmake_minimum_required (VERSION 2.8)

set(CMAKE_C_COMPILER "mpicc")
set(CMAKE_CXX_COMPILER "mpic++")
set(CMAKE_Fortran_COMPILER "mpif90")

project (National_Water_Model)

#set versions number
set (National_Water_Model_VERSION_MAJOR 2)
set (National_Water_model_VERSION_MINOR 2)

# set cmake to work with fortran
enable_language (Fortran)

#try to find the installed MPI library
find_package(MPI REQUIRED)
#message ("-- MPI Include directories: " ${MPI_INCLUDE_PATH} )
#message ("-- MPI C COMPILER : " ${MPI_C_COMPILER} )
#message ("-- MPI CXX COMPILER : " ${MPI_CXX_COMPILER} )
#message ("-- MPI Fortran COMPILER : " ${MPI_Fortran_COMPILER} )
#message ("-- MPI COMPILE FLAGS : " ${MPI_COMPILE_FLAGS} )
#message ("-- MPI LINK FLAGS : " ${MPI_LINK_FLAGS} )
#message ("-- MPI Fortran LINK FLAGS : " ${MPI_Fortran_LINK_FLAGS} )
#message ("-- MPI LIBRARY : " ${MPI_LIBRARY} )
#message ("-- MPI EXTRA LIBRARY : " ${MPI_EXTRA_LIBRARY} )
#message ("-- MPI LIBRARIES : " ${MPI_LIBRARIES} )

# netcdf does not have a built in package locator so add custom module directory
# that contains FindNetCDF.cmake
set (CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake-modules )

#try to find the installed NETCDF library
set (NETCDF_F90 "YES")
set (NETCDF_F77 "YES")
find_package(NetCDF REQUIRED)
message("-- NetCDF Include Dir: ${NETCDF_INCLUDES}")

#set user controled enviorment variables
set (NWM_LSM $ENV{NWM_LSM} CACHE STRING "Name of the Land Surface Model to Use")

# set enviorment variables if they have not been set
if ("${NWM_LSM}" STREQUAL "")
	set (NWM_LSM "NoahMP")
	message ("-- Setting LSM to: NoahMP")
endif("${NWM_LSM}" STREQUAL "")

#get the variables defined by setEnvar.sh
set (WRF_HYDRO $ENV{WRF_HYDRO} CACHE String "WRF Enviorment vairable allways set to 1 for WRF-Hydro")
set (HYDRO_D $ENV{HYDRO_D} CACHE String "Print additonal debug information in WRF-Hydro")
set (HYDRO_REALTIME $ENV{HYDRO_REALTIME} CACHE String "WRF Enviorment vairable for real time coupling 0=off 1=on")
set (WRF_HYDRO_RAPID $ENV{WRF_HYDRO_RAPID} CACHE String "WRF Enviorment vairable for rapid routing 0=off 1=on")
set (SPATIAL_SOIL $ENV{SPATIAL_SOIL} CACHE String "Spatial Soil Parameters for Noah MP 0=off 1=on")
set (WRFIO_NCD_LARGE_FILE_SUPPORT $ENV{WRFIO_NCD_LARGE_FILE_SUPPORT} CACHE String "Large netcdf file support 0=off, 1=on")
set (NCEP_WCOSS $ENV{NCEP_WCOSS} CACHE String "WCOSS file units 0=off 1=on")
set (WRF_HYDRO_NUDGING $ENV{WRF_HYDRO_NUDGING} CACHE String "Streamflow nudging 0=off 1=on")
set (OUTPUT_CHAN_CONN $ENV{OUTPUT_CHAN_CONN} CACHE String "Output channel connections")
set (PRECIP_DOUBLE $ENV{PRECIP_DOUBLE} CACHE String "Precipitation as double")

#set default values for env variables
if (WRF_HYDRO STREQUAL "")
	set (WRF_HYDRO "1" CACHE String "WRF Enviorment vairable allways set to 1 for WRF-Hydro" FORCE)
endif (WRF_HYDRO STREQUAL "")

if (HYDRO_D STREQUAL "")
	set (HYDRO_D "0" CACHE String "Print additonal debug information in WRF-Hydro" FORCE)
endif (HYDRO_D STREQUAL "")

if (HYDRO_REALTIME STREQUAL "")
	set (HYDRO_REALTIME "0" CACHE String "WRF Enviorment vairable for real time coupling 0=off 1=on" FORCE)
endif (HYDRO_REALTIME STREQUAL "")

if (WRF_HYDRO_RAPID STREQUAL "")
	set (WRF_HYDRO_RAPID "0" CACHE String "WRF Enviorment vairable for rapid routing 0=off 1=on" FORCE)
endif (WRF_HYDRO_RAPID STREQUAL "")

if (SPATIAL_SOIL STREQUAL "")
	set (SPATIAL_SOIL "0" CACHE String "Spatial Soil Parameters for Noah MP 0=off 1=on" FORCE)
endif (SPATIAL_SOIL STREQUAL "")

if (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "")
	set (WRFIO_NCD_LARGE_FILE_SUPPORT "0" CACHE String "Large netcdf file support 0=off, 1=on" FORCE)
endif (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "")

if (NCEP_WCOSS STREQUAL "")
	set (NCEP_WCOSS "0" CACHE String "WCOSS file units 0=off, 1=on" FORCE)
endif (NCEP_WCOSS STREQUAL "")

if (WRF_HYDRO_NUDGING STREQUAL "")
	set (WRF_HYDRO_NUDGING "0" CACHE String "Streamflow nudging 0=off, 1=on" FORCE)
endif (WRF_HYDRO_NUDGING STREQUAL "")

if (OUTPUT_CHAN_CONN STREQUAL "")
	set (OUTPUT_CHAN_CONN "0" CACHE String "Output channel connections" FORCE)
endif (OUTPUT_CHAN_CONN STREQUAL "")

if (PRECIP_DOUBLE STREQUAL "")
	set (PRECIP_DOUBLE "0" CACHE String "Precipitation as double" FORCE)
endif (PRECIP_DOUBLE STREQUAL "")


# add preprocessor defines using env variables

message ("=============================================================")
message ("-- Start of WRF-Hydro Env VARIABLES" )

#always use -DMPP_LAND
add_definitions(-DMPP_LAND)

# set -DWRF_HYDRO from env
message ("WRF_HYDRO = " ${WRF_HYDRO} )
if (WRF_HYDRO STREQUAL "1" )
	add_definitions(-DWRF_HYDRO)
endif (WRF_HYDRO STREQUAL "1") 

#set -DHYDRO_D from env
message ("HYDRO_D = " ${HYDRO_D} )
if (HYDRO_D STREQUAL "1" )
	add_definitions(-DHYDRO_D)
endif (HYDRO_D STREQUAL "1") 

# set -DHYDRO_REALTIME from env
message ("HYDRO_REALTIME = " ${HYDRO_REALTIME} )
if (HYDRO_REALTIME STREQUAL "1" )
	add_definitions(-DHYDRO_REALTIME)
endif (HYDRO_REALTIME STREQUAL "1") 

# set -DWRF_HYDRO_RAPID from env
message ("WRF_HYDRO_RAPID = " ${WRF_HYDRO_RAPID} )
if (WRF_HYDRO_RAPID STREQUAL "1" )
	add_definitions(-DWRF_HYDRO_RAPID)
endif (WRF_HYDRO_RAPID STREQUAL "1") 

#set -DSPATIAL_SOIL from env
message ("SPATIAL_SOIL = " ${SPATIAL_SOIL} )
if (SPATIAL_SOIL STREQUAL "1" )
	add_definitions(-DSPATIAL_SOIL)
endif (SPATIAL_SOIL STREQUAL "1")

#set -DWRFIO_NCD_LARGE_FILE_SUPPORT from env
message ("WRFIO_NCD_LARGE_FILE_SUPPORT = " ${WRFIO_NCD_LARGE_FILE_SUPPORT} )
if (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "1" )
	add_definitions(-DWRFIO_NCD_LARGE_FILE_SUPPORT)
endif (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "1") 

#set -DNCEP_WCOSS from env
message ("NCEP_WCOSS = " ${NCEP_WCOSS} )
if (NCEP_WCOSS STREQUAL "1" )
	add_definitions(-DNCEP_WCOSS)
endif (NCEP_WCOSS STREQUAL "1") 

#set -DSPATIAL_SOIL from env
message ("WRF_HYDRO_NUDGING = " ${WRF_HYDRO_NUDGING} )
if (WRF_HYDRO_NUDGING STREQUAL "1" )
	add_definitions(-DWRF_HYDRO_NUDGING)
endif (WRF_HYDRO_NUDGING STREQUAL "1") 

#set -DSPATIAL_SOIL from env
message ("OUTPUT_CHAN_CONN = " ${OUTPUT_CHAN_CONN} )
if (OUTPUT_CHAN_CONN STREQUAL "1" )
	add_definitions(-DOUTPUT_CHAN_CONN)
endif (OUTPUT_CHAN_CONN STREQUAL "1") 

#set -DSPATIAL_SOIL from env
message ("PRECIP_DOUBLE = " ${PRECIP_DOUBLE} )
if (PRECIP_DOUBLE STREQUAL "1" )
	add_definitions(-DPRECIP_DOUBLE)
endif (PRECIP_DOUBLE STREQUAL "1") 

message("=============================================================")

#set compile flags based on compiler id

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
    # set compile flags for gfortran
    message ( "-- Using gfortran")
    set (CMAKE_Fortran_FLAGS " -O2 -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel.*")
    # set compile flags for ifort
    message ( "Using ifort")
    set (CMAKE_Fortran_FLAGS "O2 -g -w -c -ftz -align all -fno-alias -fp-model precise -FR -convert big_endian")
else (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
    message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
    message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
    message ("No optimized Fortran compiler flags are known, we just try -O2...")
    set (CMAKE_Fortran_FLAGS_RELEASE "-O2")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif ( CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*" )

#set output directories for libraries binaries and fortran .mod files
set(CMAKE_BINARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/mods)

#add common include directores need in the build
include_directories(AFTER ${PROJECT_BINARY_DIR}/mods)
include_directories(AFTER ${MPI_INCLUDE_PATH})
include_directories(AFTER ${NETCDF_INCLUDES})
include_directories(AFTER ${PROJECT_SOURCE_DIR}/Data_Rec)

# build the various sup-projects
add_subdirectory("utils")
add_subdirectory("MPP")
add_subdirectory("Debug_Utilities")
add_subdirectory("Routing/Overland")
#add_subdirectory("Routing/Subsurface")
#add_subdirectory("Routing/Groundwater")
add_subdirectory("Routing/Reservoirs")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")

if (WRF_HYDRO_NUDGING STREQUAL "1")
	add_subdirectory("nudging")
	add_dependencies(nwm_driver nwm_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")

# add module dependencies
add_dependencies(nwm_debug_utils nwm_mpp) 

add_dependencies(nwm_routing nwm_mpp)
add_dependencies(nwm_routing nwm_routing_overland)
#add_dependencies(nwm_routing nwm_routing_subsurface)
#add_dependencies(nwm_routing nwm_routing_groundwater)
#add_dependencies(nwm_routing nwm_routing_groundwater_bucket)
#add_dependencies(nwm_routing nwm_routing_groundwater_nhd)
#add_dependencies(nwm_routing nwm_routing_groundwater_simple)
add_dependencies(nwm_routing nwm_utils)

add_dependencies(nwm_routing_overland nwm_mpp)

#add_dependencies(nwm_routing_groundwater nwm_mpp)
#add_dependencies(nwm_routing_groundwater_bucket nwm_routing_groundwater)
#add_dependencies(nwm_routing_groundwater_simple nwm_routing_groundwater)
#add_dependencies(nwm_routing_groundwater_simple nwm_routing_groundwater_bucket)
#add_dependencies(nwm_routing_groundwater_nhd nwm_routing_groundwater)
#add_dependencies(nwm_routing_groundwater_nhd nwm_routing_groundwater_bucket)

add_dependencies(nwm_driver nwm_routing)
add_dependencies(nwm_driver nwm_debug_utils)

add_dependencies(nwm_data_rec nwm_routing_overland)
#add_dependencies(nwm_data_rec nwm_routing_subsurface)
#add_dependencies(nwm_data_rec nwm_routing_groundwater)
#add_dependencies(nwm_data_rec nwm_routing_groundwater_bucket)
#add_dependencies(nwm_data_rec nwm_routing_groundwater_simple)
#add_dependencies(nwm_data_rec nwm_routing_groundwater_nhd)
add_dependencies(nwm_data_rec nwm_routing_reservoirs)


if (NWM_LSM MATCHES "NoahMP")
	message ("-- Building NoahMP LSM")
	add_subdirectory("Land_models/NoahMP")
	
	add_subdirectory("CPL/NoahMP_cpl")
	add_dependencies(nwm_noahmp_cpl nwm_routing)
	add_dependencies(nwm_noahmp_cpl nwm_mpp )
	add_dependencies(nwm_noahmp_cpl nwm_driver )
	
	
	add_executable(wrfhydro.exe
		Land_models/NoahMP/IO_code/main_hrldas_driver.F
		Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.F
		Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F
	)
	
	target_link_libraries(wrfhydro.exe nwm_utils)
	target_link_libraries(wrfhydro.exe nwm_mpp)
	target_link_libraries(wrfhydro.exe nwm_debug_utils)
	target_link_libraries(wrfhydro.exe nwm_routing_overland)
	#target_link_libraries(wrfhydro.exe nwm_routing_subsurface)
	#target_link_libraries(wrfhydro.exe nwm_routing_groundwater)
	#target_link_libraries(wrfhydro.exe nwm_routing_groundwater_bucket)
	#target_link_libraries(wrfhydro.exe nwm_routing_groundwater_nhd)
	#target_link_libraries(wrfhydro.exe nwm_routing_groundwater_simple)
	target_link_libraries(wrfhydro.exe nwm_routing_reservoirs)
	target_link_libraries(wrfhydro.exe nwm_data_rec)
	target_link_libraries(wrfhydro.exe nwm_routing)
	target_link_libraries(wrfhydro.exe nwm_driver)
	target_link_libraries(wrfhydro.exe noahmp_util)
	target_link_libraries(wrfhydro.exe noahmp_phys)
	target_link_libraries(wrfhydro.exe nwm_noahmp_cpl)
	target_link_libraries(wrfhydro.exe ${NETCDF_LIBRARIES})
	
	add_dependencies(wrfhydro.exe nwm_utils)
	add_dependencies(wrfhydro.exe nwm_mpp)
	add_dependencies(wrfhydro.exe nwm_debug_utils)
	add_dependencies(wrfhydro.exe nwm_routing_overland)
	#add_dependencies(wrfhydro.exe nwm_routing_subsurface)
	#add_dependencies(wrfhydro.exe nwm_routing_groundwater)
	#add_dependencies(wrfhydro.exe nwm_routing_groundwater_bucket)
	#add_dependencies(wrfhydro.exe nwm_routing_groundwater_nhd)
	#add_dependencies(wrfhydro.exe nwm_routing_groundwater_simple)
	add_dependencies(wrfhydro.exe nwm_data_rec)
	add_dependencies(wrfhydro.exe nwm_routing)
	add_dependencies(wrfhydro.exe nwm_driver)
	add_dependencies(wrfhydro.exe noahmp_util)
	add_dependencies(wrfhydro.exe noahmp_phys)
	add_dependencies(wrfhydro.exe nwm_noahmp_cpl)
	
	if (WRF_HYDRO_NUDGING STREQUAL "1")
		target_link_libraries(wrfhydro.exe nwm_nudging)
 		add_dependencies(wrfhydro.exe nwm_nudging)
	endif (WRF_HYDRO_NUDGING STREQUAL "1")	
		
elseif(NWM_LSM MATCHES "Noah")
	message ("-- Building Noah LSM")
else()
	message ("Unknown Land Surface Model:" ${NWM_LSM} )
endif (NWM_LSM MATCHES "NoahMP")

