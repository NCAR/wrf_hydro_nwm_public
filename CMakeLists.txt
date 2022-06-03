cmake_minimum_required (VERSION 2.8)

if (NOT CMAKE_C_COMPILER)
	set (CMAKE_C_COMPILER "mpicc")
endif (NOT CMAKE_C_COMPILER)
if (NOT CMAKE_CXX_COMPILER)
	set (CMAKE_CXX_COMPILER "mpicxx")
endif (NOT CMAKE_CXX_COMPILER)
if (NOT CMAKE_Fortran_COMPILER)
	set (CMAKE_Fortran_COMPILER "mpif90")
endif (NOT CMAKE_Fortran_COMPILER)

project (WRF_Hydro)

#set version numbers
set (WRF_Hydro_VERSION_MAJOR 5)
set (WRF_Hydro_VERSION_MINOR 2)
set (WRF_Hydro_VERSION_PATCH dev)
set (National_Water_Model_VERSION_MAJOR 2)
set (National_Water_Model_VERSION_MINOR 1)
set (National_Water_Model_VERSION_PATCH dev)

# set cmake to work with fortran
enable_language (Fortran)

#try to find the installed MPI library
#enable if compiler wrappers dont find MPI libraries
#find_package(MPI REQUIRED)

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
set (CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/src/cmake-modules)

#try to find the installed NETCDF library
set (NETCDF_F90 "YES")
set (NETCDF_F77 "YES")
find_package(NetCDF REQUIRED)
message("-- NetCDF Include Dir: ${NETCDF_INCLUDES}")

#set user controled enviorment variables
set (HYDRO_LSM $ENV{HYDRO_LSM} CACHE STRING "Name of the Land Surface Model to Use")

# set enviorment variables if they have not been set
if ("${HYDRO_LSM}" STREQUAL "")
	set (HYDRO_LSM "NoahMP")
	message ("-- Setting LSM to: NoahMP")
endif ("${HYDRO_LSM}" STREQUAL "")

#get the variables defined by setEnvar.sh
set (WRF_HYDRO $ENV{WRF_HYDRO} CACHE STRING "WRF environment variable. Always set to 1 for WRF-Hydro")
set (HYDRO_D $ENV{HYDRO_D} CACHE STRING "Print additional debug information in WRF-Hydro")
set (WRF_HYDRO_RAPID $ENV{WRF_HYDRO_RAPID} CACHE STRING "WRF-Hydro coupling to RAPID routing model 0=off 1=on")
set (SPATIAL_SOIL $ENV{SPATIAL_SOIL} CACHE STRING "Spatially distributed soil parameters for NoahMP 0=off 1=on")
set (WRFIO_NCD_LARGE_FILE_SUPPORT $ENV{WRFIO_NCD_LARGE_FILE_SUPPORT} CACHE STRING "Large netCDF file support 0=off, 1=on")
set (NCEP_WCOSS $ENV{NCEP_WCOSS} CACHE STRING "WCOSS file units 0=off 1=on")
set (NWM_META $ENV{NWM_META} CACHE STRING "NWM output metadata 0=off 1=on")
set (WRF_HYDRO_NUDGING $ENV{WRF_HYDRO_NUDGING} CACHE STRING "Streamflow nudging 0=off 1=on")
set (OUTPUT_CHAN_CONN $ENV{OUTPUT_CHAN_CONN} CACHE STRING "Output channel connections")
set (PRECIP_DOUBLE $ENV{PRECIP_DOUBLE} CACHE STRING "Precipitation as double")
set (WRF_HYDRO_NUOPC $ENV{WRF_HYDRO_NUOPC} CACHE STRING "NUOPC library 0=off 1=on")

#set default values for env variables
if (WRF_HYDRO STREQUAL "")
	set (WRF_HYDRO "1" CACHE STRING "WRF environment variable. Always set to 1 for WRF-Hydro" FORCE)
endif (WRF_HYDRO STREQUAL "")

if (HYDRO_D STREQUAL "")
	set (HYDRO_D "0" CACHE STRING "Print additonal debug information in WRF-Hydro" FORCE)
endif (HYDRO_D STREQUAL "")

if (WRF_HYDRO_RAPID STREQUAL "")
	set (WRF_HYDRO_RAPID "0" CACHE STRING "WRF-Hydro coupling to RAPID routing model 0=off 1=on" FORCE)
endif (WRF_HYDRO_RAPID STREQUAL "")

if (SPATIAL_SOIL STREQUAL "")
	set (SPATIAL_SOIL "0" CACHE STRING "Spatially distributed soil parameters for NoahMP 0=off 1=on" FORCE)
endif (SPATIAL_SOIL STREQUAL "")

if (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "")
	set (WRFIO_NCD_LARGE_FILE_SUPPORT "0" CACHE STRING "Large netCDF file support 0=off, 1=on" FORCE)
endif (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "")

if (NCEP_WCOSS STREQUAL "")
	set (NCEP_WCOSS "0" CACHE STRING "WCOSS file units 0=off, 1=on" FORCE)
endif (NCEP_WCOSS STREQUAL "")

if (NWM_META STREQUAL "")
        set (NWM_META "0" CACHE STRING "NWM output metadata 0=off, 1=on" FORCE)
endif (NWM_META STREQUAL "")

if (WRF_HYDRO_NUDGING STREQUAL "")
	set (WRF_HYDRO_NUDGING "0" CACHE STRING "Streamflow nudging 0=off, 1=on" FORCE)
endif (WRF_HYDRO_NUDGING STREQUAL "")

if (OUTPUT_CHAN_CONN STREQUAL "")
	set (OUTPUT_CHAN_CONN "0" CACHE STRING "Output channel connections" FORCE)
endif (OUTPUT_CHAN_CONN STREQUAL "")

if (PRECIP_DOUBLE STREQUAL "")
	set (PRECIP_DOUBLE "0" CACHE STRING "Precipitation as double" FORCE)
endif (PRECIP_DOUBLE STREQUAL "")

if (WRF_HYDRO_NUOPC STREQUAL "")
        set (WRF_HYDRO_NUOPC "0" CACHE STRING "NUOPC library 0=off, 1=on" FORCE)
endif (WRF_HYDRO_NUOPC STREQUAL "")

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

#set -DNCEP_WCOSS from env
message ("NWM_META = " ${NWM_META} )
if (NWM_META STREQUAL "1" )
        add_definitions(-DNWM_META)
endif (NWM_META STREQUAL "1")

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

#set -DWRF_HYDRO_NUOPC from env
message ("WRF_HYDRO_NUOPC = " ${WRF_HYDRO_NUOPC} )
if (WRF_HYDRO_NUOPC STREQUAL "1" )
        add_definitions(-DWRF_HYDRO_NUOPC)
endif (WRF_HYDRO_NUOPC STREQUAL "1")

message("=============================================================")

#set compile flags based on compiler id

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
    # set compile flags for gfortran
    message ( "-- Using gfortran")
    set (CMAKE_Fortran_FLAGS "-cpp -O2 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4 -fallow-argument-mismatch")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel.*")
    # set compile flags for ifort
    message ( "Using ifort")
    set (CMAKE_Fortran_FLAGS "-fpp -O2 -g -w -ftz -align all -fno-alias -fp-model precise -FR -convert big_endian")
else (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
    message ("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
    message ("Fortran compiler: " ${Fortran_COMPILER_NAME})
    message ("No optimized Fortran compiler flags are known, we just try -O2...")
    set (CMAKE_Fortran_FLAGS_RELEASE "-cpp -O2")
    set (CMAKE_Fortran_FLAGS_DEBUG   "-cpp -O0 -g")
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
include_directories(AFTER ${PROJECT_SOURCE_DIR}/src/Data_Rec)


add_subdirectory("src")
