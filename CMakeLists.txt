cmake_minimum_required (VERSION 3.12)
cmake_policy(SET CMP0074 NEW) # use xxxx_ROOT env vars
if (NOT CMAKE_BUILD_TYPE)
        set(CMAKE_BUILD_TYPE "Release")
endif()


# set project name and version numbers
project (WRF_Hydro LANGUAGES Fortran C)
set (WRF_Hydro_VERSION_MAJOR 5)
set (WRF_Hydro_VERSION_MINOR 4)
set (WRF_Hydro_VERSION_PATCH 0)
set (National_Water_Model_VERSION_MAJOR 3)
set (National_Water_Model_VERSION_MINOR 1)
set (National_Water_Model_VERSION_PATCH beta)

# set cmake to work with MPI Fortran
find_package(MPI REQUIRED)
add_compile_options(${MPI_Fortran_COMPILE_FLAGS})
include_directories(${MPI_Fortran_INCLUDE_PATH})
link_directories(${MPI_Fortran_LIBRARIES})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)
set(CMAKE_SKIP_BUILD_RPATH  FALSE)
set(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)

# enable ctests
enable_testing()

#message("-- MPI Include directories: " ${MPI_INCLUDE_PATH} )
#message("-- MPI C COMPILER : " ${MPI_C_COMPILER} )
#message("-- MPI CXX COMPILER : " ${MPI_CXX_COMPILER} )
#message("-- MPI Fortran COMPILER : " ${MPI_Fortran_COMPILER} )
#message("-- MPI COMPILE FLAGS : " ${MPI_COMPILE_FLAGS} )
#message("-- MPI LINK FLAGS : " ${MPI_LINK_FLAGS} )
#message("-- MPI Fortran LINK FLAGS : " ${MPI_Fortran_LINK_FLAGS} )
#message("-- MPI Fortran LINK LIBRARIES : " ${MPI_Fortran_LIBRARIES} )
#message("-- MPI LIBRARY : " ${MPI_LIBRARY} )
#message("-- MPI EXTRA LIBRARY : " ${MPI_EXTRA_LIBRARY} )
#message("-- MPI LIBRARIES : " ${MPI_LIBRARIES} )

# netcdf does not have a built in package locator so add custom module directory
# that contains FindNetCDF.cmake
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/src/cmake-modules)

# try to find the installed NETCDF library
set(NETCDF_F90 "YES")
set(NETCDF_F77 "YES")
find_package(NetCDF REQUIRED)
message("-- NetCDF Include Dir(s): ${NETCDF_INCLUDES_F77} ${NETCDF_INCLUDES_F90}")

# set user controled enviorment variables
set(HYDRO_LSM $ENV{HYDRO_LSM} CACHE STRING "Name of the Land Surface Model to Use")

# set enviorment variables if they have not been set
if ("${HYDRO_LSM}" STREQUAL "")
        set(HYDRO_LSM "NoahMP")
        message("-- Setting LSM to: NoahMP")
endif()

# get the variables defined by setEnvar.sh
set(WRF_HYDRO $ENV{WRF_HYDRO} CACHE STRING "WRF environment variable. Always set to 1 for WRF-Hydro")
set(HYDRO_D $ENV{HYDRO_D} CACHE STRING "Print additional debug information in WRF-Hydro")
set(WRF_HYDRO_RAPID $ENV{WRF_HYDRO_RAPID} CACHE STRING "WRF-Hydro coupling to RAPID routing model 0=off 1=on")
set(SPATIAL_SOIL $ENV{SPATIAL_SOIL} CACHE STRING "Spatially distributed soil parameters for NoahMP 0=off 1=on")
set(WRFIO_NCD_LARGE_FILE_SUPPORT $ENV{WRFIO_NCD_LARGE_FILE_SUPPORT} CACHE STRING "Large netCDF file support 0=off, 1=on")
set(NCEP_WCOSS $ENV{NCEP_WCOSS} CACHE STRING "WCOSS file units 0=off 1=on")
set(NWM_META $ENV{NWM_META} CACHE STRING "NWM output metadata 0=off 1=on")
set(WRF_HYDRO_NUDGING $ENV{WRF_HYDRO_NUDGING} CACHE STRING "Streamflow nudging 0=off 1=on")
set(OUTPUT_CHAN_CONN $ENV{OUTPUT_CHAN_CONN} CACHE STRING "Output channel connections")
set(PRECIP_DOUBLE $ENV{PRECIP_DOUBLE} CACHE STRING "Precipitation as double")
set(WRF_HYDRO_NUOPC $ENV{WRF_HYDRO_NUOPC} CACHE STRING "NUOPC library 0=off 1=on")

#set default values for env variables
if (WRF_HYDRO STREQUAL "")
        set(WRF_HYDRO "1" CACHE STRING "WRF environment variable. Always set to 1 for WRF-Hydro" FORCE)
endif()

if (HYDRO_D STREQUAL "")
        if (CMAKE_BUILD_TYPE STREQUAL "Debug")
                set(HYDRO_D "1" CACHE STRING "Print additonal debug information in WRF-Hydro" FORCE)
        else()
                set(HYDRO_D "0" CACHE STRING "Print additonal debug information in WRF-Hydro" FORCE)
        endif()
endif()

if (WRF_HYDRO_RAPID STREQUAL "")
        set(WRF_HYDRO_RAPID "0" CACHE STRING "WRF-Hydro coupling to RAPID routing model 0=off 1=on" FORCE)
endif()

if (SPATIAL_SOIL STREQUAL "")
        set(SPATIAL_SOIL "0" CACHE STRING "Spatially distributed soil parameters for NoahMP 0=off 1=on" FORCE)
endif()

if (WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "")
        set(WRFIO_NCD_LARGE_FILE_SUPPORT "0" CACHE STRING "Large netCDF file support 0=off, 1=on" FORCE)
endif()

if (NCEP_WCOSS STREQUAL "")
        set(NCEP_WCOSS "0" CACHE STRING "WCOSS file units 0=off, 1=on" FORCE)
endif()

if (NWM_META STREQUAL "")
        set(NWM_META "0" CACHE STRING "NWM output metadata 0=off, 1=on" FORCE)
endif()

if (WRF_HYDRO_NUDGING STREQUAL "")
        set(WRF_HYDRO_NUDGING "0" CACHE STRING "Streamflow nudging 0=off, 1=on" FORCE)
endif()

if (OUTPUT_CHAN_CONN STREQUAL "")
        set(OUTPUT_CHAN_CONN "0" CACHE STRING "Output channel connections" FORCE)
endif()

if (PRECIP_DOUBLE STREQUAL "")
        set(PRECIP_DOUBLE "0" CACHE STRING "Precipitation as double" FORCE)
endif()

if (WRF_HYDRO_NUOPC STREQUAL "")
        set(WRF_HYDRO_NUOPC "0" CACHE STRING "NUOPC library 0=off, 1=on" FORCE)
endif()

# add preprocessor defines using env variables

message("=============================================================")
message("-- Start of WRF-Hydro Env VARIABLES" )

#always use -DMPP_LAND
add_definitions(-DMPP_LAND)

# set -DWRF_HYDRO from env
message("WRF_HYDRO = " ${WRF_HYDRO} )
if (WRF_HYDRO STREQUAL "1" )
        add_definitions(-DWRF_HYDRO)
endif()

#set -DHYDRO_D from env
message("HYDRO_D = " ${HYDRO_D} )
if (HYDRO_D STREQUAL "1" )
        add_definitions(-DHYDRO_D)
endif()

# set -DWRF_HYDRO_RAPID from env
message("WRF_HYDRO_RAPID = " ${WRF_HYDRO_RAPID} )
if (WRF_HYDRO_RAPID STREQUAL "1" )
        add_definitions(-DWRF_HYDRO_RAPID)
endif()

#set -DSPATIAL_SOIL from env
message("SPATIAL_SOIL = " ${SPATIAL_SOIL} )
if (SPATIAL_SOIL STREQUAL "1" )
        add_definitions(-DSPATIAL_SOIL)
endif()

#set -DWRFIO_NCD_LARGE_FILE_SUPPORT from env
message("WRFIO_NCD_LARGE_FILE_SUPPORT = " ${WRFIO_NCD_LARGE_FILE_SUPPORT} )
if(WRFIO_NCD_LARGE_FILE_SUPPORT STREQUAL "1" )
        add_definitions(-DWRFIO_NCD_LARGE_FILE_SUPPORT)
endif()

#set -DNCEP_WCOSS from env
message("NCEP_WCOSS = " ${NCEP_WCOSS} )
if (NCEP_WCOSS STREQUAL "1" )
        add_definitions(-DNCEP_WCOSS)
endif()

#set -DNWM_META from env
message("NWM_META = " ${NWM_META} )
if (NWM_META STREQUAL "1" )
        add_definitions(-DNWM_META)
endif()

#set -DWRF_HYDRO_NUDGING from env
message("WRF_HYDRO_NUDGING = " ${WRF_HYDRO_NUDGING} )
if (WRF_HYDRO_NUDGING STREQUAL "1" )
        add_definitions(-DWRF_HYDRO_NUDGING)
endif()

#set -DOUTPUT_CHAN_CONN from env
message("OUTPUT_CHAN_CONN = " ${OUTPUT_CHAN_CONN} )
if (OUTPUT_CHAN_CONN STREQUAL "1" )
        add_definitions(-DOUTPUT_CHAN_CONN)
endif()

#set -DPRECIP_DOUBLE from env
message("PRECIP_DOUBLE = " ${PRECIP_DOUBLE} )
if (PRECIP_DOUBLE STREQUAL "1" )
        add_definitions(-DPRECIP_DOUBLE)
endif()

#set -DWRF_HYDRO_NUOPC from env
message("WRF_HYDRO_NUOPC = " ${WRF_HYDRO_NUOPC} )
if (WRF_HYDRO_NUOPC STREQUAL "1" )
        add_definitions(-DWRF_HYDRO_NUOPC)
endif()

option(BUILD_CROCUS "Build Crocus" ON)

message("=============================================================")

#set compile flags based on compiler id
if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
        # set compile flags for gfortran
        message( "-- Using gfortran")
        set(CMAKE_Fortran_FLAGS "-cpp -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4")
        if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 9)
                set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
        endif()
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -fbacktrace")
elseif (CMAKE_Fortran_COMPILER_ID MATCHES "Intel.*")
        # set compile flags for ifort
        message( "-- Using ifort")
        set(CMAKE_Fortran_FLAGS "-fpp -w -ftz -align all -fno-alias -fp-model precise -FR -convert big_endian")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -march=core-avx2")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -traceback")
elseif ((CMAKE_Fortran_COMPILER_ID MATCHES "PGI.*") OR (CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC.*"))
        message("-- Using NVHPC / PGI")
        set(CMAKE_Fortran_FLAGS "-Mpreprocess -Mfree -byteswapio -Kieee ")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -traceback")
elseif ((CMAKE_Fortran_COMPILER_ID MATCHES "Cray*"))
        message("-- Using Cray")
        set(CMAKE_Fortran_FLAGS "-eZ -ffree -ef -h alias=none -h fp1 -hbyteswapio")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -G2")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -G0") # -traceback")
else()
        message("CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})
        message("Fortran compiler: " ${Fortran_COMPILER_NAME})
        message("No optimized Fortran compiler flags are known, we just try -O2...")
        set(CMAKE_Fortran_FLAGS "-cpp")
        set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
        set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g")
endif()
message("-- CMAKE_Fortran_COMPILER full path: " ${CMAKE_Fortran_COMPILER})

#set output directories for libraries binaries and fortran .mod files
set(CMAKE_BINARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/mods)

#add common include directores need in the build
include_directories(AFTER ${PROJECT_BINARY_DIR}/mods)
include_directories(AFTER ${MPI_INCLUDE_PATH})
# include_directories(AFTER ${NETCDF_INCLUDES})  # NOTE: We don't need the C include for netCDF
include_directories(AFTER ${NETCDF_INCLUDES_F77})
include_directories(AFTER ${NETCDF_INCLUDES_F90})
include_directories(AFTER ${PROJECT_SOURCE_DIR}/src/Data_Rec)

add_subdirectory("src")
add_subdirectory("tests/ctests")
