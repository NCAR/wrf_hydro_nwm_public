cmake_minimum_required (VERSION 2.8)

set (CMAKE_C_COMPILER "mpicc")
set (CMAKE_CXX_COMPILER "mpicxx")
set (CMAKE_Fortran_COMPILER "mpif90")

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
set (CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake-modules)

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

# Beheen 12/17/19
# source setEnvar.sh and source esmf-impi-env.sh
# To run: cd cmakeBuild  rm -rf *; cmake -DNUOPCMODE=1 -DBUILD_PREC=r4 -DDBUG=on -Wno-dev ..  
#                                  make VERBOSE=1
# set user controled environment variable, NUOPC mode
set (NUOPCMODE $ENV{NUOPCMODE} CACHE STRING "NWM Running in NUOPC Coupled Mode")
#set default value for nuopc
if ("${NUOPCMODE}" STREQUAL "")
	set (NUOPCMODE "0" CACHE STRING "NWM NUOPC Coupled Mode" FORCE)
	set (BUILD_PREC "" CACHE STRING "NWM NUOPC Build Prec." FORCE)
	set (DEBUG "" CACHE STRING "NWM NUOPC Debug Mode" FORCE)
endif ("${NUOPCMODE}" STREQUAL "")

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


message("=============================================================")

#set compile flags based on compiler id

if (CMAKE_Fortran_COMPILER_ID MATCHES "GNU.*")
    # set compile flags for gfortran
    message ( "-- Using gfortran")
    set (CMAKE_Fortran_FLAGS "-cpp -O2 -g -w -ffree-form -ffree-line-length-none -fconvert=big-endian -frecord-marker=4")
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
include_directories(AFTER ${PROJECT_SOURCE_DIR}/Data_Rec)

# build the various sub-projects
add_subdirectory("MPP")
add_subdirectory("utils")
add_subdirectory("IO")
add_subdirectory("OrchestratorLayer")
add_subdirectory("Debug_Utilities")
add_subdirectory("Routing/Overland")
add_subdirectory("Routing/Subsurface")
##add_subdirectory("Routing/Groundwater")
add_subdirectory("Routing/Reservoirs")
add_subdirectory("Data_Rec")
add_subdirectory("Routing")
add_subdirectory("HYDRO_drv")

if (WRF_HYDRO_NUDGING STREQUAL "1")
	add_subdirectory("nudging")
	add_dependencies(hydro_driver hydro_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")

# add module dependencies
add_dependencies(hydro_debug_utils hydro_mpp)
add_dependencies(hydro_utils hydro_mpp)

add_dependencies(hydro_orchestrator hydro_netcdf_layer)

add_dependencies(hydro_routing hydro_mpp)
add_dependencies(hydro_routing hydro_routing_overland)
add_dependencies(hydro_routing hydro_routing_subsurface)
add_dependencies(hydro_routing hydro_routing_reservoirs)
add_dependencies(hydro_routing hydro_routing_reservoirs_levelpool)
add_dependencies(hydro_routing hydro_routing_reservoirs_hybrid)
add_dependencies(hydro_routing_reservoirs_hybrid hydro_routing_reservoirs_levelpool)
##add_dependencies(hydro_routing hydro_routing_groundwater)
##add_dependencies(hydro_routing hydro_routing_groundwater_bucket)
##add_dependencies(hydro_routing hydro_routing_groundwater_nhd)
##add_dependencies(hydro_routing hydro_routing_groundwater_simple)
add_dependencies(hydro_routing hydro_utils)

# Beheen 1/2/2020 - added to compile hydro_routing, not needed when add_exec.
if (WRF_HYDRO_NUDGING STREQUAL "1")
	add_dependencies(hydro_routing hydro_nudging)
endif (WRF_HYDRO_NUDGING STREQUAL "1")
# End Beheen - can be removed when adding executable statements

add_dependencies(hydro_routing_overland hydro_mpp)

##add_dependencies(hydro_routing_groundwater hydro_mpp)
##add_dependencies(hydro_routing_groundwater_bucket hydro_routing_groundwater)
##add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater)
##add_dependencies(hydro_routing_groundwater_simple hydro_routing_groundwater_bucket)
##add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater)
##add_dependencies(hydro_routing_groundwater_nhd hydro_routing_groundwater_bucket)

add_dependencies(hydro_driver hydro_routing)
add_dependencies(hydro_driver hydro_debug_utils)

add_dependencies(hydro_data_rec hydro_routing_overland)
add_dependencies(hydro_data_rec hydro_routing_subsurface)
##add_dependencies(hydro_data_rec hydro_routing_groundwater)
##add_dependencies(hydro_data_rec hydro_routing_groundwater_bucket)
##add_dependencies(hydro_data_rec hydro_routing_groundwater_simple)
##add_dependencies(hydro_data_rec hydro_routing_groundwater_nhd)
add_dependencies(hydro_data_rec hydro_routing_reservoirs)


# Beheen 12/17/19
# get environment variable from command line for NUOPC, if set
if (HYDRO_LSM MATCHES "NoahMP" AND  NUOPCMODE STREQUAL "1" )
	add_definitions(-DNUOPCMODE=1)
	message ("-- Building NoahMP LSM in NUOPC Mode")

	add_subdirectory("Land_models/NoahMP")
	add_subdirectory("Land_models/NoahMP/IO_code")
	add_dependencies(hydro_io_code noahmp_util)
	add_dependencies(hydro_io_code hydro_utils)
	add_dependencies(hydro_io_code hydro_mpp)
	
	add_subdirectory("CPL/NoahMP_cpl")
	add_dependencies(hydro_noahmp_cpl hydro_routing)
	add_dependencies(hydro_noahmp_cpl hydro_mpp )
	add_dependencies(hydro_noahmp_cpl hydro_driver )
	
	
	# set ESMF modules and variables
	set (ESMFMKFILE $ENV{ESMFMKFILE} CACHE STRING "ESMF Makefile Path")
	if (ESMFMKFILE STREQUAL "")
		message(FATAL_ERROR "ESMFMKFILE environment variable is not set!")
	endif (ESMFMKFILE STREQUAL "")
	
	set (ESMFMKFILE_PATH ${ESMFMKFILE})
	string(REPLACE "/lib/esmf.mk" "" ESMF_INSTALL_PREFIX ${ESMFMKFILE_PATH})
	
        #set (ESMF_F90LINKOPTS $ENV{ESMF_F90LINKOPTS} CACHE STRING "ESMF Link Options")
        #set (ESMF_F90LINKPATHS $ENV{ESMF_F90LINKPATHS} CACHE STRING "ESMF Link Path") 
        #set (ESMF_F90LINKRPATHS $ENV{ESMF_F90LINKRPATHS} CACHE STRING "ESMF Linker Path") 
        #set (ESMF_F90ESMFLINKLIBS $ENV{ESMF_F90ESMFLINKLIBS} CACHE STRING "ESMF Link Libs")

        #set (ESMF_F90COMPILER $ENV{ESMF_F90COMPILER} CACHE STRING "ESMF F90 Compiler") 
        #set (ESMF_F90COMPILEOPTS $ENV{ESMF_F90COMPILEOPTS} CACHE STRING "ESMF F90 Compiler Options") 
        #set (ESMF_F90COMPILEPATHS $ENV{ESMF_F90COMPILEPATHS} CACHE STRING "ESMF F90 Compiler Paths") 
        #set (ESMF_F90COMPILEFREECPP $ENV{ESMF_F90COMPILEFREECPP} CACHE STRING "ESMF F90 Compile FREECPP") 
        #set (ESMF_F90COMPILECPPFLAGS $ENV{ESMF_F90COMPILECPPFLAGS} CACHE STRING "ESMF F90 Compile CPP Flags") 
        #add_definitions(-DESMF_VERSION_MAJOR=$ENV{ESMF_VERSION_MAJOR} CACHE STRING "ESMF Version")

	set (ESMF_INSTALL_MODDIR ${ESMF_INSTALL_PREFIX}/mod)
	include_directories(AFTER ${ESMF_INSTALL_MODDIR})
	
	set (BUILD_PREC $ENV{BUILD_PREC} CACHE STRING "NWM NUOPC Build Prec." FORCE)
	set (DEBUG $ENV{DEBUG} CACHE STRING "NWM NUOPC Debug Mode" FORCE)
	
	if ("${BUILD_PREC}" STREQUAL "r4")
		set (${ESMF_F90COMPILECPPFLAGS} -DREAL4)
	elseif ("${BUILD_PREC}" STREQUAL "r8")
		set (${ESMF_F90COMPILECPPFLAGS} -DREAL8)
	else ()
		set (${ESMF_F90COMPILECPPFLAGS} -DREAL4)
	endif ("${BUILD_PREC}" STREQUAL "r4")

	# #################################
	# Compile with Debugging Directives
	# #################################
	if (${DEBUG} STREQUAL "on")
		set (${ESMF_F90COMPILECPPFLAGS} -DDEBUG)
		set (${ESMF_CXXCOMPILECPPFLAGS} -DDEBUG)
	endif (${DEBUG} STREQUAL "on")
	
	
	# Add common include directores needed in the build
	#find_package (ESMF REQUIRED)
	#message ("-- ESMF Include Dir: ${ESMF_APPSDIR}")
	#message ("-- ESMF Include Dir: $ENV{ESMF_NETCDF_INCLUDE}")
	
	add_subdirectory("CPL/NUOPC_cpl")
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_io_code)
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_mpp)
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_data_rec)
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_routing)
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_netcdf_layer)
	add_dependencies(hydro_noahmp_nuopc_cpl hydro_utils)
	
	add_executable(nemsApp_test.exe CPL/NUOPC_cpl/mainApp.F90 CPL/NUOPC_cpl/driver.F90) 
	add_dependencies(nemsApp_test.exe hydro_noahmp_nuopc_cpl)

	target_include_directories(nemsApp_test.exe BEFORE PUBLIC ${ESMF_INSTALL_MODDIR})
	target_include_directories(nemsApp_test.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)
	
	set (ESMF_INSTALL_LIBDIR ${ESMF_INSTALL_PREFIX}/lib)
	target_link_libraries(nemsApp_test.exe ${ESMF_INSTALL_LIBDIR}/libesmf.so)
	
	target_link_libraries(nemsApp_test.exe hydro_noahmp_nuopc_cpl)
	 #$(ESMF_F90LINKER) $(ESMF_F90LINKOPTS) $(ESMF_F90LINKPATHS) $(ESMF_F90LINKRPATHS) -o mainApp $(OBJS) $(MODEL_OBJCS) $(ESMF_F90ESMFLINKLIBS)
         #$(ESMF_F90COMPILER) $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -c NWM_ESMF_Extensions.F90
        #target_link_libraries(nemsApp_test.exe hydro_routing_reservoirs_rfc)
	target_link_libraries(nemsApp_test.exe ${NETCDF_LIBRARIES})
	
	if (WRF_HYDRO_NUDGING STREQUAL "1")
		target_link_libraries(nemsApp_test.exe hydro_nudging)
		add_dependencies(nemsApp_test.exe hydro_nudging)
	endif (WRF_HYDRO_NUDGING STREQUAL "1")
	
	add_custom_command(TARGET nemsApp_test.exe POST_BUILD
		COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
		COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/NoahMP/* ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/Land_models/NoahMP/run/*.TBL ${CMAKE_BINARY_DIR}/Run
	)
	
	# end Beheen

elseif (HYDRO_LSM MATCHES "NoahMP")
	message ("-- Building NoahMP LSM")
	add_subdirectory("Land_models/NoahMP")
	
	add_subdirectory("CPL/NoahMP_cpl")
	add_dependencies(hydro_noahmp_cpl hydro_routing)
	add_dependencies(hydro_noahmp_cpl hydro_mpp )
	add_dependencies(hydro_noahmp_cpl hydro_driver )
	
	
	add_executable(wrfhydro.exe
		Land_models/NoahMP/IO_code/main_hrldas_driver.F
		Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.F
		Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.F
	)

	target_include_directories(wrfhydro.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

	target_link_libraries(wrfhydro.exe hydro_utils)
	target_link_libraries(wrfhydro.exe hydro_mpp)
	target_link_libraries(wrfhydro.exe hydro_debug_utils)
	target_link_libraries(wrfhydro.exe hydro_routing_overland)
	target_link_libraries(wrfhydro.exe hydro_routing_subsurface)
	##target_link_libraries(wrfhydro.exe hydro_routing_groundwater)
	##target_link_libraries(wrfhydro.exe hydro_routing_groundwater_bucket)
	##target_link_libraries(wrfhydro.exe hydro_routing_groundwater_nhd)
	##target_link_libraries(wrfhydro.exe hydro_routing_groundwater_simple)
	target_link_libraries(wrfhydro.exe hydro_data_rec)
	target_link_libraries(wrfhydro.exe hydro_routing)
        target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_levelpool)
        target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_hybrid)
	target_link_libraries(wrfhydro.exe hydro_routing_reservoirs)
	target_link_libraries(wrfhydro.exe hydro_driver)
	target_link_libraries(wrfhydro.exe noahmp_util)
	target_link_libraries(wrfhydro.exe noahmp_phys)
	target_link_libraries(wrfhydro.exe noahmp_data)
	target_link_libraries(wrfhydro.exe hydro_noahmp_cpl)
	target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_rfc)
	target_link_libraries(wrfhydro.exe ${NETCDF_LIBRARIES})
	
	if (WRF_HYDRO_NUDGING STREQUAL "1")
		target_link_libraries(wrfhydro.exe hydro_nudging)
 		add_dependencies(wrfhydro.exe hydro_nudging)
	endif (WRF_HYDRO_NUDGING STREQUAL "1")

	add_custom_command(TARGET wrfhydro.exe POST_BUILD 
		COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
		COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*
		COMMAND cp ${CMAKE_BINARY_DIR}/wrfhydro.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro_NoahMP.exe
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/NoahMP/* ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/Land_models/NoahMP/run/*.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND ln -s wrf_hydro_NoahMP.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
		COMMAND rm ${CMAKE_BINARY_DIR}/wrfhydro.exe
	)	
		

elseif(HYDRO_LSM MATCHES "Noah")
	message ("-- Building Noah LSM")
	add_subdirectory("Land_models/Noah")
	add_subdirectory("CPL/Noah_cpl")

        add_dependencies(hydro_noah_cpl hydro_routing)
        add_dependencies(hydro_noah_cpl hydro_mpp )
        add_dependencies(hydro_noah_cpl hydro_driver )

        add_executable(wrfhydro.exe
                Land_models/Noah/IO_code/module_hrldas_netcdf_io.F
                Land_models/Noah/IO_code/Noah_hrldas_driver.F
        )

        target_include_directories(wrfhydro.exe BEFORE PUBLIC ${PROJECT_BINARY_DIR}/mods)

        target_link_libraries(wrfhydro.exe hydro_utils)
        target_link_libraries(wrfhydro.exe hydro_mpp)
        target_link_libraries(wrfhydro.exe hydro_debug_utils)
        target_link_libraries(wrfhydro.exe hydro_routing_overland)
        target_link_libraries(wrfhydro.exe hydro_routing_subsurface)
        #target_link_libraries(wrfhydro.exe hydro_routing_groundwater)
        #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_bucket)
        #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_nhd)
        #target_link_libraries(wrfhydro.exe hydro_routing_groundwater_simple)
        target_link_libraries(wrfhydro.exe hydro_data_rec)
        target_link_libraries(wrfhydro.exe hydro_routing)
        target_link_libraries(wrfhydro.exe hydro_driver)
        target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_levelpool)
        target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_hybrid)
        target_link_libraries(wrfhydro.exe hydro_routing_reservoirs)        
	target_link_libraries(wrfhydro.exe noah_util)
        target_link_libraries(wrfhydro.exe noah)
        target_link_libraries(wrfhydro.exe hydro_noah_cpl)
	target_link_libraries(wrfhydro.exe hydro_routing_reservoirs_rfc)
        target_link_libraries(wrfhydro.exe ${NETCDF_LIBRARIES})
        
	if (WRF_HYDRO_NUDGING STREQUAL "1")
                target_link_libraries(wrfhydro.exe hydro_nudging)
                add_dependencies(wrfhydro.exe hydro_nudging)
        endif (WRF_HYDRO_NUDGING STREQUAL "1")

        add_custom_command(TARGET wrfhydro.exe POST_BUILD 
		COMMAND mkdir -p ${CMAKE_BINARY_DIR}/Run
		COMMAND rm -f ${CMAKE_BINARY_DIR}/Run/*
		COMMAND cp ${CMAKE_BINARY_DIR}/wrfhydro.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro_Noah.exe
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/Noah/* ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/CHANPARM.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/hydro.namelist ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/template/HYDRO/HYDRO.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND cp ${CMAKE_SOURCE_DIR}/Land_models/Noah/Run/*.TBL ${CMAKE_BINARY_DIR}/Run
		COMMAND ln -s wrf_hydro_Noah.exe ${CMAKE_BINARY_DIR}/Run/wrf_hydro.exe
		COMMAND rm ${CMAKE_BINARY_DIR}/wrfhydro.exe
        )


else()
	message ("Unknown land surface model:" ${HYDRO_LSM} )
endif (HYDRO_LSM MATCHES "NoahMP" AND  NUOPCMODE STREQUAL "1" )
