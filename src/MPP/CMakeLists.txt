add_library(hydro_mpp STATIC
	CPL_WRF.F
	module_mpp_GWBUCKET.F
	module_mpp_ReachLS.F
	mpp_land.F
	hashtable.F
)

target_link_libraries(hydro_mpp ${MPI_Fortran_LIBRARIES})