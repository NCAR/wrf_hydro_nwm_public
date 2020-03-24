cmake_minimum_required (VERSION 2.8)

# rm -rf *; cmake -DNUOPCMODE=1 -Wno-dev ..    must source setEnvar.sh and source esmf-impi-env.sh

add_library(hydro_noahmp_nuopc_cpl STATIC
	NWM_ESMF_Extensions.F90  
	NWM_NUOPC_Cap.F90
	NWM_NUOPC_Gluecode.F90
)

target_link_libraries(hydro_noahmp_nuopc_cpl PUBLIC hydro_io_code)
target_link_libraries(hydro_noahmp_nuopc_cpl PUBLIC hydro_mpp)
