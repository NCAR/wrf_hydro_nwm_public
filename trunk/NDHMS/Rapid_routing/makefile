#*******************************************************************************
#If want to use RAPID without TAO, in which case the optimization is unavailable
#*******************************************************************************
FPPFLAGS=
include ${TAO_DIR}/bmake/tao_common

#FPPFLAGS=-D NO_TAO
#include ${PETSC_DIR}/conf/variables
#include ${PETSC_DIR}/conf/rules
#include ${PETSC_DIR}/conf/test


#*******************************************************************************
#Location of netCDF include and lib directories
#*******************************************************************************
NETCDF_LIB=-L ${TACC_NETCDF_LIB} -lnetcdf
NETCDF_INCLUDE=-I ${TACC_NETCDF_INC}
PHDF5_INC=-I ${TACC_HDF5_INC}

#*******************************************************************************
#makefile instructions 
#*******************************************************************************
rapid:	\
	rapid_main.o \
	rapid_init.o \
	rapid_read_namelist.o \
	rapid_create_obj.o \
	rapid_net_mat.o \
	rapid_obs_mat.o \
	rapid_routing.o \
	rapid_routing_param.o \
	rapid_phiroutine.o \
	rapid_destro_obj.o \
	rapid_final.o \
	rapid_var.o \
	hrldas_RAPID_drv.o
	-${FLINKER} ${FPPFLAGS} -o \
	rapid \
	hrldas_RAPID_drv.o \
	rapid_main.o \
	rapid_init.o \
	rapid_read_namelist.o \
	rapid_create_obj.o \
	rapid_net_mat.o \
	rapid_routing.o \
	rapid_routing_param.o \
	rapid_obs_mat.o \
	rapid_phiroutine.o \
	rapid_destro_obj.o \
	rapid_final.o \
	rapid_var.o \
	${TAO_FORTRAN_LIB} ${TAO_LIB} ${PETSC_LIB} ${NETCDF_LIB} ${PHDF5_INC} \
        -Wl,-rpath,${TACC_HDF5_LIB} -L${TACC_HDF5_LIB} -lhdf5 -lz

dummy: 
	echo ${FLINKER} ${FPPFLAGS}

rapid_main.o:	rapid_main.F90 rapid_var.o 
	-${FLINKER} ${FPPFLAGS} -c rapid_main.F90 ${PETSC_INCLUDE} ${TAO_INCLUDE} ${NETCDF_INCLUDE}

rapid_final.o:		rapid_final.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_final.F90 ${PETSC_INCLUDE}

rapid_destro_obj.o: 	rapid_destro_obj.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_destro_obj.F90 ${PETSC_INCLUDE} ${TAO_INCLUDE}

rapid_phiroutine.o: 	rapid_phiroutine.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_phiroutine.F90 ${PETSC_INCLUDE} ${TAO_INCLUDE} ${NETCDF_INCLUDE}

rapid_routing.o: 	rapid_routing.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_routing.F90 ${PETSC_INCLUDE} ${NETCDF_INCLUDE}

rapid_init.o: 		rapid_read_namelist.o rapid_net_mat.o rapid_obs_mat.o \
                        rapid_routing_param.o rapid_create_obj.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_init.F90 ${PETSC_INCLUDE}

rapid_routing_param.o: 	rapid_routing_param.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_routing_param.F90 ${PETSC_INCLUDE}

rapid_obs_mat.o: 	rapid_obs_mat.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_obs_mat.F90 ${PETSC_INCLUDE}

rapid_net_mat.o: 	rapid_net_mat.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_net_mat.F90 ${PETSC_INCLUDE}

rapid_create_obj.o: 	rapid_create_obj.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_create_obj.F90 ${PETSC_INCLUDE} ${TAO_INCLUDE}

rapid_read_namelist.o:	rapid_read_namelist.F90 rapid_var.o
	-${FLINKER} ${FPPFLAGS} -c rapid_read_namelist.F90
	
rapid_var.o:	rapid_var.F90
	-${FLINKER} ${FPPFLAGS} -c rapid_var.F90 ${PETSC_INCLUDE} ${TAO_INCLUDE} 

hrldas_RAPID_drv.o:     hrldas_RAPID_drv.F90 rapid_main.o
	-${FLINKER} ${FPPFLAGS} -c hrldas_RAPID_drv.F90  ${PETSC_INCLUDE} ${TAO_INCLUDE}
	
clean::
	rm -f *.o *.mod rapid

