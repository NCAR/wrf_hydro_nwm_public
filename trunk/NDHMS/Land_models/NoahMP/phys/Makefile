# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
	module_sf_noahmp_groundwater.o\
	modd_csts.o\
	mode_surf_coefs.o\
	modd_snow_metamo.o\
	tridiag_ground_snowcro.o\
	mode_snow3l.o\
	modd_snow_par.o\
	ini_csts.o\
	module_snowcro.o\
	mode_thermos.o\
	modd_surf_atm.o


CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c -I../Utility_routines $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $(*).F
	@echo ""

#
# Dependencies:
#
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o module_snowcro.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o
mode_snow3l.o: modd_snow_par.o 
module_snowcro.o: tridiag_ground_snowcro.o mode_thermos.o mode_surf_coefs.o modd_snow_par.o modd_snow_metamo.o mode_snow3l.o 
mode_thermos.o: modd_csts.o modd_snow_par.o 
mode_surf_coefs.o: modd_surf_atm.o 

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

