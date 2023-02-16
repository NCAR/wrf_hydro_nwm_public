#
# Makefile
#

include ../../user_build_options
SRCS := $(wildcard *.F)
OBJS := $(SRCS:%.F=%.o)
CPPHRLDAS = -D_HRLDAS_OFFLINE_

all: $(OBJS)

%.o:%.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $@ -c -I../Utility_routines \
	   $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $<
	@echo ""

#
# Dependencies:
#
ini_csts.o: modd_csts.o
mode_snow3l.o: modd_snow_par.o modd_csts.o modd_snow_metamo.o
mode_thermos.o: modd_csts.o modd_snow_par.o
mode_surf_coefs.o: modd_surf_atm.o mode_thermos.o
module_snowcro.o: mode_snow3l.o modd_snow_par.o modd_csts.o modd_snow_metamo.o modd_surf_atm.o \
	mode_thermos.o mode_surf_coefs.o tridiag_ground_snowcro.o
module_snowcro_intercept.o:

#
# This command cleans up object (etc) files:
#
clean:
	$(RM) *.o *.mod *.stb *~
