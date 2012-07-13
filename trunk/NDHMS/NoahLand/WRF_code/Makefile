# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F
include ../macros

#	module_date_utilities.o \
#	module_Noahlsm_hrldas_input.o \
#	module_Noahlsm_utility.o \
#	Noah_driver_hrldas.o
OBJS = \
	module_sf_noahlsm_rt.o
#	module_model_constants.o \

all:	$(OBJS)

# noah:	$(OBJS)
# 	$(COMPILER90) -o $(@) $(OBJS) $(NETCDFLIB)

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -D_HRLDAS_OFFLINE_ -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@)  $(MODFLAG) -I ../Noah_code $(F90FLAGS) $(*).f
	$(RMD) $(*).f
	@echo ""

#
# Dependencies:
#

# module_Noahlsm.o:	module_Noahlsm_param_init.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

