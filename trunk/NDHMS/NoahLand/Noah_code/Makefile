# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F
include ../macros

OBJS = \
	module_model_constants_rt.o \
	module_date_utilities_rt.o \
	module_Noahlsm_param_init_rt.o \
	module_Noahlsm_utility_rt.o

all:	$(OBJS)

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(RMD) $(*).f
	@echo ""

#
# Dependencies:
#

module_Noahlsm.o:	module_Noahlsm_param_init_rt.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

