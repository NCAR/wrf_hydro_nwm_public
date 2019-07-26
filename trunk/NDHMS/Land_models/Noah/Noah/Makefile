# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_sf_noahlsm.o \
	module_sf_urban.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(MODFLAG). $(MODFLAG)../Utility_routines $(F90FLAGS) $(FREESOURCE) $(*).F
	@echo ""

#
# Dependencies:
#
module_sf_noahlsm.o:	module_sf_urban.o

#
# This command cleans up object (etc) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

