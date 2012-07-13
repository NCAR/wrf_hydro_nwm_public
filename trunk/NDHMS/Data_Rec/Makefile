# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_namelist.o \
	module_LSM_wrf_data.o \
	module_RT_data.o \
	module_GW_baseflow_data.o

all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I../mod $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libNDHMS.a $(@)
	cp *.mod ../mod

# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
