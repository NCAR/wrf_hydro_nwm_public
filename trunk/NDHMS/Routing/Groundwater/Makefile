#simple compilation test for modules in overland routing

include ../../macros

# Settings for testing with ifort
FC=ifort
FFLAGS=-c -free -O3

# Setting for testing with gfortran
#FC=gfortran
#FFLAGS=-c --free-form -std=f2003 -O3
#F90FLAGS :=-std=f2008 $(F90FLAGS)

MODFLAG := $(MODFLAG) -I ../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_UDMAP.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_groundwater_base.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_properties.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_basin.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_groundwater_static_data.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_tests.F
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_data.F
	ar -r ../../lib/libHYDRO.a module_UDMAP.o
	ar -r ../../lib/libHYDRO.a module_groundwater_base.o
	ar -r ../../lib/libHYDRO.a module_nhd_properties.o
	ar -r ../../lib/libHYDRO.a module_nhd_state.o
	ar -r ../../lib/libHYDRO.a module_nhd_basin.o
	ar -r ../../lib/libHYDRO.a module_groundwater_static_data.o
	ar -r ../../lib/libHYDRO.a module_nhd_tests.o
	ar -r ../../lib/libHYDRO.a module_nhd_data.o
	
	cp *.mod ../../mod
test:
	$(COMPILER90) $(F90FLAGS) $(MODFLAG)  groundwater_tests.F
	$(COMPILER90) $(F90FLAGS) $(MODFLAG)  hydro_stop.F
	$(COMPILER90) $(NETCDFLIB) -o groundwater_tests \
		module_groundwater_base.o \
		module_nhd_properties.o \
		module_nhd_state.o \
		module_nhd_basin.o \
		module_groundwater_static_data.o \
		module_nhd_data.o \
		module_UDMAP.o \
		module_nhd_tests.o \
		../../MPP/mpp_land.o \
		../../MPP/CPL_WRF.o \
		../../MPP/module_mpp_ReachLS.o\
		groundwater_tests.o \
		hydro_stop.o
clean:
	rm -f *.o
	rm -f *.mod
	rm -f groundwater_tests
