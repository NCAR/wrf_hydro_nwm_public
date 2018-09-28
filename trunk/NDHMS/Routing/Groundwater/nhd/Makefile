#simple compilation test for modules in overland routing

include ../../../macros

# Settings for testing with ifort
FC=ifort
FFLAGS=-c -free -O3

# Setting for testing with gfortran
#FC=gfortran
#FFLAGS=-c --free-form -std=f2003 -O3
#F90FLAGS :=-std=f2008 $(F90FLAGS)

MODFLAG := -I ../../../MPP -I ../../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_properties.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_basin.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_tests.F
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_nhd_data.F
	#ar -r ../../../lib/libHYDRO.a module_nhd_properties.o
	#ar -r ../../../lib/libHYDRO.a module_nhd_state.o
	ar -r ../../../lib/libHYDRO.a module_nhd_basin.o
	ar -r ../../../lib/libHYDRO.a module_nhd_tests.o
	ar -r ../../../lib/libHYDRO.a module_nhd_data.o
	
	cp *.mod ../../../mod
clean:
	rm -f *.o
	rm -f *.mod
	rm -f groundwater_tests
