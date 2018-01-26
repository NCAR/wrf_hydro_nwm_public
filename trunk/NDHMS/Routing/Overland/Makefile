#simple compilation test for modules in overland routing

# Settings for testing with ifort
#FC=ifort
#FFLAGS=-c -free 

# Setting for testing with gfortran
FC=gfortran
FFLAGS=-c --free-form -std=f2003 -O3

FLFLAGS=

.PHONY: all mod test

all: mod test

mod:
	$(FC) $(FFLAGS) module_overland_control.F
	$(FC) $(FFLAGS) module_overland_streams_and_lakes.F
	$(FC) $(FFLAGS) module_overland_routing_properties.F
	$(FC) $(FFLAGS) module_overland_mass_balance.F
	$(FC) $(FFLAGS) module_overland.F
test:
	$(FC) $(FFLAGS) overland_tests.F
	$(FC) -o overland_tests \
		module_overland_control.o \
		module_overland_streams_and_lakes.o \
		module_overland_routing_properties.o \
		module_overland_mass_balance.o \
		module_overland.o \
		overland_tests.o
clean:
	rm -f *.o 
	rm -f *.mod
	rm -f overland_tests
