#simple compilation test for modules in overland routing

# Settings for testing with ifort
#FC=ifort
#FFLAGS=-c -free 

# Setting for testing with gfortran
FC=gfortran
FFLAGS=-c --free-form -std=f2003

FLFLAGS=

.PHONY: all mod test

all: mod test

mod:
	$(FC) $(FFLAGS) module_overland.F
test:
	$(FC) $(FFLAGS) overland_tests.F
	$(FC) -o overland_tests module_overland.o overland_tests.o
clean:
	rm -f module_overland.o module_overland.mod overland_tests.o overland_tests
