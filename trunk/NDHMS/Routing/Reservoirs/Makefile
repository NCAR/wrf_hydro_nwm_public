#simple compilation test for modules in reservoir

include ../../macros

# Settings for testing with ifort
FC=ifort
FFLAGS=-c -free -O3


# Setting for testing with gfortran
#FC=gfortran
#FFLAGS=-c --free-form -std=f2003 -O3
#F90FLAGS :=-std=f2008 $(F90FLAGS)

MODFLAG := $(MODFLAG) -I ../../mod

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_parameters.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_base.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_input.F
	#ar -r ../../lib/libHYDRO.a module_levelpool.o
	#ar -r ../../lib/libHYDRO.a module_levelpool_parameters.o
	ar -r ../../lib/libHYDRO.a module_reservoir_base.o
	ar -r ../../lib/libHYDRO.a module_reservoir_input.o

	cp *.mod ../../mod


#test:
#     	$(COMPILER90) $(F90FLAGS) $(MODFLAG) reservoir_tests.F
#    	$(COMPILER90) -o reservoir_tests \
#            module_levelpool.o \
#            module_levelpool_parameters.o \
#            module_reservoir_base.o \
#            reservoir_tests.o



clean:
	rm -f *.o
	rm -f *.mod
#	rm -f reservoir_tests
