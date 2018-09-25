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
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_bucket_properties.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_bucket_state.F
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_bucket_impl.F
	ar -r ../../../lib/libHYDRO.a module_bucket_properties.o
	ar -r ../../../lib/libHYDRO.a module_bucket_state.o
	ar -r ../../../lib/libHYDRO.a module_bucket_impl.o
	
	cp *.mod ../../../mod
clean:
	rm -f *.o
	rm -f *.mod
	rm -f groundwater_tests
