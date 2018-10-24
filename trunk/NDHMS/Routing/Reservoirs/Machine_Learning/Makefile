#simple compilation test for modules in machine learning

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
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_parameters.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_model.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_tests.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_utilities.F
	ar -r ../../../lib/libHYDRO.a module_machine_learning_parameters.o
	ar -r ../../../lib/libHYDRO.a module_machine_learning_state.o
	ar -r ../../../lib/libHYDRO.a module_machine_learning_model.o
	ar -r ../../../lib/libHYDRO.a module_machine_learning_tests.o
	ar -r ../../../lib/libHYDRO.a module_machine_learning.o
	#ar -r ../../../lib/libHYDRO.a module_machine_learning_utilities.o

	cp *.mod ../../../mod
clean:
	rm -f *.o
	rm -f *.mod
	rm -f reservoir_tests
