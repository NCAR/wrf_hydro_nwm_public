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

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod test

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_base.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_parameters.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_state.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_tests.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_parameters.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_model.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_read_q_type.F


	ar -r ../../lib/libHYDRO.a module_reservoir_base.o
	ar -r ../../lib/libHYDRO.a module_levelpool_parameters.o
	ar -r ../../lib/libHYDRO.a module_levelpool_state.o
	#ar -r ../../lib/libHYDRO.a module_levelpool_tests.o
	ar -r ../../lib/libHYDRO.a module_levelpool.o
	ar -r ../../lib/libHYDRO.a module_machine_learning_parameters.o
	ar -r ../../lib/libHYDRO.a module_machine_learning_state.o
	ar -r ../../lib/libHYDRO.a module_machine_learning_model.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning.o
	#ar -r ../../lib/libHYDRO.a module_read_q_type.o

	cp *.mod ../../mod

#test:
#	$(COMPILER90) $(F90FLAGS) $(MODFLAG) reservoir_tests.F
#	$(COMPILER90) $(F90FLAGS) $(MODFLAG) hydro_stop.F
#	$(COMPILER90) $(NETCDFLIB) -o reservoir_tests \
#		module_reservoir_base.o \
#		module_levelpool_parameters.o \
#		module_levelpool_state.o \
#		module_levelpool.o \
#		module_read_q_type.o \
#		module_levelpool_tests.o \
#		reservoir_tests.o \
#		hydro_stop.o



#     	$(COMPILER90) $(F90FLAGS) $(MODFLAG) module_reservoir_base.F
#		$(COMPILER90) $(NETCDFLIB) -o reservoir_tests \
#				module_reservoir_base.o \
#				module_levelpool_parameters.o \
#				module_levelpool_state.o \
#				module_levelpool.o
#				#module_levelpool_tests.o \
#				#reservoir_tests.o



#test:
#	$(COMPILER90) $(F90FLAGS) $(MODFLAG) module_reservoir_base.F
#	$(COMPILER90) $(NETCDFLIB) -o module_reservoir_base.o


clean:
	rm -f *.o
	rm -f *.mod
	#rm -f reservoir_tests
