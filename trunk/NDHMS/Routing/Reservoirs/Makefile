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
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_utilities.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_base.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_io.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_parameters.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_state.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool_tests.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_levelpool.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_parameters.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_state.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_model.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning_tests.F
	#$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_machine_learning.F


	ar -r ../../lib/libHYDRO.a module_reservoir_utilities.o
	ar -r ../../lib/libHYDRO.a module_reservoir_base.o
	#ar -r ../../lib/libHYDRO.a module_reservoir_io.o
	#ar -r ../../lib/libHYDRO.a module_levelpool_parameters.o
	#ar -r ../../lib/libHYDRO.a module_levelpool_state.o
	#ar -r ../../lib/libHYDRO.a module_levelpool_tests.o
	#ar -r ../../lib/libHYDRO.a module_levelpool.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning_parameters.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning_state.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning_model.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning_tests.o
	#ar -r ../../lib/libHYDRO.a module_machine_learning.o

	cp *.mod ../../mod

	#Build the modules
	make -C Level_Pool
	make -C Machine_Learning
	make -C io




test: mod
	$(COMPILER90) $(F90FLAGS) $(MODFLAG) reservoir_tests.F
	$(COMPILER90) $(F90FLAGS) $(MODFLAG) hydro_stop.F
	$(COMPILER90) $(NETCDFLIB) -o reservoir_tests \
		module_reservoir_utilities.o \
		module_reservoir_base.o \
		Level_Pool/module_levelpool_parameters.o \
		Level_Pool/module_levelpool_state.o \
		Level_Pool/module_levelpool_tests.o \
		Level_Pool/module_levelpool.o \
		Machine_Learning/module_machine_learning_parameters.o \
		Machine_Learning/module_machine_learning_state.o \
		Machine_Learning/module_machine_learning_model.o \
		Machine_Learning/module_machine_learning_tests.o \
		Machine_Learning/module_machine_learning.o \
		io/module_reservoir_io.o \
		reservoir_tests.o \
		hydro_stop.o



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
	rm -f reservoir_tests

	make -C Level_Pool clean
	make -C Machine_Learning clean
	make -C io clean
