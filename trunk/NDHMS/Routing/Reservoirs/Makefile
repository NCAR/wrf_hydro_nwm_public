
include ../../macros

MODFLAG := $(MODFLAG) -I ../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_utilities.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_base.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_read_timeslice_data.F

	ar -r ../../lib/libHYDRO.a module_reservoir_utilities.o
	ar -r ../../lib/libHYDRO.a module_reservoir_base.o
	ar -r ../../lib/libHYDRO.a module_read_timeslice_data.o

	cp *.mod ../../mod

	#Build the modules
	make -C Level_Pool
	make -C Persistence_Level_Pool_Hybrid


test:
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) hydro_stop.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_utilities.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_base.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_read_timeslice_data.F

	make -C Level_Pool
	make -C Persistence_Level_Pool_Hybrid

	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) reservoir_tests.F


	$(COMPILER90) $(NETCDFLIB) -o reservoir_tests \
		hydro_stop.o \
		module_reservoir_utilities.o \
		module_reservoir_base.o \
		module_read_timeslice_data.o \
		Level_Pool/module_levelpool_parameters.o \
		Level_Pool/module_levelpool_state.o \
		Level_Pool/module_levelpool_tests.o \
		Level_Pool/module_levelpool.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_parameters.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_state.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid_tests.o \
		Persistence_Level_Pool_Hybrid/module_persistence_levelpool_hybrid.o \
		reservoir_tests.o


clean:
	rm -f *.o
	rm -f *.mod
	rm -f reservoir_tests

	make -C Level_Pool clean
	make -C Persistence_Level_Pool_Hybrid clean
