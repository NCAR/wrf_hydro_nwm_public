
include ../../../macros

MODFLAG := -I ../../../MPP -I ../../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_rfc_forecasts_properties.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_rfc_forecasts_state.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_rfc_forecasts.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_rfc_forecasts_tests.F
	ar -cr ../../../lib/libHYDRO.a module_rfc_forecasts_properties.o \
	                               module_rfc_forecasts_state.o \
	                               module_rfc_forecasts.o \
	                               module_rfc_forecasts_tests.o

	@cp *.mod ../../../mod

clean:
	rm -f *.o
	rm -f *.mod
