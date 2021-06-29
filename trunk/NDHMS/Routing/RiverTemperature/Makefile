
#FC=gfortran
#FC=ifort

include ../../macros

MODFLAG := -I ../../MPP -I ../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_channel_routing_local.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_lsm_forcing_local.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_sf_noahmpdrv_local.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) weather_disaggrgate.F
  $(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_river_temperature.F
	ar -r ../../lib/libHYDRO.a module_channel_routing_local.o
	ar -r ../../lib/libHYDRO.a module_lsm_forcing_local.o
	ar -r ../../lib/libHYDRO.a module_sf_noahmpdrv_local.o
	ar -r ../../lib/libHYDRO.a weather_disaggrgate.o
	ar -r ../../lib/libHYDRO.a module_river_temperature.o

	cp *.mod ../../mod

clean:
	rm -f *.o
	rm -f *.mod
