
#FC=gfortran
#FC=ifort

include ../../macros

MODFLAG := -I ../../MPP -I ../../mod  -I ../../Land_models/NoahMP/IO_code/

OBJS := weather_disaggrgate.o module_river_temperature.o
CPPHRLDAS = -D_HRLDAS_OFFLINE_

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: $(OBJS) # mod

# type: rivertemp_type.o
# physics: $(OBJS)

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) weather_disaggrgate.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_river_temperature.F

	ar -r ../../lib/libHYDRO.a weather_disaggrgate.o
	ar -r ../../lib/libHYDRO.a module_river_temperature.o

	cp *.mod ../../mod

clean:
	rm -f *.o
	rm -f *.mod
