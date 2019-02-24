
include ../../../macros

MODFLAG := -I ../../../MPP -I ../../../mod

%.o : %.F
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod test

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_reservoir_io.F

	ar -r ../../../lib/libHYDRO.a module_reservoir_io.o

	cp *.mod ../../../mod

clean:
	rm -f *.o
	rm -f *.mod

