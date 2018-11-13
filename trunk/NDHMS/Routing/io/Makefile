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
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) $<

.PHONY: all mod

all: mod

mod:
	#Build each sub module then build the module that depends on all sub modules
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_io_utilities.F

	ar -r ../../lib/libHYDRO.a module_io_utilities.o

	cp *.mod ../../mod


clean:
	rm -f *.o
	rm -f *.mod
