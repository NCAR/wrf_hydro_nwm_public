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
	#Build the base files for all modules
	$(COMPILER90) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) module_hydraulic_base.F
	#Copy the object files into libHydro.a
	ar -r ../../../lib/libHYDRO.a module_hydrualic_base.o

	# copy the mod files
	cp *.mod  ../../../mod

	#build sub modules
	make -C diffusive_wave
	make -C dynamic_wave

clean:
	rm -f *.o
	rm -f *.mod

	make -C diffusive_wave clean
	make -C dynamic_wave clean


