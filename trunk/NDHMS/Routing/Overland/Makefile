#simple compilation test for modules in overland routing

FFLAGS=
FLFLAGS=

.PHONY: all mod test

all: mod test

mod:
	gfortran -c -ffree-form -std=f2003 module_overland.F
test:
	gfortran -c -ffree-form -std=f2003 overland_tests.F
	gfortran -o overland_tests module_overland.o overland_tests.o
