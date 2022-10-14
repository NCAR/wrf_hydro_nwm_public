# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../../../macros

OBJS = \
	state.o \
	forcing.o \
	parameters.o \
	geometry.o

all:	$(OBJS)

#module_RT.o: module_RT.F
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f
#	$(RMD) $(*).f
#	@echo ""
#	cp *.mod ../mod

.f90.o:
	@echo "Data structures Makefile:"
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(COMPILER90) $(CPPINVOKE) -o $(@) $(FPPFLAGS) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) -I$(NETCDFINC) -I../../../OrchestratorLayer $(*).f90
#	$(RMD) $(*).f
	@echo ""
	ar -r ../../../lib/libHYDRO.a $(@)
	cp *.mod ../../../mod

#
# Dependencies:
#
# io_manager.o: ../IO/netcdf_layer.o

# orchestrator.o: ig.o

clean:	
	rm -f *.o *.mod *.stb *~ *.f
