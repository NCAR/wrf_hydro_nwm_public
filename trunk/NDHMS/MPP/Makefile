# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

COMPILERCC = icpc -O3

OBJS =  CPL_WRF.o mpp_land.o module_mpp_ReachLS.o module_mpp_GWBUCKET.o

all:	$(OBJS)
mpp_land.o: mpp_land.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)

CPL_WRF.o: CPL_WRF.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) $(*).F
        
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)

module_mpp_ReachLS.o: module_mpp_ReachLS.F hash_routines.cc
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILERCC) -O3 -c hash_routines.cc
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)

module_mpp_GWBUCKET.o: module_mpp_GWBUCKET.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $(*).F
	ar -r ../lib/libHYDRO.a $(@)

clean:
	$(RMD) *.o *.mod *.stb *~
