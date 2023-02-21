# Makefile
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS =  hashtable.o CPL_WRF.o mpp_land.o module_mpp_ReachLS.o module_mpp_GWBUCKET.o

all:	$(OBJS)

hashtable.o: hashtable.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $<
	cp hashtable.mod ../mod
	ar -cr ../lib/libHYDRO.a $(@)

CPL_WRF.o: CPL_WRF.F
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(LDFLAGS) $(MODFLAG) $<

	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $<
	ar -cr ../lib/libHYDRO.a $(@)

mpp_land.o: mpp_land.F CPL_WRF.o
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $<
	ar -cr ../lib/libHYDRO.a $(@)

module_mpp_ReachLS.o: module_mpp_ReachLS.F mpp_land.o
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $<
	ar -cr ../lib/libHYDRO.a $(@)

module_mpp_GWBUCKET.o: module_mpp_GWBUCKET.F mpp_land.o CPL_WRF.o
	@echo ""
	$(RMD) $(*).o $(*).mod $(*).stb *~
	$(COMPILER90) $(F90FLAGS) $(LDFLAGS) -c $<
	ar -cr ../lib/libHYDRO.a $(@)

clean:
	$(RMD) *.o *.mod *.stb *~
