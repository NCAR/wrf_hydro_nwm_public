# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	Noah_hrldas_driver.o \
	module_hrldas_netcdf_io.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_

all:	$(OBJS)

Noah_hrldas_driver.o: Noah_hrldas_driver.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG). \
	$(MODFLAG)../Noah $(MODFLAG)../Utility_routines -I../../../MPP $(NETCDFMOD) $(*).F
	@echo ""

module_hrldas_netcdf_io.o: module_hrldas_netcdf_io.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS)  -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG)../Utility_routines $(NETCDFMOD)  $(*).F
	@echo ""

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(*).F
	@echo ""



#
# Dependencies:
#

Noah_hrldas_driver.o:	module_hrldas_netcdf_io.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.stb *~
