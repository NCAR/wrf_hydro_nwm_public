# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS_NoahMP = module_NoahMP_hrldas_driver.o

OBJS = \
	main_hrldas_driver.o \
	module_hrldas_netcdf_io.o

CPPHRLDAS = -D_HRLDAS_OFFLINE_ $(MOD_OPT)

all:	$(OBJS_NoahMP) $(OBJS)

NoahMP : $(OBJS_NoahMP) $(OBJS)

module_NoahMP_hrldas_driver.o: module_NoahMP_hrldas_driver.F ../../../HYDRO_drv/module_HYDRO_drv.o  ../../../Data_Rec/module_namelist.o ../../../Data_Rec/module_RT_data.o ../../../CPL/NoahMP_cpl/module_hrldas_HYDRO.o
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(MODFLAG) -I. \
	-I../phys -I../Utility_routines -I../../../mod $(NETCDFMOD) $(*).F
	@echo ""

main_hrldas_driver.o: main_hrldas_driver.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) -I ../MPP -I. \
	-I../phys -I../Utility_routines -I../../../mod -I../../../MPP $(NETCDFMOD) $(*).F
#	$(COMPILERF90) -o $(@) -c $(F90FLAGS) $(FREESOURCE) -I ../MPP -I. \
#	-I../phys -I../Utility_routines $(NETCDFMOD) $(*).f90
	@echo ""

module_hrldas_netcdf_io.o: module_hrldas_netcdf_io.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(FREESOURCE) -I ../MPP -I../Utility_routines $(MODFLAG) $(NETCDFMOD) $(*).F
	@echo ""

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(FREESOURCE) $(*).F
	@echo ""

#
# Dependencies:
#

main_hrldas_driver.o:	$(OBJS_NoahMP) \
			../../../MPP/mpp_land.o \
                        ../../../Routing/module_NWM_io.o
$(OBJS_NoahMP):	module_hrldas_netcdf_io.o

# This command cleans up object files, etc.
clean:
	$(RM) *.o *.mod *.stb *~
