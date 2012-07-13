# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
        module_Noah_chan_param_init_rt.o \
	module_RTLAND.o \
	module_Noahlsm_wrf_input_rt.o \
	module_Noah_distr_routing.o \
	module_Noah_gw_baseflow.o \
	module_Noah_channel_routing.o 
#	module_Noah_grads_output.o \
#	read_IHOP.o
#	module_Noahlsm_rtfdda_input.o
#	module_Noahlsm_gridded_input.o

all:	$(OBJS)

Noah_driver_wrfcode.o: Noah_driver_wrfcode.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../WRF_code -I ../Noah_code $(*).f
	$(RMD) $(*).f
	@echo ""

Noah_driver.o: Noah_driver.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

Noah_driver_hrldas.o: Noah_driver_hrldas.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_Noahlsm_hrldas_input.o: module_Noahlsm_hrldas_input.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_RTLAND.o: module_RTLAND.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../WRF_code -I ../Noah_code -I ../mod $(*).f
	$(RMD) $(*).f
	@echo ""

drive_rtland.o: drive_rtland.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../WRF_code -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_Noahlsm_wrf_input_rt.o: module_Noahlsm_wrf_input_rt.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) -I ../WRF_code $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""
	cp module_sf_noahlsm_wrf_input_rt.mod ../mod

module_Noah_distr_routing.o: module_Noah_distr_routing.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_Noah_gw_baseflow.o: module_Noah_gw_baseflow.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_Noah_channel_routing.o: module_Noah_channel_routing.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

module_Noah_chan_param_init_rt.o: module_Noah_chan_param_init_rt.F
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../Noah_code  $(*).f
	$(RMD) $(*).f
	@echo ""

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(*).f
	$(RMD) $(*).f
	@echo ""

#
# Dependencies:
#

Noah_driver_hrldas.o:	module_Noahlsm_hrldas_input.o
module_RTLAND.o:	module_Noahlsm_wrf_input_rt.o module_Noah_distr_routing.o module_Noah_channel_routing.o module_Noah_gw_baseflow.o
drive_rtland.o:	module_Noahlsm_wrf_input_rt.o module_Noah_distr_routing.o module_Noah_channel_routing.o module_Noah_gw_baseflow.o module_Noah_chan_param_init_rt.o module_RTLAND.o

Noah_driver.o:	module_Noah_grads_output.o
Noah_driver.o:	read_IHOP.o
Noah_driver.o:	../Noah_code/module_Noahlsm_param_init_rt.o
Noah_driver.o:	../Noah_code/module_Noahlsm_utility_rt.o
Noah_driver.o:	../Noah_code/module_date_utilities_rt.o

#Noah_driver_rtfdda.o:	../Noah_code/module_Noahlsm_param_init_rt.o
#Noah_driver_rtfdda.o:	../Noah_code/module_Noahlsm_utility_rt.o
#Noah_driver_rtfdda.o:	../Noah_code/module_date_utilities_rt.o
#Noah_driver_rtfdda.o:	module_Noahlsm_rtfdda_input.o
Noah_driver_wrfcode.o:        module_Noahlsm_wrf_input_rt.o module_Noah_distr_routing.o module_Noah_channel_routing.o module_Noah_chan_param_init_rt.o module_RTLAND.o

# This command cleans up object files
clean:
	$(RM) *.o *.mod *.stb *~
