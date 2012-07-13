# Makefile 
#
#INCLUDES=
.SUFFIXES:
.SUFFIXES: .o .f

include ../macros

NOAHBETA_OBJS = 

#MAINOBJ2 = ../IO_code/Noah_driver_rtfdda.o
MAINOBJ3 = ../IO_code/Noah_driver_hrldas.o ../IO_code/module_Noahlsm_hrldas_input.o

OBJS = \
	../Noah_code/module_Noahlsm.o \
	../Noah_code/module_Noahlsm_param_init_rt.o \
	../Noah_code/module_Noahlsm_utility_rt.o \
	../Noah_code/module_date_utilities_rt.o

WRFCODE_OBJS = \
	../IO_code/Noah_driver_wrfcode.o \
	../IO_code/module_Noahlsm_wrf_input_rt.o \
	../IO_code/module_Noah_distr_routing.o \
	../IO_code/module_Noah_channel_routing.o \
	../IO_code/module_Noah_chan_param_init_rt.o	\
	../IO_code/module_RTLAND.o		\
	../WRF_code/module_sf_noahlsm_rt.o \
	../Noah_code/module_Noahlsm_utility_rt.o \
	../Noah_code/module_date_utilities_rt.o \
	../IO_code/module_Noah_gw_baseflow.o \
	../Noah_code/module_Noahlsm_param_init.o \

#	../IO_code/module_Noahlsm_rtfdda_input.o \
#	../IO_code/module_Noahlsm_gridded_input.o \

#CMD = Noah_beta Noah_rtfdda_beta Noah_wrfdriver_beta
CMD = Noah_wrfdriver_beta 
all:	$(CMD)

Noah_beta: $(OBJS) $(NOAHBETA_OBJS)
	@echo ""
	$(COMPILER90) -o $(@) -I../Noah_code -I../IO_code $(OBJS) $(NOAHBETA_OBJS)
	@echo ""

Noah_rtfdda_beta: $(OBJS) $(MAINOBJ2)
	@echo ""
	$(COMPILER90) -o $(@) -I../Noah_code -I../IO_code $(OBJS) $(MAINOBJ2)
	@echo ""

Noah_hrldas_beta: $(OBJS) $(MAINOBJ3)
	@echo ""
	$(COMPILER90) -o $(@) -I../Noah_code -I../IO_code $(OBJS) $(MAINOBJ3) $(NETCDFLIB)
	@echo ""

Noah_wrfdriver_beta: $(WRFCODE_OBJS)
	@echo ""
	$(COMPILER90) -o $(@) -I../IO_code $(WRFCODE_OBJS) ../Noah_code/module_model_constants_rt.o $(NETCDFLIB)
	@echo ""

#$(MODFLAG)../Noah_code 
#

# This command cleans up
clean:
	rm -f *~ $(CMD) Noah_wrfdriver_beta_mpp librtland.a
