# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	module_NDHMS_utils.o \
	module_diag_field.o \
	module_noah_chan_param_init_rt.o \
	module_GW_baseflow.o \
 	module_RT.o Noah_distr_routing.o \
	module_channel_routing.o \
	rtFunction.o restart_io.o RT_IO.o module_ndhms_io.o

all:	$(OBJS)

#module_RT.o: module_RT.F
#	@echo ""
#	$(CPP) $(CPPFLAGS) $(*).F > $(*).f
#	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG)  $(*).f
#	$(RMD) $(*).f
#	@echo ""
#	cp *.mod ../mod

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libNDHMS.a $(@)
	cp *.mod ../mod

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
