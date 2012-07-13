# Makefile 
#
#INCLUDES=
.SUFFIXES:
.SUFFIXES: .o .f

include ../macros

WRFCODE_OBJS = \
	mainDriver.o 

CMD = Noah_wrfdriver_beta 
all:	$(CMD) 

Noah_wrfdriver_beta: $(WRFCODE_OBJS)
	@echo ""
	$(COMPILER90) -o $(@) $(WRFCODE_OBJS) -L../lib -lNDHMS -lland $(NETCDFLIB)
	@echo ""

mainDriver.o: mainDriver.F
	$(RMD) $(*).f
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) -I../mod $(*).f
	$(RMD) $(*).f
	@echo ""

# This command cleans up
clean:
	rm -f *~ $(CMD) Noah_wrfdriver_beta_mpp librtland.a *.o *.f

# Dependencies:

Noah_wrfdriver_beta:	$(WRF_CODE_OBJS)

mainDriver.o:		../lib/libland.a ../lib/libNDHMS.a