# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../macros

OBJS = \
	NDHMS_Noah_lsm_drv.o NDHMS_rt_drv.o
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I../mod $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libNDHMS.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
