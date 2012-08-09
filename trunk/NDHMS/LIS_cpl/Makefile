# Makefile 
#

.SUFFIXES:
.SUFFIXES: .o .F
LIS_MOD = -I ../mod -I../../../make

include ../../../make/configure.lis
include ../macros

OBJS = \
	module_lis_ndhms.o\
	lis_drv_ndhms.o
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I../mod $(LIS_MOD) -I$(MOD_ESMF) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libNDHMS.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
