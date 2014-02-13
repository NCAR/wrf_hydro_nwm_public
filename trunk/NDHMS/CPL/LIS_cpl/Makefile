# Makefile 
#

.SUFFIXES:
.SUFFIXES: .o .F
LIS_ROOT = ../../../..

LIS_MOD = -I ../../mod -I$(LIS_ROOT)/make
include $(LIS_ROOT)/make/configure.lis
include ../../macros

MODFLAG =       -I./ -I ../../MPP -I ../../mod

OBJS = \
	module_lis_HYDRO.o\
	lis_drv_HYDRO.o
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I../../mod $(LIS_MOD) -I$(MOD_ESMF) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
