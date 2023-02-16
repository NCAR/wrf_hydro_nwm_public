# Makefile
#

.SUFFIXES:
.SUFFIXES: .o .F
LIS_ROOT = ../../../..

LIS_MOD = -I ../../mod -I$(LIS_ROOT)/$(MAKE)
include $(LIS_ROOT)/make/configure.lis
include ../../macros

MODFLAG =       -I./ -I ../../MPP -I ../../mod

OBJS = \
	module_lis_HYDRO.o\
	lis_drv_HYDRO.o

all:	$(OBJS)
	ar -r ../../lib/libHYDRO.a $(OBJS)

%.o: %.F
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(MODFLAG) -I../../mod $(LIS_MOD) -I$(MOD_ESMF) $(*).F
	@echo ""

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~
