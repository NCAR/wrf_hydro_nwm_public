# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

CLMOBJPATH = /d1/weiyu/scratch/merra_boulder

CLM_MOD = -I $(CLMOBJPATH)/lnd/obj -I $(CLMOBJPATH)/lib/include

include ../../macros

MODFLAG =       -I./ -I ../../MPP -I ../../mod

OBJS = \
	module_clm_HYDRO.o \
	clm_drv_HYDRO.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -I$(NETCDFINC) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../../mod $(CLM_MOD) $(*).F
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ 
	cd ../..; make -f Makefile.comm clean
