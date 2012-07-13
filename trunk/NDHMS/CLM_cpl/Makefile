# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

CLMOBJPATH = /raid/weiyu/scratch/clm4_ndhms

CLM_MOD = -I $(CLMOBJPATH)/lnd/obj -I $(CLMOBJPATH)/lib/include

include ../macros

OBJS = \
	module_clm_ndhms.o \
	clm_drv_ndhms.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../mod $(CLM_MOD) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libNDHMS.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
