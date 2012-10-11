# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F



include ../macros

WRF_ROOT = /raid/weiyu/MM_R/code_20120718/CVS_reference/cvs/apps/4dwx/RTFDDA/src/RT_WRF/WRF_V3.3.1_8layer
OBJS = \
	module_wrf_HYDRO.o \
	wrf_drv_HYDRO.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I ../mod -I$(WRF_ROOT)/main -I$(WRF_ROOT)/external/esmf_time_f90 $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
	cd ..; make clean
