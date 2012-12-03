# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F



include ../macros

<<<<<<< HEAD
WRF_ROOT = ../..
=======
WRF_ROOT = /raid/weiyu/WRF_RELEASE/r6019/mod_test
>>>>>>> 87ce796d24e5f8cc8e737ff7140f809f18fc31b2
OBJS = \
	module_wrf_HYDRO.o \
	wrf_drv_HYDRO.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) -I$(WRF_ROOT)/frame -I$(WRF_ROOT)/external/esmf_time_f90 $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../lib/libHYDRO.a $(@)

#
# Dependencies:
#

clean:
	rm -f *.o *.mod *.stb *~ *.f
	cd ..; make clean
