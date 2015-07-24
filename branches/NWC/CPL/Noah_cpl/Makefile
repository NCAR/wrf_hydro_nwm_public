# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F



include ../../macros

MODFLAG =       -I./ -I ../../MPP -I ../../mod

OBJS = \
	module_hrldas_HYDRO.o \
	hrldas_drv_HYDRO.o    
all:	$(OBJS) 

.F.o:
	@echo ""
	$(CPP) $(CPPFLAGS) -I$(NETCDFINC) $(*).F > $(*).f
	$(COMPILER90) -o $(@) $(F90FLAGS) $(MODFLAG) $(*).f
	$(RMD) $(*).f
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)

#
# Dependencies:
#
module_hrldas_HYDRO.o: ../../Data_Rec/module_RT_data.o ../../Data_Rec/module_namelist.o ../../HYDRO_drv/module_HYDRO_drv.o

hrldas_drv_HYDRO.o: module_hrldas_HYDRO.o ../../Data_Rec/module_namelist.o ../../Routing/module_lsm_forcing.o


clean:
	rm -f *.o *.mod *.stb *~ *.f Noah_hrldas_beta
