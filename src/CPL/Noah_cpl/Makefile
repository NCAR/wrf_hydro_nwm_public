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
	$(COMPILER90) $(CPPINVOKE) $(CPPFLAGS) -o $(@) $(F90FLAGS) $(MODFLAG) -I$(NETCDF_INC) $(*).F
	@echo ""
	ar -r ../../lib/libHYDRO.a $(@)

#
# Dependencies:
#
module_hrldas_HYDRO.o: ../../Data_Rec/module_RT_data.o ../../Data_Rec/module_namelist.o ../../HYDRO_drv/module_HYDRO_drv.o

hrldas_drv_HYDRO.o: module_hrldas_HYDRO.o ../../Data_Rec/module_namelist.o ../../Routing/module_lsm_forcing.o


clean:
	rm -f *.o *.mod *.stb *~ Noah_hrldas_beta
