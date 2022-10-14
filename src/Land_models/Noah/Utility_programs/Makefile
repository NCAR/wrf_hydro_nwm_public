SHELL=/bin/sh
.SUFFIXES:	
.SUFFIXES:	.F .c .o .exe

include ../user_build_options

# OBJS=	hrldas_extract_point.o \
# 	module_date_utilities.o \
# 	arguments_module.o \
# 	llxy_generic.o \
# 	lccone.o

CMD=	hrldas_extract_point gribextract gribedition gribbyte modify_wrfinput geth_newdate

all: $(CMD)
	( cd gcip_sw_to_grib; make )

#
# Compile the module_date_utilities from the Utility_routines directory
#
module_date_utilities.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(FREESOURCE) $(F90FLAGS) -c ../Utility_routines/$(*).F

#
# Compile the arguments_module from the HRLDAS_COLLECT_DATA/lib directory
#
arguments_module.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(FREESOURCE) $(F90FLAGS) -c ../HRLDAS_COLLECT_DATA/lib/$(*).F

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(FREESOURCE) $(F90FLAGS) -c $(NETCDFMOD) $(MODFLAG). $(*).F


hrldas_extract_point: hrldas_extract_point.o
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(MODFLAG). \
		hrldas_extract_point.o arguments_module.o module_date_utilities.o llxy_generic.o lccone.o \
		$(NETCDFLIB)

gribextract:	gribextract.c
	$(CC) -o $(@) gribextract.c -lm


gribedition:	gribedition.c
	$(CC) -o $(@) gribedition.c -lm

gribbyte:	gribbyte.c
	$(CC) -o $(@) gribbyte.c -lm

geth_newdate:
	$(CC) -o $(@) geth_newdate.c -lm

modify_wrfinput: modify_wrfinput.o
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(MODFLAG). \
		modify_wrfinput.o \
		$(NETCDFLIB)


hrldas_extract_point.o:	arguments_module.o
hrldas_extract_point.o:	module_date_utilities.o
hrldas_extract_point.o:	llxy_generic.o
hrldas_extract_point.o:	lccone.o

clean:
	$(RM) *.o *~ $(CMD) *.mod
	( cd gcip_sw_to_grib; make clean )
neat:
	$(RM) *.o *~ *.mod
	( cd gcip_sw_to_grib; make neat )
#

