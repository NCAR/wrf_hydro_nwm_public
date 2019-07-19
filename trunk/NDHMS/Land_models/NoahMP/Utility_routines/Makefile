# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .F

include ../user_build_options

OBJS = \
	module_date_utilities.o \
	module_model_constants.o \
	module_wrf_utilities.o \
	kwm_string_utilities.o
	
CPPHRLDAS = -D_HRLDAS_OFFLINE_


all:	$(OBJS)

.F.o:
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $(*).F
	@echo ""

#
# Dependencies:
#

#
# This command cleans up object (etc.) files:
#

clean:
	$(RM) *.o *.mod *.stb *~

