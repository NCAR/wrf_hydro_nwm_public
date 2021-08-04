# Makefile
#
include ../user_build_options

SRCS := $(wildcard *.F)
OBJS := $(SRCS:%.F=%.o)
CPPHRLDAS = -D_HRLDAS_OFFLINE_

all: $(OBJS)

%.o:%.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c -I../Utility_routines -Isurfex $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $(*).F
	@echo ""

#
# Dependencies:
#
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o module_snowcro.o
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o
module_snowcro.o: surfex

.PHONY: surfex
surfex:
	$(MAKE) --directory=$@

#
# This command cleans up object (etc) files:
#
clean:
	$(RM) *.o *.mod *.stb *~
