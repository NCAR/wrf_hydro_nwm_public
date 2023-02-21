# Makefile
#
include ../user_build_options

SRCS := $(wildcard *.F)
OBJS := $(SRCS:%.F=%.o)
CPPHRLDAS = -D_HRLDAS_OFFLINE_

# Check whether or not to build Crocus
ifndef BUILD_CROCUS
BUILD_CROCUS:=1
endif
ifeq ($(BUILD_CROCUS), 1)
        SUR_OBJS := surfex/module_snowcro.o surfex/ini_csts.o
else
        SUR_OBJS := surfex/module_snowcro_intercept.o
endif

all: $(OBJS)

%.o:%.F
	@echo ""
	$(COMPILERF90) $(CPPINVOKE) $(CPPFLAGS) $(CPPHRLDAS) -o $(@) -c -I../Utility_routines -Isurfex $(F90FLAGS) $(LDFLAGS) $(FREESOURCE) $<
	@echo ""

surfex/%.o:
	$(MAKE) --directory=surfex/ $(@F)

#
# Dependencies:
#
module_sf_noahmpdrv.o:	module_sf_noahmplsm.o module_sf_noahmp_glacier.o module_sf_noahmp_groundwater.o $(SUR_OBJS)
module_sf_noahmp_groundwater.o: module_sf_noahmplsm.o

#
# This command cleans up object (etc) files:
#
clean:
	$(MAKE) clean --directory=surfex
	$(RM) *.o *.mod *.stb *~
