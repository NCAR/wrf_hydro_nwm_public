#
# --- NWM makefile 
# --- Requires ESMVv8+
# --- NWM ESMF component.
#

# ###############
# Local Variables
# ###############

HR    := " "
HR    := $(HR)$(HR)
COMMA := ,
DIR   := $(CURDIR)

# ###########################
# Include ESMFMKFILE fragment
# ###########################

ifneq ($(origin ESMFMKFILE), environment)
  $(warning Environment variable ESMFMKFILE was not set.)
  export ESMFMKFILE=/scratch1/NCEPDEV/nems/emc.nemspara/soft/esmf/8.0.0bs48g-intel18.0.5.274-impi2018.0.4-netcdf4.6.1/lib/esmf.mk
  $(info Setting to $(ESMFMKFILE))
endif
include $(ESMFMKFILE)

# #########################
# Determine Build Precision
# #########################

ifeq ($(BUILD_PREC),r4)
override ESMF_F90COMPILECPPFLAGS += -DREAL4
else ifeq ($(BUILD_PREC),r8)
override ESMF_F90COMPILECPPFLAGS += -DREAL8
else
override ESMF_F90COMPILECPPFLAGS += -DREAL4
endif

# #################################
# Compile with Debugging Directives
# #################################

ifeq ($(DEBUG),on)
override ESMF_F90COMPILECPPFLAGS += -DDEBUG
override ESMF_CXXCOMPILECPPFLAGS += -DDEBUG
endif

# ###########################
# Determine Installation Path
# ###########################

ifndef DESTDIR     # this var is coming from NEMS/src/incmake/component_NWM.mk
  $(info Environment variable DESTDIR was not set.)
  DESTDIR  := $(DIR)
  $(info Setting to $(DESTDIR) )
else
  $(info DESTDIR=$(DESTDIR))
endif


ifndef INSTDIR     # this var is coming from NEMS/src/incmake/component_NWM.mk
  $(warning Environment variable INSTDIR was not set.)
  INSTDIR  := NWM_$(shell date '+%Y-%m-%d-%H-%M-%S')
  $(info Setting to $(INSTDIR) )
else
  $(info INSTDIR=$(INSTDIR) )
endif

ifndef INSTPATH   
  $(warning Environment variable INSTPATH was not set.)
  INSTPATH := $(abspath $(DESTDIR)/$(INSTDIR))
  $(info Setting to $(INSTPATH) )
else
  $(info INSTPATH=$(INSTPATH) )
endif

# ###############
# Model Variables
# ###############

ifndef COMP_SRCDIR      # this var is coming from NEMS/src/incmake/component_NWM.mk
  $(warning Environment variable COMP_SRCDIR was not set.)
  MODEL_DIR    := $(abspath $(DIR)/../..)
  $(info Setting to $(MODEL_DIR) )
else 
  MODEL_DIR    := $(abspath $(COMP_SRCDIR))
  $(info Environment variable COMP_SRCDIR=$(MODEL_DIR) )
endif

MODEL_LIB := $(abspath $(MODEL_DIR)/lib/libHYDRO.a)


MODEL_OBJS := ../../MPP/mpp_land.o ../../Land_models/NoahMP/IO_code/module_hrldas_driver.o ../../Land_models/NoahMP/IO_code/module_NoahMP_hrldas_driver.o ../../Land_models/NoahMP/IO_code/module_hrldas_netcdf_io.o ../../Land_models/NoahMP/Utility_routines/module_date_utilities.o ../../Land_models/NoahMP/Utility_routines/kwm_string_utilities.o ../../Land_models/NoahMP/phys/module_sf_noahmpdrv.o ../../Land_models/NoahMP/phys/module_sf_noahmplsm.o ../../Land_models/NoahMP/phys/module_sf_noahmp_groundwater.o ../../Land_models/NoahMP/phys/module_sf_noahmp_glacier.o ../../Land_models/NoahMP/Utility_routines/module_wrf_utilities.o


# #############
# Cap Variables
# #############

CAP_DIR       := $(abspath $(DIR))
CAP_LIB       := libnwm_nuopc.a
CAP_DEP_FRONT := nwm_nuopc_cap    # for NEMS, must be same name as its module
CAP_VERS      := VERSION
CAP_MK        := nwm.mk           # for NEMS

CAP_OBJS      := NWM_NUOPC_Cap.o
CAP_OBJS      += NWM_NUOPC_Gluecode.o
CAP_OBJS      += NWM_ESMF_Extensions.o

CAP_MODS      := nwm_esmf_extensions.mod  nwm_nuopc_cap.mod  nwm_nuopc_gluecode.mod

CAP_FILES     := $(CAP_OBJS) $(CAP_LIB) $(CAP_MODS) $(CAP_VERS) $(CAP_MK)


# #######################
# Primary Makefile Target
# #######################
.PHONY: nuopc nuopcinstall nuopcdistclean nuopcclean install_mk dust


nuopc: $(CAP_FILES)

nuopcinstall: $(CAP_LIB) $(CAP_MODS) $(CAP_VERS) \
 $(addprefix $(CAP_DIR)/,$(CAP_LIB)) \
 $(addprefix $(CAP_DIR)/,$(CAP_MODS)) \
 $(addprefix $(CAP_DIR)/,$(CAP_VERS)) \
 install_mk

# ############
# Dependencies
# ############

NWM_NUOPC_Cap.o: NWM_NUOPC_Macros.h \
 NWM_NUOPC_Gluecode.o NWM_ESMF_Extensions.o
NWM_NUOPC_Gluecode.o: NWM_NUOPC_Macros.h \
 NWM_ESMF_Extensions.o 


# ##############
# Build Settings
# ##############

.SUFFIXES: 
.SUFFIXES: .c .C .f90 .F90 .F .f

.F:
	@echo "Must have an explicit rule for" $*
.f:
	@echo "Must have an explicit rule for" $*
.C:
	@echo "Must have an explicit rule for" $*
.c: 
	@echo "Must have an explicit rule for" $*

%.o : %.f90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $< \
	-DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) -I ../../Land_models/NoahMP/Utility_routines -I ../../Land_models/NoahMP/IO_code \
	-I ../../Land_models/NoahMP/phys -I ../../MPP -I ../../mod -I ../../OrchestratorLayer -I ../../Land_models/NoahMP/data_structures/
        

# #####################
# Build NUOPC Component
# #####################

$(CAP_LIB): $(MODEL_OBJS) $(MODEL_LIB) $(CAP_OBJS)
	@echo $(HR)
	@echo "Creating static library $@..."
	@echo
	$(call checkfile, $(MODEL_LIB))	
	cp $(MODEL_LIB) $@
	ar cr $@ $(CAP_OBJS)
	ar r $(CAP_LIB) $(MODEL_OBJS)


$(CAP_VERS):
	@echo $(HR)
	@echo "Generating Version Information"
	@echo
	@echo "# NUOPC Cap Version" > $(CAP_VERS)
	@if [ -d .svn ]; then \
	  echo "SVN Repository" > $(CAP_VERS); \
	  svn info . | grep URL >> $(CAP_VERS); \
	  svn info . | grep "Last Changed Rev" >> $(CAP_VERS); \
	elif [ -d .git ]; then \
	  echo "Git Repository" > $(CAP_VERS); \
	  git show . | grep -m 1 "commit " >> $(CAP_VERS); \
	  git show . | grep -m 1 "Author: " >> $(CAP_VERS); \
	  git show . | grep -m 1 "Date: " >> $(CAP_VERS); \
	fi

$(CAP_MK): 
	@echo $(HR)
	@echo "Generating NUOPC Makefile Fragment"
	@echo
	@echo "# ESMF self-describing build dependency makefile fragment" > $(CAP_MK)
	@echo "" >> $(CAP_MK)
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $(CAP_MK)
	@echo "ESMF_DEP_INCPATH   = $(CAP_DIR)" >> $(CAP_MK)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(CAP_MK)
	@echo "ESMF_DEP_LINK_OBJS = $(CAP_DIR)/$(CAP_LIB)" >> $(CAP_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(CAP_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(CAP_MK)

# -----------------------------------------------------------------------------
# Install Library, Modules, and Makefile Fragment
# -----------------------------------------------------------------------------

$(INSTPATH)/%:
	@echo $(HR)
	@echo "Installing $(notdir $@)"
	@echo
	@mkdir -p $(INSTPATH)
	cp $(notdir $@) $@

install_mk: 
	@echo $(HR)
	@echo "Installing NUOPC Makefile Fragment"
	@mkdir -p $(INSTPATH)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(INSTPATH)/$(CAP_MK)
	@echo "" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_INCPATH   = $(CAP_DIR) $(INSTPATH)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_LINK_OBJS = $(INSTPATH)/$(CAP_LIB)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(INSTPATH)/$(CAP_MK)
	cp -f $(CAP_LIB) $(INSTPATH)
	cp -f $(CAP_MODS) $(INSTPATH)
	@echo $(HR)

# ###########
# Check Build
# ###########

define checkfile
	@if [ ! -e $(1) ]; then \
	echo "File is missing:$(1)"; \
	exit 1; fi;
endef # blank line in checkfile is required

define checkdir
	@if [ ! -d $(1) ]; then \
	echo "Directory is missing:$(1)"; \
	exit 1; fi;
endef # blank line in checkdir is required

check: check_esmf

# ##################
# Check ESMF Version
# ##################

check_esmf:
	@echo $(HR)
	@echo "Checking ESMFMKFILE file..."
	@echo
	@echo "ESMFMKFILE=$(ESMFMKFILE)"
	@if [ "$(ESMF_VERSION_MAJOR)" -lt 8 ]; then \
	echo "Please use ESMF version 8+"; \
	exit 1; fi;
	@echo "ESMF Version=$(ESMF_VERSION_STRING)"


# Clean Cap and Model
# ###################

nuopcdistclean: nuopcclean
	@echo $(HR)
	@echo "Cleaning Model build..."             # used for local installation/build
	@echo ""
	$(call checkdir, $(MODEL_DIR))
	make -C $(MODEL_DIR) -f $(MODEL_MK) clean PET*.ESMF_LogFile *.nc
	@echo $(HR)

# #########
# Clean Cap
# #########

nuopcclean:
	@echo $(HR)
	@echo "Cleaning Cap build..."
	@echo
	rm -f $(CAP_FILES)
	@echo $(HR)

dust:
	@echo $(HR)
	@echo "Dusting ..."
	@echo
	rm -f PET*.ESMF_LogFile *.nc
	@echo $(HR)

# ------------------------------------------------------------------------------
