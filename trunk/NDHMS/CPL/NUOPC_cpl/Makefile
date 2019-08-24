#
# --- WRF-HYDRO makefile 
# --- Requires ESMVv7+
# --- WRF-HYDRO ESMF component.
#

# ###############
# Local Variables
# ###############

HR    := ========================================
HR    := $(HR)$(HR)
COMMA := ,
DIR   := $(CURDIR)

# ###########################
# Include ESMFMKFILE fragment
# ###########################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
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

ifdef DBGBUILD
ESMF_F90COMPILEOPTS += -g -traceback
ESMF_CXXCOMPILEOPTS += -g -traceback
endif

# ###########################
# Determine Installation Path
# ###########################

ifndef DESTDIR
DESTDIR  := $(DIR)
endif

ifndef INSTDIR
INSTDIR  := WRFHYDRO_$(shell date '+%Y-%m-%d-%H-%M-%S')
endif

ifndef INSTPATH
INSTPATH := $(abspath $(DESTDIR)/$(INSTDIR))
endif

# ###############
# Model Variables
# ###############

ifndef WRFHYDRO_DIR
MODEL_RDIR   := $(abspath $(DIR)/../../../..)
else
MODEL_RDIR   := $(abspath $(WRFHYDRO_DIR))
endif
MODEL_DIR    := $(abspath $(MODEL_RDIR)/trunk/NDHMS)
MODEL_MKDIR  := $(abspath $(MODEL_DIR))
MODEL_LIBDIR := $(abspath $(MODEL_DIR)/lib)
MODEL_MODDIR := $(abspath $(MODEL_DIR)/mod)
MODEL_MPPDIR := $(abspath $(MODEL_DIR)/MPP)
MODEL_RTGDIR := $(abspath $(MODEL_DIR)/Routing)

MODEL_MK     := $(abspath $(MODEL_MKDIR)/Makefile.comm)
MODEL_MKINC  := $(abspath $(MODEL_DIR)/macros)
MODEL_LIB    := $(abspath $(MODEL_LIBDIR)/libHYDRO.a)

MODEL_MODS   := $(abspath $(MODEL_MPPDIR)/module_mpp_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MPPDIR)/module_cpl_land.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_drv.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_rt_data.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_namelist.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_hydro_io.mod)
MODEL_MODS   += $(abspath $(MODEL_MODDIR)/module_lsm_forcing.mod)
MODEL_MODS   += $(abspath $(MODEL_RTGDIR)/Overland/overland_data.mod)
MODEL_MODS   += $(abspath $(MODEL_RTGDIR)/Overland/overland_control.mod)

MODEL_FILES  := $(MODEL_LIB) $(MODEL_MODS)

# #############
# Cap Variables
# #############

CAP_DIR       := $(abspath $(DIR))
CAP_LIB       := libwrfhydro_nuopc.a
CAP_DEP_FRONT := wrfhydro_nuopc
CAP_VERS      := VERSION
CAP_MK        := wrfhydro.mk

CAP_OBJS      := WRFHydro_NUOPC_Cap.o
CAP_OBJS      += WRFHydro_NUOPC_Gluecode.o
CAP_OBJS      += WRFHydro_ESMF_Extensions.o

CAP_MODS      := wrfhydro_nuopc.mod
CAP_MODS      += wrfhydro_nuopc_gluecode.mod
CAP_MODS      += wrfhydro_esmf_extensions.mod

CAP_FILES     := $(CAP_OBJS) $(CAP_MODS) $(CAP_LIB) $(CAP_VERS) $(CAP_MK)

# ###############################
# Include Model Makefile Fragment
# ###############################

include $(MODEL_MKINC)
override ESMF_F90COMPILEPATHS    += -I$(MODEL_MODDIR)
override ESMF_F90COMPILEPATHS    += -I$(MODEL_MPPDIR)
override ESMF_F90COMPILECPPFLAGS += $(HYDRO_D)

# #######################
# Primary Makefile Target
# #######################
.PHONY: nuopc nuopcinstall nuopcdistclean nuopcclean install_mk

nuopc: $(CAP_FILES)

nuopcinstall: $(CAP_LIB) $(CAP_MODS) $(CAP_VERS) \
 $(addprefix $(INSTPATH)/,$(CAP_LIB)) \
 $(addprefix $(INSTPATH)/,$(CAP_MODS)) \
 $(addprefix $(INSTPATH)/,$(CAP_VERS)) \
 install_mk

# ############
# Dependencies
# ############

WRFHydro_NUOPC_Cap.o: WRFHydro_NUOPC_Macros.h \
 WRFHydro_NUOPC_Gluecode.o WRFHydro_ESMF_Extensions.o
WRFHydro_NUOPC_Gluecode.o: WRFHydro_NUOPC_Macros.h \
 WRFHydro_ESMF_Extensions.o $(MODEL_MODS)

wrfhydro_nuopc.mod: WRFHydro_NUOPC_Cap.o
wrfhydro_nuopc_gluecode.mod: WRFHydro_NUOPC_Gluecode.o
wrfhydro_esmf_extensions.mod: WRFHydro_ESMF_Extensions.o

# ###############
# Build Model
# ###############

build_model:
	@echo $(HR)
	@echo "Building Model..."
	@echo ""
	$(call checkdir, $(MODEL_DIR))
	mkdir -p $(MODEL_LIBDIR)
	mkdir -p $(MODEL_MODDIR)
	make -C $(MODEL_DIR) -f $(MODEL_MK)

$(MODEL_MODS): build_model

$(MODEL_LIB): build_model

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
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<

# #####################
# Build NUOPC Component
# #####################

$(CAP_LIB): $(MODEL_LIB) $(CAP_OBJS)
	@echo $(HR)
	@echo "Creating static library $@..."
	@echo
	$(call checkfile, $(MODEL_LIB))
	cp $(MODEL_LIB) $@
	ar cr $@ $(CAP_OBJS)

$(CAP_VERS):
	@echo $(HR)
	@echo "Generating Version Information"
	@echo
ifneq (,$(wildcard $(MODEL_RDIR)/.git))
	@echo "# NUOPC Cap Revision #" > $(CAP_VERS)
	@git log . | grep -m 1 "commit " >> $(CAP_VERS)
	@git log . | grep -m 1 "Author: " >> $(CAP_VERS)
	@git log . | grep -m 1 "Date: " >> $(CAP_VERS)
	@echo >> $(CAP_VERS)
	@echo "# Model Revision     #" >> $(CAP_VERS)
	@git log $(MODEL_RDIR) | grep -m 1 "commit " >> $(CAP_VERS)
	@git log $(MODEL_RDIR) | grep -m 1 "Author: " >> $(CAP_VERS)
	@git log $(MODEL_RDIR) | grep -m 1 "Date: " >> $(CAP_VERS)
	@echo >> $(CAP_VERS)
else ifneq (,$(wildcard $(MODEL_RDIR)/.svn))
	@echo "# NUOPC Cap Revision #" > $(CAP_VERS)
	@svn info . | grep URL >> $(CAP_VERS)
	@svn info . | grep "Last Changed Rev" >> $(CAP_VERS)
	@echo >> $(CAP_VERS)
	@echo "# Model Revision     #" >> $(CAP_VERS)
	@svn info $(MODEL_RDIR) | grep URL >> $(CAP_VERS)
	@svn info $(MODEL_RDIR) | grep "Last Changed Rev" >> $(CAP_VERS)
	@echo >> $(CAP_VERS)
else
	@echo "# Version Information Not Found" > $(CAP_VERS)
endif

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
	@cp $(notdir $@) $@

install_mk: 
	@echo $(HR)
	@echo "Installing NUOPC Makefile Fragment"
	@echo
	@mkdir -p $(INSTPATH)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(INSTPATH)/$(CAP_MK)
	@echo "" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_FRONT     = $(CAP_DEP_FRONT)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_INCPATH   = $(INSTPATH)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_LINK_OBJS = $(INSTPATH)/$(CAP_LIB)" >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(INSTPATH)/$(CAP_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(INSTPATH)/$(CAP_MK)

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

check: check_esmf check_model check_cap

# ##################
# Check ESMF Version
# ##################

check_esmf:
	@echo $(HR)
	@echo "Checking ESMFMKFILE file..."
	@echo
	@echo "ESMFMKFILE=$(ESMFMKFILE)"
	@if [ "$(ESMF_VERSION_MAJOR)" -lt 7 ]; then \
	echo "Please use ESMF version 7+"; \
	exit 1; fi;
	@echo "ESMF Version=$(ESMF_VERSION_STRING)"

# ###########
# Check Model
# ###########

check_model:
	@echo $(HR)
	@echo "Checking for Model files..."
	@echo
	$(foreach FILENAME, $(MODEL_FILES), $(call checkfile, $(FILENAME)))

# #########
# Check Cap
# #########

check_cap: 
	@echo $(HR)
	@echo "Checking for WRF-Hydro NUOPC files..."
	@echo
	$(foreach FILENAME, $(CAP_FILES), $(call checkfile, $(FILENAME)))

# ###################
# Clean Cap and Model
# ###################

nuopcdistclean: nuopcclean
	@echo $(HR)
	@echo "Cleaning Model build..."
	@echo ""
	$(call checkdir, $(MODEL_DIR))
	make -C $(MODEL_DIR) -f $(MODEL_MK) clean

# #########
# Clean Cap
# #########

nuopcclean:
	@echo $(HR)
	@echo "Cleaning Cap build..."
	@echo
	rm -f $(CAP_FILES)

# ------------------------------------------------------------------------------
