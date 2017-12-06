#
# --- WRF-HYDRO makefile 
# --- Requires ESMVv7+
# --- WRF-HYDRO ESMF component.
#

# ###########################
# Include ESMFMKFILE fragment
# ###########################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif
include $(ESMFMKFILE)

# ##############
# Local Variable
# ##############
HR="================================================================================"

ifndef WRFHYDRODIR
$(error WRFHYDRODIR was not set. Please set this variable to the WRF-Hydro root directory.)
endif

WRFHYDRO_DIR=$(WRFHYDRODIR)
WRFHYDRO_TDIR=$(notdir $(WRFHYDRO_DIR))
WRFHYDRO_LIBDIR=lib
WRFHYDRO_MODDIR=mod
WRFHYDRO_MPPDIR=MPP

WRFHYDRO_LIB=libHYDRO.a
WRFHYDRO_MPPMODS=module_mpp_land.mod \
	module_cpl_land.mod
WRFHYDRO_MODS=module_hydro_drv.mod \
	module_rt_data.mod \
	module_namelist.mod \
	module_hydro_io
WRFHYDRO_FILES=$(addprefix $(WRFHYDRO_DIR)/$(WRFHYDRO_LIBDIR)/,$(WRFHYDRO_LIB)) \
	$(addprefix $(WRFHYDRO_DIR)/$(WRFHYDRO_MPPDIR)/,$(WRFHYDRO_MPPMODS)) \
	$(addprefix $(WRFHYDRO_DIR)/$(WRFHYDRO_MODDIR)/,$(WRFHYDRO_MODS))

WRFHYDRONUOPC_DIR=$(shell pwd)
WRFHYDRONUOPC_TDIR=$(notdir $(WRFHYDRONUOPC_DIR))

WRFHYDRONUOPC_LIB=libwrfhydro_nuopc.a
WRFHYDRONUOPC_MK=wrfhydro.mk
WRFHYDRONUOPC_DEP_FRONT=wrfhydro_nuopc
WRFHYDRONUOPC_MODS=wrfhydro_nuopc.mod
WRFHYDRONUOPC_FILES= $(WRFHYDRONUOPC_MK) \
	$(WRFHYDRONUOPC_LIB) \
	$(WRFHYDRONUOPC_MODS)

# ########
# Make all
# ########
all: build_wrfhydro_nuopc

# ###############
# Build WRF-Hydro
# ###############
$(WRFHYDRO_FILES):
	@echo $(HR)
	@echo "Building WRF-Hydro..."
	@echo $@
	@echo
	@if [ -z "$(WRF_HYDRO)" ]; then \
	echo "WRF_HYDRO environment variable is not defined."; \
	exit 1; fi;
	@echo "WRF_HYDRO=$(WRF_HYDRO)"
	@echo
	$(call checkdir, $(WRFHYDRO_DIR))
	$(call checkfile, $(WRFHYDRO_DIR)/macros)
	(cd $(WRFHYDRO_DIR); make -f Makefile.comm)
	
$(WRFHYDRO_LIB): $(addprefix $(WRFHYDRO_DIR)/$(WRFHYDRO_LIBDIR)/,$(WRFHYDRO_LIB))
	cp $^ $@
	
# ##############
# Build Settings
# ##############

.SUFFIXES: 
.SUFFIXES: .c .C .f90 .F90 .F .f

WRFHYDRO_F90COMPILEPATHS =-I$(WRFHYDRO_DIR)/$(WRFHYDRO_MPPDIR) -I$(WRFHYDRO_DIR)/$(WRFHYDRO_MODDIR)

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
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(WRFHYDRO_F90COMPILEPATHS) $(ESMF_F90COMPILEFREENOCPP) $<

%.o : %.F90
	@echo $(HR)
	@echo "Compiling $@..."
	@echo
	$(ESMF_F90COMPILER) -c $(ESMF_F90COMPILEOPTS) $(ESMF_F90COMPILEPATHS) $(WRFHYDRO_F90COMPILEPATHS) $(ESMF_F90COMPILEFREECPP) $(ESMF_F90COMPILECPPFLAGS) -DESMF_VERSION_MAJOR=$(ESMF_VERSION_MAJOR) $<
        
# #####################################
# Generate Local ESMF Makefile fragment
# #####################################
%.mk :
	@echo $(HR)	
	@echo "Generating WRF-Hydro Makefile fragment $@"
	@echo
	@echo "# ESMF self-describing build dependency makefile fragment" > $@
	@echo >> $@
	@echo "ESMF_DEP_FRONT     = $(WRFHYDRONUOPC_DEP_FRONT)"                  >> $@
	@echo "ESMF_DEP_INCPATH   = $(WRFHYDRONUOPC_DIR)"                  >> $@
	@echo "ESMF_DEP_CMPL_OBJS = $(WRFHYDRONUOPC_DIR)/$(WRFHYDRONUOPC_LIB)" >> $@
	@echo "ESMF_DEP_LINK_OBJS = $(WRFHYDRONUOPC_DIR)/$(WRFHYDRONUOPC_LIB)" >> $@
	@echo "ESMF_DEP_SHRD_PATH = "                       >> $@
	@echo "ESMF_DEP_SHRD_LIBS = "                       >> $@

# #####################
# Build NUOPC Component
# #####################

.PHONY: wrfhydro_nuopc clean nuopcinstall

build_wrfhydro_nuopc: $(WRFHYDRONUOPC_FILES)

$(WRFHYDRONUOPC_LIB): $(WRFHYDRO_LIB) wrfhydro_nuopc_cap.o wrfhydro_nuopc_gluecode.o wrfhydro_nuopc_addonutils.o
	@echo $(HR)
	@echo "Copying static library $@..."
	@echo
	mv -f $(WRFHYDRO_LIB) $@
	ar rc $@ *.o

# Dependencies
wrfhydro_nuopc.mod: wrfhydro_nuopc_cap.o
wrfhydro_nuopc_cap.o: wrfhydro_nuopc_gluecode.mod \
	wrfhydro_nuopc_addonutils.mod
wrfhydro_nuopc_gluecode.mod: wrfhydro_nuopc_gluecode.o
wrfhydro_nuopc_gluecode.o: wrfhydro_nuopc_addonutils.mod \
	$(WRFHYDRO_FILES)
wrfhydro_nuopc_addonutils.mod: wrfhydro_nuopc_addonutils.o

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

check: check_esmf check_wrfhydro check_wrfhydro_nuopc

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
	

# ###############
# Check WRF-Hydro
# ###############

check_wrfhydro:
	@echo $(HR)
	@echo "Checking for WRF-Hydro files..."
	@echo
	$(foreach FILENAME, $(WRFHYDRO_FILES), $(call checkfile, $(FILENAME)))

# #####################
# Check WRF-Hydro NUOPC
# #####################
check_wrfhydro_nuopc: 
	@echo $(HR)
	@echo "Checking for WRF-Hydro NUOPC files..."
	@echo
	$(foreach FILENAME, $(WRFHYDRONUOPC_FILES), $(call checkfile, $(FILENAME)))

# #########
# Clean all
# #########
clean: clean_wrfhydro_nuopc clean_wrfhydro

# ###############################
# Clean WRF-HYDRO NUOPC Component
# ###############################
clean_wrfhydro_nuopc:
	@echo $(HR)
	@echo "Cleaning WRF-Hydro NUOPC build..."
	@echo
	rm -f *.o *.mod *.a *.mk VERSION

# ###############
# Clean WRF-Hydro
# ###############
clean_wrfhydro:
	@echo $(HR)
	@echo "Cleaning WRF-Hydro build..."
	@echo ""
	$(call checkdir, $(WRFHYDRO_DIR))
	(cd $(WRFHYDRO_DIR); make -f Makefile.comm clean)

# #################################
# Install WRF-HYDRO NUOPC Component
# #################################

ifndef DESTDIR
DESTDIR := $(WRFHYDRONUOPC_DIR)
endif

INSTDATE := $(shell date '+%Y-%m-%d-%H-%M-%S')
ifndef INSTDIR
INSTDIR  := WRFHYDRO_$(INSTDATE)
endif

install:
	@mkdir -p $(DESTDIR)/$(INSTDIR)
	@cp $(WRFHYDRONUOPC_FILES) $(DESTDIR)/$(INSTDIR)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_FRONT     = $(WRFHYDRONUOPC_DEP_FRONT)" >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_INCPATH   = $(DESTDIR)/$(INSTDIR)" >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_CMPL_OBJS = $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_LIB)" >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_LINK_OBJS = $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_LIB)" >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_SHRD_PATH = " >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)
	@echo "ESMF_DEP_SHRD_LIBS = " >> $(DESTDIR)/$(INSTDIR)/$(WRFHYDRONUOPC_MK)

	@touch VERSION
	@if [ -d .svn ]; then \
	  echo "SVN Repository" > VERSION; \
	  svn info . | grep URL >> VERSION; \
	  svn info . | grep "Last Changed Rev" >> VERSION; \
	fi
	@if [ -d .git ]; then \
	  echo "Git Repository" > VERSION; \
	  git show . | grep -m 1 "commit " >> VERSION; \
	  git show . | grep -m 1 "Author: " >> VERSION; \
	  git show . | grep -m 1 "Date: " >> VERSION; \
	fi
	@cp VERSION $(DESTDIR)/$(INSTDIR)/
	@echo Installation into \"$(DESTDIR)/$(INSTDIR)\" complete!
	@echo
# ------------------------------------------------------------------------------
