# Makefile
#
CMD = Run/wrf_hydro.exe
.PHONY: $(CMD)

all: $(CMD)

$(CMD):
	@if [ ! -d "Run" ]; then \
		(mkdir Run);\
	fi
	(rm -f Run/wrf_hydro.exe   )
	($(MAKE) -f Makefile.comm)
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; $(MAKE)) \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd lib;rm -f librapid.a); \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd Rapid_routing; $(MAKE) -f makefile.cpl rapid); \
	fi

	@if [ -d "LandModel" ]; then \
	(cd LandModel; $(MAKE) ) \
	fi

debug::
	@echo 'F90FLAGS := $$(DEBUGFLAGS) $$(F90FLAGS)' >> ./macros
	@echo 'F90FLAGS := $$(DEBUGFLAGS) $$(F90FLAGS)' >> ./LandModel/user_build_options
debug:: $(CMD)

install:
	rm -f ./Run/wrf_hydro.exe; \
	mv LandModel/run/hrldas.exe  ./Run/wrf_hydro.exe
test:
	@echo "No libraries or utilities are built, skip testing."
clean:
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; $(MAKE) clean) \
	fi
	($(MAKE) -f Makefile.comm clean)
	@if [ -d "LandModel" ]; then \
	(cd LandModel; $(MAKE) clean) \
	fi
	if [ $(WRF_HYDRO_RAPID) -eq 1 ]; then \
		(cd Rapid_routing; $(MAKE) -f makefile.cpl clean); \
	fi
	(rm -f */*.mod */*.o lib/*.a Run/wrf_hydro.exe)
