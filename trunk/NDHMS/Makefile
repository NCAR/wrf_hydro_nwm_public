# Makefile 
all:
	(rm -f Run/wrf_hydro.exe   )
	(make -f Makefile.comm BASIC)
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; make) \
	fi
	@if [ -d "LandModel" ]; then \
	(cd LandModel; make; rm -f ../../Run/wrf_hydro.exe; mv Run/Noah_hrldas_beta ../../Run/wrf_hydro.exe  ) \
	fi

clean:
	@if [ -d "LandModel_cpl" ]; then \
	(cd LandModel_cpl; make clean) \
	fi
	(make -f Makefile.comm clean)
	@if [ -d "LandModel" ]; then \
	(cd LandModel; make clean) \
	fi
	(rm -f */*.mod */*.o lib/*.a Run/wrf_hydro.exe)
