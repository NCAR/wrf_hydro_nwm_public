# Makefile 
all:
	(make -f Makefile.comm BASIC)
	(cd HRLDAS_cpl; make)
	(cd HRLDAS; make; rm -f ../Run/Noah_hrldas_beta; cp Run/Noah_hrldas_beta ../Run/Noah_hrldas_beta  )

clean:
	(cd HRLDAS_cpl; make clean)
	(make -f Makefile.comm clean)
	(cd HRLDAS; make clean)
	(rm -f */*.mod */*.o lib/*.a)
