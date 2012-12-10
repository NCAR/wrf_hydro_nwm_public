# Makefile 
all:
	(make -f Makefile.comm BASIC)
	(rm -f Run/Noah_hrldas_beta; cd HRLDAS_cpl; make)
	(cd HRLDAS; make; mv Run/Noah_hrldas_beta ../Run/Noah_hrldas_beta  )

clean:
	(cd HRLDAS_cpl; make clean)
	(make -f Makefile.comm clean)
	(cd HRLDAS; make clean)
	(rm -f */*.mod */*.o mod/*.mod lib/*.a Run/Noah_hrldas_beta)
