# Makefile 

all:
	(cd ../; make -f Makefile)
	(make -f Makefile.cpl)
	(cd ../HRLDAS; make; cp Run/Noah_hrldas_beta ../HRLDAS_cpl  )

clean:
	(make -f Makefile.cpl clean)
	(cd ../; make -f Makefile clean)
	(cd ../HRLDAS; make clean)
