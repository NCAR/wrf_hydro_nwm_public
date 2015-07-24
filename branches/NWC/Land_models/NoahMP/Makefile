

all: user_build_options
	(cd Utility_routines;		make)
	(cd Noah;			make)
	(cd IO_code;			make)
	(cd Run;			make -f Makefile all)
#	(cd HRLDAS_COLLECT_DATA;	make)

clean:
	(cd Utility_routines;		make clean)
	(cd Noah;			make clean)
	(cd IO_code;			make clean)
	(cd Run;			make -f Makefile clean)
#	(cd HRLDAS_COLLECT_DATA;	make clean)

test: all
	(cd TEST; ../HRLDAS_COLLECT_DATA/consolidate_grib.exe )
	(cd TEST; ../Run/Noah_hrldas_beta )

### explicitly point to other land model options
NoahMP: user_build_options
	(cd Utility_routines;		make)
	(cd Noah;			make)
	(cd IO_code;			make NoahMP MOD_OPT="-DNoahMP")
	(cd Run;			make -f Makefile NoahMP)
#	(cd HRLDAS_COLLECT_DATA;	make)
