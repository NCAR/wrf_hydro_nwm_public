
all: user_build_options
ifeq ($(MPPFLAG),1)
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make)
	(cd run;			make)
	(cd HRLDAS_forcing;		make)

clean:
	(cd Utility_routines;		make clean)
	(cd phys;			make clean)
	(cd IO_code;			make clean)
	(cd run;			make clean)
	(cd HRLDAS_forcing;		make clean)
ifeq ($(MPPFLAG),1)
	(cd MPP; make -f Makefile.NoahMP clean)
endif

### explicitly point to other land model options
NoahMP: user_build_options
ifdef MPPFLAG
	(cd MPP; make -f Makefile.NoahMP)
endif
	(cd Utility_routines;		make)
	(cd phys;			make)
	(cd IO_code;			make NoahMP MOD_OPT="-DNoahMP")
	(cd run;			make -f Makefile NoahMP)
	(cd HRLDAS_forcing;		make)
