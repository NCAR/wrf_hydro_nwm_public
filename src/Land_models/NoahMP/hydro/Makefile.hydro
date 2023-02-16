
all: user_build_options
	(cd Utility_routines;		$(MAKE))
	(cd phys;			$(MAKE))
	(cd data_structures;		$(MAKE))
	(cd IO_code;			$(MAKE))
	(cd run;			$(MAKE))

clean:
	(cd Utility_routines;		$(MAKE) clean)
	(cd phys;			$(MAKE) clean)
	(cd data_structures;		$(MAKE) clean)
	(cd IO_code;			$(MAKE) clean)
	(cd run;			$(MAKE) clean)

### explicitly point to other land model options
NoahMP: user_build_options
	(cd Utility_routines;		$(MAKE))
	(cd phys;			$(MAKE))
	(cd data_structures;		$(MAKE))
	(cd IO_code;			$(MAKE) NoahMP MOD_OPT="-DNoahMP")
	(cd run;			$(MAKE) -f Makefile NoahMP)
