
all: user_build_options
	(cd Utility_routines;		$(MAKE))
	(cd Noah;			$(MAKE))
	(cd IO_code;			$(MAKE))
	(cd Run;			$(MAKE) -f Makefile)
#	(cd HRLDAS_COLLECT_DATA;	$(MAKE))

clean:
	(cd Utility_routines;		$(MAKE) clean)
	(cd Noah;			$(MAKE) clean)
	(cd IO_code;			$(MAKE) clean)
	(cd Run;			$(MAKE) -f Makefile clean)
	(cd HRLDAS_COLLECT_DATA;	$(MAKE) clean)

test: all
	(cd TEST; ../HRLDAS_COLLECT_DATA/consolidate_grib.exe )
	(cd TEST; ../Run/Noah_hrldas_beta )

#all: check user_build_options
#	(cd Utility_routines;	$(MAKE))
#	(cd Noah;		$(MAKE))
#	(cd IO_code;		$(MAKE))
#	(cd Run;		$(MAKE))
#
#clean:
#	(cd Utility_routines;	$(MAKE) clean)
#	(cd Noah;		$(MAKE) clean)
#	(cd IO_code;		$(MAKE) clean)
#	(cd Run;		$(MAKE) clean)
#
#include user_build_options
#
#check:
#ifeq ($(COMPILERF90),)
#	@ echo ""
#	@ echo "*************************************************************"
#	@ echo "*************************************************************"
#	@ echo "*****                                                   *****"
#	@ echo "***** Did you set options in file 'user_build_options'? *****"
#	@ echo "*****                                                   *****"
#	@ echo "*************************************************************"
#	@ echo "*************************************************************"
#	@ echo ""
#	@ exit 1
#endif

