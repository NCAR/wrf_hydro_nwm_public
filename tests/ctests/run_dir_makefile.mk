# this file is to be copied into the CMake ${CMAKE_BINARY_DIR}/Run/ directory
all: build

build:
	make -C ..

run:
	./wrf_hydro.exe

# download and extract Croton, NY test case to Run directory
# testcase options: {Gridded, Gridded_no_lakes, NWM, Reach, ReachLakes}
croton:
	make -C .. $@
croton-gridded:
	make -C .. $@
croton-gridded-no-lakes:
	make -C .. $@
croton-nwm:
	make -C .. $@
croton-nwm_ana:
	make -C .. $@
croton-nwm_long_range:
	make -C .. $@
croton-reach:
	make -C .. $@
croton-reach_lakes:
	make -C .. $@
# run croton targets
run-croton:
	make -C .. $@
run-croton-gridded:
	make -C .. $@
run-croton-gridded-no-lakes:
	make -C .. $@
run-croton-nwm:
	make -C .. $@
run-croton-nwm_ana:
	make -C .. $@
run-croton-nwm_long_range:
	make -C .. $@
run-croton-reach:
	make -C .. $@
run-croton-reach_lakes:
	make -C .. $@
# run in parallel
run-croton-gridded-parallel:
	make -C .. $@
run-croton-gridded-no-lakes-parallel:
	make -C .. $@
run-croton-nwm-parallel:
	make -C .. $@
run-croton-nwm_ana-parallel:
	make -C .. $@
run-croton-nwm_long_range-parallel:
	make -C .. $@
run-croton-reach-parallel:
	make -C .. $@
run-croton-reach_lakes-parallel:
	make -C .. $@

clean:
	rm -f *.CHANOBS_DOMAIN1 *.CHRTOUT_DOMAIN1 *.GWOUT_DOMAIN1
	rm -f *.LAKEOUT_DOMAIN1 *.LDASOUT_DOMAIN1 *.LSMOUT_DOMAIN1
	rm -f *.RTOUT_DOMAIN1 RESTART.*_DOMAIN1 HYDRO_RST.*_DOMAIN1
	rm -f diag_hydro.*
