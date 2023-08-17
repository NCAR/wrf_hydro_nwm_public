# this file is to be copied into the CMake ${CMAKE_BINARY_DIR}/Run/ directory
all: build

build:
	make -C ..

run:
	./wrf_hydro.exe

# download and extract Croton, NY test case to Run directory
# testcase options: {Gridded, Gridded_no_lakes, NWM, Reach, ReachLakes}
croton:
	make -C .. croton
croton-gridded:
	make -C .. croton-gridded
croton-gridded-no-lakes:
	make -C .. croton-gridded-no-lakes
croton-nwm:
	make -C .. croton-nwm
croton-reach:
	make -C .. croton-reach
croton-reach-lakes:
	make -C .. croton-reach-lakes

clean:
	rm -f *.LDASOUT_DOMAIN1 *.LAKEOUT_DOMAIN1 *.CHRTOUT_DOMAIN1
	rm -f *.GWOUT_DOMAIN1 *.RTOUT_DOMAIN1 *.CHANOBS_DOMAIN1
	rm -f RESTART.*_DOMAIN1 HYDRO_RST.*_DOMAIN1
	rm -f diag_hydro.*
