#!/bin/bash

# Bash script is meant to be used by CMake. It downloads and extracts the
# Croton, NY testcase to the CMake build Run directory. It then setups up the
# files so ctest can run wrf_hydro.exe

# setup directory variables in script
# match the input case to valid testcase_dir
case ${1} in
    gridded)
        testcase_dir="Gridded"
        ;;
    gridded_no_lakes)
        testcase_dir="Gridded_no_lakes"
        ;;
    nwm_ana)
        testcase_dir="NWM"
        ;;
    nwm_ana)
        testcase_dir="NWM"
        ;;
    nwm_long_range)
        testcase_dir="NWM"
        ;;
    reach)
        testcase_dir="Reach"
        ;;
    reach_lakes)
        testcase_dir="ReachLakes"
        ;;
    *)
        echo "setup_cmake_testcase.sh: first command line argument did not match"
        exit 1
        ;;
esac
binary_dir=${2}
test_file_dir=${binary_dir}/tests
run_dir=${binary_dir}/Run

# download testcase if not present
version=5.3
croton_tarball=croton_NY_training_example_v${version}.tar.gz
if [ ! -f ${test_file_dir}/${croton_tarball} ]
then
    cd ${test_file_dir}
    wget -nv https://github.com/NCAR/wrf_hydro_nwm_public/releases/download/v${version}.0/${croton_tarball}
fi

# extract testcase
cd ${run_dir}
if [ ! -d example_case ]
then
   tar zxf ${test_file_dir}/${croton_tarball}
fi

# setup testcase
testcase=example_case/
testcase_dir=${testcase}/${testcase_dir}
cp ${testcase_dir}/hydro.namelist .
cp ${testcase_dir}/namelist.hrldas .
ln -sf ${testcase_dir}/DOMAIN .
ln -sf ${testcase_dir}/RESTART .
ln -sf ${testcase}/FORCING .

# turn on all output. Done with sed instead of Python for performance
sed -i 's/^\( *CHRTOUT_DOMAIN *= *\).*$/\11/' hydro.namelist
sed -i 's/^\( *CHANOBS_DOMAIN *= *\).*$/\11/' hydro.namelist
sed -i 's/^\( *CHRTOUT_GRID *= *\).*$/\11/' hydro.namelist
sed -i 's/^\( *LSMOUT_DOMAIN *= *\).*$/\11/' hydro.namelist
sed -i 's/^\( *RTOUT_DOMAIN *= *\).*$/\11/' hydro.namelist

# if NWM test fix for ana and long_range runs
case ${1} in
    nwm_ana)
        sed -i 's/^\( *RUNOFF_OPTION *= *\).*$/\17/I' namelist.hrldas
        ;;
    nwm_longrange)
        sed -i 's/^\( *RUNOFF_OPTION *= *\).*$/\17/I' namelist.hrldas
        sed -i 's/^\( *AGGFACTRT *= *\).*$/\11/I' hydro.namelist
        sed -i 's/^\( *DXRT *= *\).*$/\11000/I' hydro.namelist
        sed -i 's/^\( *OVRTSWCRT *= *\).*$/\10/I' hydro.namelist
        sed -i 's/^\( *RST_TYP *= *\).*$/\10/I' hydro.namelist
        sed -i 's/^\( *RTOUT_DOMAIN *= *\).*$/\10/I' hydro.namelist
        sed -i 's/^\( *SUBRTSWCRT *= *\).*$/\10/I' hydro.namelist
        sed -i 's/^\( *maxagepairsbiaspersist *= *\).*$/\124/I' hydro.namelist
        sed -i 's/^\( *persistBias *= *\).*$/\1.false./I' hydro.namelist
        ;;
    *)
        ;;
esac
