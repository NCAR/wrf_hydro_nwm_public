#!/bin/bash

# Bash script is meant to be used by CMake. It downloads and extracts the
# Croton, NY testcase to the CMake build Run directory. It then setups up the
# files so ctest can run wrf_hydro.exe

# setup directory variables in script
# testcase_type values = {Gridded, Gridded_no_lakes, NWM, Reach, ReachLakes}
testcase_type=${1}
binary_dir=${2}
test_file_dir=${binary_dir}/tests
run_dir=${binary_dir}/Run

# download testcase if not present
croton_tarball=croton_NY_training_example_v5.2.tar.gz
if [ ! -f ${test_file_dir}/${croton_tarball} ]
then
    cd ${test_file_dir}
    wget -nv https://github.com/NCAR/wrf_hydro_nwm_public/releases/download/v5.2.0/${croton_tarball}
fi

# extract testcase
cd ${run_dir}
if [ ! -d example_case ]
then
   tar zxf ${test_file_dir}/${croton_tarball}
fi

# setup testcase
testcase=example_case/
testcase_dir=${testcase}/${testcase_type}
cp ${testcase_dir}/hydro.namelist .
cp ${testcase_dir}/namelist.hrldas .
ln -sf ${testcase_dir}/DOMAIN .
ln -sf ${testcase_dir}/RESTART .
ln -sf ${testcase}/FORCING .
