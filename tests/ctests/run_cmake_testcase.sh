#!/bin/bash

# Bash script is meant to be used by CMake. It runs the Croton testcases after
# setup_cmake_testcase.sh uses them

# setup directory variables in script
testcase_type=${1}
binary_dir=${2}
np=${3}
run_dir=${binary_dir}/Run
output_dir=output_${testcase_type}

exe=wrf_hydro
if [ "$testcase_type" = "bmi" ]; then
    exe=wrf_hydro_bmi
fi

if [ "$np" -eq "1" ]; then
    run_cmd="./${exe}"
else
    run_cmd="mpiexec -np ${np} ./${exe}"
fi

# run testcase
cd $run_dir
echo "$ ${run_cmd}"
${run_cmd}

# collect output, and fail silently if files not outputted at this point
mkdir -p ${output_dir}
mv *.CHANOBS_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.CHRTOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.CHRTOUT_GRID1 ${output_dir}/ 2>/dev/null
mv *.GWOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.LAKEOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.LDASOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.LSMOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv *.RTOUT_DOMAIN1 ${output_dir}/ 2>/dev/null
mv HYDRO_RST.*_DOMAIN1 ${output_dir}/ 2>/dev/null
mv RESTART.*_DOMAIN1 ${output_dir}/ 2>/dev/null
mv diag_hydro.* ${output_dir}/ 2>/dev/null
