#!/bin/bash

## This script takes one optional argument:
## a file which sets the environment variables
## to use in the compile. The template for
## this file is trunk/NDHMS/setEnvar.sh. Please
## copy that file to trunk/NDHMS, make copies
## for your favorite compile configurations, and
## pass the appropriate file name to this script
## as desired.
env_file=$1

if [[ ! -z $env_file ]]; then

    ## unset these in the env so we are not mixing
    ## and matching env vars and the sourced file.
    unset WRF_HYDRO
    unset HYDRO_D
    unset SPATIAL_SOIL
    unset WRF_HYDRO_RAPID
    unset NCEP_WCOSS
    unset WRF_HYDRO_NUDGING

    echo "configure: Sourcing $env_file for the compile options."
    source $env_file
else
    echo "configure: Using the compile options in the calling environment."
fi

if [[ "$WRF_HYDRO" -ne 1 ]]; then
    echo
    echo "The WRF_HYDRO compile option is required to be 1 for compile_offline_Noah.sh"
    exit 1
fi

rm -f LandModel LandModel_cpl
cp arc/Makefile.Noah Makefile
ln -sf CPL/Noah_cpl LandModel_cpl
ln -sf Land_models/Noah LandModel
cat macros LandModel/user_build_options.bak  > LandModel/user_build_options
make clean ; rm -f Run/wrf_hydro_Noah.exe ; rm -f Run/*TBL ; rm -f Run/*namelist*

cat macros LandModel/user_build_options.bak > LandModel/user_build_options

make


if [[ $? -eq 0 ]]; then
    echo
    echo '*****************************************************************'
    echo "Make was successful"
else 
    echo
    echo '*****************************************************************'
    echo "Make NOT successful"
    exit 1
fi

cd Run
mv wrf_hydro.exe wrf_hydro_Noah.exe ; ln -sf wrf_hydro_Noah.exe wrf_hydro.exe
cp ../Land_models/Noah/Run/GENPARM.TBL .
cp ../Land_models/Noah/Run/SOILPARM.TBL .
cp ../Land_models/Noah/Run/VEGPARM.TBL .
cp ../template/Noah/namelist.hrldas .
cp ../template/HYDRO/HYDRO.TBL .
cp ../template/HYDRO/CHANPARM.TBL .
cp ../template/HYDRO/hydro.namelist .

echo
echo '*****************************************************************'
echo "The environment variables used in the compile:"
grepStr="(WRF_HYDRO)|(HYDRO_D)|(SPATIAL_SOIL)|(WRF_HYDRO_RAPID)|(HYDRO_REALTIME)|(NCEP_WCOSS)|(WRF_HYDRO_NUDGING)|(NETCDF)"
printenv | egrep -w "${grepStr}" | sort

exit 0
